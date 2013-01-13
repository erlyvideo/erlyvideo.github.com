---
layout: post
title: "Overload protection"
date: 2013-01-13 00:29
comments: true
categories: 
---

How to protect Erlang system from overload
------------------------------------------

Lets discuss a bit such a complicated and deep question like designing your system to be able to
survive load spikes and performance failures.

This post will describe expirience of building video streaming server erlyvideo so you should assume following facts:

1. erlyvideo does a very small amount of calculations. Major part of work can be described like "take data, repack and put it into next sink"
2. erlyvideo has large amount of very different IO tasks: read data from sockets, from files, write different replies to sockets and files.

So you can imagine streaming server as a system that serves like a rather fast pipe with very unreliable ends. Thousands of very unreliable ends.

Lets discuss how to protect this pipe from consuming more data via inputs that can be transmitted via outputs.

<!-- more -->

Major failures
--------------

There are two most often reasons for erlyvideo to fail:

1. some error happened that involved giant terms (more than 10 MB of data) and error_logger is trying to print it. It consumes several gigabytes of RAM and OOM killer stops erlang VM
2. some process is receiving messages slower than they arrive and it consumes all available RAM.

It is very easy to deal with first problem: just install [lager](https://github.com/basho/lager) You should understand that `gen_server:format_status` callback doesn't work. error_logger tries to print last message, gen_server state and error reason. Each of these three items can be very big, so lager is the only way to protect your system from such kind of failure.

Rest of this post will be about second problem.


gen_server:call
---------------

Very fast and easy answer to problem of big message box is: don't use gen_server:cast, use gen_server:call. But this answer is wrong as any too fast and too easy answer to a complicated problem.

When should we use `gen_server:call` according to documentation? Looks like if we want some reply to our message. We send command to perform some calculation to other process and receive calculated data.

In short: `gen_server:cast` just sends asynchronous non-blocking message from one process to other and `gen_server:cast` waits for reply from 
server process to our request. While server process is processing client's request, client process is absolutely unresponsible and cannot do
any job.

If you relapse into sin of synthetic microbenchmarks you can easily find out that `gen_server:cast` is much much faster than `gen_server:call`.
You may even write a complicated benchmark, launch it for several days and say me: look, there are no memory leaks and code with `gen_server:cast`
consumes only 50% CPU of `gen_server:call` benchmark. You are tricked by wrong target of your benchmarks.


You should remember a very important thing: no matter how much CPU is consumed by your system, matters only how many users can be served and how stable is their service.

What does this fact means to us? It means that if client process is sending some data to server process (for example, user is publishing video frames) to server and you are using `gen_server:cast` to deliver frames you may suffer from slowness of server process.

For example server process is writing data on disk. Everything is working fine on your new shiny Macbook Air with SSD but then you install your system on Amazon EC2 with rusty HDD and you find out that sometimes disk access can be VERY slow. Server process cannot receive frames from message queue because it is writing data on disk. Client process doesn't know anything about it and will send frames unless memory is over.

It is good when you find out such a problem with video stream, because memory is exhausted in minutes. Much worser is when memory can leak many days.

How can you protect server process from such overload? Switch to `gen_server:call` even if you don't need any response on your message.
When `gen_server:call(Server, Command)` returns you can be absolutely sure, that server process have consumed your message and it is no more in
incoming message queue.

So we have understood how to deal with slow-consumer-fast-producer problem in case of one client processe and one server process. We should move point of data accumulation to the beginning of data stream. Don't let data to accumulate inside your system, use backpush mechanism to
slow down producer. `gen_server:call` is a good example of such backpush mechanism. One client process will be limited by speed of server process.



Many clients, one server
------------------------

What will happen if there is not one client, but thousand of them. Or ten thousands? If each of them sends `gen_server:call` response to server,
there will appear 10 000 of incoming messages in server message queue.

First 300 of them will be consumed and rest of clients will receive exit(timeout) from gen_server library. If this are smart clients,
they will resend request and will duplicate their requests in server's message queue. Now you may be sure: your system is reliably overloaded
almost with any chance to survive this storm.

This is a big caveat of `gen_server:call` API: client cannot tell server that it refuses from request. This problem appears because we misused
`gen_server:call`, it is designed for fast response. Better say so: erlang processes should keep small message queue to be responsive.

There is a simple tip for you to protect code from such problem: introspect server message queue length before sending gen_server:call.
This method is non-deterministic and should be used only if you experience such problem. However it is easy to understand that you have problem
with overloaded server: many timeouts in client processes.

If `process_info(Server, message_queue_len)` is bigger than some empiric constant than refuse to make request and respond to client with 503 response telling external client that your system have not accepted request at all.

Mention this important fact: we don't accept user request at all, not leave it unmaintained inside our system. 504 Gateway timeout code usually means
that request is accepted but not handled in affordable time.


Deadlocks
---------

By switching from `gen_server:cast` to `gen_server:call` you will face with deadlocks. Process 1 calls process 2, process 2 starts process 3, process
3 calls process 2 and in 5 seconds all of them die with error(timeout). 

Erlyvideo2 had a good example of such deadlock-prone component. It is stream registrator. Video stream should be atomically started on demand if
not started yet. There is playlist type of streams that requires other stream to be started.

Client asks media_registrator to start a stream via `gen_server:call`. Media registrator looks in process list and starts new playlist stream.
Playlist stream loads entries and asks media_registrator to start other stream, but media_registrator is busy launching this playlist stream.

This is an example of silly deadlock which can be easily fixed but you may experience more complicated deadlocks or just simple locks.

There are many different ways to fight them. First way is just to fix removing simultaneous calls.

For example lets take a look at [ranch](https://github.com/extend/ranch). It is a pool of network connection acceptors that starts your process
to handle network client. First your callback must return Pid of your process, than your process should receive message that socket
ownership is transferred to a new process.

Let's take a look at a wrong code:

```erlang wrong ranch callback
-module(my_worker).
-export([start_link/4, init/1]).

start_link(ListenerPid, Socket, Transport, Args) ->
  gen_server:start_link(?MODULE, [ListenerPid, Socket], []). % Here start_link blocks until init/1 is returned

init([ListenerPid, Socket]) ->
  ranch:accept_ack(ListenerPid), % Here init/1 is blocked until ranch transfer socket to new process
  {ok, Socket}.
```

This code has an easily detected deadlock because ranch doesn't get new Pid until init/1 is done and it is waiting for ranch
to transfer socket and ranch cannot do it because it waits for pid.

Simple way to deal with this kind of deadlocks is to change initialization of process:

```erlang better ranch callback
-module(my_worker).
-export([start_link/4, init/1]).

start_link(ListenerPid, Socket, Transport, Args) ->
  proc_lib:start_link(?MODULE, [[ListenerPid, Socket]]).

init([ListenerPid, Socket]) ->
  proc_lib:init_ack({ok, self()}), % first we unblock proc_lib:start_link
  ranch:accept_ack(ListenerPid), % and now we wait for ranch
  gen_server:enter_loop(?MODULE, Socket, []).
```


This example is more about dealing with deadlocks, not protecting system from overload, but these topics are related and we'll return to it 
later.


Unobtrusive reads
-----------------


















