---
layout: post
title: "Overload protection 1"
date: 2013-01-01 00:29
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

In short: `gen_server:cast` just sends asynchronous non-blocking message from one process to other and `gen_server:call` waits for reply from 
server process to our request. While server process is processing client's request, client process is absolutely unresponsible and cannot do
any job.

If you relapse into sin of synthetic microbenchmarks you can easily find out that `gen_server:cast` is much much faster than `gen_server:call`.
You may even write a complicated benchmark, launch it for several days and say me: look, there are no memory leaks and code with `gen_server:cast`
consumes only 50% CPU of `gen_server:call` benchmark. You are tricked by wrong target of your benchmarks.


You should remember a very important thing: no matter how much CPU is consumed by your system, matters only how many users can be served and how stable is their service.

What does this fact means to us? It means that if client process is sending some data to server process (for example, user is publishing video frames) to server and you are using `gen_server:cast` to deliver frames you may suffer from slowness of server process.

For example server process is writing data on disk. Everything is working fine on your new shiny Macbook Air with SSD but then you install your system on Amazon EC2 with rusty HDD and you find out that sometimes disk access can be VERY slow. Server process cannot receive frames from message queue because it is writing data on disk. Client process doesn't know anything about it and will send frames unless memory is over.

It is good when you find out such a problem with video stream, because memory is exhausted in minutes. Much worse is when memory can leak many days.

How can you protect server process from such overload? Switch to `gen_server:call` even if you don't need any response on your message.
When `gen_server:call(Server, Command)` returns you can be absolutely sure, that server process have consumed your message and it is no more in
incoming message queue.

So we have understood how to deal with slow-consumer-fast-producer problem in case of one client processe and one server process. We should move point of data accumulation to the beginning of data stream. Don't let data to accumulate inside your system, use backpressure mechanism to
slow down producer. `gen_server:call` is a good example of such backpressure mechanism. One client process will be limited by speed of server process.


[Next chapter](/2013/01/02/overload-protection-2/)














