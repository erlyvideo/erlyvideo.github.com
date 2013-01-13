---
layout: post
title: "Maintain long requests"
date: 2013-01-05 00:38
comments: true
categories: 
---

Previous chapters in this series were about [fighting with overloads inside system](/2013/01/01/overload-protection-1/), 
[dropping requests that will overload system](/2013/01/02/overload-protection-2/),
[dealing with consequences of our measures](/2013/01/03/deadlocks 1/),
[making system less tied and faster](/2013/01/04/unobtrusive-read/).

This last chapter will be about maintaining long user requests.

If out load control system allows user to make a request that will consume large amount of resources, our system
should understand that user doesn't want to wait anymore.

Here is an example: erlyvideo can export video archive as an mp4 file via HTTP request. It requires reading video twice: to build
frame map and send frames. Two hours of video can take about 6 gigabytes, so one user request will lead to
12 gigabytes transfer through our system. Such export task may take many seconds and user's software may
decide that request has failed due to timeout and restart it.

After 10-15 restarts your system will be full of very consuming requests that are not required anymore. Lets discuss how to
drop such requests.

<!-- more -->


Maintaining long requests
-------------------------

First fact you should remember is that erlang by default sets send_timeout on socket to infinity.
It means that you will wait for 3 hours to send data to socket. 3 hours is ok to repair wire after nuclear attack,
but it rarely happens nowadays, so I advise you to change socket send_timeout so several seconds.

Next you should spawn task into a separate process and listen for both: process and socket, which of them is first to die.

It looks so:

```erlang
  Socket = cowboy_req:get(socket, Req),
  Transport = cowboy_req:get(transport, Req),
  Transport:send(Socket, ["HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: video/mp4\r\n", 
    io_lib:format("Content-Disposition: attachment; filename=~s.mp4\r\n", [SaveName]), LenHeader, "\r\n"])

  Self = self(),
  Pid = proc_lib:spawn(fun() ->
    put(name, {mp4_export_worker,Root,Name,From,Duration}),
    mp4_writer:dump_media([{header,Header},{writer,Writer},{reader,Reader},{start_pos,StartPos}]),
    Self ! done
  end),
  erlang:monitor(process, Pid),
  inet:setopts(Socket, [{active,once}]),
  receive
    done -> ok;
    {tcp_closed, Socket} -> ok;
    {'DOWN', _, _, Pid, Reason} -> ?D(Reason);
    Else -> ?D(Else)
  end,
  erlang:exit(Pid,kill),
  {done, cowboy_req:set([{connection,close},{resp_state,done}], Req)}.
```

Again:

1. prepare HTTP response before task starts
2. run task in separate process
3. move http socket to {active,once} mode
4. wait either for task finish, either for socket close
5. close everything after task is finished or cancelled

Mention that this snippet behaves badly with [cowboy](https://github.com/extend/cowboy) internals, but it is ok,
because there is no sense in making keepalive requests to a resource that is generated for many, many seconds.


Conclusions
-----------

I've tried to tell some very small piece of information in this five chapters about my experience in making erlyvideo a fault-tolerable and a rock-solid streaming server.

You should understand that erlang itself is an excelent platform, but you should do something to make your system on erlang a rock-solid.

Sometimes it is hard to plan capacity of your system, so it should behave properly under stress load: accept only those requests that can be handled.
I hope that this series of posts will help you a bit in this hard and interesting task.








