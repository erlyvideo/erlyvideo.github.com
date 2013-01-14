---
layout: post
title: "Unobtrusive read"
date: 2013-01-04 00:26
comments: true
categories: 
---

Post about [deadlocks](/2013/01/03/deadlocks-1/) was finished with promise to tell about other ways to remove deadlocks from system,
while maintaining it responsive and keeping control of system load.

This chapter will be about unobtrusive reads: how to read data from process without affecting its performance.


<!-- more -->


Unobtrusive reads from ets
--------------------------

Very soon you understand that sometimes it is not a very good idea to ask central process for some read-only information via `gen_server:call` API.
For example you are writing a multiplayer browser game and want to read a map of the world. What for to go to server process and
ask it to copy all data? This process is very, very busy with other important tasks that require modification of data from single point.

Erlyvideo has such a place in code when clients come and ask video segments. They are 1 megabyte binaries and are changed only once in 5 seconds.

So you come to an idea that some data should be kept in public, named ets table. If client process needs some data like a video segment,
or map of a game world or some other data that can be easily stored in a central process, than put it there.

Statistics is a very good example of such data. It may be rather expensive to calculate statistics (for example recalculate sliding window for
average response time may take significant resources). Don't allow system administrator to crash your system by pressing F5 in browser!
Put all generated data into ETS and serve it to clients in microseconds without any affecting of other system.

[gproc](https://github.com/uwiger/gproc) does exactly this job for you.

You may experience new problems with this approach. Using `gen_server:call` allows client process to wait until data is available and reading from
ets table doesn't block your process. Gproc has ability to lock caller until key is available, but sometimes it doesn't work.

Erlyvideo can serve stream as MPEG-TS. Client will teardown connection if no data is sent and this is why I had to use other approach:
read in loop key from ets packet and if no key available, send null keepalive packet if no data available and sleep for one second.

You should choose the way that better fits your requirements, but please, choose easiest one.


Reading from process dictionary
-------------------------------

Sometimes keeping separate ets table may look like not a best idea. In this case I advise you to use process dictionary. Server process may write there its name, its status and some other short information that maybe useful for introspecting and into logging.

For example erlyvideo stream is a very isolated process. It makes calls to a very small amount of processes, but it makes them and can block there.
Especially it can block while writing archive on disk. This is why it happened to be a great idea to write:

```erlang status debugging
handle_call(#video_frame{} = Frame, _From, Stream) ->
  put(status, write_frame_on_disk),
  write_frame_on_disk(Frame, Stream),
  put(status, send_frame_to_repeater),
  send_frame_to_repeater(Frame, Stream),
  {reply, ok, Stream}.

send_frame(Stream, Frame) ->
  try gen_server:call(Stream, Frame)
  catch
    error:{timeout,_} ->
      {dictionary, Dict} = process_info(Stream, dictionary),
      Name = proplists:get_value(name, Dict),
      Status = proplists:get_value(status, Dict),
      erlang:raise(error, {send_frame_timeout,Name,Status}, erlang:get_stacktrace())
  end.
```

This tricky code will give you more informative stacktraces in logs that will tell you that source cannot publish frames to stream because stream
is blocked in status `write_frame_on_disk` and you should do something with it.



Main idea of this capter is: block if you need other process to modify required data but don't touch remote process at all if you need "just ask for existing data". Mention that this method behaves badly if data does not exists. It works for already existing data.



Last chapter of this series will be devoted to [maintaining long user requests](/2013/01/05/maintain-long-requests/)

