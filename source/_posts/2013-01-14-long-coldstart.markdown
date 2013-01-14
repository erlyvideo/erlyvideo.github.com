---
layout: post
title: "Long coldstart"
date: 2013-01-14 17:46
comments: true
categories: 
---

Erlyvideo has an ability to write video stream on disk. Video is written in small video chunks, convenient to serve via nginx or varnish.
User can request long video from moment T1 to moment T2 and erlyvideo need to take a look at list of chunks to calculate reply.

So was decided to keep list of chunks in memory and maintain it while new video is written and old video is deleted. Number of chunks may
be impressive: up to several hundreds of thousands.

So we get a problem of cold start: erlyvideo has to load list of chunks from disk. Usually it is rather fast, because chunks are
laying in nested directories and large amount is not a big problem for modern hardware, but even on fast server it can take several
seconds to scan all chunks. How to build system that will not freeze on start?

<!-- more -->

Freezing on start is not just a service degradation, it may lead to restart loop: neighbour is requesting something from archive process,
but archive process is busy and neighbour dies with timeout. Supervisor kills them all and restarts. It can become an endless loop,
that will never give up because you have setup monit that will restart everything.

Let's discuss some tips to make workaround for this.

Make coldstart fast
-------------------

It is a good advice just like "write fast code". For example, it is possible to add chunk index and it is a very interesting task, because
dets is a bad storage for such information and other database engines require to manage some NIF or drivers across all OS where erlyvideo
should be installed. But you may experience other situation when you cannot just put index on disk, but need to download some
data from the network or collect required information via some other ways.

I can give you advice to plan your real requirements. Don't build system for 500 millions of users if you will have only 500 of them.
For example I have rewritten erlyvideo archive scan code from os:wildcard to direct prim_file usage and scan time lowered from 60 seconds to 5.

5 seconds downtime of archive on startup satisfy me. But things always go wrong and something breaks. 5 seconds can raise back to 120 seconds
and freeze whole system. Let's think how to deal with it.



Delayed startup
---------------

First I advise to move such cold load functions to separate process. Separate process is easy to monitor and kill. You can always
introspect it and look how many resources does it consume. Also remember that building of initial data can involve generation of large
amount of temporary objects and it may be easier for system to drop whole process heap rather than garbage collect some long-living process.


Ets may be a good way to communicate between service process (that requires initial data) and loader. You can even add some incremental loading.
For example archive in erlyvideo is loaded to shared ets table from the newest chunks to ancient. So early users may use this service before all initialization is done. If your service is a cache server, than you may start serving already indexed data from cache.

If initial data cannot be split into chunks, you can use another tip: delayed reply. I really don't advice you to make such a process
that is waiting in blocking call to loader. Launch loader, wait for its status via handle_info and accept all incoming requests via
handle_call. If request is done before all data is loaded, than use following code:

```erlang
handle_call({request, Request}, From, #state{delayed = Delayed} = State) ->
  {noreply, State#state{delayed = [{From,Request}|Delayed]}};
....

handle_info({data_loaded, Data}, #state{delayed = Delayed} = State) ->
  [gen_server:reply(From, response(Data, Request)) || {From, Request} <- Delayed],
  {noreply, State#state{delayed = [], data = Data}},
```

Doing things this way you should remember that default gen_server:call timeout is only 5 seconds.

Conclusion
----------

Your system may require some initial data loading. Please, make your system more responsive while doing it. Extract loading to separate process,
try to serve clients somehow before all initialization is done and delay responses until required data is loaded.




















