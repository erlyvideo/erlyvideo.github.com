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
