---
layout: post
title: "Overload protection 2"
date: 2013-01-02 23:59
comments: true
categories: 
---

[Previous chapter](/2013/01/01/overload-protection-1/)

We have discussed in previous chapter that your system may experience unbalanced load in different components that can lead to
extremely large message queues in some central processes that are accessed via asynchronous `gen_server:cast` API.

Switching to `gen_server:call` was advised as a generic approach to move overload points to borders of system. Now lets discuss
how to protect borders of your system from requests that do not fit into capacity of your system.


<!-- more -->


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

By switching from `gen_server:cast` to `gen_server:call` you will suffer from deadlocks. Read [chapter about deadlocks](/2013/01/03/deadlocks-1/) please.





