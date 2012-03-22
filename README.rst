WebSockets for Tsung
====================

A plugin for Tsung (http://tsung.erlang-projects.org/) for
load-testing WebSockets servers.

Draws inspiration from the tsung_websocket project
(https://github.com/onlychoice/tsung_websocket), various other Erlang
WebSockets implementations and the Tsung plugin tutorial
(http://www.process-one.net/en/wiki/Writing_a_Tsung_plugin/).

Why?
====

Why not just build on tsung_websockets? The reasons are:

 * for some reason it didn't work out of the box
 * it supports WebSockets as a transport protocol so you can run others like
   XMPP on top of it, and I didn't need that
 * it includes a lot of unrelated code
 * I wanted to try Erlang

Usage
=====

Get the source for Tsung. On Debian/Ubuntu use::

  apt-get source tsung

Copy the DTD and provided Erlang files from tsung_ws to the Tsung source tree::

  cp tsung_ws/tsung-1.0.dtd tsung-x.x.x/
  cp tsung_ws/include/ts_websocket.hrl tsung-x.x.x/include/
  cp tsung_ws/src/tsung_controller/ts_config_websocket.erl tsung-x.x.x/src/tsung_controller/
  cp tsung_ws/src/tsung/ts_websocket.erl tsung-x.x.x/src/tsung/

Compile Tsung::

  cd tsung-x.x.x/
  ./configure --prefix=$HOME/local/tsung
  make install

Edit the example websocket.xml configuration file and test your WebSockets server!
