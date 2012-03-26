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

Get the source and build dependencies for Tsung. On Debian use::

  apt-get source tsung
  sudo apt-get build-dep tsung

For Ubuntu or other distros you'll have to download the source from upstream::

  sudo apt-get install erlang-nox python-matplotlib gnuplot libtemplate-perl openssh-client openssh-server
  wget http://tsung.erlang-projects.org/dist/tsung-1.4.2.tar.gz
  tar xzf tsung-1.4.2.tar.gz

Copy the DTD and provided Erlang files from tsung_ws to the Tsung source tree::

  cp tsung_ws/tsung-1.0.dtd tsung-x.x.x/
  cp tsung_ws/include/ts_websocket.hrl tsung-x.x.x/include/
  cp tsung_ws/src/tsung_controller/ts_config_websocket.erl tsung-x.x.x/src/tsung_controller/
  cp tsung_ws/src/tsung/ts_websocket.erl tsung-x.x.x/src/tsung/

Compile Tsung::

  cd tsung-x.x.x/
  ./configure --prefix=$HOME/local/tsung
  make install

Tsung uses SSH to launch monitoring and controller processes, make sure your
user can connect to localhost without needing a password (this assumes you have
generated an SSH key before)::

  cat $HOME/.ssh/id_rsa.pub >> $HOME/.ssh/authorized_keys

Start the example Python WebSockets server (you will need Twisted and a fork of
txWebSocket) and run Tsung using the provided configuration file::

  sudo apt-get install python-twisted
  sudo pip install -e git://github.com/wulczer/txWebSocket.git#egg=txWebSocket
  cd ../tsung_ws/
  python math_server.py
  $HOME/local/tsung/bin/tsung -f websocket.xml -m /tmp/tsung.log start

After the run is done, you can generate an HTML report::

  mkdir report
  cd report
  $HOME/local/tsung/lib/tsung/bin/tsung_stats.pl --stats /tmp/tsung.log

Loose ends
==========

More error checking, handling continuation and binary frames, better logging.
