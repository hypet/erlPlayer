erlPlayer
=========

ErlPlayer is media player server based on MPlayer and powered by Erlang.
ErlPlayer uses Ranch to manage TCP connections.

How to use
----------

* Install Erlang. You may get it here: http://www.erlang.org/download.html
* Get MPlayer here: http://www.mplayerhq.hu/design7/dload.html
* Git clone this project.
* Edit sys.config: change the path to your music folder and the path to mplayer binary (if it is on your PATH then leave only binary name, e.g. <pre>{mplayer_path, "mplayer.exe"}</pre>).
* Run: 
<pre>
make all
</pre>

Then you may connect with telnet to default TCP port 15588 and send command:
<pre>
play:1
</pre>
and hit Enter. All available commands you may see in erlplayer_protocol.erl.

Android application to rule ErlPlayer is coming soon!

Known problems
--------------

When you try to play a file with unicode symbols in it's name it will crash. I'm going to use something like iconv in the nearest future.

