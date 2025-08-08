rtorrent-cli
============

A simple rtorrent command line interface.

Add `aliases.zsh` to your `.zshrc` to use `exec` and `cd`.

You need to set 
`network.scgi.open_local = (cat,(session.path),rpc.socket)` or 
`network.scgi.open_port = "127.0.0.1:5000"` 
in your `.rtorrent.rc`
so that `rtorrent-rpc` can connect to rtorrent.

Then you can set the environment variable `RT_URL` to be 
`unix://~/.rtorrent/rpc.socket` or `tcp://localhost:5000`,
but `exec` and `cd` won't work nicely unless
rtorrent is running on localhost.
