rtorrent-cli
============

A simple rtorrent command line interface.

Add `aliases.zsh` to your `.zshrc` to use `exec` and `cd`.

You need to set `scgi_port = localhost:5000` in your `.rtorrent.rc`
so that `rtorrent-rpc` can connect to rtorrent.
Other ports and hosts can be used by setting environment variables
`RT_HOST` and `RT_PORT`, but `exec` and `cd` won't work nicely unless
rtorrent is running on localhost.
