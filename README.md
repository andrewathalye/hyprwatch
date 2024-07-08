Hyprwatch
=========
A tool to watch over Hyprland and communicate via other scripts over DBus.

This makes up the core of my personal two-dimensional Hyprland workspace setup.
Run `./hyprwatch` and then `./hyprlisten` to get a decent idea of how it all works.

`hyprlisten` reads signals from `hyprwatch` and prints out information in JSON format.
I personally use this for EWW integration.

DBus Interface
--------------

See `src/introspect.xml` for the DBus interface documentation or
call `org.freedesktop.DBus.Introspectable.Introspect` on `/tk/zenithseeker/hyprwatch`

`direction` can be one of `("l", "r", "u", "d")`.

Development
-----------
`nix develop ./flake.nix` followed by `gprbuild -j0`

Use `DEBUG=1 ./hyprwatch` if you need to debug the protocol support or DBus messages.

Installing
----------
Use the Nix flake. The package is called `hyprwatch`
   


