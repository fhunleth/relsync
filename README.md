# relsync

[![Build Status](https://travis-ci.org/fhunleth/relsync.png)](https://travis-ci.org/fhunleth/relsync)

Relsync synchronizes the contents of a local Erlang/OTP release with a
remote node. It is similar to rsync in that it attempts to only copy
files that have been added or changed, but also reloads changed .beam
files and supports Erlang pre and post synchronization scripts.

The intended use case is for copying cross-compiled Erlang releases to
their target hardware. It probably can be used in other scenarios, but
other tools like [sync](https://github.com/rustyio/sync) may be easier
to use. The advantage to using `relsync` is that it copies ports over
as well and let's you add scripts to perform custom reloading or run
other code needed to update the remote filesystem. In fact, it
synchronizes almost everything that's safe to synchronize. Shared
libraries (.so) are one of the main exceptions. Ports can be syncronized
so long as they are stopped in the presync part of the script.

Here's an example usage to synchronize the release in the `_rel` directory
with a remote node on a Beaglebone. Note that the remote node has to already
have its name and cookie configured.

    relsync --destnode testnode@beaglebone --hooks relsync_hooks.erl --cookie beagle --sname relsync

## Building

Building is similar to other Erlang projects. Make sure `rebar` is in
your path.

    git clone https://github.com/fhunleth/relsync.git
	cd relsync
	make

To install, copy the `relsync` output to anywhere convenient in your `$PATH`.

## Usage

    Usage: relsync [-d [<destnode>]] [-p [<destpath>]] [-l [<localpath>]]
                   [-h <hooks>] [-c [<cookie>]] [-s <sname>] [-n <name>]

      -d, --destnode   Destination node [default: node@other]
      -p, --destpath   Path to release on the destination [default:
                       /srv/erlang]
      -l, --localpath  Path to local release [default: ./_rel]
      -h, --hooks      Erlang module containing hooks to run on the destination
      -c, --cookie     Erlang magic cookie to use [default: cookie]
      -s, --sname      Short name for the local node
      -n, --name       Long name for the local node

## Hooks

Relsync will look for the module specified by `--hooks` parameter and if it
isn't found, it will look for a `.erl` file of the same name and use it. The
code in the module is run on the destination node.

The following example hooks kill one of the ports so that it can be updated. 
It also remounts the filesystem so that it is writable and can receive the updates.

```erlang
-module(relsync_hooks).

-export([presync/0, postsync/0]).

presync() ->
    io:format("Got a presync~n"),

    % Stop the application so that any active ports are
    % exited. This is needed or relsync won't be able to update
    % the binary.
    application:stop(myapp).

postsync() ->
    io:format("Got a postsync~n"),

    % Start the app back up
    application:start(myapp).
```
