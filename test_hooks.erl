-module(test_hooks).

-export([presync/0, postsync/0]).

presync() ->
    io:format("Got a presync~n").

postsync() ->
    io:format("Got a postsync~n").

