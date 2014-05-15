-module(test_hooks).

-export([presync/0, postsync/0]).

presync() ->
    application:stop(testapp).

postsync() ->
    application:start(testapp).

