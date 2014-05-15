-module(testapp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, { {simple_one_for_one, 5, 10},
	   [?CHILD(bogus, bogus, worker, [])]} }.
