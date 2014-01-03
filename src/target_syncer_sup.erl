%%%-------------------------------------------------------------------
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2014, Frank Hunleth
%%% @doc
%%%
%%% @end
%%% Created :  1 Jan 2014 by Frank Hunleth <fhunleth@troodon-software.com>
%%%-------------------------------------------------------------------
-module(target_syncer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_child(atom()) ->
			 {ok, undefined | pid()} | {ok, undefined | pid(), _} |
			 {error, _}.
start_child(Node) ->
    % Make sure that the code is up to date on the remote
    {Mod, Bin, File} = code:get_object_code(target_syncer),
    {module, Mod} = rpc:call(Node, code, load_binary, [Mod, File, Bin], 5000),

    % Start the supervisor
    AChild = {{target_syncer, Node},
	      {target_syncer, start_link, [Node]},
	      transient, 2000, worker, [target_syncer]},
    supervisor:start_child(?SERVER, AChild).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
