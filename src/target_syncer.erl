%%%-------------------------------------------------------------------
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2014, Frank Hunleth
%%% @doc
%%%
%%% @end
%%% Created :  1 Jan 2014 by Frank Hunleth <fhunleth@troodon-software.com>
%%%-------------------------------------------------------------------
-module(target_syncer).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, get_file_hashes/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_file_hashes(atom(), string()) -> [{string(), binary()}].
get_file_hashes(Node, Path) ->
    gen_server:call({?SERVER, Node}, {get_file_hashes, Path}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server locally (called by the supervisor)
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Starts the server on the specified remote node.
start_link(Node) ->
    io:format("Going to start a server on ~p~n", [Node]),
    Result = rpc:call(Node, gen_server, start, [{local, ?SERVER}, ?MODULE, [], []]),
    io:format("Back with ~p~n", [Result]),
    case Result of
	{ok, Pid} ->
	    link(Pid)
    end,
    Result.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("Started target_syncer on node ~p~n", [node()]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_file_hashes, Path}, _From, State) ->
    FileOrdset = filelib:fold_files(Path, ".*", true,
				    fun(Y, Acc) -> ordsets:add_element(hashfile(Y), Acc) end,
				    ordsets:new()),
    {reply, FileOrdset, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("target_syncer:terminate~n"),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec hashfile(string()) -> {string(), binary()}.
hashfile(Filename) ->
    {ok, Data}=file:read_file(Filename),
    {Filename, crypto:hash(sha, Data)}.
