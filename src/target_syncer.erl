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
-export([start_link/0, start_link/1,
	 get_file_hashes/2,
	 get_local_file_hashes/1,
	 copy_file/3,
	 rm_file/2,
	 apply/3]).

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

-spec get_local_file_hashes(string()) -> [{string(), binary()}].
get_local_file_hashes(Path) ->
    NormalizedPath = normalize_path(Path),
    PrefixLength = length(NormalizedPath),
    filelib:fold_files(NormalizedPath, ".*", true,
		       fun(Y, Acc) -> [{lists:nthtail(PrefixLength, Y), hashfile(Y)} | Acc] end,
		       []).

-spec copy_file(atom(), string(), binary()) -> ok | {error, _}.
copy_file(Node, Path, Contents) ->
    gen_server:call({?SERVER, Node}, {copy_file, Path, Contents}).

-spec rm_file(atom(), string()) -> ok | {error, _}.
rm_file(Node, Path) ->
    gen_server:call({?SERVER, Node}, {rm_file, Path}).

-spec apply(atom(), function(), [term()]) -> {ok, _}.
apply(Node, Fun, Args) ->
    gen_server:call({?SERVER, Node}, {apply, Fun, Args}).

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
    Result = rpc:call(Node, gen_server, start, [{local, ?SERVER}, ?MODULE, [], []]),
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
    Reply = get_local_file_hashes(Path),
    {reply, Reply, State};
handle_call({copy_file, Path, Contents}, _From, State) ->
    ok = filelib:ensure_dir(Path),
    Reply = file:write_file(Path, Contents),
    {reply, Reply, State};
handle_call({rm_file, Path}, _From, State) ->
    Reply = file:delete(Path),
    {reply, Reply, State};
handle_call({apply, Fun, Args}, _From, State) ->
    Reply = apply(Fun, Args),
    {reply, {ok, Reply}, State}.

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
-spec hashfile(string()) -> binary().
hashfile(Filename) ->
    {ok, Data}=file:read_file(Filename),
    crypto:hash(sha, Data).

-spec normalize_path(string()) -> string().
normalize_path(Path) ->
    case lists:last(Path) of
	$/ -> Path;
	_ -> Path ++ "/"
    end.
