%%%-------------------------------------------------------------------
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2014, Frank Hunleth
%%% @doc
%%%
%%% @end
%%% Created :  1 Jan 2014 by Frank Hunleth <fhunleth@troodon-software.com>
%%%-------------------------------------------------------------------
-module(target_syncer).
-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
	 get_file_listing/2,
	 set_hooks/2,
	 get_local_file_listing/1,
	 copy_file/4,
	 rm_file/2,
	 notify_presync/1,
	 notify_postsync/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  % hooks holds the module name that provides an alternative
	  % implementation to the default synchronization
	  hooks}).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_file_listing(atom(), string()) -> [{string(), {integer(),binary()}}].
get_file_listing(Node, Path) ->
    gen_server:call({?SERVER, Node}, {get_file_listing, Path}).

-spec get_local_file_listing(string()) -> [{string(), {integer(), binary()}}].
get_local_file_listing(Path) ->
    NormalizedPath = normalize_path(Path),
    PrefixLength = length(NormalizedPath),
    filelib:fold_files(NormalizedPath, ".*", true,
		       fun(Y, Acc) -> [{lists:nthtail(PrefixLength, Y), file_info(Y)} | Acc] end,
		       []).

% Use the specified Module to customize the behavior of the
% synchronization process. The object code for the Module
% is sent to the remote Node as well.
-spec set_hooks(atom(), atom()) -> ok.
set_hooks(Node, undefined) ->
    gen_server:call({?SERVER, Node}, clear_hooks);
set_hooks(Node, ModuleName) ->
    {Module, Bin, File} = maybe_compile(ModuleName),
    gen_server:call({?SERVER, Node}, {set_hooks, Module, Bin, File}).

maybe_compile(ModuleName) ->
    Module = list_to_atom(ModuleName),
    case code:get_object_code(Module) of
	{Module, Bin, File} ->
	    {Module, Bin, File};
	_ ->
	    {ok, CompiledModule, Bin} = compile:file(ModuleName, [binary]),
	    {CompiledModule, Bin, ModuleName}
    end.

-spec copy_file(atom(), string(), integer(), binary()) -> ok | {error, _}.
copy_file(Node, Path, Mode, Contents) ->
    gen_server:call({?SERVER, Node}, {copy_file, Path, Mode, Contents}).

-spec rm_file(atom(), string()) -> ok | {error, _}.
rm_file(Node, Path) ->
    gen_server:call({?SERVER, Node}, {rm_file, Path}).

% Called to let the remote node know that a synchronization
% run is coming.
-spec notify_presync(atom()) -> ok.
notify_presync(Node) ->
    gen_server:call({?SERVER, Node}, notify_presync).

% Called to let the remote node know that a synchronization
% run has finished.
-spec notify_postsync(atom()) -> ok.
notify_postsync(Node) ->
    gen_server:call({?SERVER, Node}, notify_postsync).

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
handle_call({get_file_listing, Path}, _From, State) ->
    Reply = get_local_file_listing(Path),
    {reply, Reply, State};
handle_call(clear_hooks, _From, State) ->
    NewState = State#state{hooks=undefined},
    {reply, ok, NewState};
handle_call({set_hooks, Module, Bin, File}, _From, State) ->
    code:load_binary(Module, File, Bin),
    NewState = State#state{hooks=Module},
    {reply, ok, NewState};
handle_call({copy_file, Path, Mode, Contents}, _From, State) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Contents),
    ok = file:change_mode(Path, Mode),
    maybe_update_beam(Path),
    {reply, ok, State};
handle_call({rm_file, Path}, _From, State) ->
    Reply = file:delete(Path),
    {reply, Reply, State};
handle_call(notify_presync, _From, State) ->
    #state{hooks=Hooks} = State,
    call_hook_or_not(Hooks, presync),
    {reply, ok, State};
handle_call(notify_postsync, _From, State) ->
    #state{hooks=Hooks} = State,
    call_hook_or_not(Hooks, postsync),
    {reply, ok, State}.

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
-spec file_info(string()) -> {integer(), binary()}.
file_info(Filename) ->
    {ok, Data}=file:read_file(Filename),
    Hash = crypto:hash(sha, Data),

    {ok, #file_info{mode = Mode}} = file:read_file_info(Filename),
    {Mode, Hash}.

-spec normalize_path(string()) -> string().
normalize_path(Path) ->
    case lists:last(Path) of
	$/ -> Path;
	_ -> Path ++ "/"
    end.

%%-spec call_hook_or_not(atom(), atom()) ->
call_hook_or_not(undefined, presync) ->
    ok;
call_hook_or_not(undefined, postsync) ->
    ok;
call_hook_or_not(M, F) ->
    M:F().

maybe_update_beam(Path) ->
    case filename:extension(Path) of
	".beam" ->
	    update_beam(Path);
	_ ->
	    ok
    end.

update_beam(Path) ->
    Module = list_to_atom(filename:rootname(filename:basename(Path))),
    case code:which(Module) of
	non_existing ->
	    % Code not loaded yet. Let the VM load it on demand.
	    ok;
	Path ->
	    case code:is_sticky(Module) of
		true ->
		    io:format("Not reloading sticky module ~p.~n", [Module]);
		false ->
		    % Updating code that has been loaded
		    io:format("Reloading ~p...~n", [Module]),
		    code:purge(Module),
		    {module, Module} = code:load_file(Module)
	    end;
	Filename when is_binary(Filename) orelse is_list(Filename) ->
	    % Same module, but different location. Not sure what to do.
	    io:format("Confused. Module was originally loaded from ~p, but similar name is at ~p~n", [Filename, Path]),
	    ok
    end.
