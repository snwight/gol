%%%-------------------------------------------------------------------
%%% @author Stephen Wight <>
%%% @copyright (C) 2013, Stephen Wight
%%% @doc
%%     Game of Life
%%     Cell gen_server module
%%     Responsible for:
%%     - Update and maintenance of attributes of one cell process:
%%       status:: atom representing state of this cell
%%           'live' or 'dead'
%%       cell:: tuple of integers representing location in world
%%           {Row, Col}
%%       nbrs:: list of 8 closest neighbor cells as 'row:col' keys
%%           [N, NE, E, SE, S, SW, W, NW]
%%% @end
%%% Created : 21 Nov 2013 by Stephen Wight <>
%%%-------------------------------------------------------------------
-module(gol_cell).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% update
-export([live/1, die/1, find_neighbors/1, predict/1, tick/1]).

%% query
-export([status/1]).

% utility
-export([key/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { 
	  status = dead :: 'alive' | 'dead',
	  pending = dead :: 'alive' | 'dead',
	  cell = {} :: tuple( integer(), integer() ), 
	  nbrs = {} :: tuple( atom(), atom(), atom(), atom(),
			     atom(), atom(), atom(), atom() )
	}).

%%%===================================================================
%%% API
%%%===================================================================
-spec live(atom()) -> ok.
live(CellKey) -> gen_server:call(CellKey, live).

-spec die(atom()) -> ok.
die(CellKey) -> gen_server:call(CellKey, die).

-spec status(atom()) -> 'dead' | 'alive'.
status(CellKey) -> gen_server:call(CellKey, status).

-spec find_neighbors(atom()) -> ok.
find_neighbors(Cell) -> gen_server:call(Cell, find_neighbors).
    
-spec predict(atom()) -> ok.
predict(CellKey) -> gen_server:call(CellKey, predict).

-spec tick(atom()) -> 'dead' | 'alive'.
tick(CellKey) -> gen_server:call(CellKey, tick).

-spec start_link(tuple()) -> {ok, pid()}.
start_link(Cell) ->
    gen_server:start_link({local, key(Cell)}, ?MODULE, Cell, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Cell) -> {ok, #state{cell=Cell}}.

handle_call(find_neighbors, _From, State) ->
    {Row, Col} = State#state.cell,
    N = key({Row - 1, Col}),
    NE = key({Row - 1 , Col + 1}),
    E = key({Row, Col + 1}),
    SE = key({Row + 1, Col + 1}),
    S = key({Row + 1, Col}),
    SW = key({Row + 1, Col - 1}),
    W = key({Row, Col - 1}),
    NW = key({Row - 1, Col - 1}),
    Nbrs = 
	lists:map(
	  fun(C) -> 
		  case whereis(C) of
		      undefined -> undefined;
		      _ -> C
		  end
	  end, [N, NE, E, SE, S, SW, W, NW]),
    {reply, ok, State#state{nbrs=Nbrs}};
handle_call(predict, _From, State) ->
    NbrSum = poll_neighbors(State#state.nbrs),
    NewStatus = dead_or_alive(State#state.status, NbrSum),
    {reply, ok, State#state{pending=NewStatus}};
handle_call(tick, _From, State) ->
    {reply, State#state.pending, State#state{status=State#state.pending}};
handle_call(live, _From, State)     -> {reply, ok, State#state{status=alive}};
handle_call(die, _From, State)      -> {reply, ok, State#state{status=dead}};
handle_call(status, _From, State)   -> {reply, State#state.status, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec key(tuple()) -> atom().
key({Row, Col}) -> list_to_atom(lists:concat([Row, ":", Col])).

-spec poll_neighbors(record()) -> integer().
poll_neighbors(Nbrs) ->
    NbrState = 
	lists:map(
	  fun (undefined) -> 0;
	      (C) -> 
		  case status(C) of
		      dead -> 0;
		      alive -> 1
		  end
	  end, Nbrs),
    lists:foldl(fun(S, Sum) -> S + Sum end, 0, NbrState).

-spec dead_or_alive(integer(), integer()) -> 'dead' | 'alive'.
dead_or_alive(alive, Sum) when Sum < 2 orelse Sum > 3 -> dead;
dead_or_alive(_Status, 3) -> alive;
dead_or_alive(Status, _Sum) -> Status.
