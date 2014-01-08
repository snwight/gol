%%%-------------------------------------------------------------------
%%% @author Stephen Wight
%%% @doc
%%     Game of Life
%%     Cell gen_server module
%%     Responsible for:
%%     - Update and maintenance of attributes of one cell process:
%%       status:: atom representing state of this cell
%%           'alive' or 'dead'
%%       cell:: tuple of integers representing location in world
%%           {Row, Col}
%%       nbrs:: list of 8 closest neighbor cells as 'row:col' keys
%%           [N, NE, E, SE, S, SW, W, NW]
%%% @end
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
-import(gol_server, [key/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  status = dead :: 'alive' | 'dead',
	  pending = dead :: 'alive' | 'dead',
	  predictor :: fun(),
	  cell = {} :: tuple( integer(), integer(), tuple() ), 
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
init(Cell) -> 
    {ok, Rules} = application:get_env(gol, rules),
    {ok, #state{predictor=rule_func(Rules), cell=Cell}}.

handle_call(find_neighbors, _From, State) ->
    NbrCells = neighbors(State#state.cell),
    NbrPids = 
	lists:map(
	  fun(C) -> 
		  case whereis(C) of
		      undefined -> undefined;
		      _ -> C
		  end
	  end, NbrCells),
    {reply, ok, State#state{nbrs=NbrPids}};
handle_call(predict, _From, State) ->
    NbrSum = poll_neighbors(State#state.nbrs),
    Predictor = State#state.predictor,
    NewStatus = Predictor(State#state.status, NbrSum),
    {reply, ok, State#state{pending=NewStatus}};
handle_call(tick, _From, State) ->
    {reply, State#state.pending, State#state{status=State#state.pending}};
handle_call(live, _From, State)     -> {reply, ok, State#state{status=alive}};
handle_call(die, _From, State)      -> {reply, dead, State#state{status=dead}};
handle_call(status, _From, State)   -> {reply, State#state.status, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec neighbors(tuple()) -> list().
neighbors({Row, Col}) ->
    %% 2-D world, 8 neighbors
    Hood = [{Row - 1, Col},
	    {Row - 1 , Col + 1},
	    {Row, Col + 1},
	    {Row + 1, Col + 1},
	    {Row + 1, Col},
	    {Row + 1, Col - 1},
	    {Row, Col - 1},
	    {Row - 1, Col - 1}],
    lists:map(fun(E) -> key(E) end, Hood);
neighbors(_Root={Row, Col, _Layer}) ->
    neighbors({Row, Col}).
    %% %% 3-D world, 26 neighbors
    %% Hood = [Root,
    %% 	    {Row - 1, Col, Layer},
    %% 	    {Row - 1 , Col + 1, Layer},
    %% 	    {Row, Col + 1, Layer},
    %% 	    {Row + 1, Col + 1, Layer},
    %% 	    {Row + 1, Col, Layer},
    %% 	    {Row + 1, Col - 1, Layer},
    %% 	    {Row, Col - 1, Layer},
    %% 	    {Row - 1, Col - 1, Layer}],
    %% LayerHood = 
    %% 	lists:map(
    %% 	  fun({R, C, L}) -> 
    %% 		  lists:map(fun(Z) ->
    %% 				    K = key({R, C, L + Z}) ,
    %% 				    K
    %% 			    end, [-1, 1]) 
    %% 	  end, Hood),
    %% [_|H] = Hood,
    %% lists:flatten([LayerHood, lists:map(fun(E) -> key(E) end, H)]).

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

%% A higher-order approach for rule designation:
%% e.g. for B3/S23 rules, initialize as:
%%     NewStateFunc = rule_func([3], [2, 3]).
%% and utilize as:
%%     NewState = NewStateFun(Status, Sum).
-spec rule_func(tuple()) -> 'dead' | 'alive'.
rule_func({Born, Survive}) ->
    fun (dead, Sum) -> 
	    case lists:member(Sum, Born) of
		true -> alive;
		false -> dead
	    end;
	(alive, Sum) ->
	    case lists:member(Sum, Survive) of
		true -> alive;
		false -> dead
	    end
    end.
