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
%%           {Row, Col, Layer}
%%       nbrs:: list of 8 closest neighbor cells as 'row:col' keys
%%           [N, NE, E, SE, S, SW, W, NW]
%%              or list of 26 closest neighbors in a 3-D world
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

-include("gol.hrl").

-record(state, {
	  status = 'dead' :: 'alive' | 'dead',
	  pending = 'dead' :: 'alive' | 'dead',
	  predictor :: fun(),
	  dims = #dims{} :: record(),
	  cell = #cell{} :: record(),
	  nbrs = [] :: list()
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
start_link(Args=[#cell{row=Row, col=Col, layer=Layer}, _Dims]) ->
    gen_server:start_link({local, key({Row, Col, Layer})}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Cell, Dims]) -> 
    {ok, Rules} = application:get_env(gol, rules),
    {ok, #state{predictor=rule_func(Rules), cell=Cell, dims=Dims}}.

handle_call(find_neighbors, _From, State) ->
    NbrCells = neighbors(State#state.cell, State#state.dims),
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
    NbrState = poll_neighbors(State#state.nbrs),
    Predictor = State#state.predictor,
    NewStatus = Predictor(State#state.status, NbrState),
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
-spec neighbors(record(), record()) -> list().
neighbors(#cell{row=0, col=Col, layer=0}, #dims{rows=1, cols=Cols, layers=1}) ->
    %% 1-D (linear) wraparound world, 2 neighbors
    ColL = if Col == 0 -> Cols; Col > 0 -> Col - 1 end,
    ColR = if Col == Cols -> 0; Col < Cols -> Col + 1 end,
    [key({0, ColL, 0}), key({0, ColR, 0})];
neighbors(#cell{row=Row, col=Col, layer=Layer}, 
	  #dims{rows=Rows, cols=Cols, layers=1}) ->
    %% 2-D wraparound world, 8 neighbors
    RowUp = if Row == 0 -> Rows; Row > 0 -> Row - 1  end,
    RowDown = if Row == Rows -> 0; Row < Rows -> Row + 1 end,
    ColL = if Col == 0 -> Cols; Col > 0 -> Col - 1 end,
    ColR = if Col == Cols -> 0; Col < Cols -> Col + 1 end,
    Hood = [{RowUp, Col, Layer},
	    {RowUp , ColR, Layer},
	    {Row, ColR, Layer},
	    {RowDown, ColR, Layer},
	    {RowDown, Col, Layer},
	    {RowDown, ColL, Layer},
	    {Row, ColL, Layer},
	    {RowUp, ColL, Layer}],
    lists:map(fun(E) -> key(E) end, Hood);
neighbors(#cell{row=Row, col=Col, layer=Layer}, 
	  #dims{rows=Rows, cols=Cols, layers=Layers}) ->
    %% XXX snwight
    %% temporarily neutered 3-D world neighbor consultations
    %% 3-D wraparound world, 26 neighbors
    RowUp = if Row == 0 -> Rows; true -> Row end,
    RowDown = if Row == Rows -> 0; true -> Rows end,
    ColL = if Col == 0 -> Cols; true -> Col end,
    ColR = if Col == Cols -> 0; true -> Cols end,
    %% Hood is the 2-D planar 8-member neighborhood around our root cell
    Hood = [{RowUp, Col, Layer},
	    {RowUp , ColR, Layer},
	    {Row, ColR, Layer},
	    {RowDown, ColR, Layer},
	    {RowDown, Col, Layer},
	    {RowDown, ColL, Layer},
	    {Row, ColL, Layer},
	    {RowUp, ColL, Layer}],
    %% RootHood is our 8 2-D neighbors PLUS the cell itself ("root")
    RootHood = [{Row, Col, Layer}|Hood],
    %% LayerHood is 2 9-member planes based on RootHood, offset +-1 layer
    LayerHood = 
    	lists:map(
    	  fun({R, C, L}) -> 
    		  LayerFg = if L == 0 -> Layers; L > 0 -> L - 1 end,
    		  LayerBg = if L == Layers -> 0; L < Layers -> L + 1 end,
    		  [{R, C, LayerFg}, {R, C, LayerBg}]
    	  end, RootHood),
    lists:flatten([LayerHood, lists:map(fun(E) -> key(E) end, Hood)]).

-spec poll_neighbors(list()) -> non_neg_integer().
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
    process_neighbors(NbrState).

process_neighbors(NbrState) when length(NbrState) == 2 -> 
    NbrState;
process_neighbors(NbrState) ->
    lists:foldl(fun(S, Sum) -> S + Sum end, 0, NbrState).

%% A higher-order approach for rule designation:
%% e.g. for B3/S23 rules, initialize as:
%%     NewStateFunc = rule_func([3], [2, 3]).
%% and utilize as:
%%     NewState = NewStateFun(Status, Sum).
-spec rule_func(tuple() | list()) -> any().
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
    end;
rule_func(Live) ->
    fun (Status, [L, R]) ->
	    case lists:member({L, Status, R}, Live) of
		true -> alive;
		false -> dead
	    end
    end.
