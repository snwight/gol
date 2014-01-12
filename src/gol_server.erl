%%%-------------------------------------------------------------------
%%% @author Stephen Wight
%%% @doc
%%     Game of Life
%%     Administrative gen_server module
%%     Responsible for:
%%     - Creation of 'world matrix', specified as 2-dimensional
%%       lattice of gen_server processes (gol_cell module). Each
%%       cell is given a unique registered name '<<row>>:<<col>>'
%%     - Initial seeding of matrix, from list of node names
%%     - Maintenance of 'world clock' which sends the tick message
%%       to the matrix of cells which triggers state transitions 
%%% @end
%%%-------------------------------------------------------------------
-module(gol_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([seed/1, seed/2, tick/0, run/1, display/0, display/1, clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% util
-export([key/1]).

-include("gol.hrl").

-define(SERVER, ?MODULE). 
-define(SAFE_BORDER, 5).

-record(state, {dims = #dims{} :: record(), world = [] :: list()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec seed(list()) -> ok.

seed(Spec) -> 
    seed(ctr, Spec).

seed(Pos, block)                   -> seed(Pos, ?BLOCK);
seed(Pos, beehive)                 -> seed(Pos, ?BEEHIVE);
seed(Pos, loaf)                    -> seed(Pos, ?LOAF);
seed(Pos, boat)                    -> seed(Pos, ?BOAT);
seed(Pos, blinker)                 -> seed(Pos, ?BLINKER);
seed(Pos, toad)                    -> seed(Pos, ?TOAD);
seed(Pos, beacon)                  -> seed(Pos, ?BEACON);
seed(Pos, fumarole)                -> seed(Pos, ?FUMAROLE);
seed(Pos, pulsar)                  -> seed(Pos, ?PULSAR);
seed(Pos, glider)                  -> seed(Pos, ?GLIDER);
seed(Pos, lwss)                    -> seed(Pos, ?LWSS);
seed(Pos, gosper)                  -> seed(Pos, ?GOSPER);
seed(Pos, pentomino)               -> seed(Pos, ?PENTOMINO);
seed(Pos, diehard)                 -> seed(Pos, ?DIEHARD);
seed(Pos, acorn)                   -> seed(Pos, ?ACORN);
seed(Pos, blsse1)                  -> seed(Pos, ?BLSSE1);
seed(Pos, blsse2)                  -> seed(Pos, ?BLSSE2);
seed(Pos, linear1)                 -> seed(Pos, ?LINEAR1);
seed(Pos, Spec) when is_list(Spec) -> 
    gen_server:call(gol_server, {seed, {Pos, Spec}}).

-spec tick() -> ok.
tick() -> gen_server:cast(gol_server, tick).

-spec display() -> ok.
display() -> gen_server:cast(gol_server, {display, 0}).

-spec display(integer()) -> ok.
display(Layer) -> gen_server:cast(gol_server, {display, Layer}).

-spec clear() -> ok.
clear() -> gen_server:cast(gol_server, clear).

-spec run(integer()) -> ok.
run(0) -> ok;
run(NumCycles) when NumCycles > 0 -> 
    {ok, Tempo} = application:get_env(gol, tempo),
    tick(),
    display(),
    timer:sleep(Tempo),
    run(NumCycles - 1).

start_link(Dims) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Dims, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Dims) ->
    World = add_layers(Dims),
    lists:foreach(
      fun(LD) ->
	      lists:foreach(fun(C) -> gol_cell:find_neighbors(C) end, LD)
      end, World),
    {ok, #state{dims=Dims, world=World}}.

handle_call({seed, SeedSpec={_Pos, Spec}}, _From, State) -> 
    io:format("gol_server:handle_call seed ~p~n", [SeedSpec]),
    %% derive footprint of seed pattern and center point as deltas  
    {Rd, Cd, _Ld} = seed_pos_offset(State#state.dims, SeedSpec),
    lists:foreach(
      fun(CellKey) ->
              {R, C, L} = unpack_key(CellKey),
	      %% neutering layer offsets for now
              gol_cell:live(key({R + Rd, C + Cd, L})) 
      end, Spec),
    {reply, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(tick, State) ->
    %% two-phase state change here to clumsily mimic global synchronicity
    lists:foreach(
      fun(LD) -> 
	      lists:foreach(fun(C) -> gol_cell:predict(C) end, LD) 
      end, State#state.world),
    lists:foreach(
      fun(LD) -> 
	      lists:foreach(fun(C) -> gol_cell:tick(C) end, LD) 
      end, State#state.world),
    {noreply, State};
handle_cast({display, Layer}, State) ->
    display_layer(State#state.world, (State#state.dims)#dims.cols, Layer),
    {noreply, State};
handle_cast(clear, State) ->
    lists:foreach(
      fun(LD) -> 
    	      lists:foreach(fun(C) -> gol_cell:die(C) end, LD)
      end, State#state.world),
    %% for now we default is to only display layer 1
    display_layer(State#state.world, (State#state.dims)#dims.cols, 0),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec add_layers(record()) -> list().
add_layers(Dims) -> 
    %% fugly -1 is because I want zero-origin planes
    add_layers(Dims#dims.rows - 1, 
	       Dims#dims.cols - 1, 
	       Dims#dims.layers - 1, Dims, []).

-spec add_layers(integer(), integer(), integer(), record(), list()) -> list().
add_layers(_Row, _Col, -1, _Dims, World) -> World;
add_layers(Row, Col, Layer, Dims, World) ->
    add_layers(Row, Col, Layer - 1, Dims, 
	       [add_cells(Row, Col, Layer, Dims, []) | World]).

-spec add_cells(integer(), integer(), integer(), record(), list()) -> list().
add_cells(-1, _Col, _Layer, _Dims, World) -> World;
add_cells(Row, Col, Layer, Dims, World) -> 
    add_cells(Row - 1, Col, Layer, Dims, add_row(Row, Col, Layer, Dims, World)).

-spec add_row(integer(), integer(), integer(), record(), list()) -> list().
add_row(_Row, -1, _Layer, _Dims, World) -> World;
add_row(Row, Col, Layer, Dims, World) ->
    %% create a new cell
    gol_cell:start_link([#cell{row=Row, col=Col, layer=Layer}, Dims]),
    add_row(Row, Col - 1, Layer, Dims, [key({Row, Col, Layer}) | World]).

-spec display_layer(list(), integer(), integer()) -> ok.
display_layer(World, NumCols, Layer) ->
    DL = lists:map(
	   fun(C) -> 
		   case gol_cell:status(C) of
		       dead -> " ";
		       alive -> "@"
		   end
	   end, lists:nth(Layer + 1, World)),
    display_layer(DL, NumCols).

display_layer([], _NumCols) -> ok;
display_layer(DL, NumCols) ->
    io:format("~n~p", [lists:concat(lists:sublist(DL, NumCols))]),
    display_layer(lists:nthtail(NumCols, DL), NumCols).

-spec key(tuple()) -> atom().
key({Row, Col, Layer}) -> 
    list_to_atom(lists:concat([Row, ":", Col, ":", Layer])).

-spec unpack_key(atom()) -> tuple().
unpack_key(Key) ->
    L = lists:map(fun(W) -> {V, _} = string:to_integer(W), V  end, 
		  string:tokens(atom_to_list(Key), ":")),
    list_to_tuple(
      case length(L) of
	  3 -> L;
	  2 -> lists:append(L, [0])  %% force 2-D key to 3-D
      end).

%%
%% utilities for auto-positioning seed patterns
%%
seed_pos_offset(#dims{rows=Rows, cols=Cols, layers=Layers}, {ctr, Spec}) ->
    {Width, Height, Depth} = find_seed_bounds(Spec),
    {Rows div 2 - Height div 2, Cols div 2 - Width div 2, 
     Layers div 2 - Depth div 2};
seed_pos_offset(#dims{cols=Cols, layers=Layers}, {top, Spec}) ->
    {Width, _Height, Depth} = find_seed_bounds(Spec),
    {?SAFE_BORDER, Cols div 2 - Width div 2, 
     Layers div 2 - Depth div 2};
seed_pos_offset(#dims{rows=Rows, cols=Cols, layers=Layers}, {bottom, Spec}) ->
    {Width, Height, Depth} = find_seed_bounds(Spec),
    {Rows - Height - ?SAFE_BORDER, Cols div 2 - Width div 2, 
     Layers div 2 - Depth div 2};
seed_pos_offset(#dims{rows=Rows, cols=Cols, layers=Layers}, {right, Spec}) ->
    {Width, Height, Depth} = find_seed_bounds(Spec),
    {Rows div 2 - Height div 2, Cols - Width - ?SAFE_BORDER, 
     Layers div 2 - Depth div 2};
seed_pos_offset(#dims{rows=Rows, cols=_Cols, layers=Layers}, {left, Spec}) ->
    {_Width, Height, Depth} = find_seed_bounds(Spec),
    {Rows div 2 - Height div 2, ?SAFE_BORDER, 
     Layers div 2 - Depth div 2}.

find_seed_bounds([H|T]) -> 
    {R, C, L} = unpack_key(H),
    bounds(T, R, R, C, C, L, L).

bounds([], MinR, MaxR, MinC, MaxC, MinL, MaxL) -> 
    {MaxC - MinC,  MaxR - MinR, MinL - MaxL};
bounds([H |T], MinR, MaxR, MinC, MaxC, MinL, MaxL) ->
    {R, C, L} = unpack_key(H),
    NewMinR = if R < MinR -> R; R >= MinR -> MinR end,
    NewMaxR = if R > MaxR -> R; R =< MaxR -> MaxR end,
    NewMinC = if C < MinC -> C; C >= MinC -> MinC end,
    NewMaxC = if C > MaxC -> C; C =< MaxC -> MaxC end,
    NewMinL = if L < MinL -> C; L >= MinL -> MinL end,
    NewMaxL = if L > MaxL -> C; L =< MaxL -> MaxL end,
    bounds(T, NewMinR, NewMaxR, NewMinC, NewMaxC, NewMinL, NewMaxL).
