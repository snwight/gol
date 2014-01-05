%%%-------------------------------------------------------------------
%%% @author Stephen Wight
%%% @doc
%%   from http://en.wikipedia.org/wiki/Conway's_Game_of_Life
%%      The universe of the Game of Life is an infinite two-dimensional 
%%      orthogonal grid of square cells, each of which is in one of two 
%%      possible states, alive or dead. Every cell interacts with its
%%      eight neighbours, which are the cells that are horizontally, 
%%      vertically, or diagonally adjacent. At each step in time, the 
%%      following transitions occur:
%%
%%      Any live cell with fewer than two live neighbours dies, as if 
%%      caused by under-population.
%%      Any live cell with two or three live neighbours lives on to the 
%%      next generation.
%%      Any live cell with more than three live neighbours dies, as if 
%%      by overcrowding.
%%      Any dead cell with exactly three live neighbours becomes a live 
%%      cell, as if by reproduction.
%%
%%      The initial pattern constitutes the seed of the system. 
%%      The first generation is created by applying the above rules 
%%      simultaneously to every cell in the seed—births and deaths occur 
%%      simultaneously, and the discrete moment at which this happens is 
%%      sometimes called a tick (in other words, each generation is a pure 
%%      function of the preceding one). The rules continue to be applied 
%%      repeatedly to create further generations.
%%
%%  ...that said, this implementation actually allows any BX/SX ruleset
%%% @end
%%%-------------------------------------------------------------------
-module(gol_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link(list()) -> {ok, pid()}.
start_link(WorldDimensions) ->
    supervisor:start_link({local, gol_sup}, ?MODULE, WorldDimensions).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init(list()) -> {ok, {tuple(), [tuple()]}}.
init(WorldDimensions) ->
    {ok, {{one_for_one, 1000, 3600},
	  [{gol_server,
	    {gol_server, start_link, [WorldDimensions]}, 
	    permanent, brutal_kill, worker, [gol_server]}]}}.
