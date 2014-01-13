%%% @author Steve Wight <northwight@gmail.com>
%%% @doc
%%%     Some data types and some example seed patterns naively implemented
%%% @end
%%% Created : 11 Jan 2014 by Steve Wight

%% this holds the width, height, depth of the world
-record(dims, {rows = 0 :: non_neg_integer(), 
	       cols = 0 :: non_neg_integer(), 
	       layers = 0 :: non_neg_integer()}).

%% this holds a single cell's location in world matrix
-record(cell, {row = 0 :: non_neg_integer(), 
	       col = 0 :: non_neg_integer(), 
	       layer = 0 :: non_neg_integer()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% interesting rule sets - rules { [ BirthConditions ], [ SurvivalConditions] }
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% 1-D
%%
%% from Mirek: 
%% One-dimensional totalistic CA notation
%% The notation of one-dimensional totalistic CA rules has the "R,C,M,S,B" form:
%%    R - specifies the range (1..10)
%%    C - specifies the count of states, 0..256. A value smaller than 3 means 
%%        the history is not active. Values greater than 2 activate the history,
%%        with the given count of states.
%%    M - specifies activity of the center (middle) cell. 1 is on, 0 is off.
%%    S - specifies a single total of neighbors allowing the cell to survive. '
%%        S' can appear any times in the rule. Example: S2,S3,S8.
%%    B - specifies a single total of neighbors allowing the cell to be born. 
%%        'B' can appear any times in the rule. Example: B0,B3,B4,B17
%%
%% WOLFRAM - just the survival conditions are used
%% 111/0 110/1 101/0 100/1 011/1 010/0 001/1 000/0
%%	   die = [{0,dead,0}, {0,alive,0}, {1,dead,1}]}).
-define(RULE90,	[{0,dead,1}, {0,alive,1}, {1,dead,0},{1,alive,0},{1,alive,1}]).
%% 111/0 110/0 101/0 100/1 011/1 010/1 001/1 000/0
%%         die = [{1,alive,1}, {1,alive,0}, {1,dead,1}, {0,dead,0}]}).
-define(RULE30, [{0,dead,1}, {0,alive,0}, {0,alive,1}, {1,dead,0}]).
%% 111/0 110/1 101/1 100/0 011/1 010/1 001/1 000/0
%%	   die = {0,dead,0}, {1,alive,1}]}).
-define(RULE110, [{0,dead,1}, {0,alive,0}, {0,alive,1}, {1,dead,1}, {1,alive,0}]).
%% 111/1 110/0 101/1 100/1 011/1 010/0 001/0 000/0
%%	   die = [{0,dead,0}, {0,dead,1}, {0,alive,0}, {1,alive,0}] }).
-define(RULE184, [{0,alive,1}, {1,dead,0}, {1,dead,1}, {1,alive,1}]).

%%
%% 2-D Birth/Survive
%% credits to Mirek Wojtowicz, http://www.mirekw.com/ca
%%
-define(CONWAY,         {[3], [2,3]}).
-define(LIFE34,         {[3,4], [3,4]}).
-define(HIGHLIFE,       {[3,6], [2,3]}).
-define(DAYNIGHT,       {[3,6,7,8], [3,4,6,7,8]}).
-define(CORAL,          {[3], [4,5,6,7,8]}).
-define(COAGULATIONS,   {[3,7,8], [2,3,5,6,7,8]}).
-define(ASSIMILATION,   {[3,4,5], [4,5,6,7]}).
-define(AMOEBA,         {[3,5,7], [1,3,5,8]}).
-define(BLOCKS2X2,      {[3,6], [1,2,5]}).
-define(FLAKES,         {[3], [0,1,2,3,4,5,6,7,8]}).
-define(GNARL,          {[1], [1]}).
-define(LONGLIFE,       {[3,4,5], [5]}).
-define(MAZE,           {[3], [1,2,3,4,5]}).
-define(MAZECTRIC,      {[3], [1,2,3,4]}).
-define(MOVE,           {[3,6,8], [2,4,5]}).
-define(PSEUDOLIFE,     {[3,5,7], [2,3,8]}).
-define(REPLICATOR,     {[1,3,5,7], [1,3,5,7]}).
-define(STAINS,         {[3,6,7,8], [2,3,5,6,7,8]}).
-define(WALLEDCITIES,   {[4,5,6,7,8], [2,3,4,5]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% interesting seed patterns
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% "still lifes"
%%
-define(BLOCK, 
	['2:2', '2:3', '3:2', '3:3']).
-define(BEEHIVE,
	['2:3', '2:4', '3:2', '3:5', '4:3', '4:4']).
-define(LOAF, 
	['2:3', '2:4', '3:2', '3:5', '4:3', '4:5', '5:4']).
-define(BOAT, 
	['2:2', '2:3', '3:2', '3:4', '4:3']).
%%
%% oscillators
%%
-define(BLINKER, 
	['3:2', '3:3', '3:4']).
-define(TOAD, 
	['3:3', '3:4', '3:5', '4:2', '4:3', '4:4']).
-define(BEACON, 
	['2:2', '2:3', '3:2', '4:5', '5:4', '5:5']).
-define(FUMAROLE, 
	['1:4', '1:5', '2:2', '2:7', '3:2', '3:7', '4:2', '4:7', '5:3', 
	 '5:6', '6:1', '6:3', '6:6', '6:8', '7:1', '7:2', '7:7', '7:8']).
-define(PULSAR,
	['3:5', '3:6', '3:7', '3:11', '3:12', '3:13', '5:3', '5:8', 
	 '5:10', '5:15', '6:3', '6:8', '6:10', '6:15', '7:3', '7:8',
	 '7:10', '7:15', '8:5', '8:6', '8:7', '8:11', '8:12', '8:13',
	 '10:5', '10:6', '10:7', '10:11', '10:12', '10:13', '11:3', 
	 '11:8', '11:10', '11:15', '12:3', '12:8', '12:10', '12:15',
	 '13:3', '13:8', '13:10', '13:15', '15:5', '15:6', '15:7', 
	 '15:11', '15:12', '15:13']).
%%
%% spaceships and gliders
%%
-define(GLIDER,
	['2:3', '3:4', '4:2', '4:3', '4:4']).
-define(LWSS,         %% lightweight spaceship
	['2:2', '2:5', '3:6', '4:2', '4:6', '5:3', '5:4', '5:5', '5:6']).
-define(GOSPER,       %% gosper glider gun
	['6:2', '6:3', '7:2', '7:3', '4:14', '4:15', '5:13', '5:17', 
	 '6:12', '6:18', '7:12', '7:16', '7:18', '7:19', '8:12', '8:18', 
	 '9:13', '9:17', '10:14', '10:15', '2:26', '3:24', '3:26', '4:22', 
	 '4:23', '5:22', '5:23', '6:22', '6:23', '7:24', '7:26', '8:26',
	 '4:36', '4:37', '5:36', '5:37']).
%%
%% methuselahs, mortal and otherwise
%%
-define(PENTOMINO,
	['2:3', '2:4', '3:2', '3:3', '4:3']).
-define(DIEHARD,      %% dies in 130 generations
	['2:8', '3:4', '3:4', '4:3', '4:7', '4:8', '4:9']).
-define(ACORN,
	['2:3', '3:5', '4:2', '4:3', '4:6', '4:7', '4:8']).
-define(BLSSE1,       %% block-laying switch engine 1
	['2:8', '3:6', '3:8', '3:9', '4:6', '4:8', '5:6', '6:4', '7:2', '7:4']).
-define(BLSSE2,       %% block-laying switch engine 2
	['2:2', '2:3', '2:4', '2:6', '3:2', '4:5', '4:6', 
	 '5:3', '5:4', '5:6', '6:2', '6:4', '6:6']).
-define(LINEAR1,      %% 1-dimensional - dimensions [1, <<N>>, 1]
	['0:2', '0:3', '0:4', '0:5', '0:6', '0:7', '0:8', '0:9',
	 '0:11', '0:12', '0:13', '0:14', '0:15', '0:19', '0:20', '0:21',
	 '0:28', '0:29', '0:30', '0:31', '0:32', '0:33', '0:34',
	 '0:36', '0:37', '0:38', '0:39', '0:40']).
