%%%-------------------------------------------------------------------
%%% @author Steve Wight
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(gol_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("gol.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(term(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    {ok, [Rows, Cols, Layers]} = application:get_env(gol, dimensions),
    
    case gol_sup:start_link(#dims{rows=Rows, cols=Cols, layers=Layers}) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
		end.

stop(_State) ->
    ok.
