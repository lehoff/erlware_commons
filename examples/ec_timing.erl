%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  A module that supports association of keys to values. A map cannot
%%%  contain duplicate keys; each key can map to at most one value.
%%%
%%%  This interface is a member of the Erlware Commons Library.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_timing).

%% API
-export([time_direct_vs_signature_dict/0,
	 time_direct_vs_signature_rbdict/0,
	 time_dictionary_implementations/0]).

%%%===================================================================
%%% API
%%%===================================================================

time_direct_vs_signature_rbdict() ->
    io:format("Timing rbdict~n"),
    Dict = create_rbdict(),
    test_avg(fun() ->
		     ec_rbdict:size(ec_rbdict:add(some_key, some_value, Dict))
	     end,
	     1000000),
    io:format("Timing ec_dict implementation of ec_dictionary~n"),
    time_dict_type(ec_rbdict).

time_direct_vs_signature_dict() ->
    io:format("Timing dict~n"),
    Dict = create_dict(),
    test_avg(fun() ->
		     dict:size(dict:store(some_key, some_value, Dict))
	     end,
	     1000000),
    io:format("Timing ec_dict implementation of ec_dictionary~n"),
    time_dict_type(ec_dict).

time_dictionary_implementations() ->
    lists:foreach(fun(Type) ->
			  time_dict_type(Type)
		  end,
		  [ec_dict, ec_orddict, ec_assoc_list, ec_gb_trees, ec_rbdict]).

%%%===================================================================
%%% Enternal functions
%%%===================================================================

time_dict_type(Type) ->
    io:format("Testing ~p~n", [Type]),
    Dict = create_dictionary(Type),
    test_avg(fun() ->
		     ec_dictionary:size(ec_dictionary:add(some_key, some_value, Dict))
	     end,
	     1000000).

create_rbdict() ->
    lists:foldl(fun(El, Dict) ->
			ec_rbdict:add(El, El, Dict)
		end, ec_rbdict:new(),
		lists:seq(1,100)).

create_dict() ->
    lists:foldl(fun(El, Dict) ->
			dict:store(El, El, Dict)
		end, dict:new(),
		lists:seq(1,100)).

create_dictionary(Type) ->
    lists:foldl(fun(El, Dict) ->
			ec_dictionary:add(El, El, Dict)
		end,
		ec_dictionary:new(Type),
		lists:seq(1,100)).

test_avg(Fun, N) when N > 0 ->
    L = test_loop(Fun, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
	      "Median: ~b mics~n"
	      "Average: ~b mics~n",
	      [Min, Max, Med, Avg]),
    Med.

test_loop(_Fun, 0, List) ->
    List;
test_loop(Fun, N, List) ->
    {T, _Result} = timer:tc(Fun),
    test_loop(Fun, N - 1, [T | List]).
