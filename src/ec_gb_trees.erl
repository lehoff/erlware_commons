%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%% This provides an implementation of the type ec_dictionary using
%%% gb_trees as a backin
%%% @end
%%% @see ec_dictionary
%%%-------------------------------------------------------------------
-module(ec_gb_trees).

-behaviour(ec_dictionary).

%% API
-export([new/0,
	 has_key/2,
	 get/2,
	 add/3,
	 remove/2,
	 has_value/2,
	 size/1,
	 to_list/1,
	 from_list/1]).

-export_type([dictionary/0]).

%%%===================================================================
%%% Types
%%%===================================================================
-opaque dictionary() :: gb_tree().

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> dictionary().
new() ->
    gb_trees:empty().

-spec has_key(ec_dictionary:key(), Object::dictionary()) -> boolean().
has_key(Key, Data) ->
    case gb_trees:lookup(Key, Data) of
	{value, _Val} ->
	    true;
	none ->
	    false
    end.

-spec get(ec_dictionary:key(), Object::dictionary()) -> ec_dictionary:value().
get(Key, Data) ->
    case gb_trees:lookup(Key, Data) of
	{value, Value} ->
	    Value;
	none ->
	    throw(not_found)
    end.

-spec add(ec_dictionary:key(), ec_dictionary:value(), Object::dictionary()) ->
    dictionary().
add(Key, Value, Data) ->
    gb_trees:enter(Key, Value, Data).

-spec remove(ec_dictionary:key(), Object::dictionary()) ->
    dictionary().
remove(Key, Data) ->
    gb_trees:delete(Key, Data).

-spec has_value(ec_dictionary:value(), Object::dictionary()) -> boolean().
has_value(Value, Data) ->
    lists:member(Value, gb_trees:values(Data)).

-spec size(Object::dictionary()) -> integer().
size(Data) ->
    gb_trees:size(Data).

-spec to_list(dictionary()) -> [{ec_dictionary:key(), ec_dictionary:value()}].
to_list(Data) ->
    gb_trees:to_list(Data).

-spec from_list([{ec_dictionary:key(), ec_dictionary:value()}]) -> dictionary().
from_list(List) when is_list(List) ->
    lists:foldl(fun({Key, Value}, Dict) ->
			gb_trees:enter(Key, Value, Dict)
		end,
		gb_trees:empty(),
		List).

