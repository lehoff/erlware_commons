%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  This provides an implementation of the ec_dictionary type using
%%%  erlang orddicts as a base. The function documentation for
%%%  ec_dictionary applies here as well.
%%% @end
%%% @see ec_dictionary
%%% @see orddict
%%%-------------------------------------------------------------------
-module(ec_orddict).

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

%%%===================================================================
%%% Types
%%%===================================================================
-opaque dictionary() :: orddict:dictionary().

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> dictionary().
new() ->
    orddict:new().

-spec has_key(ec_dictionary:key(), Object::dictionary()) -> boolean().
has_key(Key, Data) ->
    orddict:is_key(Key, Data).

-spec get(ec_dictionary:key(), Object::dictionary()) -> ec_dictionary:value().
get(Key, Data) ->
    case orddict:find(Key, Data) of
	{ok, Value} ->
	    Value;
	 error ->
	    throw(not_found)
    end.

-spec add(ec_dictionary:key(), ec_dictionary:value(), Object::dictionary()) ->
    dictionary().
add(Key, Value, Data) ->
    orddict:store(Key, Value, Data).

-spec remove(ec_dictionary:key(), Object::dictionary()) ->
    dictionary().
remove(Key, Data) ->
    orddict:erase(Key, Data).

-spec has_value(ec_dictionary:value(), Object::dictionary()) -> boolean().
has_value(Value, Data) ->
    orddict:fold(fun(_, NValue, _) when NValue == Value ->
		      true;
		 (_, _, Acc) ->
		      Acc
	      end,
	      false,
	      Data).

-spec size(Object::dictionary()) -> integer().
size(Data) ->
    orddict:size(Data).

-spec to_list(dictionary()) -> [{ec_dictionary:key(), ec_dictionary:value()}].
to_list(Data) ->
    orddict:to_list(Data).

-spec from_list([{ec_dictionary:key(), ec_dictionary:value()}]) -> dictionary().
from_list(List) when is_list(List) ->
    orddict:from_list(List).
