%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  provides an implementation of ec_dictionary using an association
%%%  list as a basy
%%% @end
%%% @see ec_dictionary
%%%-------------------------------------------------------------------
-module(ec_assoc_list).

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
-opaque dictionary() :: [{term(), term()}].

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> dictionary().
new() ->
    {ec_assoc_list, []}.

-spec has_key(ec_dictionary:key(), Object::dictionary()) -> boolean().
has_key(Key, {ec_assoc_list, Data}) ->
    lists:keymember(Key, 1, Data).

-spec get(ec_dictionary:key(), Object::dictionary()) -> ec_dictionary:value().
get(Key, {ec_assoc_list, Data}) ->
    case lists:keyfind(Key, 1, Data) of
	{Key, Value} ->
	    Value;
	 false ->
	    throw(not_found)
    end.

-spec add(ec_dictionary:key(), ec_dictionary:value(), Object::dictionary()) ->
    dictionary().
add(Key, Value, {ec_assoc_list, Data}) ->
    {ec_assoc_list, [{Key, Value} | Data]}.

-spec remove(ec_dictionary:key(), Object::dictionary()) ->
    dictionary().
remove(Key, {ec_assoc_list, Data}) ->
    {ec_assoc_list, lists:keydelete(Key, 1, Data)}.

-spec has_value(ec_dictionary:value(), Object::dictionary()) -> boolean().
has_value(Value, {ec_assoc_list, Data}) ->
    lists:keymember(Value, 2, Data).

-spec size(Object::dictionary()) -> integer().
size({ec_assoc_list, Data}) ->
    length(Data).

-spec to_list(dictionary()) -> [{ec_dictionary:key(),
				 ec_dictionary:value()}].
to_list({ec_assoc_list, Data}) ->
   Data.

-spec from_list([{ec_dictionary:key(), ec_dictionary:value()}]) ->
    dictionary().
from_list(List) when is_list(List) ->
    {ec_assoc_list, List}.
