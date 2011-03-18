%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  provides an implementation of a map using an association list.
%%% @end
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
	 to_list/1]).

%%%===================================================================
%%% Types
%%%===================================================================
-opaque assoc_list() :: [{term(), term()}].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create a new dictionary object from the specified module. The
%% module should implement the dictionary behaviour. In the clause
%% where an existing object is passed in new empty dictionary of the
%% same implementation is created and returned.
%%
%% @param ModuleName|Object The module name or existing dictionary object.
-spec new() -> assoc_list().
new() ->
    {ec_assoc_list, []}.

%% @doc check to see if the dictionary provided has the specified key.
%%
%% @param Object The dictory object to check
%% @param Key The key to check the dictionary for
-spec has_key(ec_dictionary:key(), Object::assoc_list()) -> boolean().
has_key(Key, {ec_assoc_list, Data}) ->
    lists:keymember(Key, 1, Data).

%% @doc given a key return that key from the dictionary. If the key is
%% not found throw a 'not_found' exception.
%%
%% @param Object The dictionary object to return the value from
%% @param Key The key requested
%% @throws not_found when the key does not exist
-spec get(ec_dictionary:key(), Object::assoc_list()) -> ec_dictionary:value().
get(Key, {ec_assoc_list, Data}) ->
    case lists:keyfind(Key, 1, Data) of
	{Key, Value} ->
	    Value;
	 false ->
	    throw(not_found)
    end.

%% @doc add a new value to the existing dictionary. Return a new
%% dictionary containing the value.
%%
%% @param Object the dictionary object to add too
%% @param Key the key to add
%% @param Value the value to add
-spec add(ec_dictionary:key(), ec_dictionary:value(), Object::assoc_list()) ->
    assoc_list().
add(Key, Value, {ec_assoc_list, Data}) ->
    {ec_assoc_list, [{Key, Value} | Data]}.

%% @doc Remove a value from the dictionary returning a new dictionary
%% with the value removed.
%%
%% @param Object the dictionary object to remove the value from
%% @param Key the key of the key/value pair to remove
-spec remove(ec_dictionary:key(), Object::assoc_list()) ->
    assoc_list().
remove(Key, {ec_assoc_list, Data}) ->
    {ec_assoc_list, lists:keydelete(Key, 1, Data)}.

%% @doc Check to see if the value exists in the dictionary
%%
%% @param Object the dictionary object to check
%% @param Value The value to check if exists
-spec has_value(ec_dictionary:value(), Object::assoc_list()) -> boolean().
has_value(Value, {ec_assoc_list, Data}) ->
    lists:keymember(Value, 2, Data).

%% @doc return the current number of key value pairs in the dictionary
%%
%% @param Object the object return the size for.
-spec size(Object::assoc_list()) -> integer().
size({ec_assoc_list, Data}) ->
    length(Data).

%% @doc Return the contents of this dictionary as a list of key value
%% pairs.
-spec to_list(assoc_list()) -> [{ec_dictionary:key(),
				 ec_dictionary:value()}].
to_list({ec_assoc_list, Data}) ->
   Data.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").
new_test() ->
    Object1 = ec_dictionary:new(ec_assoc_list),
    Object2 = ec_assoc_list:new(),
    {_, Data} = ec_implements:concrete_objects(Object1),
    ?assertMatch(Object2, Data),
    ?assertMatch({ec_assoc_list, []}, Object2),
    ?assertMatch({ec_assoc_list, []}, Data).

has_key_test() ->
    Object = ec_dictionary:add(ec_dictionary:add(
				 ec_dictionary:new(ec_assoc_list), foo, bar),
			       baz, bang),
    ?assertMatch(true, ec_dictionary:has_key(Object, foo)),
    ?assertMatch(true, ec_dictionary:has_key(Object, baz)),
    ?assertMatch(false, ec_dictionary:has_key(Object, should_not_find)).

get_test() ->
    Object = ec_dictionary:add(ec_dictionary:add(
				 ec_dictionary:new(ec_assoc_list), foo, bar),
			       baz, bang),
    ?assertMatch(bar, ec_dictionary:get(Object, foo)),
    ?assertMatch(bang, ec_dictionary:get(Object, baz)),
    ?assertThrow(not_found, ec_dictionary:get(Object, should_blow_up)).

remove_test() ->
    Object = ec_dictionary:add(ec_dictionary:add(
				 ec_dictionary:new(ec_assoc_list), foo, bar),
			       baz, bang),
    ?assertMatch(bar, ec_dictionary:get(Object, foo)),

    NewObject = ec_dictionary:remove(Object, baz),

    ?assertThrow(not_found, ec_dictionary:get(NewObject, baz)),
    ?assertMatch(1, ec_dictionary:size(NewObject)),

    EmptyObject = ec_dictionary:remove(NewObject, foo),

    ?assertThrow(not_found, ec_dictionary:get(EmptyObject, foo)),
    ?assertMatch(0, ec_dictionary:size(EmptyObject)).


has_value_test() ->
    Object = ec_dictionary:add(ec_dictionary:add(
				 ec_dictionary:new(ec_assoc_list), foo, bar),
			       baz, bang),
    ?assertMatch(true, ec_dictionary:has_value(Object, bar)),
    ?assertMatch(true, ec_dictionary:has_value(Object, bang)),
    ?assertMatch(false, ec_dictionary:has_value(Object, should_not_find)).


-endif.
