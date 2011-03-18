%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  provides an implementation of a map using an association list.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_dict).

-behaviour(ec_dictionary).

%% API
-export([new/0,
	 has_key/2,
	 get/2,
	 add/3,
	 remove/2,
	 has_value/2,
	 size/1]).

%%%===================================================================
%%% Types
%%%===================================================================
-opaque dictionary() :: dict:dictionary().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create a new dictionary object from the specified module. The
%% module should implement the dictionary behaviour. In the clause
%% where an existing object is passed in new empty dictionary of the
%% same implementation is created and returned.
%%
%% @param ModuleName|Object The module name or existing dictionary object.
-spec new() -> dictionary().
new() ->
    dict:new().

%% @doc check to see if the dictionary provided has the specified key.
%%
%% @param Object The dictory object to check
%% @param Key The key to check the dictionary for
-spec has_key(ec_dictionary:key(), Object::dictionary()) -> boolean().
has_key(Key, Data) ->
    dict:is_key(Key, Data).

%% @doc given a key return that key from the dictionary. If the key is
%% not found throw a 'not_found' exception.
%%
%% @param Object The dictionary object to return the value from
%% @param Key The key requested
%% @throws not_found when the key does not exist
-spec get(ec_dictionary:key(), Object::dictionary()) -> ec_dictionary:value().
get(Key, Data) ->
    case dict:find(Key, Data) of
	{ok, Value} ->
	    Value;
	 error ->
	    throw(not_found)
    end.

%% @doc add a new value to the existing dictionary. Return a new
%% dictionary containing the value.
%%
%% @param Object the dictionary object to add too
%% @param Key the key to add
%% @param Value the value to add
-spec add(ec_dictionary:key(), ec_dictionary:value(), Object::dictionary()) ->
    dictionary().
add(Key, Value, Data) ->
    dict:store(Key, Value, Data).

%% @doc Remove a value from the dictionary returning a new dictionary
%% with the value removed.
%%
%% @param Object the dictionary object to remove the value from
%% @param Key the key of the key/value pair to remove
-spec remove(ec_dictionary:key(), Object::dictionary()) ->
    dictionary().
remove(Key, Data) ->
    dict:erase(Key, Data).

%% @doc Check to see if the value exists in the dictionary
%%
%% @param Object the dictionary object to check
%% @param Value The value to check if exists
-spec has_value(ec_dictionary:value(), Object::dictionary()) -> boolean().
has_value(Value, Data) ->
    dict:fold(fun(_, NValue, _) when NValue == Value ->
		      true;
		 (_, _, Acc) ->
		      Acc
	      end,
	      false,
	      Data).

%% @doc return the current number of key value pairs in the dictionary
%%
%% @param Object the object return the size for.
-spec size(Object::dictionary()) -> integer().
size(Data) ->
    dict:size(Data).
