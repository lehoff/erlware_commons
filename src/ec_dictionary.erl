%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%%
%%% @doc
%%% A module that supports association of keys to values. A map cannot
%%% contain duplicate keys; each key can map to at most one value.
%%%
%%%  This interface is a member of the Erlware Commons Library.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_dictionary).

%%% Behaviour Callbacks
-export([behaviour_info/1]).

%% API
-export([new/1,
	 has_key/2,
	 get/2,
	 add/3,
	 remove/2,
	 has_value/2,
	 size/1,
	 to_list/1,
	 from_list/2]).



-export_type([dictionary/0,
	      key/0,
	      value/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(dict_t,
	{callback,
	  data}).

-opaque dictionary() :: #dict_t{}.
-type key() :: term().
-type value() :: term().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc export the behaviour callbacks for this type
%% @private
behaviour_info(callbacks) ->
    [{new, 0},
     {has_key, 2},
     {get, 2},
     {add, 3},
     {remove, 2},
     {has_value, 2},
     {size, 1},
     {to_list, 1},
     {from_list, 1}];
behaviour_info(_) ->
    undefined.

%% @doc create a new dictionary object from the specified module. The
%% module should implement the dictionary behaviour.
%%
%% @param ModuleName The module name.
-spec new(module()) -> dictionary().
new(ModuleName) when is_atom(ModuleName) ->
    #dict_t{callback = ModuleName, data = ModuleName:new()}.

%% @doc check to see if the dictionary provided has the specified key.
%%
%% @param Dict The dictory object to check
%% @param Key The key to check the dictionary for
-spec has_key(key(), dictionary()) -> boolean().
has_key(Key, #dict_t{callback = Mod, data = Data}) ->
    Mod:has_key(Key, Data).

%% @doc given a key return that key from the dictionary. If the key is
%% not found throw a 'not_found' exception.
%%
%% @param Dict The dictionary object to return the value from
%% @param Key The key requested
%% @throws not_found when the key does not exist
-spec get(key(), dictionary()) -> value().
get(Key, #dict_t{callback = Mod, data = Data}) ->
    Mod:get(Key, Data).

%% @doc add a new value to the existing dictionary. Return a new
%% dictionary containing the value.
%%
%% @param Dict the dictionary object to add too
%% @param Key the key to add
%% @param Value the value to add
-spec add(key(), value(), dictionary()) -> dictionary().
add(Key, Value, #dict_t{callback = Mod, data = Data} = Dict) ->
    Dict#dict_t{data = Mod:add(Key, Value, Data)}.

%% @doc Remove a value from the dictionary returning a new dictionary
%% with the value removed.
%%
%% @param Dict the dictionary object to remove the value from
%% @param Key the key of the key/value pair to remove
-spec remove(key(), dictionary()) -> dictionary().
remove(Key, #dict_t{callback = Mod, data = Data} = Dict) ->
    Dict#dict_t{data = Mod:remove(Key, Data)}.

%% @doc Check to see if the value exists in the dictionary
%%
%% @param Dict the dictionary object to check
%% @param Value The value to check if exists
-spec has_value(value(), dictionary()) -> boolean().
has_value(Value, #dict_t{callback = Mod, data = Data}) ->
    Mod:has_value(Value, Data).

%% @doc return the current number of key value pairs in the dictionary
%%
%% @param Dict the object return the size for.
-spec size(dictionary()) -> integer().
size(#dict_t{callback = Mod, data = Data}) ->
    Mod:size(Data).

%% @doc Return the contents of this dictionary as a list of key value
%% pairs.
%%
%% @param Dict the base dictionary to make use of.
-spec to_list(Dict::dictionary()) -> [{key(), value()}].
to_list(#dict_t{callback = Mod, data = Data}) ->
    Mod:to_list(Data).

%% @doc Create a new dictionary, of the specified implementation using
%% the list provided as the starting contents.
%%
%% @param ModuleName the type to create the dictionary from
%% @param List The list of key value pairs to start with
-spec from_list(module(), [{key(), value()}]) -> dictionary().
from_list(ModuleName, List) when is_list(List) ->
    #dict_t{callback = ModuleName, data = ModuleName:from_list(List)}.

