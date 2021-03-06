-module(ec_set).

-export([behaviour_info/1]).

%%% API
-export([new/1]).
-export([size/1]).
-export([add_element/2]).
-export([del_element/2]).
-export([is_element/2]).
-export([to_list/1]).
-export([from_list/2]).
-export([filter/2]).


-export_type([set/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(set_t,
	{callback,
	 data}).

%% @todo: change to opaque when 
-type set(_E) :: #set_t{}.


%%%===================================================================
%%% API
%%%===================================================================

%% @doc export the behaviour callbacks for this type
%% @private
behaviour_info(callbacks) ->
    [{new, 0},
     {add_element, 2},
     {del_element, 2},
     {size, 1},
     {to_list, 1},
     {from_list, 1},
     {filter, 2}
    ];
behaviour_info(_) ->
    undefined.


-spec new(module()) -> set(_T).
new(ModuleName) when is_atom(ModuleName) ->
    #set_t{callback=ModuleName, data = ModuleName:new()}.

-spec add_element(Element::T, Set1::set(T)) -> set(T).
add_element(Element, #set_t{callback=Mod, data=Data}=Set1) ->
    Set1#set_t{data = Mod:add_element(Element, Data)}.

-spec del_element(Element::T, Set1::set(T)) -> set(T).
del_element(Element, #set_t{callback=Mod, data=Data}=Set1) ->
    Set1#set_t{data = Mod:del_element(Element,Data)}.
		 
-spec is_element(Element::T, Set::set(T)) -> boolean().
is_element(Element, #set_t{callback=Mod, data=Data}=_Set) ->
    Mod:is_element(Element,Data).
			

-spec size(set(_T)) -> non_neg_integer().
size(#set_t{callback=Mod, data=Data}) ->
    Mod:size(Data).

-spec to_list(set(T)) -> list(T).
to_list(#set_t{callback=Mod, data=Data}) ->
    Mod:to_list(Data).
		     
-spec from_list(module(),list(T)) -> set(T).
from_list(Mod,List) ->
    #set_t{callback=Mod, data=Mod:from_list(List)}.
		   
-spec filter(fun((T) -> boolean()), set(T)) -> set(T).
filter(Pred, #set_t{callback=Mod, data=Data}=Set) ->
    Set#set_t{data = Mod:filter(Pred, Data)}.
		    
		     
