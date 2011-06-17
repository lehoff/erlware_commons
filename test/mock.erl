-module(mock).

-compile(export_all).

%%-include_lib("proper/include/proper.hrl").

new_dictionary() ->
    meck:new(ec_dictionary_proper),
    meck:expect(ec_dictionary_proper, dictionary, fun() ->
							  proper_types:union([ec_dict])
						  end).
							  
