%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Provide a platform to define abstract and concrete types.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_extends).

%% API
-export([new/3,
	 new_with_check/3,
	 implements/2,
	 deep_check_implements/2,
	 implements_type/2,
	 implements_type_with_check/2
	]).

-export_type([type/1,
	     type_name/0]).

%%%===================================================================
%%% Types
%%%===================================================================
-type type_name() :: atom().
-opaque type(T) :: {ec_extends, module(), T,
		   [type_name() | atom()]}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create a new implementation of the specified type. Do this by
%% specifying the module name as the concrete type, the data for the
%% type and the module name of the abstract type as the Abstract type.
-spec new(atom(), T, atom()) -> type(T).
new(ConcreteType, Data, AbstractType)
  when is_atom(AbstractType) andalso is_atom(ConcreteType) ->
   {ec_extends, ConcreteType, Data, [AbstractType]}.

%% @doc do the same thing as new/3 but do a deep check that the
%% concrete type implements everything that the abstract type
%% requieres. The abstract type must be a behaviour implementation
-spec new_with_check(atom(), T, atom()) -> type(T).
new_with_check(ConcreteType, Data, Implements)
  when is_atom(Implements) andalso is_atom(ConcreteType) ->
    implements_type_with_check({ec_extends, ConcreteType, Data, []},
			       Implements).

%% @doc Indicate that the concrete implements the abstract
%% type specified. no checking occurs.
-spec implements_type(type(T), type_name()) -> type(T).
implements_type({ec_extends, ConcreteType, Data, Implementations}, Type) ->
    {ec_extends, ConcreteType, Data, [Type | Implementations]}.

%% @doc Indicate that the concrete type implements the abstract type
%% specified. Do a deep check at the function/arity level to make sure
%% that this is the case.
-spec implements_type_with_check(type(T), atom()) -> type(T).
implements_type_with_check(Object = {ec_extends, _, _, _},
			   Type) ->
    NewObject = implements_type(Object, Type),
    case deep_check_implements(NewObject, Type) of
	true ->
	    NewObject;
	false ->
	    throw(does_not_implement_type)
    end.

%% @doc check that the specified concrete type implments the specified
%% abstract type. Do this check to the function/arity level.
-spec deep_check_implements(type(_T), atom()) -> boolean().
deep_check_implements(Object =
		      {ec_extends, ConcreteType, _Data, _Implementations},
		      Type)
  when is_atom(Type) ->
    Callbacks = Type:behaviour_info(callbacks),
    ModuleInfo = ConcreteType:module_info(),
    {exports, ExpList} = lists:keyfind(exports, 1, ModuleInfo),
    has_all_callbacks(Callbacks, ExpList) andalso implements(Object, Type).

%% @doc do a cheep check to see if the concrete type implements the
%% specified abstract type.
-spec implements(type(_T), type_name()) -> boolean().
implements({ec_extends, _ConcreteType, _Data, Implementations}, Type) ->
    lists:member(Type, Implementations);
implements(_Object, _Type) ->
    false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @dog given a list of function/arity tuples check that every single
%% one of those in the first list exists in the second list.
-spec has_all_callbacks([{atom(), number()}],
			[{ModuleName::atom(), Arity::number()}]) ->
    boolean().
has_all_callbacks(Callbacks, Exports) ->
    lists:foldl(fun(Callback, Acc) ->
			  lists:member(Callback, Exports) andalso Acc
		end,
		true,
		Callbacks).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    ConcreteType = ec_assoc_list,
    Data = [],
    Implements = sequence,
    Object = new(ConcreteType, Data, Implements),
    ?assertMatch({ec_extends, ConcreteType, Data, [Implements]},
		 Object).

implements_type1_test() ->
    ConcreteType = ec_assoc_list,
    Data = [],
    Implements = sequence,
    Object = new(ConcreteType, Data, Implements),

    SecondImplementation = map,

    NewObject = implements_type(Object, SecondImplementation),

    ?assertMatch({ec_extends, ConcreteType, Data,
		  [SecondImplementation, Implements]},
		 NewObject).

implements_type_with_check_test() ->
    ConcreteType = auth,
    AbstractType = gen_server,
    Data = [],
    Object = new_with_check(ConcreteType, Data, AbstractType),
    ?assertMatch({ec_extends, ConcreteType,
		  Data, [AbstractType]},
		 Object),

    NewConcreteType = file,

    ?assertThrow(does_not_implement_type,
		 new_with_check(NewConcreteType, Data,
				AbstractType)),

    ?assertThrow(does_not_implement_type,
		 implements_type_with_check(Object, gen_fsm)).

has_all_callbacks_test() ->
    Callbacks = [{foo, 3},
		 {bar, 0},
		 {baz, 1}],
    ModuleInfo = [{boo1, 1},
		  {biz, 3},
		  {foo, 3},
		  {bar, 0},
		  {bart, 1},
		  {baz, 1}],
    ?assertMatch(true, has_all_callbacks(Callbacks, ModuleInfo)),

    ModuleInfo2 = [{bust, 1},
		   {bicker, 2}],
    ?assertMatch(false, has_all_callbacks(Callbacks, ModuleInfo2)).

implements_test() ->
    ConcreteType = ec_assoc_list,
    Data = [],
    Implements = sequence,
    Object = new(ConcreteType, Data, Implements),
    SecondImplementation = map,
    ThirdImplementation = pp,

    NewObject = implements_type(implements_type(Object, SecondImplementation),
				ThirdImplementation),
    ?assertMatch(true, implements(NewObject, Implements)),
    ?assertMatch(false, implements(NewObject, foobar)),
    ?assertMatch(true, implements(NewObject, SecondImplementation)),
    ?assertMatch(true, implements(NewObject, ThirdImplementation)),
    ?assertMatch(false, implements(NewObject, baz)),

    ?assertMatch(false, implements(some_non_type, foo)).

-endif.
