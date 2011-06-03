Signatures
==========

It often occurs in software that we have a requirement for
functionality. We don't care how that functionality is implemented we
only care that the functionality is provided. A great example of this
is signatures in Standard ML or Interfaces in Java.  This is basically
a contract where both sides have some investment.

There are two sides of the contract. The first side is specifing the
the actual contract. In Erlang we do this by defining a custom
[behaviour](http://www.trapexit.org/Defining_Your_Own_Behaviour). This
is well understood in the Erlang comunity, in fact you have probably
don this yourself once or twice. The other side of this is using
functionality that implements the behaviour. That is where the trouble
comes in.

Lets start out with an example. Lets take key, value pairs or in
common Erlang parlance dicts. There are a ton of ways to implement a
dict.

* Associative Arrays
* Binary Trees
* Hash Tables
* Skip Lists
* Many, many more ....

Each of these approaches has there own performance characteristics,
memory footprints etc. Whats right for your library may not be known
when you are writing the code. So you want to accept a dict instead of
a specific implementation of a dict.

How would we do that. First and foremost we need to define the
interface. Lets do that now.

    :::erlang
    -module(ec_dictionary).

    -export([behaviour_info/1]).

    behaviour_info(callbacks) ->
        [{new, 0},
         {has_key, 2},
         {get, 2},
         {add, 3},
         {remove, 2},
         {has_value, 2},
         {size, 1},
         {to_list, 1},
         {from_list, 1},
         {keys, 1}];
    behaviour_info(_) ->
        undefined.

So now we have our behaviour, which we actually expect folks to
implement. However, how do we make use of it? The first idea is
probably to just accept a module name as an argument.


    :::erlang
    some_fun_that_uses_dict(DictModuleName) ->
        Dict = DictModuleName:new(),
	DictModuleName:add(some_key, some_value, Dict).

This works just fine. However, it has a big, big drawback. Non of the
tools that help make source better work with this. That is, because
the module identified by DictModuleName is not known at compile time
tools like dialyzer and xref can't give you any useful information.


