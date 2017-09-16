NOTES
=====

When the gen_server in the shell crashes, we lose the environment. Can we use the erlang callbacks to preserve the state?
    Need to store in db: http://erlang.org/pipermail/erlang-questions/2009-March/042473.html


Reset line number in interpreter_server so that in the shell we can stop showing every error as line 1?

Better error notification in scanner. See string()

When the interpreter reports an error, erlang prints the tokens with {t, ...} at the start to denote the record type.
Write a pretty-printer that represents the tokens and AST in an easy-to-read format.


The parser supports prefix & postfix ++/-- ops, but so far the interpreter doesn't. Will we cover this in statements?


Fun stuff to try adding:

lists & list comprehensions

immutable variables: val x = 123;

multiple return values:
    fun getCoords() {
        return 3, 4;
    }
    x, y = getCoords();

unless keyword

arrays/lists and basic list comprehensions

tuples

simple pattern matching?

Null coalescing operator, like in C#: ??

Conditional call operator: ?.
    eg:
        var x = new Foo();
        x?.Bar(); //returns result of bar
        x = nil;
        x?.Bar(); // returns nil



Produce float results to same precision (and rounded) as input.
    eg:
    0.3-0.1 // returns 0.199999999999999998








DONE
====

Using the minus sign to trim part of a string:

    var x = "Foobar";
    x - "bar"; //returns "Foo"