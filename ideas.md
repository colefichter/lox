NOTES
=====

The parser supports prefix & postfix ++/-- ops, but so far the interpreter doesn't. Will we cover this in statements?


Fun stuff to try adding:

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