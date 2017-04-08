NOTES
=====

Fun stuff to try adding:

Null coalescing operator, like in C#: ??

Conditional call operator: ?.
    eg:
        var x = new Foo();
        x?.Bar(); //returns result of bar
        x = nil;
        x?.Bar(); // returns nil

Using the minus sign to trim part of a string:

    var x = "Foobar";
    x - "bar"; //returns "Foo"

