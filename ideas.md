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

