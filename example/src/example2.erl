
-module(example2).
-author('Edward L. Blake at < edwardlblake at gmail.com >').

-export([ test/1 ]).

-auto_include_udg([ \\integer_in_second_place/1 ]).

test(Compound \\ integer_in_second_place(Val)) -> Val.
