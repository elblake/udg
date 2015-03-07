%%% Example use of user-defined guard.
%%%

-module(example).
-author('E. L. Blake at < edwardlblake at gmail.com >').

-export([ test/1 ]).

-auto_include_udg([ \\display_uuid/5 ]).

test(UUID \\ display_uuid(U1,U2,U3,U4,U5)) when is_binary(UUID), true ->
    io:format("UUID: ~s", [UUID]).
