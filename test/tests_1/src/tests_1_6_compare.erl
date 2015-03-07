
%%
%% tests_1 compare file 6
%%

-module(tests_1_6).

-record(rec, {a, b, c, d, f, g}).
-record(rec2, {e, h}).

tests_1_6_1() ->
    try true catch Error -> true end,
    try true after catch true, true end.

tests_1_6_2(A) when is_binary(A) ->
    try non_existing_module:call1(A) of
        _ -> true catch Error -> true
    end,
    try catch non_existing_module:call2(A) of
        _ -> catch true catch Error2 -> true
    end.

tests_1_6_3(A) when is_binary(A); is_list(A) ->
    try non_existing_module:call1(A) of
        A when is_binary(A) -> true catch error:Error -> true
    end,
    non_existing_module:call2(try catch A of
        A when is_binary(A) -> catch catch catch catch catch true catch Error2 -> true
    end).

tests_1_6_4(A) when is_binary(A) ->
    try non_existing_module:call_1(A)
    catch C:Error -> is_atom(C),true; Error when true, is_atom(A) -> true
    end,
    non_existing_module:call_2(try A of
        A when is_binary(A); is_list(A) -> true
    catch Error2 -> true
    after true
    end);
tests_1_6_4(A) when is_list(A) ->
    [try non_existing_module:call(A) of
        _ -> true
    catch Error -> true
    after catch catch catch true, catch catch true, catch true
    end].

tests_1_6_5(A,B,C) when A < B,B < C; A < C,C < B; B < A,A < C; B < C,C < A; C < A,A < B; C < B,B < A ->
    try non_existing_module:call_a(A,B,C) of
        A when is_binary(A) ->
            non_existing_module:call_1(A),
            non_existing_module:call_2(A);
        {A} when is_list(A) ->
            non_existing_module:call(A)
    catch Error -> true
    after non_existing_module:call_3(A), catch catch true, catch true
    end,
    non_existing_module:call_b(A,B,C),
    try true of
        {tests_1_6_4,A} when is_binary(A) ->
            non_existing_module:call_1(A),
            non_existing_module:call_2(A);
        [tests_1_6_4,A] when is_list(A) ->
            non_existing_module:call(A)
    catch Error_ -> true
    end.

tests_1_6_6(A,B,C) when A < B,B < C ->
    try catch catch catch catch non_existing_module:call_(A,B,C) of
        {A,B,C} when A < B,B < C; A < C,C < B; B < A,A < C; B < C,C < A; C < A,A < B; C < B,B < A ->
            non_existing_module:call_a(A,B,C),
            non_existing_module:call_b(A,B,C)
    catch Error -> true
    end, non_existing_module:call_2(A,B,C);
tests_1_6_6(A,B,C) when A < C,C < B ->
    non_existing_module:call__(A,B,C),
    try non_existing_module:call__2(A,B,C) of
        [A,B,C|D] when A < B,B < C; A < C,C < B; B < A,A < C; B < C,C < A; C < A,A < B; C < B,B < A ->
            non_existing_module:call_a(A,B,C),
            non_existing_module:call_b(A,B,C)
    catch Error -> true
    end;
tests_1_6_6(A,B,C) when B < A,A < C ->
    non_existing_module:call___(A,B,C),
    try true of
        #rec{a = A,b = B,c = C} when A < B,B < C; A < C,C < B; B < A,A < C; B < C,C < A; C < A,A < B; C < B,B < A ->
            non_existing_module:call_a(A,B,C),
            non_existing_module:call_b(A,B,C)
    catch Error -> true
    end, non_existing_module:call___2(A,B,C);
tests_1_6_6(A,B,C) when B < C,C < A -> catch catch catch catch catch
    [non_existing_module:call____(A,B,C)],
    non_existing_module:call____2(A,B,C);
tests_1_6_6(A,B,C) when C < A,A < B ->
    try true of
        #{a := A,b := B,c := C} when A < B,B < C; A < C,C < B; B < A,A < C; B < C,C < A; C < A,A < B; C < B,B < A ->
            non_existing_module:call_a(A,B,C),
            non_existing_module:call_b(A,B,C),
            non_existing_module:call_____(A,B,C),
            non_existing_module:call_____2(A,B,C)
    catch Error -> true
    end;
tests_1_6_6(A,B,C) when C < B,B < A ->
    non_existing_module:call______(A,B,C),
    non_existing_module:call______2(A,B,C).

tests_1_6_7(A=#{key:=E},<<B/binary>>,[C|_],D=#rec{g=G}) ->
    try catch true of
        {A,B,C} when A < B,B < C -> catch catch non_existing_module:call_(A,B,C), catch catch catch catch catch catch catch non_existing_module:call_2(A,B,C);
        {A,B,C} when A < C,C < B -> catch catch catch non_existing_module:call__(A,B,C), catch catch catch catch catch catch non_existing_module:call__2(A,B,C);
        {A,B,C} when B < A,A < C -> catch catch catch catch non_existing_module:call___(A,B,C), catch catch catch catch catch non_existing_module:call___2(A,B,C);
        {A,B,C} when B < C,C < A -> catch catch catch catch catch non_existing_module:call____(A,B,C), catch catch catch catch non_existing_module:call____2(A,B,C);
        {A,B,C} when C < A,A < B -> catch catch catch catch catch catch non_existing_module:call_____(A,B,C), catch catch catch non_existing_module:call_____2(A,B,C);
        {A,B,C} when C < B,B < A -> catch catch catch catch catch catch catch non_existing_module:call______(A,B,C), catch catch non_existing_module:call______2(A,B,C)
    catch Error -> true
    end;
tests_1_6_7(A=#{key:=E},<<B/binary>>,[C|_],D) ->
    try true of
        {{tests_1_6_6,A,B,C}} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        {{{tests_1_6_6,A,B,C}}} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        {{{{tests_1_6_6,A,B,C}}}} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        {{{{{tests_1_6_6,A,B,C}}}}} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        [[[[[[tests_1_6_6,A,B,C]]]]]] when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        {{[{[{[tests_1_6_6,A,B,C]}]}]}} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    catch Error -> true
    end;
tests_1_6_7(A=#{key:=E},<<B/binary>>,C,D=#rec{g=G}) ->
    try true of
        [{tests_1_6_6,A},B,C] when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        [tests_1_6_6,{A,B},C] when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        [tests_1_6_6,A,{B,C}] when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        [{tests_1_6_6,A,B},C] when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        [tests_1_6_6,{A,B,C}] when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        [{tests_1_6_6,A,B,C}] when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    catch Error -> true
    end;
tests_1_6_7(A=#{key:=E},<<B/binary>>,C,D) -> true;
tests_1_6_7(A=#{key:=E},B,[C|_],D=#rec{g=G}) ->
    try true of
        {[tests_1_6_6|A],B,C} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        {tests_1_6_6,[A|B],C} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        {tests_1_6_6,A,[B|C]} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        {[tests_1_6_6,A|B],C} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        {tests_1_6_6,[A,B|C]} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        {[tests_1_6_6,A,B|C]} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    catch Error -> true
    end;
tests_1_6_7(A=#{key:=E},B,[C|_],D) -> true;
tests_1_6_7(A=#{key:=E},B,C,D=#rec{g=G}) ->
    try true of
        {tests_1_6_6,A,B,C} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        [tests_1_6_6,A,B,C] when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        #rec{a = A,b = B,c = C} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        {A,B,<<C>>} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        {A,<<B>>,C} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        {<<A>>,B,C} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    catch Error -> true
    end;
tests_1_6_7(A=#{key:=E},B,C,D) -> true;
tests_1_6_7(A,<<B/binary>>,[C|_],D=#rec{g=G}) ->
    try true of
        #rec{a = A,b = B,c = C} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        #rec{a = A,c = B,b = C} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        #rec{b = A,a = B,c = C} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        #rec{b = A,c = B,a = C} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        #rec{c = A,a = B,b = C} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        #rec{c = A,b = B,a = C} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    catch Error -> true
    end;
tests_1_6_7(A,<<B/binary>>,[C|_],D) -> true;
tests_1_6_7(A,<<B/binary>>,C,D=#rec{g=G}) -> true;
tests_1_6_7(A,<<B/binary>>,C,D) ->
    try true of
        #{a := A,b := B,c := C} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        #{a := A,c := B,b := C} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        #{b := A,a := B,c := C} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        #{b := A,c := B,a := C} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        #{c := A,a := B,b := C} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        #{c := A,b := B,a := C} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    catch Error -> true
    end;
tests_1_6_7(A,B,[C|_],D=#rec{g=G}) -> true;
tests_1_6_7(A,B,[C|_],D) -> true;
tests_1_6_7(A,B,C,D=#rec{g=G}) ->
    try true of
        {{A,B,C}} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        {[A,B,C]} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        {#rec{a = A,b = B,c = C}} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        {<<A>>,B,C} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        {A,<<B>>,C} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        {A,B,<<C>>} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    catch Error -> true
    end;
tests_1_6_7(A,B,C,D) -> true.

tests_1_6_8(A=#{key:=E},<<B/binary>>,[C|_],D=#rec{g=G}) when C =:= 1 -> true;
tests_1_6_8(A=#{key:=E},<<B/binary>>,[C|_],D) when true, C =:= 2 -> true;
tests_1_6_8(A=#{key:=E},<<B/binary>>,C,D=#rec{g=G}) when true, true, C =:= 3  -> true;
tests_1_6_8(A=#{key:=E},<<B/binary>>,C,D) when true, true, true, C =:= 4 -> true;
tests_1_6_8(A=#{key:=E},B,[C|_],D=#rec{g=G}) when true, true, true, true, C =:= 5 -> true;
tests_1_6_8(A=#{key:=E},B,[C|_],D) when true, true, true, true, true, C =:= 6 -> true;
tests_1_6_8(A=#{key:=E},B,C,D=#rec{g=G}) when true, true, true, true, true, true, C =:= 7 -> true;
tests_1_6_8(A=#{key:=E},B,C,D) when true, true, true, true, true, true, true, C =:= 8 -> true;
tests_1_6_8(A,<<B/binary>>,[C|_],D=#rec{g=G}) when C /= 1 -> true;
tests_1_6_8(A,<<B/binary>>,[C|_],D) when C /= 2; true -> true;
tests_1_6_8(A,<<B/binary>>,C,D=#rec{g=G}) when C /= 3; false; true -> true;
tests_1_6_8(A,<<B/binary>>,C,D) when C /= 4; false; false; true -> true;
tests_1_6_8(A,B,[C|_],D=#rec{g=G}) when C /= 5; false; false; false; true -> true;
tests_1_6_8(A,B,[C|_],D) when C /= 6; false; false; false; false; true -> true;
tests_1_6_8(A,B,C,D=#rec{g=G}) when C /= 7; false; false; false; false; false; true -> true;
tests_1_6_8(A,B,C,D) when C /= 8; false; false; false; false; false; false; false; true -> true.

tests_1_6_9(A=#{key:=E=#{key:=F}},<<B/binary>>,[[C|_]|_],D=#rec{g = G = #rec2{h=H}}) when C =:= 1 -> true;
tests_1_6_9(A=#{key:=E=#{key:=F}},<<B/binary>>,[[C|_]|_],D) when true andalso C =:= 2 -> true;
tests_1_6_9(A=#{key:=E=#{key:=F}},<<B/binary>>,C,D=#rec{g = G = #rec2{h=H}}) when true andalso true andalso C =:= 3  -> true;
tests_1_6_9(A=#{key:=E=#{key:=F}},<<B/binary>>,C,D) when true andalso true andalso true andalso C =:= 4 -> true;
tests_1_6_9(A=#{key:=E=#{key:=F}},B,[[C|_]|_],D=#rec{g = G = #rec2{h=H}}) when true andalso true andalso true andalso true andalso C =:= 5 -> true;
tests_1_6_9(A=#{key:=E=#{key:=F}},B,[[C|_]|_],D) when true, true andalso true andalso true andalso true andalso C =:= 6 -> true;
tests_1_6_9(A=#{key:=E=#{key:=F}},B,[C|_],D=#rec{g = G = #rec2{h=H}}) when true, true andalso true andalso true andalso true, true andalso C =:= 7 -> true;
tests_1_6_9(A=#{key:=E=#{key:=F}},B,[C|_],D) when true andalso true andalso true, true andalso true, true andalso true, C =:= 8 -> true;
tests_1_6_9(A,<<B/binary>>,[[C|_]|_],D=#rec{g = G = #rec2{h=H}}) when C /= 1 -> true;
tests_1_6_9(A,<<B/binary>>,[[C|_]|_],D) when C /= 2 orelse true -> true;
tests_1_6_9(A,<<B/binary>>,C,D=#rec{g = G = #rec2{h=H}}) when C /= 3; false orelse true -> true;
tests_1_6_9(A,<<B/binary>>,C,D) when C /= 4 orelse false; false orelse true -> true;
tests_1_6_9(A,B,[[C|_]|_],D=#rec{g = G = #rec2{h=H}}) when C /= 5; false orelse false; false orelse true -> true;
tests_1_6_9(A,B,[[C|_]|_],D) when C /= 6 orelse false orelse false; false; false orelse true -> true;
tests_1_6_9(A,B,C,D=#rec{g = G = #rec2{h=H}}) when C /= 7 orelse false; false; false; false orelse false orelse true -> true;
tests_1_6_9(A,B,C,D) when C /= 8 orelse false orelse false; false; false; false orelse false; false orelse true -> true.
