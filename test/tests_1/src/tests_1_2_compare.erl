
%%
%% tests_1 compare file 2
%%

-module(tests_1_2).

-record(rec, {a, b, c, d, f, g}).
-record(rec2, {e, h}).

tests_1_2_1() ->
    case true of
        true -> true
    end.

tests_1_2_2(A) when is_binary(A) ->
    catch case catch non_existing_module:call1(A) of
        _ -> true
    end,
    case non_existing_module:call2(A) of
        _ -> true
    end.

tests_1_2_3(A) when is_binary(A); is_list(A) ->
    case non_existing_module:call1(A) of
        A when is_binary(A) ->
            non_existing_module:call1(A)
    end,
    non_existing_module:call2(case A of
        A when is_binary(A) ->
            non_existing_module:call2(A)
    end).

tests_1_2_4(A) when is_binary(A) ->
    case non_existing_module:call_1(A) of
        A when is_binary(A); is_list(A) ->
            non_existing_module:call1(A),
            non_existing_module:call2(A)
    end,
    non_existing_module:call_2(case A of
        A when is_binary(A); is_list(A) ->
            non_existing_module:call1(A),
            non_existing_module:call2(A)
    end);
tests_1_2_4(A) when is_list(A) ->
    [case non_existing_module:call(A) of
        A when is_binary(A); is_list(A) ->
            non_existing_module:call1(A),
            non_existing_module:call2(A)
    end].

tests_1_2_5(A,B,C) when A < B,B < C; A < C,C < B; B < A,A < C; B < C,C < A; C < A,A < B; C < B,B < A ->
    case non_existing_module:call_a(A,B,C) of
        A when is_binary(A) ->
            non_existing_module:call_1(A),
            non_existing_module:call_2(A);
        {A} when is_list(A) ->
            non_existing_module:call(A)
    end,
    non_existing_module:call_b(A,B,case C of
        {tests_1_2_4,A} when is_binary(A) ->
            non_existing_module:call_1(A),
            non_existing_module:call_2(A);
        [tests_1_2_4,A] when is_list(A) ->
            non_existing_module:call(A)
    end).

tests_1_2_6(A,B,C) when A < B,B < C ->
    case non_existing_module:call_(A,B,C) of
        {A,B,C} when A < B,B < C; A < C,C < B; B < A,A < C; B < C,C < A; C < A,A < B; C < B,B < A ->
            non_existing_module:call_a(A,B,C),
            non_existing_module:call_b(A,B,C)
    end, non_existing_module:call_2(A,B,C);
tests_1_2_6(A,B,C) when A < C,C < B ->
    non_existing_module:call__(A,B,C),
    case non_existing_module:call__2(A,B,C) of
        [A,B,C] when A < B,B < C; A < C,C < B; B < A,A < C; B < C,C < A; C < A,A < B; C < B,B < A ->
            non_existing_module:call_a(A,B,C),
            non_existing_module:call_b(A,B,C)
    end;
tests_1_2_6(A,B,C) when B < A,A < C ->
    non_existing_module:call___(A,B,C),
    case true of
        #rec{a = A, b = B, c = C} when A < B,B < C; A < C,C < B; B < A,A < C; B < C,C < A; C < A,A < B; C < B,B < A ->
            non_existing_module:call_a(A,B,C),
            non_existing_module:call_b(A,B,C)
    end,
    non_existing_module:call___2(A,B,C);
tests_1_2_6(A,B,C) when B < C,C < A ->
    [non_existing_module:call____(A,B,C)],
    non_existing_module:call____2(A,B,C);
tests_1_2_6(A,B,C) when C < A,A < B ->
    case non_existing_module:call_____(A,B,C) of
        #{a := A,b := B, c := C} when A < B,B < C; A < C,C < B; B < A,A < C; B < C,C < A; C < A,A < B; C < B,B < A ->
            non_existing_module:call_a(A,B,C),
            non_existing_module:call_b(A,B,C),
            non_existing_module:call_____2(A,B,C)
    end;
tests_1_2_6(A,B,C) when C < B,B < A ->
    non_existing_module:call______(A,B,C),
    non_existing_module:call______2(A,B,C).

tests_1_2_7(A=#{key:=E},<<B/binary>>,[C|_],D=#rec{g=G}) ->
    case true of
        {A,B,C} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        {A,B,C} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        {A,B,C} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        {A,B,C} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        {A,B,C} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        {A,B,C} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    end;
tests_1_2_7(A=#{key:=E},<<B/binary>>,[C|_],D) ->
    case true of
        {{tests_1_2_6,A,B,C}} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        {{{tests_1_2_6,A,B,C}}} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        {{{{tests_1_2_6,A,B,C}}}} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        {{{{{tests_1_2_6,A,B,C}}}}} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        {[[[[[tests_1_2_6,A,B,C]]]]]} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        {{[{[{[tests_1_2_6,A,B,C]}]}]}} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    end;
tests_1_2_7(A=#{key:=E},<<B/binary>>,C,D=#rec{g=G}) -> 
    case true of
        [{tests_1_2_6,A},B,C] when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        [tests_1_2_6,{A,B},C] when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        [tests_1_2_6,A,{B,C}] when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        [{tests_1_2_6,A,B},C] when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        [tests_1_2_6,{A,B,C}] when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        [{tests_1_2_6,A,B,C}] when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    end;
tests_1_2_7(A=#{key:=E},<<B/binary>>,C,D) -> true;
tests_1_2_7(A=#{key:=E},B,[C|_],D=#rec{g=G}) ->
    case true of
        {[tests_1_2_6|A],B,C} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        {tests_1_2_6,[A|B],C} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        {tests_1_2_6,A,[B|C]} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        {[tests_1_2_6,A|B],C} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        {tests_1_2_6,[A,B|C]} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        {[tests_1_2_6,A,B|C]} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    end;
tests_1_2_7(A=#{key:=E},B,[C|_],D) -> true;
tests_1_2_7(A=#{key:=E},B,C,D=#rec{g=G}) ->
    case true of
        {tests_1_2_6,A,B,C} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        [tests_1_2_6,A,B,C] when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        #rec{a = A,b = B,c = C} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        {A,B,<<C>>} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        {A,<<B>>,C} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        {<<A>>,B,C} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    end;
tests_1_2_7(A=#{key:=E},B,C,D) -> true;
tests_1_2_7(A,<<B/binary>>,[C|_],D=#rec{g=G}) ->
    case true of
        #rec{a = A,b = B,c = C} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        #rec{a = A,c = B,b = C} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        #rec{b = A,a = B,c = C} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        #rec{b = A,c = B,a = C} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        #rec{c = A,a = B,b = C} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        #rec{c = A,b = B,a = C} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    end;
tests_1_2_7(A,<<B/binary>>,[C|_],D) -> true;
tests_1_2_7(A,<<B/binary>>,C,D=#rec{g=G}) -> true;
tests_1_2_7(A,<<B/binary>>,C,D) ->
    case true of
        #{a := A,b := B,c := C} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        #{a := A,c := B,b := C} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        #{b := A,a := B,c := C} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        #{b := A,c := B,a := C} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        #{c := A,a := B,b := C} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        #{c := A,b := B,a := C} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    end;
tests_1_2_7(A,B,[C|_],D=#rec{g=G}) -> true;
tests_1_2_7(A,B,[C|_],D) -> true;
tests_1_2_7(A,B,C,D=#rec{g=G}) ->
    case true of
        {{A,B,C}} when A < B,B < C -> non_existing_module:call_(A,B,C), non_existing_module:call_2(A,B,C);
        {[A,B,C]} when A < C,C < B -> non_existing_module:call__(A,B,C), non_existing_module:call__2(A,B,C);
        {#rec{a = A,b = B,c = C}} when B < A,A < C -> non_existing_module:call___(A,B,C), non_existing_module:call___2(A,B,C);
        {<<A>>,B,C} when B < C,C < A -> non_existing_module:call____(A,B,C), non_existing_module:call____2(A,B,C);
        {A,<<B>>,C} when C < A,A < B -> non_existing_module:call_____(A,B,C), non_existing_module:call_____2(A,B,C);
        {A,B,<<C>>} when C < B,B < A -> non_existing_module:call______(A,B,C), non_existing_module:call______2(A,B,C)
    end;
tests_1_2_7(A,B,C,D) -> true.

tests_1_2_8(A=#{key:=E},<<B/binary>>,[C|_],D=#rec{g=G}) when C =:= 1 -> true;
tests_1_2_8(A=#{key:=E},<<B/binary>>,[C|_],D) when true, C =:= 2 -> true;
tests_1_2_8(A=#{key:=E},<<B/binary>>,C,D=#rec{g=G}) when true, true, C =:= 3  -> true;
tests_1_2_8(A=#{key:=E},<<B/binary>>,C,D) when true, true, true, C =:= 4 -> true;
tests_1_2_8(A=#{key:=E},B,[C|_],D=#rec{g=G}) when true, true, true, true, C =:= 5 -> true;
tests_1_2_8(A=#{key:=E},B,[C|_],D) when true, true, true, true, true, C =:= 6 -> true;
tests_1_2_8(A=#{key:=E},B,C,D=#rec{g=G}) when true, true, true, true, true, true, C =:= 7 -> true;
tests_1_2_8(A=#{key:=E},B,C,D) when true, true, true, true, true, true, true, C =:= 8 -> true;
tests_1_2_8(A,<<B/binary>>,[C|_],D=#rec{g=G}) when C /= 1 -> true;
tests_1_2_8(A,<<B/binary>>,[C|_],D) when C /= 2; true -> true;
tests_1_2_8(A,<<B/binary>>,C,D=#rec{g=G}) when C /= 3; false; true -> true;
tests_1_2_8(A,<<B/binary>>,C,D) when C /= 4; false; false; true -> true;
tests_1_2_8(A,B,[C|_],D=#rec{g=G}) when C /= 5; false; false; false; true -> true;
tests_1_2_8(A,B,[C|_],D) when C /= 6; false; false; false; false; true -> true;
tests_1_2_8(A,B,C,D=#rec{g=G}) when C /= 7; false; false; false; false; false; true -> true;
tests_1_2_8(A,B,C,D) when C /= 8; false; false; false; false; false; false; false; true -> true.

tests_1_2_9(A=#{key:=E=#{key:=F}},<<B/binary>>,[[C|_]|_],D=#rec{g = G = #rec2{h=H}}) when C =:= 1 -> true;
tests_1_2_9(A=#{key:=E=#{key:=F}},<<B/binary>>,[[C|_]|_],D) when true andalso C =:= 2 -> true;
tests_1_2_9(A=#{key:=E=#{key:=F}},<<B/binary>>,C,D=#rec{g = G = #rec2{h=H}}) when true andalso true andalso C =:= 3  -> true;
tests_1_2_9(A=#{key:=E=#{key:=F}},<<B/binary>>,C,D) when true andalso true andalso true andalso C =:= 4 -> true;
tests_1_2_9(A=#{key:=E=#{key:=F}},B,[[C|_]|_],D=#rec{g = G = #rec2{h=H}}) when true andalso true andalso true andalso true andalso C =:= 5 -> true;
tests_1_2_9(A=#{key:=E=#{key:=F}},B,[[C|_]|_],D) when true, true andalso true andalso true andalso true andalso C =:= 6 -> true;
tests_1_2_9(A=#{key:=E=#{key:=F}},B,[C|_],D=#rec{g = G = #rec2{h=H}}) when true, true andalso true andalso true andalso true, true andalso C =:= 7 -> true;
tests_1_2_9(A=#{key:=E=#{key:=F}},B,[C|_],D) when true andalso true andalso true, true andalso true, true andalso true, C =:= 8 -> true;
tests_1_2_9(A,<<B/binary>>,[[C|_]|_],D=#rec{g = G = #rec2{h=H}}) when C /= 1 -> true;
tests_1_2_9(A,<<B/binary>>,[[C|_]|_],D) when C /= 2 orelse true -> true;
tests_1_2_9(A,<<B/binary>>,C,D=#rec{g = G = #rec2{h=H}}) when C /= 3; false orelse true -> true;
tests_1_2_9(A,<<B/binary>>,C,D) when C /= 4 orelse false; false orelse true -> true;
tests_1_2_9(A,B,[[C|_]|_],D=#rec{g = G = #rec2{h=H}}) when C /= 5; false orelse false; false orelse true -> true;
tests_1_2_9(A,B,[[C|_]|_],D) when C /= 6 orelse false orelse false; false; false orelse true -> true;
tests_1_2_9(A,B,C,D=#rec{g = G = #rec2{h=H}}) when C /= 7 orelse false; false; false; false orelse false orelse true -> true;
tests_1_2_9(A,B,C,D) when C /= 8 orelse false orelse false; false; false; false orelse false; false orelse true -> true.
