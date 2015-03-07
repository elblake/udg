
%%
%% tests_1 process file 10
%%

-module(tests_1_10).

-auto_include_udg([ is_up_pattern_condition1_2/2 ]).
-auto_include_udg([ \\ pattern1_2/2, \\ pattern2_2/1 ]).

-include_udg(patterns456_2, [ \\ pattern4_2/3, \\ pattern5_2/6, \\ pattern6_2/2 ]).


example3a(A = #{ a := B \\ pattern2_2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 500, UDG_TMP_2 =:= any ->
    ok;
example3a(A = #{ a := B \\ pattern2_2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 500, UDG_TMP_2 =:= any2 ->
    ok;
example3a(A = #{ a := B \\ pattern2_2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any ->
    ok;
example3a(A = #{ a := B \\ pattern2_2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any2 ->
    ok;
example3a(A = #{ a := B \\ pattern2_2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok;
example3a(A = #{ a := B \\ pattern2_2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 100, UDG_TMP_2 =:= any2 ->
    ok.

example3b(A = #{ a := B, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 500, is_up_pattern_condition1_2(B,C) ->
    ok;
example3b(A = #{ a := B, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, is_up_pattern_condition1_2(B,C) ->
    ok;
example3b(A = #{ a := B, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 100, is_up_pattern_condition1_2(B,C) ->
    ok.

example3c(A \\ pattern1_2(B,C)) ->
    ok.

example_only_pattern6_2(_ = #{a := A = #{c := UDG_TMP_2 \\ pattern6_2(B,UDG_TMP_4), d := UDG_TMP_3 \\ pattern6_2(UDG_TMP_5,UDG_TMP_6)}, b := UDG_TMP_7 = #{c := UDG_TMP_8 \\ pattern6_2(C,UDG_TMP_10), d := UDG_TMP_9 \\ pattern6_2(UDG_TMP_11,UDG_TMP_12)}})
    when
        UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, B =:= 1;
        UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, is_integer(B)
    -> ok.

example4b(_ = #{a := A \\ pattern5_2(UDG_TMP_2,UDG_TMP_3,B,UDG_TMP_4,UDG_TMP_5,UDG_TMP_6), b := UDG_TMP_7 \\ pattern5_2(UDG_TMP_8,UDG_TMP_9,C,UDG_TMP_10,UDG_TMP_11,UDG_TMP_12)})
    when
        UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, B =:= 1;
        UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, is_integer(B)
    -> ok.

example4c(_ \\ pattern4_2(A,B,C)) when B =:= 1; is_integer(B) -> ok.
