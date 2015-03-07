
%%
%% tests_1 process file 9
%%

-module(tests_1_9).

-auto_include_udg([ is_up_pattern_condition1_1/2, \\pattern1_1/2, \\pattern2_1/1 ]).

example3a(A = #{ a := B \\ pattern2_1(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok.

example3b(A = #{ a := B, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_1 > 100, is_up_pattern_condition1_1(B,C) ->
    ok.

example3c(A \\ pattern1_1(B,C)) ->
    ok.
