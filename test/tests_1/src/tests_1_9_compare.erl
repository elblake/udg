
%%
%% tests_1 compare file 9
%%

-module(tests_1_9).
%
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok.

example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok.

example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok.


