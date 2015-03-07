
%%
%% tests_1 compare file 10
%%

-module(tests_1_10).

example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 500, UDG_TMP_2 =:= any ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 500, UDG_TMP_2 =:= any ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 500, UDG_TMP_2 =:= any2 ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 500, UDG_TMP_2 =:= any2 ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any2 ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any2 ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any2 ->
    ok;
example3a(A = #{ a := B = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 100, UDG_TMP_2 =:= any2 ->
    ok.

example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 500, UDG_TMP_2 =:= any ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 500, UDG_TMP_2 =:= any ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 500, UDG_TMP_2 =:= any2 ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 500, UDG_TMP_2 =:= any2 ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any2 ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any2 ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any2 ->
    ok;
example3b(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 100, UDG_TMP_2 =:= any2 ->
    ok.

example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 500, UDG_TMP_2 =:= any ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 500, UDG_TMP_2 =:= any ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 500, UDG_TMP_2 =:= any2 ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 500, UDG_TMP_2 =:= any2 ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any2 ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any2 ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any2 ->
    ok;
example3c(A = #{ a := B = UDG_TMP_3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
    when UDG_TMP_2 /= none2, UDG_TMP_1 > 100, UDG_TMP_2 =:= any2 ->
    ok.

example_only_pattern6_2(_ = #{a := A = #{c := UDG_TMP_2 = #{e := B, f := UDG_TMP_4}, d := UDG_TMP_3 = #{e := UDG_TMP_5, f := UDG_TMP_6}}, b := UDG_TMP_7 = #{c := UDG_TMP_8 = #{e := C, f := UDG_TMP_10}, d := UDG_TMP_9 = #{e := UDG_TMP_11, f := UDG_TMP_12}}})
    when
        UDG_TMP_11 > 300, is_list(UDG_TMP_12), C > 300, is_list(UDG_TMP_10), UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, B =:= 1;
        UDG_TMP_11 > 300, is_list(UDG_TMP_12), C > 300, is_list(UDG_TMP_10), UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, is_integer(B)
    -> ok.

example4b(_ = #{a := A = #{c := UDG_TMP_2 = #{e := B, f := UDG_TMP_4}, d := UDG_TMP_3 = #{e := UDG_TMP_5, f := UDG_TMP_6}}, b := UDG_TMP_7 = #{c := UDG_TMP_8 = #{e := C, f := UDG_TMP_10}, d := UDG_TMP_9 = #{e := UDG_TMP_11, f := UDG_TMP_12}}})
    when
        UDG_TMP_11 > 300, is_list(UDG_TMP_12), C > 300, is_list(UDG_TMP_10), UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, B =:= 1;
        UDG_TMP_11 > 300, is_list(UDG_TMP_12), C > 300, is_list(UDG_TMP_10), UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, is_integer(B)
    -> ok.

example4c(_ = #{a := A = #{c := UDG_TMP_2 = #{e := B, f := UDG_TMP_4}, d := UDG_TMP_3 = #{e := UDG_TMP_5, f := UDG_TMP_6}}, b := UDG_TMP_7 = #{c := UDG_TMP_8 = #{e := C, f := UDG_TMP_10}, d := UDG_TMP_9 = #{e := UDG_TMP_11, f := UDG_TMP_12}}})
    when
        UDG_TMP_11 > 300, is_list(UDG_TMP_12), C > 300, is_list(UDG_TMP_10), UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, B =:= 1;
        UDG_TMP_11 > 300, is_list(UDG_TMP_12), C > 300, is_list(UDG_TMP_10), UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, is_integer(B)
    -> ok.
