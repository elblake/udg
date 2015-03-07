
%%
%% tests_1 process file 8
%%

-module(tests_1_8).

-auto_include_udg([ is_u_example_guard1/3, is_u_example_guard2/3, is_u_example_test/3 ]).
-auto_include_udg([ is_ue_list_and_bin/2, is_uep_elem_condition/1 ]).
-auto_include_udg([ is_up_corner_cases_or_example_test/3 ]).

example1a(A,B,C)
  when true, C > 0, is_ue_list_and_bin(A,B);
       true, C < 0, true;
       true, C =:= 0, true -> ok.

example1b1(A,B,C)
  when true, C > 0, is_ue_list_and_bin(A,B);
       true, C < 0, not is_ue_list_and_bin(A,B) andalso is_tuple(A);
       true, C =:= 0, true -> ok.

example1b2(A,B,C)
  when true, is_u_example_guard1(A,B,C);
       true, is_u_example_guard2(C,A,B);
       true, C =:= 0, true -> ok.

example1b3(A,B,C) when true, is_u_example_test(A,B,C) -> ok.


example2a(none,_,none) -> ok;
example2a(A,B,C) when A =:= none -> ok;
example2a(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C > 0, is_ue_list_and_bin(A,B);
                                                  is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C < 0, not is_ue_list_and_bin(A,B) andalso is_tuple(A);
                                                  is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C =:= 0, true -> ok;
example2a(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C > 0, is_ue_list_and_bin(A,B);
                                                    is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C < 0, not is_ue_list_and_bin(A,B) andalso is_tuple(A);
                                                    is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C =:= 0, true -> ok;
example2a(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, C > 0, is_ue_list_and_bin(A,B);
                                    is_integer(C), false orelse C > 1000, C < 0, not is_ue_list_and_bin(A,B) andalso is_tuple(A);
                                    is_integer(C), false orelse C > 1000, C =:= 0, true -> ok;
example2a(A,B,C) when true, C > 0, is_ue_list_and_bin(A,B);
                      true, C < 0, not is_ue_list_and_bin(A,B) andalso is_tuple(A);
                      true, C =:= 0, true -> ok.

                      
example2b(none,_,none) -> ok;
example2b(A,B,C) when A =:= none -> ok;
example2b(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, is_u_example_guard1(A,B,C);
                                                  is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, is_u_example_guard2(C,A,B);
                                                  is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C =:= 0, true -> ok;
example2b(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, is_u_example_guard1(A,B,C);
                                                    is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, is_u_example_guard2(C,A,B);
                                                    is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C =:= 0, true -> ok;
example2b(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, is_u_example_guard1(A,B,C);
                                    is_integer(C), false orelse C > 1000, is_u_example_guard2(C,A,B);
                                    is_integer(C), false orelse C > 1000, C =:= 0, true -> ok;
example2b(A,B,C) when true, is_u_example_guard1(A,B,C);
                      true, is_u_example_guard2(C,A,B);
                      true, C =:= 0, true -> ok.


example2c(none,_,none) -> ok;
example2c(A,B,C) when A =:= none -> ok;
example2c(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, is_u_example_test(A,B,C) -> ok;
example2c(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, is_u_example_test(A,B,C) -> ok;
example2c(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, is_u_example_test(A,B,C) -> ok;
example2c(A,B,C) when true, is_u_example_test(A,B,C) -> ok.


example2d(none,_,none) -> ok;
example2d(A,B,C) when A =:= none -> ok;
example2d(A=[UDG_TMP_1|_],B,C) when is_integer(C), is_uep_elem_condition(UDG_TMP_1) orelse C > 1000, is_u_example_test(A,B,C) -> ok;
example2d(A,B,C) when true, is_u_example_test(A,B,C) -> ok.


example2e(none,_,none) -> ok;
example2e(A,B,C) when A =:= none; is_up_corner_cases_or_example_test(A,B,C) -> ok.
