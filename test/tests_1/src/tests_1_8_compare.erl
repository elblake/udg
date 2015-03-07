
%%
%% tests_1 compare file 8
%%

-module(tests_1_8).

example1a(A,B,C)
  when true, C > 0, is_list(A) andalso is_binary(B);
       true, C < 0, true;
       true, C =:= 0, true -> ok.

example1b1(A,B,C)
  when true, C > 0, is_list(A) andalso is_binary(B);
       true, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
       true, C =:= 0, true -> ok.

example1b2(A,B,C)
  when true, C > 0, is_list(A) andalso is_binary(B);
       true, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
       true, C =:= 0, true -> ok.

example1b3(A,B,C)
  when true, C > 0, is_list(A) andalso is_binary(B);
       true, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
       true, C =:= 0, true -> ok.


example2a(none,_,none) -> ok;
example2a(A,B,C) when A =:= none -> ok;
example2a(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                 is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                 is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C =:= 0, true -> ok;
example2a(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                   is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                   is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C =:= 0, true -> ok;
example2a(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                   is_integer(C), false orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                   is_integer(C), false orelse C > 1000, C =:= 0, true -> ok;
example2a(A,B,C) when true, C > 0, is_list(A) andalso is_binary(B);
                    true, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                    true, C =:= 0, true -> ok.


example2b(none,_,none) -> ok;
example2b(A,B,C) when A =:= none -> ok;
example2b(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                 is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                 is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C =:= 0, true -> ok;
example2b(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                   is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                   is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C =:= 0, true -> ok;
example2b(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                   is_integer(C), false orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                   is_integer(C), false orelse C > 1000, C =:= 0, true -> ok;
example2b(A,B,C) when true, C > 0, is_list(A) andalso is_binary(B);
                    true, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                    true, C =:= 0, true -> ok.


example2c(none,_,none) -> ok;
example2c(A,B,C) when A =:= none -> ok;
example2c(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                 is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                 is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C =:= 0, true -> ok;
example2c(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                   is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                   is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C =:= 0, true -> ok;
example2c(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                   is_integer(C), false orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                   is_integer(C), false orelse C > 1000, C =:= 0, true -> ok;
example2c(A,B,C) when true, C > 0, is_list(A) andalso is_binary(B);
                    true, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                    true, C =:= 0, true -> ok.


example2d(none,_,none) -> ok;
example2d(A,B,C) when A =:= none -> ok;
example2d(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                 is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                 is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C =:= 0, true -> ok;
example2d(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                   is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                   is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C =:= 0, true -> ok;
example2d(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                   is_integer(C), false orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                   is_integer(C), false orelse C > 1000, C =:= 0, true -> ok;
example2d(A,B,C) when true, C > 0, is_list(A) andalso is_binary(B);
                    true, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                    true, C =:= 0, true -> ok.


example2e(none,_,none) -> ok;
example2e(A,B,C) when A =:= none -> ok;
example2e(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                 is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                 is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C =:= 0, true -> ok;
example2e(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                   is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                   is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C =:= 0, true -> ok;
example2e(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                   is_integer(C), false orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                   is_integer(C), false orelse C > 1000, C =:= 0, true -> ok;
example2e(A,B,C) when true, C > 0, is_list(A) andalso is_binary(B);
                    true, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                    true, C =:= 0, true -> ok.


