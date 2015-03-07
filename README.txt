
An Erlang preprocessor to provide a mechanism for user defined guards and pattern templates to facilitate pattern and guard bundle reuse and composition, written in pure Erlang.

Quick overview
==============

How the preprocessor works involves token rewriting, disjunction duplication and pattern argument attachments.

An example with simple guards:

    -module(example1a).
    -auto_include_udg([ is_ue_list_and_bin/2 ]).
    
    example(A,B,C)
      when true, C > 0, is_ue_list_and_bin(A,B);
           true, C < 0, true;
           true, C =:= 0, true -> ok.
    
With an ERG (ERlang Guard file) definition of:

    -user_defined_guard(list_and_bin, [ is_ue_list_and_bin/2 ]).
        
    is_ue_list_and_bin(A,B) when is_list(A) andalso is_binary(B).
    
The above expands to: 

    -module(example1a).
    
    example(A,B,C)
      when true, C > 0, is_list(A) andalso is_binary(B);
           true, C < 0, true;
           true, C =:= 0, true -> ok.

Since its a guard starting with "is_ue_", which its significance will be explained later, it can also be negated and nested:

    -module(example1b).
    -auto_include_udg([ is_ue_list_and_bin/2 ]).
    
    example(A,B,C)
      when true, C > 0, is_ue_list_and_bin(A,B);
           true, C < 0, not is_ue_list_and_bin(A,B) andalso is_tuple(A);
           true, C =:= 0, true -> ok.

Expands to:

    -module(example1b).
    
    example(A,B,C)
      when true, C > 0, is_list(A) andalso is_binary(B);
           true, C < 0, not (is_list(A) andalso is_binary(B)) also is_tuple(A);
           true, C =:= 0, true -> ok.

These user defined guards have a syntax reminiscent of erlang functions, or rather the pattern and guard portion without the sequential body:

    is_u_function_head(...) when <guard expressions>, ... := <guard expression> ;
    is_u_function_head(...) when <guard expressions>, ... := <guard expression> .

Now, lets say, we want to make the guards more tidy, we can combine the conjunction of the C variable comparisons and is_ue_list_and_bin(A,B) guards to yet more user defined guards.

We'll even switch the positions of the arguments in the second user defined guard and rename them in the definition to demonstrate the argument variable names doesn't impact how the user defined guard rewrites to the underlying expressions.

Now the example resembles:

    -module(example1b).
    -include_udg(example_guard, [ is_u_example_guard1/3, is_u_example_guard2/3 ]).

    example(A,B,C)
      when true, is_u_example_guard1(A,B,C);
           true, is_u_example_guard2(C,A,B);
           true, C =:= 0, true -> ok.

And ERG definitions:

    -user_defined_guard(example_guard, [ is_u_example_guard1/3, is_u_example_guard2/3 ]).
    -auto_include_udg([ is_ue_list_and_bin/2 ]).

    is_u_example_guard1(A,B,C) when C > 0, is_ue_list_and_bin(A,B).

    is_u_example_guard2(Num,G,H) when Num < 0, not is_ue_list_and_bin(G,H) andalso is_tuple(G).

Which again expands to the same as the previous example.

Now lets say we want to tidy the guards even further, then we could define yet another guard for all the disjunctions.

Now the example resembles:

    -module(example1b).
    -auto_include_udg([ is_u_example_test/3 ]).

    example(A,B,C) when true, is_u_example_test(A,B,C) -> ok.

With an ERG, the (:=) operator is used to delimit the "tail guard expression" and to be able to use the disjunction operator to form disjunction alternatives:

    -user_defined_guard(example_test, [ is_u_example_test/3 ]).
    -include_udg(example_guard, [ is_u_example_guard1/3, is_u_example_guard2/3 ]).

    is_u_example_test(A,B,C) := is_u_example_guard1(A,B,C);
    is_u_example_test(A,B,C) := is_u_example_guard2(C,A,B);
    is_u_example_test(A,B,C) when C =:= 0, true.

Again, it expands to the same as the previous two examples.


Now look to create a more elaborate set of matching conditions and how we can use the "is_up_" and "is_uep_" guard constructs to do it, these provide for more powerful matching, our example function with user defined guards also covers a few corner cases.

    -module(example2a).
    -auto_include_udg([ is_up_corner_cases_or_example_test/3 ]).

    example2(A,B,C) when is_up_corner_cases_or_example_test(A,B,C) -> ok.

With an ERG definition of:

    -user_defined_guard(corner_cases_or_example_test, [ is_up_corner_cases_or_example_test/3 ]).
    -auto_include_udg([ is_u_example_test/3 ]).

    is_up_corner_cases_or_example_test([{a,D}|_]=A,B,C) when
        is_integer(C), D /= any orelse C > 1000 := is_u_example_test(A,B,C);
    is_up_corner_cases_or_example_test([{b,[D]}|_]=A,B,C) when
        is_integer(C), D =:= any orelse C > 1000 := is_u_example_test(A,B,C);
    is_up_corner_cases_or_example_test(A,B,C) when
        true, is_u_example_test(A,B,C).

Now for the sake of completeness of the guard constructs being covered, we will tidy up the D comparisons into a "is_uep_" guard construct by itself, which results in is_up_corner_cases_or_example_test/3 being rewritten to the following:

    -user_defined_guard(corner_cases_or_example_test, [ is_up_corner_cases_or_example_test/3, is_uep_elem_condition/1 ]).
    -auto_include_udg([ is_u_example_test/3 ]).

    is_up_corner_cases_or_example_test([D0|_]=A,B,C) when
        is_integer(C), is_uep_elem_condition(D0) orelse C > 1000 := is_u_example_test(A,B,C);
    is_up_corner_cases_or_example_test(A,B,C) when
        true, is_u_example_test(A,B,C).

    is_uep_elem_condition({a,D}) := D /= any;
    is_uep_elem_condition({b,[D]}) when D =:= any.

Note that while "is_uep_" guard constructs can have disjunctions their definitions can't have comma separated conjunction sequences, only expressions with the andalso operator.

When a "is_uep_" guard is not nested to a andalso/orelse boolean expression it gets rewritten the same way as the "is_up_" construct, otherwise it expands to a "patterns or false" disjunction list.

Now given the following example:

    -module(example2b).
    -auto_include_udg([ is_up_corner_cases_or_example_test/3 ]).
    
    example2(none,_,none) -> ok;
    example2(A,B,C) when A =:= none; is_up_corner_cases_or_example_test(A,B,C) -> ok.

In steps, the preprocessor performs these token substitutions:

1) is_up_corner_cases_or_example_test/3 is expanded to two alternatives, since it attachs patterns to the left side the "A =:= none" condition is moved to a separate head.

    example2(none,_,none) -> ok;
    example2(A,B,C) when A =:= none -> ok;
    example2(A=[UDG_TMP_1|_],B,C) when is_integer(C), is_uep_elem_condition(UDG_TMP_1) orelse C > 1000, is_u_example_test(A,B,C) -> ok;
    example2(A,B,C) when true, is_u_example_test(A,B,C) -> ok.

2) is_uep_elem_condition/1 is expanded to three alternatives, the existing disjunctions are duplicated with different pattern possibilities and new pattern conditions merged to the left.

    example2(none,_,none) -> ok;
    example2(A,B,C) when A =:= none -> ok;
    example2(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, is_u_example_test(A,B,C) -> ok;
    example2(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, is_u_example_test(A,B,C) -> ok;
    example2(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, is_u_example_test(A,B,C) -> ok;
    example2(A,B,C) when true, is_u_example_test(A,B,C) -> ok.

3) is_u_example_test/3 is expanded to three possibilities for 4 of the existing possibilities.

    example2(none,_,none) -> ok;
    example2(A,B,C) when A =:= none -> ok;
    example2(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, is_u_example_guard1(A,B,C);
                                                     is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, is_u_example_guard2(C,A,B);
                                                     is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C =:= 0, true -> ok;
    example2(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, is_u_example_guard1(A,B,C);
                                                       is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, is_u_example_guard2(C,A,B);
                                                       is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C =:= 0, true -> ok;
    example2(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, is_u_example_guard1(A,B,C);
                                       is_integer(C), false orelse C > 1000, is_u_example_guard2(C,A,B);
                                       is_integer(C), false orelse C > 1000, C =:= 0, true -> ok;
    example2(A,B,C) when true, is_u_example_guard1(A,B,C);
                        true, is_u_example_guard2(C,A,B);
                        true, C =:= 0, true -> ok.

4) is_u_example_guard1/3 and is_u_example_guard2/3 are expanded as conjunctions.


    example2(none,_,none) -> ok;
    example2(A,B,C) when A =:= none -> ok;
    example2(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C > 0, is_ue_list_and_bin(A,B);
                                                     is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C < 0, not is_ue_list_and_bin(A,B) andalso is_tuple(A);
                                                     is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C =:= 0, true -> ok;
    example2(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C > 0, is_ue_list_and_bin(A,B);
                                                       is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C < 0, not is_ue_list_and_bin(A,B) andalso is_tuple(A);
                                                       is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C =:= 0, true -> ok;
    example2(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, C > 0, is_ue_list_and_bin(A,B);
                                       is_integer(C), false orelse C > 1000, C < 0, not is_ue_list_and_bin(A,B) andalso is_tuple(A);
                                       is_integer(C), false orelse C > 1000, C =:= 0, true -> ok;
    example2(A,B,C) when true, C > 0, is_ue_list_and_bin(A,B);
                        true, C < 0, not is_ue_list_and_bin(A,B) andalso is_tuple(A);
                        true, C =:= 0, true -> ok.

5) is_ue_list_and_bin/2 is expanded to an expression, the guard sequences are now parseable by the Erlang compiler.

    example2(none,_,none) -> ok;
    example2(A,B,C) when A =:= none -> ok;
    example2(A=[UDG_TMP_1={a,UDG_TMP_2}|_],B,C) when is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                     is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                     is_integer(C), (UDG_TMP_2 /= any) orelse C > 1000, C =:= 0, true -> ok;
    example2(A=[UDG_TMP_1={b,[UDG_TMP_2]}|_],B,C) when is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                                       is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                                       is_integer(C), (UDG_TMP_2 =:= any) orelse C > 1000, C =:= 0, true -> ok;
    example2(A=[UDG_TMP_1|_],B,C) when is_integer(C), false orelse C > 1000, C > 0, is_list(A) andalso is_binary(B);
                                       is_integer(C), false orelse C > 1000, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                                       is_integer(C), false orelse C > 1000, C =:= 0, true -> ok;
    example2(A,B,C) when true, C > 0, is_list(A) andalso is_binary(B);
                        true, C < 0, not (is_list(A) andalso is_binary(B)) andalso is_tuple(A);
                        true, C =:= 0, true -> ok.

Now another closely construct somewhat similar to the user defined guards is the template pattern construct.

Pattern templates provides a way to reuse patterns and associate some guard sequences with them:

    -module(example3).
    -auto_include_udg([ \\pattern1/2 ]).

    example3(A \\ pattern1(B,C)) ->
        ok.

This time a pattern template's definition goes by a variation of the user defined guard's syntax:

    (<pattern>) \\ pattern_template_name(<argument>,...) when <guard expressions>, ... := <guard expression> ;
    (<pattern>) \\ pattern_template_name(<argument>,...) when <guard expressions>, ... := <guard expression> .

These definitions always start with an opening parenthesis, and the pattern is always enclosed in parenthesis, and the closing parenthesis before the double back slashes that lead to the pattern template name and outward arguments.

With a few ERG definitions

    -user_defined_guard(patterns, [ \\ pattern1/2, \\pattern2/1, is_up_pattern_condition1/2 ]).

    (#{ a := A, b := B, condition := Condition }) \\ pattern1(A,B)
        when Condition > 500 := is_up_pattern_condition1(A,B);
    (#{ a := A, b := B, condition := Condition }) \\ pattern1_2(A,B)
        when Condition > 200, Condition < 300 andalso A /= true := is_up_pattern_condition1(A,B);
    (#{ a := A, b := B, condition := Condition }) \\ pattern1(A,B)
        when Condition > 100, is_up_pattern_condition1(A,B).

    ({tuple, A}) \\ pattern2(A) := A /= none;
    ({tuple, A}) \\ pattern2(A) when A /= none2.

    is_up_pattern_condition1(A \\ pattern2(C),B) := C =:= any;
    is_up_pattern_condition1(A \\ pattern2(C),B) when C =:= any2.

Expands in the following steps:

1) We start with the initial expression.

    example3(A \\ pattern1(B,C)) ->
        ok.

2) \\pattern1/2 (note: the double back slashes are used to distinguish the pattern template prototype in this case when referenced in directives) is expanded to its patterns and associated guards. Additional guards associated with a pattern template are always prepended to each of the disjunctions of the guard sequences.

    example3(A = #{ a := B, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_1 > 500, is_up_pattern_condition1(B,C) ->
        ok;
    example3(A = #{ a := B, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, is_up_pattern_condition1(B,C) ->
        ok;
    example3(A = #{ a := B, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_1 > 100, is_up_pattern_condition1(B,C) ->
        ok.

3) The user defined guard is_up_pattern_condition1/2 is expanded: 

    example3(A = #{ a := B = UDG_TMP_3 \\ pattern2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
        when UDG_TMP_1 > 500, UDG_TMP_2 =:= any ->
        ok;
    example3(A = #{ a := B = UDG_TMP_3 \\ pattern2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
        when UDG_TMP_1 > 500, UDG_TMP_2 =:= any2 ->
        ok;
    example3(A = #{ a := B = UDG_TMP_3 \\ pattern2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
        when UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any ->
        ok;
    example3(A = #{ a := B = UDG_TMP_3 \\ pattern2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
        when UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true UDG_TMP_2 =:= any2 ->
        ok;
    example3(A = #{ a := B = UDG_TMP_3 \\ pattern2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
        when UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
        ok;
    example3(A = #{ a := B = UDG_TMP_3 \\ pattern2(UDG_TMP_2), b := C, condition := UDG_TMP_1 })
        when UDG_TMP_1 > 100, UDG_TMP_2 =:= any2 ->
        ok.

4) \\pattern2/1 is expanded to its pattern, the function definitions are now parseable by the compiler.

    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none, UDG_TMP_1 > 500, UDG_TMP_2 =:= any ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none2, UDG_TMP_1 > 500, UDG_TMP_2 =:= any ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none, UDG_TMP_1 > 500, UDG_TMP_2 =:= any2 ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none2, UDG_TMP_1 > 500, UDG_TMP_2 =:= any2 ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none2, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any2 ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none2, UDG_TMP_1 > 200, UDG_TMP_1 < 300 andalso B /= true, UDG_TMP_2 =:= any2 ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none2, UDG_TMP_1 > 100, UDG_TMP_2 =:= any ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none, UDG_TMP_1 > 100, UDG_TMP_2 =:= any2 ->
        ok;
    example3(A = #{ a := B = UDG_TMP3 = {tuple, UDG_TMP_2}, b := C, condition := UDG_TMP_1 })
        when UDG_TMP_2 /= none2, UDG_TMP_1 > 100, UDG_TMP_2 =:= any2 ->
        ok.


Pattern templates also work with functions with disjunction of guard sequences.

    -module(example4).
    -auto_include_udg([ \\pattern4_2/3 ]).

    example4(_ \\ pattern4_2(A,B,C)) when B =:= 1; is_integer(B) -> ok.

ERG definitions:

    -user_defined_guard(patterns, [ \\pattern4_2/3, \\pattern5_2/6, \\pattern6_2/2 ]).

    (#{a := A \\ pattern5_2(C,D,E,F,G,H), b := B \\ pattern5_2(I,J,K,L,M,N)}) \\ pattern4_2(A,E,K)
        when N /= H, M /= G.
    
    (#{c := A \\ pattern6_2(C,D), d := B \\ pattern6_2(E,F)}) \\ pattern5_2(A,B,C,D,E,F)
        when A /= atom_5_2, not D =:= atom1_5_2.
    
    (#{e := A, f := B}) \\ pattern6_2(A,B)
        when A > 300, is_list(B).

The steps shown in this example is done from left to right of each pattern template encountered because guard prepending by pattern templates results in a sequence that would be differ than if we were to expand by one type of pattern template per step.

1) First we have the starting code.

    example4(_ \\ pattern4_2(A,B,C)) when B =:= 1; is_integer(B) -> ok.

2) pattern4_2/2 is expanded.

    example4(_ = #{a := A \\ pattern5_2(UDG_TMP_2,UDG_TMP_3,B,UDG_TMP_4,UDG_TMP_5,UDG_TMP_6), b := UDG_TMP_7 \\ pattern5_2(UDG_TMP_8,UDG_TMP_9,C,UDG_TMP_10,UDG_TMP_11,UDG_TMP_12)}) when
        UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, B =:= 1;
        UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, is_integer(B) -> ok.

3) The first pattern template encountered is the first pattern5_2/6 which is expanded:

    example4(_ = #{a := A = #{c := UDG_TMP_2 \\ pattern6_2(B,UDG_TMP_4), d := UDG_TMP_3 \\ pattern6_2(UDG_TMP_5,UDG_TMP_6)}, b := UDG_TMP_7 \\ pattern5_2(UDG_TMP_8,UDG_TMP_9,C,UDG_TMP_10,UDG_TMP_11,UDG_TMP_12)}) when
        UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, B =:= 1;
        UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, is_integer(B) -> ok.

4) Next are the two pattern6_2/2 pattern templates that are expanded in sequence from left to right.

    example4(_ = #{a := A = #{c := UDG_TMP_2 = #{e := B, f := UDG_TMP_4}, d := UDG_TMP_3 = #{e := UDG_TMP_5, f := UDG_TMP_6}}, b := UDG_TMP_7 \\ pattern5_2(UDG_TMP_8,UDG_TMP_9,C,UDG_TMP_10,UDG_TMP_11,UDG_TMP_12)}) when
        UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, B =:= 1;
        UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, is_integer(B) -> ok.

5) Next is the second pattern5_2/6 which is expanded:

    example4(_ = #{a := A = #{c := UDG_TMP_2 = #{e := B, f := UDG_TMP_4}, d := UDG_TMP_3 = #{e := UDG_TMP_5, f := UDG_TMP_6}}, b := UDG_TMP_7 = #{c := UDG_TMP_8 \\ pattern6_2(C,UDG_TMP_10), d := UDG_TMP_9 \\ pattern6_2(UDG_TMP_11,UDG_TMP_12)}}) when
        UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, B =:= 1;
        UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, is_integer(B) -> ok.

6) Finally, the last two pattern6_2/2 pattern templates are expanded, from left to right:

    example4(_ = #{a := A = #{c := UDG_TMP_2 = #{e := B, f := UDG_TMP_4}, d := UDG_TMP_3 = #{e := UDG_TMP_5, f := UDG_TMP_6}}, b := UDG_TMP_7 = #{c := UDG_TMP_8 = #{e := C, f := UDG_TMP_10}, d := UDG_TMP_9 = #{e := UDG_TMP_11, f := UDG_TMP_12}}}) when
        UDG_TMP_11 > 300, is_list(UDG_TMP_12), C > 300, is_list(UDG_TMP_10), UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, B =:= 1;
        UDG_TMP_11 > 300, is_list(UDG_TMP_12), C > 300, is_list(UDG_TMP_10), UDG_TMP_8 /= atom_5_2, not UDG_TMP_10 =:= atom1_5_2, UDG_TMP_5 > 300, is_list(UDG_TMP_6), B > 300, is_list(UDG_TMP_4), UDG_TMP_2 /= atom_5_2, not UDG_TMP_4 =:= atom1_5_2, UDG_TMP_12 /= UDG_TMP_6, UDG_TMP_11 /= UDG_TMP_5, is_integer(B) -> ok.


Types of constructs provided
============================

In this scheme the user defined guards:

* are specified in .erg files.
* are designed in such a way it can be implemented as a preprocessor over the current syntax.
* emphasize an application-centric approach to reusable guard sequences and patterns.
* provide directives for documenting type associations outside the application scope for static analysis tools.

Five types of new constructs are provided, where 4 are variations of each other:

* is_u_*/* functions
* is_up_*/* functions
* is_ue_*/* functions
* is_uep_*/* functions
* Pattern templates

The four guard function-like constructs are the result of divisions where there are tradeoffs between matching versatility and composable flexibility,

* is_u_*/* and is_ue_*/* functions can be used in "if" expression guard sequences (is_up_*/* and is_uep_*/* functions can't be used in these).
* is_up_*/* and is_uep_*/* can match to patterns in their definitions, unlike a pattern template these functions can test relations between arbitrarily given arguments in the guarded parent function.
* is_ue_*/* and is_uep_*/* can be nested within a boolean expression using orelse/andalso.
* is_u_*/* and is_up_*/* can have comma separated conjunction sequences.

The pattern template construct is closely related in mechanism but is used as a composable reusable pattern mnemonic to provide variables to the function body.

On negation (complement):

* is_up_*/* and is_uep_*/* cannot be negated.
* is_u_*/* cannot be negated at this time, some instances could be eventually implemented.
* is_ue_*/* can be negated, and so does not provide top level sequence disjunctions in its definition.

On composition:

* Pattern templates can contain pattern templates, is_up_*/*, is_uep_*/*, is_u_*/* and is_ue_*/*.
* is_up_* functions can contain pattern templates, is_up_*/*, is_uep_*/*, is_u_*/* and is_ue_*/*.
* is_uep_* functions can contain pattern templates, is_uep_*/* and is_ue_*/*.
* is_u_* functions can contain is_u_* and is_ue_*/*.
* is_ue_* functions can contain is_ue_*/*.

Pattern templates and the is_up_*/* and is_uep_*/* functions are very similar in that they augment the pattern matching portion of the clause and add to the guard sequence, the difference lies in that pattern templates can provide more variables to the clause body from one argument whereas is_up_* hides these variables and can take several arguments to test relations between them.

    example(UUID) when is_up_uuid(UUID) -> etc.
    example(UUID \\ uuid(Part1,Part2,Part3,Part4,Part5)) ->
        io:format("~w", [Part5]).

An \\ operator was opted for because the feature of hiding variables makes the pattern template a many-to-one relation as opposed to a one-to-one relation offered by a pattern match and all its variable bindings.

An example of pattern augmentation by is_up_* functions.

    A when is_up_a(A).
    A=[Hidden1|Hidden2] when is_up_b(Hidden1).
    A=[Hidden1=<<Hidden3,Hidden4>>|Hidden2] when true.

An example of pattern augmentation by \\ pattern templates.

    Digits \\ test_tpl(A,B)
    Digits=<<A:8/bytes,"-0", B:8/bytes>>

    Rec \\ test_rec(A,B)
    Rec=#rec{a = A, b = B, c = Hidden1} when is_list(B), is_binary(Hidden1)
    
    Rec \\ test_rec(A,[C|B])
    Rec=#rec{a = A, b = [C|B], c = Hidden1} when is_list([C|B]), is_binary(Hidden1)
    
    Rec \\ test_rec(A,C=[D|_])
    Rec=#rec{a = A, b = Hidden1=C=[D|_], c = Hidden2} when is_list(Hidden1), is_binary(Hidden2)

Example usage of pattern template:

    example(UUID \\ displayable_uuid(_Part1,_Part2,_Part3,_Part4,_Part5)) ->
        io:format("UUID: ~s~n", [UUID]).

Source files for definitions
============================

Default paths that are checked are "../udg" and "udg", additional paths are added with -adda_udg_path(String) and -addz_udg_path(String).

To both facilitate cross module definitions and not burden the main erlang language with more syntactical forms, a new separate source file is used with the extension .erg to contain the user defined guard and pattern template definitions.

Each erg file has the general format of:

    -user_defined_guards(filename, [ is_u_example/1, is_up_example/1, \\example/0 ]).
    
    is_u_example(...) when <conjunctions> := <tail-clause>;
    is_u_example(...) when <conjunctions>;
     ...
    is_u_example(...) when <conjunctions>.
    
    is_up_example(...) when <conjunctions>.
    
    (<pattern>) \\ example1(...).

    (<pattern>) \\ example2(...) when <conjunctions> := <tail-clause>;
    (<pattern>) \\ example2(...) when <conjunctions>.

A few optional directives found in the .erg file:

    -instance(([...],[...],[...]) -> ...).
    -for_application(sets).

The tail expression contains the same as any other expression in between top level conjunction operators, with the exception that conjunction operators cannot be found on the right side of the tail expression operator.

The tail expression operator allows the programmer to avoid unintended evaluation order as commas are very commonly used in Erlang source between lines, as well as to disambiguate that the next disjunction operator as a full disjunction with new patterns.


Notes on disjunctions in user defined guards
============================================

Care should be taken when using the disjunction ability and try using orelse expressions after enough conjunctive guards are provided to ensure unintended exception effects, due to possibility of up to 2^n terms from exponential effects of the converting to disjunctive normal form.

    [[ab, cd]] -> [[a,cd],[b,cd]] -> [[a,c],[a,d],[b,c],[b,d]].

is_uep_*/* can be found in nested expressions involving orelse if the disjunction is duplicated to itself and false:

    examp(A) when is_up_test(A) orelse true -> ....

Expands to:

    examp(A=[B|_]) when is_list(B) orelse true -> ...;
    examp(A) when false orelse true -> ....

