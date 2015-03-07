
%%
%% udg_tests_b: Test application to generate arbitrary Erlang source code
%%              with user defined guards for preprocessing and comparison.
%%
%% * Generate files of 20 functions each with up to 3 levels of blocks
%%   nested inside each other.
%%
%% * An empty directory as the first argument is required.
%%
%% * If given a seed number, generate the random sequence for that seed,
%%   otherwise, determine and indicate a random seed number and generate from
%%   that seed.
%%
%% Copyright 2015 Edward L. Blake (edwardlblake @ gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
-module(udg_tests_b).
-description('User defined guards tests randomly generated').
-author('Edward L. Blake < edwardlblake at gmail.com >').

-export([ t/2, run/0 ]).

-type tokens() :: [tuple()].

run() ->
    case init:get_plain_arguments() of
        [] -> show_info();
        Arguments ->
            {ok, InputDir, Init} = get_args(Arguments),
            t(InputDir, Init)
    end,
    erlang:halt().

show_info() ->
    io:format("erl -pa ../ebin -p ebin -noshell -run udg_tests_b run -- <outdir> <arguments>~n",[]),
    io:format("~n",[]),
    io:format("  udg_tests_b generates random source code files for testing the UDG~n",[]),
    io:format("  preprocessor.~n",[]),
    io:format("~n",[]),
    io:format("  <outdir> : Output directory for generated test source files.~n",[]),
    io:format("  <arguments> can be optionally the following:~n",[]),
    io:format("    -init <numbers> : Triple of numbers separated by dashes to initialize the~n",[]),
    io:format("                      PRNG.~n",[]),
    io:format("~n",[]).

get_args(Args) ->
    get_args(Args,none,none).
get_args(["-init",Init|Args],Dir,none) ->
    get_args(Args,Dir,Init);
get_args([InputDir|Args],none,Init) ->
    case file:read_file_info(InputDir) of
        {ok, {file_info,_,directory,_,_,_,_,_,_,_,_,_,_,_}} ->
            get_args(Args,InputDir,Init);
        _ ->
            error(directory_doesnt_exist_or_not_given)
    end;
get_args([],Dir,Init) ->
    {ok, Dir, Init}.

load_tokens(File) ->
    case file:read_file(File) of
        {ok, String} -> 
            case erl_scan:string(binary_to_list(String)) of
                {ok, Tokens, _} -> Tokens;
                _ -> error(cant_scan_tokens_of_test_file)
            end;
        _ -> error(cant_read_test_file_for_tokens)
    end.


-spec compare_tokens/2
    :: (tokens(), tokens()) ->
        same | {different, {tuple() | atom(), tuple() | atom()}, {integer(), integer()}, tokens(), tokens()}.
compare_tokens([{T0,_,T2}|Tokens1], [{T02,_,T22}|Tokens2]) when T0 =:= T02, T2 =:= T22 ->
    compare_tokens(Tokens1, Tokens2);
compare_tokens([{var,T1,T2}|Tokens1], [{var,T12,T22}|Tokens2]) ->
    case {atom_to_list(T2), atom_to_list(T22)} of
        {[$T,$M,$P,$_,$_,$U,$D,$G,$_|_], [$T,$M,$P,$_,$_,$U,$D,$G,$_|_]} ->
            compare_tokens(Tokens1, Tokens2);
        _ ->
            {different, {{var, T2},{var,T22}}, {T1,T12}, Tokens1, Tokens2}
    end;
compare_tokens([{T0,_}|Tokens1], [{T02,_}|Tokens2]) when T0 =:= T02 ->
    compare_tokens(Tokens1, Tokens2);
compare_tokens([{T0,T1,T2}|Tokens1], [{T02,T12,T22}|Tokens2]) ->
    {different, {{T0, T2},{T02,T22}}, {T1,T12}, Tokens1, Tokens2};
compare_tokens([{T0,T1}|Tokens1], [{T02,T12}|Tokens2]) ->
    {different, {T0,T02}, {T1,T12}, Tokens1, Tokens2};
compare_tokens([], []) ->
    same.


-spec generate/2 :: (string(), {integer(),integer(),integer()}) -> boolean().
generate(OutputDir, {I1,I2,I3}=PRNGInit) when is_list(OutputDir), is_integer(I1), is_integer(I2), is_integer(I3) ->
    random:seed(PRNGInit),
    UpTo = 1,
    generate(OutputDir, PRNGInit, 1, UpTo, []).
generate(OutputDir, _PRNGInit, FileNum, UpTo, Files) when FileNum > UpTo ->
    io:format("\n",[]),
    io:format("tests_b: Comparing each file generated.~n",[]),
    test_each_file(OutputDir, lists:reverse(Files));
generate(OutputDir, PRNGInit, FileNum, UpTo, Files) when FileNum =< UpTo, FileNum > 0 ->
    if (FileNum rem 10) == 0 -> io:format("~w... ", [FileNum]); true -> ok end,
    FileNumPadded = string:right(integer_to_list(FileNum),4,$0),
    File = "file" ++ FileNumPadded ++ ".erl",
    CompareFile = "file" ++ FileNumPadded ++ ".erl_compare",
    case file:open(filename:join([OutputDir, File]), [write, exclusive, raw, delayed_write]) of
        {ok, Handle} ->
            case file:open(filename:join([OutputDir, CompareFile]), [write, exclusive, raw, delayed_write]) of
                {ok, HandleC} ->
                    ok = file_write_both(Handle, HandleC, <<"% This file was automatically generated with udg_tests_b.erl\n\n">>),
                    ok = file_write_both(Handle, HandleC, lists:append(["-module(file", FileNumPadded , ").\n\n"])),
                    ok = file:write(Handle, "-auto_include_udg([ is_ue_example/1, \\\\example_pattern1/1, \\\\example_pattern2/1, is_up_example1/1, is_uep_example1/1, is_up_example2/1, is_uep_example2/1 ]).\n\n"),
                    lists:foreach(
                        fun(SeqNum) ->
                            generate_function(Handle, HandleC, SeqNum)
                        end, lists:seq(1, 20)),
                    ok = file:close(HandleC),
                    ok = file:close(Handle),
                    generate(OutputDir, PRNGInit, FileNum+1, UpTo, [{File,CompareFile}|Files])
            end
    end.
    
generate_function(Handle, HandleC, SeqNum) ->
    FunName = "fun" ++ string:right(integer_to_list(SeqNum),4,$_),
    Arity = random:uniform(6),
    generate_function(Handle, HandleC, FunName, Arity, random:uniform(5)+1, []).
generate_function(Handle, HandleC, _FunName, _Arity, 0, Funs) ->
    generate_function_output(Handle, HandleC, lists:reverse(Funs));
generate_function(Handle, HandleC, FunName, Arity, Left, Funs) when Left > 0 ->
    case generate_variation(Arity, 3) of
        {ok_w_disj, FunListWUDG, CompareList} ->
            MoreFuns = {before_and_after_simple_body,
                lists:map(arg_guards_body_with_name(FunName, {atom, ok}), FunListWUDG),
                lists:map(arg_guards_body_with_name(FunName, {atom, ok}), CompareList) },
            generate_function(Handle, HandleC, FunName, Arity, Left-1, [MoreFuns|Funs]);
        {ok_n_disj, FunListWUDG, CompareList, Body} ->
            MoreFuns = {before_and_after_no_disj, FunName,
                FunListWUDG, CompareList, Body},
            generate_function(Handle, HandleC, FunName, Arity, Left-1, [MoreFuns|Funs])
    end.

arg_guards_body_with_name(FunName, Body) ->
    fun({Args, Guards}) -> {FunName, Args, Guards, Body} end.

generate_variation(Arity, Level) ->
    case random:uniform(5) of
        1 -> nothing,
            {ok, Args, [Args], Variables, [], [[]]} = generate_arguments(Arity, []),
            {ok, GuardsBefore, [GuardsAfter]} = generate_guards(Variables, any, [], [[]]),
            {ok, Body} = generate_function_body(Variables, Level),
            {ok_n_disj, {Args, GuardsBefore}, {Args, GuardsAfter}, Body};
        
        2 -> pattern_template,
            {ok, ArgsBefore, [ArgsAfter], Variables, [], [ExtraGuardsAfter]} = generate_arguments(Arity, [pattern1]),
            {ok, GuardsBefore, [GuardsAfter]} = generate_guards(Variables, any, [], [ExtraGuardsAfter]),
            {ok, Body} = generate_function_body(Variables, Level),
            {ok_n_disj, {ArgsBefore, GuardsBefore}, {ArgsAfter, GuardsAfter}, Body};
        3 -> is_u,
            {ok, Args, [Args], Variables, [], [[]]} = generate_arguments(Arity, []),
            ExtraGuardsBefore = [],
            ExtraGuardsAfter = [],
            {ok, GuardsBefore, [GuardsAfter]} = generate_guards(Variables, any, ExtraGuardsBefore, [ExtraGuardsAfter]),
            {ok, Body} = generate_function_body(Variables, Level),
            {ok_n_disj, {Args, GuardsBefore}, {Args, GuardsAfter}, Body};
        4 -> is_up,
            {ok, ArgsBefore, [ArgsAfter], Variables, ExtraGuardsBefore, [ExtraGuardsAfter]} = generate_arguments(Arity, [up1]),
            {ok, GuardsBefore, [GuardsAfter]} = generate_guards(Variables, 1, ExtraGuardsBefore, [ExtraGuardsAfter]),
            {ok, Body} = generate_function_body(Variables, Level),
            {ok_n_disj, {ArgsBefore, GuardsBefore}, {ArgsAfter, GuardsAfter}, Body};
        5 -> is_uep,
            {ok, ArgsBefore, [ArgsAfter], Variables, ExtraGuardsBefore, [ExtraGuardsAfter]} = generate_arguments(Arity, [uep1]),
            {ok, GuardsBefore, [GuardsAfter]} = generate_guards(Variables, 1, ExtraGuardsBefore, [ExtraGuardsAfter]),
            {ok, Body} = generate_function_body(Variables, Level),
            {ok_n_disj, {ArgsBefore, GuardsBefore}, {ArgsAfter, GuardsAfter}, Body};
            
        6 -> pattern_template,
            {ok, ArgsBefore, ArgsAfterDisjs, Variables, [], ExtraGuardsAfterDisjs} = generate_arguments(Arity, [pattern2]),
            {ok, GuardsBefore, GuardsAfterDisjs} = generate_guards(Variables, any, [], ExtraGuardsAfterDisjs),
            {ok_w_disj, [{ArgsBefore, GuardsBefore}], args_and_guards(ArgsAfterDisjs,GuardsAfterDisjs)};
        7 -> is_u,
            {ok, Args, ArgsDisjs, Variables, [], _} = generate_arguments(Arity, []),
            ExtraGuardsBefore = [],
            ExtraGuardsAfterDisj = [[]],
            {ok, GuardsBefore, GuardsAfterDisj} = generate_guards(Variables, any, ExtraGuardsBefore, ExtraGuardsAfterDisj),
            {ok_w_disj, [{Args, GuardsBefore}], args_and_guards(ArgsDisjs, GuardsAfterDisj)};
        8 -> is_up,
            {ok, ArgsBefore, ArgsAfterDisjs, Variables, ExtraGuardsBefore, ExtraGuardsAfterDisjs} = generate_arguments(Arity, [up2]),
            {ok, GuardsBefore, GuardsAfterDisjs} = generate_guards(Variables, 1, ExtraGuardsBefore, ExtraGuardsAfterDisjs),
            {ok_w_disj, [{ArgsBefore, GuardsBefore}], args_and_guards(ArgsAfterDisjs,GuardsAfterDisjs)};
        9 -> is_uep,
            {ok, ArgsBefore, ArgsAfterDisjs, Variables, ExtraGuardsBefore, ExtraGuardsAfterDisjs} = generate_arguments(Arity, [uep2]),
            {ok, GuardsBefore, GuardsAfterDisjs} = generate_guards(Variables, 1, ExtraGuardsBefore, ExtraGuardsAfterDisjs),
            {ok_w_disj, [{ArgsBefore, GuardsBefore}], args_and_guards(ArgsAfterDisjs, GuardsAfterDisjs)}
    end.

args_and_guards(ArgsAfterDisjs, GuardsAfterDisjs) ->
    args_and_guards(ArgsAfterDisjs, GuardsAfterDisjs,[]).
args_and_guards([ArgsAfter|ArgsAfterDisjs],[GuardsAfter|GuardsAfterDisjs], List) ->
    args_and_guards(ArgsAfterDisjs, GuardsAfterDisjs, [{ArgsAfter,GuardsAfter}|List]);
args_and_guards([],[], List) ->
    lists:reverse(List).


generate_arguments(Arity, IncludeVariation) ->
    generate_arguments(Arity, IncludeVariation, [], [[]], [], generate_variables_names(100), [], [[]]).
generate_arguments(0, [], ArgsSoFarBefore, ArgsSoFarAfterDisjs, VarsSoFar, _NewVarsList, GuardsSoFarBefore, GuardsSoFarAfterDisjs) ->
    {ok, lists:reverse(ArgsSoFarBefore),
         [lists:reverse(ArgsSoFarAfter) || ArgsSoFarAfter <- ArgsSoFarAfterDisjs], VarsSoFar,
         lists:append(lists:reverse(GuardsSoFarBefore)),
         [lists:append(lists:reverse(GuardsSoFarAfter)) || GuardsSoFarAfter <- GuardsSoFarAfterDisjs]};
generate_arguments(Left, [], ArgsSoFarBefore, ArgsSoFarAfterDisjs, VarsSoFar, [VarName|NewVarsList], GuardsSoFarBefore, GuardsSoFarAfterDisjs) when Left > 0 ->
    Arg = {var, VarName},
    NewVars = [VarName],
    generate_arguments(Left-1, [], [Arg|ArgsSoFarBefore],
        [[Arg|ArgsSoFarAfter] || ArgsSoFarAfter <- ArgsSoFarAfterDisjs],
        ordsets:union(NewVars,VarsSoFar), NewVarsList, GuardsSoFarBefore, GuardsSoFarAfterDisjs);
generate_arguments(Left, [Variation|Rest]=IncludeVariation, ArgsSoFarBefore, ArgsSoFarAfterDisjs, VarsSoFar, [VarName|NewVarsList], GuardsSoFarBefore, GuardsSoFarAfterDisjs) when Left > 0 ->
    case random:uniform(2) of
        N when Left =< 1 orelse N =:= 1 ->
            NewVars = [VarName],
            case Variation of
                pattern1 ->
                    [VarName2|NewVarsList2] = NewVarsList,
                    ArgBefore = {pattern_template_application, {var, VarName}, example_pattern1, [{var, VarName2}]},
                    ArgAfter = {eq, {var, VarName}, {list,[{var,VarName2}]}},
                    GuardAfter = [{atom,true}],
                    generate_arguments(Left-1, Rest, [ArgBefore|ArgsSoFarBefore],
                        [[ArgAfter|ArgsSoFarAfter] || ArgsSoFarAfter <- ArgsSoFarAfterDisjs],
                        ordsets:union([[VarName2],NewVars,VarsSoFar]), NewVarsList2,
                        GuardsSoFarBefore,
                        [[GuardAfter | GuardsSoFarAfter] || GuardsSoFarAfter <- GuardsSoFarAfterDisjs]);
                pattern2 ->
                    [VarName2|NewVarsList2] = NewVarsList,
                    ArgBefore = {pattern_template_application, {var, VarName}, example_pattern2, [{var, VarName2}]},
                    ArgAfter = [
                        {eq, {var, VarName}, {list,[{var,VarName2}]}},
                        {eq, {var, VarName}, {tuple,[{atom,tuple},{var,VarName2}]}} ],
                    GuardAfter = [[{atom,true}],[{atom,true}]],
                    generate_arguments(Left-1, Rest, [ArgBefore|ArgsSoFarBefore],
                        adjust_disjs(ArgAfter, ArgsSoFarAfterDisjs),
                        ordsets:union([[VarName2],NewVars,VarsSoFar]), NewVarsList2,
                        GuardsSoFarBefore,
                        adjust_disjs(GuardAfter, GuardsSoFarAfterDisjs));
                up1 ->
                    VarName2 = new_temp_var(),
                    VarName3 = new_temp_var(),
                    ArgBefore = {var, VarName},
                    ArgAfter = {eq, {var, VarName}, {tuple, [{var, VarName2},{var, VarName3}]}},
                    GuardBefore = [{f, is_up_example1, [{var, VarName}]}],
                    GuardAfter = [{f, is_binary, [{var, VarName2}]},{f, is_list, [{var, VarName3}]}   ],
                    generate_arguments(Left-1, Rest, [ArgBefore|ArgsSoFarBefore],
                        [[ArgAfter|ArgsSoFarAfter] || ArgsSoFarAfter <- ArgsSoFarAfterDisjs],
                        ordsets:union(NewVars,VarsSoFar), NewVarsList,
                        [GuardBefore | GuardsSoFarBefore],
                        [[GuardAfter | GuardsSoFarAfter] || GuardsSoFarAfter <- GuardsSoFarAfterDisjs]);
                up2 ->
                    VarName2 = new_temp_var(),
                    VarName3 = new_temp_var(),
                    ArgBefore = {var, VarName},
                    ArgAfter = [
                        {eq, {var, VarName}, {tuple, [{atom, up1},{var, VarName2},{var, VarName3}]}},
                        {eq, {var, VarName}, {tuple, [{atom, up2},{var, VarName2},{var, VarName3}]}}],
                    GuardBefore = [{f, is_up_example2, [{var, VarName}]}],
                    GuardAfter = [
                        [{f, is_binary, [{var, VarName2}]},{f,is_list,[{var, VarName3}]}],
                        [{op, [{var, VarName2}, '>', {integer, 20}]}, {f, is_list, [{var, VarName3}]}]],
                    generate_arguments(Left-1, Rest, [ArgBefore|ArgsSoFarBefore],
                        adjust_disjs(ArgAfter, ArgsSoFarAfterDisjs),
                        ordsets:union(NewVars,VarsSoFar), NewVarsList,
                        [GuardBefore | GuardsSoFarBefore],
                        adjust_disjs(GuardAfter, GuardsSoFarAfterDisjs));
                uep1 ->
                    VarName2 = new_temp_var(),
                    ArgBefore = {var, VarName},
                    ArgAfter = {eq, {var, VarName}, {tuple, [{atom,uep}, {var, VarName2}]}},
                    GuardBefore = [{f, is_uep_example1, [{var, VarName}]}],
                    GuardAfter = [{f, is_binary, [{var, VarName2}]}],
                    generate_arguments(Left-1, Rest, [ArgBefore|ArgsSoFarBefore],
                        [[ArgAfter|ArgsSoFarAfter] || ArgsSoFarAfter <- ArgsSoFarAfterDisjs],
                        ordsets:union(NewVars,VarsSoFar), NewVarsList,
                        [GuardBefore | GuardsSoFarBefore],
                        [[GuardAfter | GuardsSoFarAfter] || GuardsSoFarAfter <- GuardsSoFarAfterDisjs]);
                uep2 ->
                    VarName2 = new_temp_var(),
                    ArgBefore = {var, VarName},
                    ArgAfter = [
                        {eq, {var, VarName}, {tuple, [{atom, uep1}, {var, VarName2}]}},
                        {eq, {var, VarName}, {tuple, [{atom, uep2}, {var, VarName2}]}}],
                    GuardBefore = [{f, is_uep_example2, [{var, VarName}]}],
                    GuardAfter = [
                        [{f, is_binary, [{var, VarName2}]}],
                        [{op, [{var, VarName2}, '>', {integer, 10}]}]],
                    generate_arguments(Left-1, Rest, [ArgBefore|ArgsSoFarBefore],
                        adjust_disjs(ArgAfter, ArgsSoFarAfterDisjs),
                        ordsets:union(NewVars,VarsSoFar), NewVarsList,
                        [GuardBefore | GuardsSoFarBefore],
                        adjust_disjs(GuardAfter, GuardsSoFarAfterDisjs))
            end;
        _ ->
            Arg = {var, VarName},
            NewVars = [VarName],
            generate_arguments(Left-1, IncludeVariation, [Arg|ArgsSoFarBefore],
                [[Arg|ArgsSoFarAfter] || ArgsSoFarAfter <- ArgsSoFarAfterDisjs],
                ordsets:union(NewVars,VarsSoFar), NewVarsList,
                GuardsSoFarBefore, GuardsSoFarAfterDisjs)
    end.

adjust_disjs([_A]=After, [_A2]=SoFarAfterDisjs) ->
    [[After|SoFarAfter] || SoFarAfter <- SoFarAfterDisjs];
adjust_disjs(AfterDisjs, [SoFarAfter]=_SoFarAfterDisjs) ->
    [[After|SoFarAfter] || After <- AfterDisjs];
adjust_disjs(_, [_|Rest]) when length(Rest) > 0 ->
    error(cannot_generate_consistent_disjunctions).

new_temp_var() ->
    U = random:uniform(1000000000),
    "TMP__UDG_" ++ integer_to_list(U).


generate_variables_names(N) ->
    generate_variables_names(N, $A, 0, [], []).
generate_variables_names(0, _Letter, _Number, _MoreLetters, VarsSoFar) -> lists:reverse(VarsSoFar);
generate_variables_names(N, Letter, Number, MoreLetters, VarsSoFar) when N > 0 ->
    NewVar =
        if
            Number > 0 -> [Letter|integer_to_list(Number)] ++ MoreLetters;
            true -> [Letter | MoreLetters]
        end,
    case random:uniform(10) of
        10 when Letter =:= $Z -> generate_variables_names(N-1, $A, 0, NewVar, [NewVar|VarsSoFar]);
        10 when Letter  /= $Z -> generate_variables_names(N-1, Letter+1, 0, MoreLetters, [NewVar|VarsSoFar]);
        _ -> generate_variables_names(N-1, Letter, Number+1, MoreLetters, [NewVar|VarsSoFar])
    end.


generate_guards([], _MaxDisjs, [], ExtraGuardsAfterDisjs) ->
    {ok, [], ExtraGuardsAfterDisjs};
generate_guards(Variables, MaxDisjs, ExtraGuardsBefore, ExtraGuardsAfterDisjs) ->
    DisjLeft = random:uniform(case MaxDisjs of N when is_integer(N) -> N; _ -> 5 end),
    ConjLeft = random:uniform(5)+1,
    EmptyDisjsSoFarAfter = [[] || _ <- ExtraGuardsAfterDisjs],
    generate_guards(Variables, MaxDisjs, ExtraGuardsBefore, ExtraGuardsAfterDisjs, DisjLeft, ConjLeft, [], EmptyDisjsSoFarAfter, []).
generate_guards(_Variables, _MaxDisjs, _ExtraGuardsBefore, _ExtraGuardsAfterDisjs, 0, _, [], [[]], []) ->
    {ok, [], [[]]};
generate_guards(_Variables, _MaxDisjs, _ExtraGuardsBefore, _ExtraGuardsAfterDisjs, 0, _, DisjSoFarBefore, DisjSoFarAfterDisjs, []) ->
    {ok, DisjSoFarBefore, DisjSoFarAfterDisjs};
generate_guards(_Variables, _MaxDisjs, ExtraGuardsBefore, ExtraGuardsAfterDisjs, 0, _, DisjSoFarBefore, DisjSoFarAfterList, ConjSoFar) when ConjSoFar /= [] ->
    {ok, [{disj, ExtraGuardsBefore ++ lists:reverse(ConjSoFar)} | DisjSoFarBefore],
         for_each_guard_disjs_in_list(ExtraGuardsAfterDisjs, ConjSoFar, DisjSoFarAfterList)};
generate_guards(Variables, MaxDisjs, ExtraGuardsBefore, ExtraGuardsAfterDisjs, DisjLeft, ConjLeft, DisjSoFarBefore, DisjSoFarAfterList, ConjSoFar) ->
    if
        ConjLeft > 0 ->
            Guard = generate_guard_expression(5, Variables),
            generate_guards(Variables, MaxDisjs, ExtraGuardsBefore, ExtraGuardsAfterDisjs, DisjLeft,
                ConjLeft-1, DisjSoFarBefore, DisjSoFarAfterList, [Guard|ConjSoFar]);
        DisjLeft > 0, ConjSoFar /= [] ->
            generate_guards(Variables, MaxDisjs, ExtraGuardsBefore, ExtraGuardsAfterDisjs, DisjLeft-1,
                random:uniform(10)+1,
                [{disj, ExtraGuardsBefore ++ lists:reverse(ConjSoFar)} | DisjSoFarBefore],
                for_each_guard_disjs_in_list(ExtraGuardsAfterDisjs, ConjSoFar, DisjSoFarAfterList),
                [])
    end.


for_each_guard_disjs_in_list(ExtraGuardsAfterDisjs, ConjSoFar, DisjSoFarAfterList) ->
    for_each_guard_disjs_in_list(ExtraGuardsAfterDisjs, ConjSoFar, DisjSoFarAfterList, []).
for_each_guard_disjs_in_list([ExtraGuardsAfter|ExtraGuardsAfterDisjs], ConjSoFar, [DisjSoFarAfter|DisjSoFarAfterList], DisjsList) ->
    for_each_guard_disjs_in_list(ExtraGuardsAfterDisjs, ConjSoFar, DisjSoFarAfterList,
        [ [{disj, ExtraGuardsAfter  ++ lists:reverse(ConjSoFar)} | DisjSoFarAfter] | DisjsList]);
for_each_guard_disjs_in_list([], _, [], DisjsList) ->
    lists:reverse(DisjsList).


%generate_guard_expression(0, _Variables) ->
%    {atom, true};
generate_guard_expression(Level, Variables) -> % when Level > 0 ->
    generate_guard_expression_1(Level, Variables).
    
maybe_paren(Expr) ->
    case random:uniform(10) of
        N when N > 8 -> {paren, Expr};
        _ -> Expr
    end.

generate_guard_expression_1(0, _Variables) ->
    {atom, true};
generate_guard_expression_1(Level, Variables) when Level > 0 ->
    case random:uniform(102) of
         1 -> {atom, true};
         2 -> {op, [
            maybe_paren(generate_guard_expression_2(Level-1, Variables)),
            case random:uniform(8) of
                1 -> '==';
                2 -> '/=';
                3 -> '=<';
                4 -> '<';
                5 -> '>=';
                6 -> '>';
                7 -> '=:=';
                8 -> '=/='
            end,
            maybe_paren(generate_guard_expression_2(Level-1, Variables))]};
         3 -> {op, [
            maybe_paren(generate_guard_expression_1(Level-1, Variables)),
            case random:uniform(2) of
                1 -> 'andalso';
                2 -> 'orelse'
            end,
            maybe_paren(generate_guard_expression_1(Level-1, Variables))]};
         N when N > 3, N < 80 -> {f,
            case random:uniform(14) of
                1 -> is_atom;
                2 -> is_binary;
                3 -> is_bitstring;
                4 -> is_boolean;
                5 -> is_float;
                6 -> is_function;
                7 -> is_integer;
                8 -> is_list;
                9 -> is_map;
                10 -> is_number;
                11 -> is_pid;
                12 -> is_port;
                13 -> is_reference;
                14 -> is_tuple
            end, [generate_guard_expression_var(Variables)]};
        N when N >= 80, N < 100 ->
            V = generate_guard_expression_var(Variables),
            {ue_udg_f, is_ue_example, [V], [ {f, is_list, [V]} ]};
        100 -> {f, is_function, [generate_guard_expression_var(Variables), {number, random:uniform(10)}]};
        101 -> {f, is_record, [generate_guard_expression_var(Variables), {atom, etc}]};
        102 -> {f, is_record, [generate_guard_expression_var(Variables), {atom, etc}, {number, random:uniform(10)} ]}
    end.
generate_guard_expression_2(0, Variables) ->
    generate_guard_expression_var(Variables);
generate_guard_expression_2(Level, Variables) when Level > 0 ->
    case random:uniform(2) of
         1 -> {atom, example_atom};
         2 -> {number, random:uniform(1000)};
         3 -> {op, [
            maybe_paren(generate_guard_expression_2(Level-1, Variables)),
            case random:uniform(2) of
                 1 -> '+';
                 2 -> '-';
                 3 -> '/';
                 4 -> '*';
                 5 -> 'div';
                 6 -> 'rem';
                 7 -> 'and';
                 8 -> 'or';
                 9 -> 'band';
                10 -> 'bor';
                11 -> 'bxor';
                12 -> 'bsl';
                13 -> 'bsr';
                14 -> 'xor'
            end,
            maybe_paren(generate_guard_expression_2(Level-1, Variables))]};
         4 -> generate_guard_expression_var(Variables)
    end.

generate_guard_expression_var([VarName|_]=_Variables) ->
    {var, VarName}.

generate_function_body(Variables, Level) ->
    {ok, generate_function_body_expr(Variables, Level)}.
generate_function_body_expr(Variables, Level) ->
    Left = random:uniform(5),
    generate_function_body_expr(Variables, Level, Left, []).
generate_function_body_expr(_Variables, 0, _, []) ->
    [{atom, true}];
    
generate_function_body_expr(_Variables, _Level, 0, ExprSoFar) ->
    lists:reverse(ExprSoFar);
generate_function_body_expr(Variables, Level, Left, ExprSoFar) when Level > 0 ->
    generate_function_body_expr(Variables, Level, Left-1,
        [generate_function_body_expr_1(Variables, Level)|ExprSoFar]).
generate_function_body_expr_1(_Variables, 0) ->
    {atom, true};
generate_function_body_expr_1(Variables, Level) when Level > 0 ->
    case random:uniform(10) of
        1 -> {'begin', generate_function_body_expr(Variables, Level-1)};
        2 -> expr_function(Variables, Level-1, 0);
        3 -> expr_function(Variables, Level-1, 1);
        4 -> expr_function(Variables, Level-1, 2);
        5 -> expr_function(Variables, Level-1, 3);
        6 -> expr_function(Variables, Level-1, 4);
        7 -> expr_function(Variables, Level-1, 5);
        8 -> {'case',
                generate_function_body_expr_1(Variables, Level-1),
                generate_case_disjs(Variables, Level-1)};
        9 -> {'receive', generate_receive_disjs(Variables, Level-1), none};
        10 -> {'fun', generate_fun_disjs(Variables, Level-1)}
    end.


generate_case_disjs(Variables, Level) when Level >= 0 ->
    Arity = random:uniform(6),
    generate_case_disjs(Variables, Level, Arity, random:uniform(5)+1, []).
generate_case_disjs(_Variables, _Level, _Arity, 0, Funs) ->
    lists:reverse(Funs);
generate_case_disjs(Variables, Level, Arity, Left, Funs) when Left > 0 ->
    case generate_variation(Arity, Level) of
        {ok_w_disj, FunListWUDG, CompareList} ->
            MoreFuns = {before_and_after_simple_body,
                lists:map(arg_guards_body_with_name('-', {atom, ok}), FunListWUDG),
                lists:map(arg_guards_body_with_name('-', {atom, ok}), CompareList) },
            generate_case_disjs(Variables, Level, Arity, Left-1, [MoreFuns|Funs]);
        {ok_n_disj, FunListWUDG, CompareList, Body} ->
            MoreFuns = {before_and_after_no_disj, '-',
                FunListWUDG, CompareList, Body},
            generate_case_disjs(Variables, Level, Arity, Left-1, [MoreFuns|Funs])
    end.


generate_receive_disjs(Variables, Level) when Level >= 0 ->
    Arity = random:uniform(6),
    generate_receive_disjs(Variables, Level, Arity, random:uniform(5)+1, []).
generate_receive_disjs(_Variables, _Level, _Arity, 0, Funs) ->
    lists:reverse(Funs);
generate_receive_disjs(Variables, Level, Arity, Left, Funs) when Left > 0 ->
    case generate_variation(Arity, Level) of
        {ok_w_disj, FunListWUDG, CompareList} ->
            MoreFuns = {before_and_after_simple_body,
                lists:map(arg_guards_body_with_name('-', {atom, ok}), FunListWUDG),
                lists:map(arg_guards_body_with_name('-', {atom, ok}), CompareList) },
            generate_receive_disjs(Variables, Level, Arity, Left-1, [MoreFuns|Funs]);
        {ok_n_disj, FunListWUDG, CompareList, Body} ->
            MoreFuns = {before_and_after_no_disj, '-',
                FunListWUDG, CompareList, Body},
            generate_receive_disjs(Variables, Level, Arity, Left-1, [MoreFuns|Funs])
    end.


generate_fun_disjs(Variables, Level) when Level >= 0 ->
    Arity = random:uniform(6),
    generate_fun_disjs(Variables, Level, Arity, random:uniform(5)+1, []).
generate_fun_disjs(_Variables, _Level, _Arity, 0, Funs) ->
    lists:reverse(Funs);
generate_fun_disjs(Variables, Level, Arity, Left, Funs) when Left > 0 ->
    case generate_variation(Arity, Level) of
        {ok_w_disj, FunListWUDG, CompareList} ->
            MoreFuns = {before_and_after_simple_body,
                lists:map(arg_guards_body_with_name('-', {atom, ok}), FunListWUDG),
                lists:map(arg_guards_body_with_name('-', {atom, ok}), CompareList) },
            generate_fun_disjs(Variables, Level, Arity, Left-1, [MoreFuns|Funs]);
        {ok_n_disj, FunListWUDG, CompareList, Body} ->
            MoreFuns = {before_and_after_no_disj, '-',
                FunListWUDG, CompareList, Body},
            generate_fun_disjs(Variables, Level, Arity, Left-1, [MoreFuns|Funs])
    end.

expr_function(Variables, Level, N) ->
    {f, expr_module_name(), expr_function_name(), expr_function_args(Variables, Level, N)}.
expr_function_args(_,_,0) -> [];
expr_function_args(Variables, Level, N) when N > 0 ->
    [generate_function_body_expr_1(Variables, Level) | expr_function_args(Variables, Level, N-1)].

expr_module_name() ->
    case random:uniform(10) of
        1 -> m1;
        2 -> m2;
        3 -> m3;
        4 -> mod4;
        5 -> mod5;
        6 -> mod6;
        7 -> module7;
        8 -> module8;
        9 -> module9;
        10 -> example_module
    end.
    
expr_function_name() ->
    case random:uniform(10) of
        1 -> fun1;
        2 -> fun2;
        3 -> fun3;
        4 -> function4;
        5 -> function5;
        6 -> function6;
        7 -> example_function7;
        8 -> example_function8;
        9 -> example_function9;
        10 -> example_function
    end.

%% generate_function_output(_Handle, _HandleC, []) -> ok;
generate_function_output(Handle, HandleC, Data) ->
    generate_output_semicolon_list(Handle, HandleC, Data, fun(Variation) ->
        generate_function_output_functions(Handle, HandleC, Variation)
    end),
    ok = file_write_both(Handle, HandleC, ".\n\n"),
    ok.
%%    generate_function_output(Handle, HandleC, []);
%%generate_function_output(Handle, HandleC, [Fun|Funs]) ->
%%    ok = generate_function_output_functions(Handle, HandleC, Fun),
%%    ok = file_write_both(Handle, HandleC, ";\n"),
%%    generate_function_output(Handle, HandleC, Funs).


file_write_both(Handle, HandleC, Data) ->
    ok = file:write(Handle, Data),
    ok = file:write(HandleC, Data),
    ok.


generate_function_output_functions(Handle, HandleC, FunctionData) ->
    generate_function_output_variations_with(Handle, HandleC, 0, FunctionData, top).


generate_function_output_args(_Write, []) ->
    ok;
generate_function_output_args(Write, [Arg]) when is_function(Write) ->
    ok = generate_function_output_argument(Write, Arg),
    ok;
generate_function_output_args(Write, [Arg|RestArgs]) when is_function(Write) ->
    ok = generate_function_output_argument(Write, Arg),
    ok = Write(", "),
    generate_function_output_args(Write, RestArgs).

generate_function_output_guards(_Write, _Type, []) ->
    ok;
generate_function_output_guards(Write, Type, [{disj, Conjunctions}]) when is_function(Write) ->
    ok = generate_function_output_guard_list(Write, Type, Conjunctions),
    ok;
generate_function_output_guards(Write, Type, [{disj, Conjunctions}|RestGuards]) when is_function(Write) ->
    ok = generate_function_output_guard_list(Write, Type, Conjunctions),
    ok = Write("; "),
    generate_function_output_guards(Write, Type, RestGuards).


generate_function_output_simple_body(Write, [BodyExpr]) when is_function(Write) ->
    case BodyExpr of
        {'atom', Atom} -> Write(io_lib:print(Atom))
    end.

generate_function_output_body(Handle, HandleC, [Expr], Indent) ->
    ok = file_write_indent(fun(Data) -> file_write_both(Handle, HandleC, Data) end, Indent),
    generate_function_output_body_expr(Handle, HandleC, Expr, Indent);
generate_function_output_body(Handle, HandleC, [Expr|RestBody], Indent) ->
    Write = fun(Data) -> file_write_both(Handle, HandleC, Data) end,
    ok = file_write_indent(Write, Indent),
    ok = generate_function_output_body_expr(Handle, HandleC, Expr, Indent),
    ok = Write(",\n"),
    generate_function_output_body(Handle, HandleC, RestBody, Indent).
generate_function_output_body_expr(Handle, HandleC, BodyExpr, Indent) ->
    Write = fun(Data) -> file:write(Handle, Data), file:write(HandleC, Data) end,
    case BodyExpr of
        {'atom', Atom} -> Write(io_lib:print(Atom));
        {'begin', Exprs} ->
            ok = Write("begin\n"),
            ok = generate_function_output_body(Handle, HandleC, Exprs, Indent+1),
            ok = Write("\n"),
            ok = file_write_indent(Write, Indent),
            ok = Write("end"),
            ok;
        {'case', Expr, Disjs} ->
            ok = Write("case "),
            ok = generate_function_output_body_expr(Handle, HandleC, Expr, Indent+1),
            ok = Write(" of\n"),
            ok = generate_function_output_cases(Handle, HandleC, Indent, Disjs),
            ok = file_write_indent(Write, Indent),
            ok = Write("end"),
            ok;
        {'receive', Disjs, none} ->
            ok = Write("receive\n"),
            ok = generate_function_output_receives(Handle, HandleC, Indent, Disjs),
            ok = file_write_indent(Write, Indent),
            ok = Write("end"),
            ok;
        {'fun', Disjs} ->
            ok = Write("fun\n"),
            ok = generate_function_output_funs(Handle, HandleC, Indent, Disjs),
            ok = file_write_indent(Write, Indent),
            ok = Write("end"),
            ok;
        {f, ModName, FunName, Exprs} ->
            ok = Write(lists:append([atom_to_list(ModName), ":", atom_to_list(FunName), "("])),
            ok = Write(")"),
            ok
    end.
    

generate_function_output_cases(Handle, HandleC, Indent, Data) ->
    generate_output_semicolon_list(Handle, HandleC, Data, fun(Variation) ->
        generate_function_output_variations_with(Handle, HandleC, Indent, Variation, no_parens)
    end).
generate_function_output_receives(Handle, HandleC, Indent, Data) ->
    generate_output_semicolon_list(Handle, HandleC, Data, fun(Variation) ->
        generate_function_output_variations_with(Handle, HandleC, Indent, Variation, no_parens)
    end).
generate_function_output_funs(Handle, HandleC, Indent, Data) ->
    generate_output_semicolon_list(Handle, HandleC, Data, fun(Variation) ->
        generate_function_output_variations_with(Handle, HandleC, Indent, Variation, parens_no_name)
    end).


generate_output_semicolon_list(_Handle, _HandleC, [Variation], Fun) ->
    ok = Fun(Variation),
    ok;
generate_output_semicolon_list(Handle, HandleC, [Variation|Variations], Fun) ->
    ok = Fun(Variation),
    ok = file_write_both(Handle, HandleC, ";\n"),
    generate_output_semicolon_list(Handle, HandleC, Variations, Fun).

    
generate_function_output_variations_with(Handle, HandleC, Indent, {before_and_after_no_disj, FunName, ArgGuardsBefore, ArgGuardsAfter, Body}, Type) ->
    {ArgsBefore, GuardsBefore}=ArgGuardsBefore,
    {ArgsAfter, GuardsAfter}=ArgGuardsAfter,
    WriteBefore = fun(Data) -> file:write(Handle, Data) end,
    WriteAfter = fun(Data) -> file:write(HandleC, Data) end,
    ok = file_write_indent(WriteBefore, Indent),
    ok = file_write_indent(WriteAfter, Indent),
    ok = case Type of
        top -> file_write_both(Handle, HandleC, FunName ++ "(");
        parens_no_name -> file_write_both(Handle, HandleC, "(");
        no_parens -> ok
    end,
    ok = generate_function_output_args(WriteBefore, ArgsBefore),
    ok = generate_function_output_args(WriteAfter, ArgsAfter),
    ok = case Type of
        top -> file_write_both(Handle, HandleC, ") ");
        parens_no_name -> file_write_both(Handle, HandleC, ") ");
        no_parens -> file_write_both(Handle, HandleC, " ")
    end,
    case GuardsBefore of
        [] -> ok;
        _ ->
            ok = WriteBefore("when "),
            ok = generate_function_output_guards(WriteBefore, 'before', GuardsBefore)
    end,
    case GuardsAfter of
        [] -> ok;
        _ ->
            ok = WriteAfter("when "),
            ok = generate_function_output_guards(WriteAfter, 'compare', GuardsAfter)
    end,
    ok = file_write_both(Handle, HandleC, " ->\n"),
    ok = generate_function_output_body(Handle, HandleC, Body, Indent+1),
    ok;
generate_function_output_variations_with(Handle, HandleC, Indent, {before_and_after_simple_body, FunNameArgsGuardsBodyBefore, FunNameArgsGuardsBodyAfter}, Type) ->
    lists:map(fun(FunNameArgsGuardsBody) ->
            ok = generate_function_output_variation(fun(Data) -> file:write(Handle, Data) end,
                Indent, 'before', FunNameArgsGuardsBody, Type)
        end, FunNameArgsGuardsBodyBefore),
    lists:map(fun(FunNameArgsGuardsBody) ->
            ok = generate_function_output_variation(fun(Data) -> file:write(HandleC, Data) end,
                Indent, 'compare', FunNameArgsGuardsBody, Type)
        end, FunNameArgsGuardsBodyAfter),
    ok.
generate_function_output_variation(Write, Indent, Type, {FunName, Args, Guards, Body}, FunType) when is_function(Write) ->
    ok = file_write_indent(Write, Indent),
    ok = case FunType of
        top -> Write(FunName ++ "(");
        parens_no_name -> Write("(");
        no_parens -> ok
    end,
    ok = generate_function_output_args(Write, Args),
    ok = case FunType of
        top -> Write(") ");
        parens_no_name -> Write(") ");
        no_parens -> Write(" ")
    end,
    case Guards of
        [] -> ok;
        _ ->
            ok = Write("when "),
            ok = generate_function_output_guards(Write, Type, Guards)
    end,
    ok = Write(" -> "),
    ok = generate_function_output_simple_body(Write, Body),
    ok.


file_write_indent(Write, Indent) ->
    Write(lists:append(lists:duplicate(Indent, "  "))).

generate_function_output_argument(Write, ArgumentExpr) ->
    case ArgumentExpr of
        {var, VarName} -> Write(lists:append([" ", VarName, " "]));
        {integer, Integer} -> Write(lists:append([" ", io_lib:print(Integer), " "]));
        {atom, Atom} -> Write(lists:append([" ", io_lib:print(Atom), " "]));
        {string, String} -> Write(lists:append([" ", io_lib:print(String), " "]));
        {eq, Expr1, Expr2} ->
            ok = generate_function_output_argument(Write, Expr1),
            ok = Write(" = "),
            ok = generate_function_output_argument(Write, Expr2),
            ok;
        {pattern_template_application, LeftArg, PatternTemplateName, ListArgs} ->
            ok = generate_function_output_argument(Write, LeftArg),
            ok = Write(lists:append([" \\\\", io_lib:print(PatternTemplateName), "("])),
            ok = generate_function_output_args(Write, ListArgs),
            ok = Write(") "),
            ok;
        {list,ListArgs} ->
            ok = Write("["),
            ok = generate_function_output_args(Write, ListArgs),
            ok = Write("]"),
            ok;
        {tuple,ListArgs} ->
            ok = Write("{"),
            ok = generate_function_output_args(Write, ListArgs),
            ok = Write("}"),
            ok
    end.

generate_function_output_guard_list(Write, Type, [Expr]) ->
    ok = generate_function_output_guard_expr(Write, Type, Expr),
    ok;
generate_function_output_guard_list(Write, Type, [Expr|Conjunctions]) ->
    ok = generate_function_output_guard_expr(Write, Type, Expr),
    ok = Write(", "),
    generate_function_output_guard_list(Write, Type, Conjunctions).
    
generate_function_output_guard_expr(Write, Type, {f, FunName, Args}) ->
    ok = Write(atom_to_list(FunName) ++ "("),
    ok = generate_function_output_guard_expr_commas(Write, Type, Args),
    ok = Write(")"),
    ok;
generate_function_output_guard_expr(Write, Type, {ue_udg_f, FunName, Args, [AfterExpansion]}) ->
    case Type of
        'before' ->
            ok = Write([$ ,atom_to_list(FunName)] ++ "("),
            ok = generate_function_output_guard_expr_commas(Write, Type, Args),
            ok = Write(") andalso true "),
            ok;
        'compare' ->
            ok = Write("("),
            generate_function_output_guard_expr(Write, Type, AfterExpansion),
            ok = Write(") andalso true "),
            ok
    end;
generate_function_output_guard_expr(Write, Type, {op, Exprs}) ->
    ok = generate_function_output_guard_expr_op(Write, Type, Exprs),
    ok;
generate_function_output_guard_expr(Write, Type, Expr) ->
    case Expr of
        {var, VarName} -> Write(VarName);
        {atom, Atom} -> Write(io_lib:print(Atom));
        {paren, Expr2} ->
            ok = Write("("),
            ok = generate_function_output_guard_expr(Write, Type, Expr2),
            ok = Write(")"),
            ok;
        {number, Number} -> Write(io_lib:print(Number))
    end.

generate_function_output_guard_expr_op(Write, Type, [Expr]) ->
    ok = generate_function_output_guard_expr(Write, Type, Expr),
    ok;
generate_function_output_guard_expr_op(Write, Type, [Expr,OpAtom|Exprs]) when is_atom(OpAtom) ->
    ok = generate_function_output_guard_expr(Write, Type, Expr),
    ok = Write(operator_to_string(OpAtom)),
    generate_function_output_guard_expr_op(Write, Type, Exprs).

operator_to_string(OpAtom) ->
    case OpAtom of
        '==' -> " == ";
        '/=' -> " /= ";
        '=<' -> " =< ";
        '<'  -> " < ";
        '>=' -> " >= ";
        '>'  -> " > ";
        '=:=' -> " =:= ";
        '=/=' -> " =/= ";
        'andalso' -> " andalso ";
        'orelse' -> " orelse ";
        '+' -> " + ";
        '-' -> " - ";
        '/' -> " / ";
        '*' -> " * ";
        'div' -> " div ";
        'rem' -> " rem ";
        'and' -> " and ";
        'or'  -> " or ";
        'band' -> " band ";
        'bor'  -> " bor ";
        'bxor' -> " bxor ";
        'bsl' -> " bsl ";
        'bsr' -> " bsr ";
        'xor' -> " xor "
    end.

    
generate_function_output_guard_expr_commas(Write, Type, [Expr]) ->
    ok = generate_function_output_guard_expr(Write, Type, Expr),
    ok;
generate_function_output_guard_expr_commas(Write, Type, [Expr|RestArgs]) ->
    ok = generate_function_output_guard_expr(Write, Type, Expr),
    ok = Write(","),
    generate_function_output_guard_expr_commas(Write, Type, RestArgs).


-spec test_each_file/2 :: (string(), [{string(), string()}]) -> boolean().
test_each_file(OutputDir, [{File,CompareFile}|Files]) ->
    test(filename:join([OutputDir, File]), File, filename:join([OutputDir, CompareFile])) andalso
        test_each_file(OutputDir, Files);
test_each_file(_OutputDir, []) ->
    io:format("tests_b: Tests complete~n", []),
    true.


-spec test/3 :: (string(), string(), string()) -> boolean().
test(FileToTest, FilenameOnly, CompareFile) ->
    {ok, _, ProcessedTokens} = udg:preprocess(FileToTest),
    case compare_tokens(ProcessedTokens, load_tokens(CompareFile)) of
        same -> io:format("tests_b: ~s: same~n", [FilenameOnly]), true;
        {different, T, L, R1, R2} ->
            io:format("tests_b: ~s: Different Tokens=~w Lines=~w~n~n", [FilenameOnly, T, L]),
            io:format("tests_b: ~s: More of processed: ~w~n~n", [FilenameOnly, lists:sublist(R1, 4)]),
            io:format("tests_b: ~s: More of compared: ~w~n~n",  [FilenameOnly, lists:sublist(R2, 4)]),
            false
    end.

t(OutputDir, PRNGInit0) ->
    case file:read_file_info(OutputDir) of
        {ok, {file_info,_,directory,_,_,_,_,_,_,_,_,_,_,_}} ->
            case file:list_dir(OutputDir) of
                {ok, []} ->
                    io:format("tests_b: Using <~s>~n", [OutputDir]),
                    PRNGInit =
                        case PRNGInit0 of
                            none ->
                                {Gen1,Gen2,Gen3}=Gen=now(),
                                io:format("tests_b: PRNG initiated with: ~w-~w-~w~n",[Gen1,Gen2,Gen3]),
                                Gen;
                            _ ->
                                {PRNG1, [$-|PRNG23]} = lists:splitwith(fun(A) -> A /= $, end, PRNGInit0),
                                {PRNG2, [$-|PRNG3]} = lists:splitwith(fun(A) -> A /= $, end, PRNG23),
                                io:format("tests_b: PRNG initiated with: ~s-~s-~s (specified)~n",[PRNG1,PRNG2,PRNG3]),
                                {list_to_integer(PRNG1), list_to_integer(PRNG2), list_to_integer(PRNG3)}
                        end,
                    t_confirm(OutputDir, PRNGInit);
                _ ->
                    error(directory_is_not_empty)
            end;
        _ ->
            error(directory_doesnt_exist)
    end.

t_confirm(OutputDir, PRNGInit) ->
    case io:get_line("tests_b: Ready to generate test files? [y/n] ") of
        [$y|_] ->
            
            io:format("tests_b: Starting~n",[]),
            io:format("tests_b: Adding erg source files~n",[]),
            ok = add_udg_files(OutputDir),
            io:format("tests_b: Creating test files~n",[]),
            generate(OutputDir, PRNGInit);
        _ ->
            io:format("tests_b: Exiting~n",[])
    end.

add_udg_files(OutputDir) ->
    UDGOutputDir = filename:join([OutputDir, "udg"]),
    file:make_dir(UDGOutputDir),
    file:write_file(filename:join([UDGOutputDir, "example.erg"]),
        <<"-user_defined_guard(example, [ is_ue_example/1 ]).\nis_ue_example(A) when is_list(A).">>),
    file:write_file(filename:join([UDGOutputDir, "example_pattern1.erg"]),
        <<"-user_defined_guard(example_pattern1, [ \\\\example_pattern1/1 ]).\n",
          "([A]) \\\\ example_pattern1(A) when true.">>),
    file:write_file(filename:join([UDGOutputDir, "example_pattern2.erg"]),
        <<"-user_defined_guard(example_pattern2, [ \\\\example_pattern2/1 ]).\n",
          "([A]) \\\\ example_pattern2(A) := true;",
          "({tuple,A}) \\\\ example_pattern2(A) when true.">>),
    file:write_file(filename:join([UDGOutputDir, "example1.erg"]),
        <<"-user_defined_guard(example1, [ is_up_example1/1, is_uep_example1/1 ]).\n",
          "is_up_example1({A,B}) when is_binary(A), is_list(B).\n",
          "is_uep_example1({uep,A}) when is_binary(A).">>),
    file:write_file(filename:join([UDGOutputDir, "example2.erg"]),
        <<"-user_defined_guard(example2, [ is_up_example2/1, is_uep_example2/1 ]).\n",
          "is_up_example2({up1,A,B}) when is_binary(A) := is_list(B);\n",
          "is_up_example2({up2,A,B}) when A > 20, is_list(B).\n",
          "is_uep_example2({uep1,A}) := is_binary(A);\n",
          "is_uep_example2({uep2,A}) when A > 10.">>),
    ok.


