
%%
%% udg_tests_2: Series of tests involving loading, parsing and comparing
%%              user defined guard definitions from ERG source files.
%%
%% The test application looks for a "tests_2" directory, reads a one line
%% file named "tests_sequence" and parses each source file named "tests_2_N.erg"
%% where N is a number specified in "tests_sequence", and then uses the
%% comparison definitions and test instances in the corresponding file named
%% "tests_2_N_compare.erg_tests". Files are parsed sequentially in the same
%% sequence as in the "tests_sequence" file.
%%
%% Each "erg_tests" file contains a single {expected_definitions, ...} tuple
%% and several test instance tuples.
%%
%% An example of a {expected_definitions, ...} tuple looks like:
%%
%% {expected_definitions, 
%%    {erg_file, '<erg-file-path>', {udg_definitions, 
%%    [
%%        {user_defined_guard,tests_2_1,[{udg,is_u_tests_2_1,0}]},
%%        {defs,
%%            [
%%                {{udg,is_u_tests_2_1,0},none,[],
%%                    {conjunction_list,[{atom,true}]}}
%%            ]}
%%    ]}}}.
%%
%% Test instance tuples have the form:
%% {M, TestType, TestFunction, TestArguments, ExpectedResult}
%%   where
%%     M = an integer()
%%     TestType = test_udg | test_pattern_template
%%     TestFunction = atom() Name of function to test
%%     TestArguments = [any()] List of token arguments
%%     ExpectedResult = [tuple()] List of pattern and guard disjunctions
%%                                returned.
%%
%% Examples of tuples found in ExpectedResult are:
%% 
%%  {guard_disjunction, [{atom,0,true}], []}
%% 
%%  {pattern_disjunction,
%%    {tokens_for, "[ {A,A}, A | _ ]"},
%%    {tokens_for, "is_list(A), is_list(A)"}}
%% 
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
-module(udg_tests_2).
-description('User defined guard tests 2').
-author('Edward L. Blake < edwardlblake at gmail.com >').

-export([ t/0 ]).

-type tokens() :: [tuple()].
-type variable_assoc() :: [{atom(), atom()}].
-type erg_definitions() :: any().
-type different_reason() ::
    {pattern, DiffTerm, tokens(), tokens(), tokens(), tokens()} |
    {guards, DiffTerm, tokens(), tokens(), tokens(), tokens()} |
    {not_a_udg_function, [ _ ], [ _ ]} |
    {not_a_pattern_template, [ _ ], [ _ ]} |
    {missing_disjunctions, [ _ ], tokens()} |
    {extra_disjunctions, tokens(), [ _ ]}.


determine_tests_dir() ->
    hd(lists:dropwhile(fun(A) -> element(1,file:read_file_info(A)) /= ok end,
        ["tests_2", "../tests_2", "test/tests_2", "../test/tests_2"])).

load_tests_sequence(TestsDir) ->
    case file:consult(filename:join(TestsDir, "tests_sequence.txt")) of
        {ok, [TestsSequence]} when is_list(TestsSequence) ->
            lists:map(fun(E) when is_integer(E) -> E end, TestsSequence);
        _ ->
            io:format("Could open ~s file.~n",[filename:join(TestsDir, "tests_sequence.txt")]),
            error(cant_load_test_terms_to_compare_to)
    end.

load_terms(File) ->
    case file:consult(File) of
        {ok, Terms} -> {ok, Terms};
        _ ->
            io:format("Could not use file:consult on term file ~s.~n",[File]),
            error(cant_load_test_terms_to_compare_to)
    end.


-spec compare_tokens/3
    :: (tokens(), tokens(), variable_assoc()) ->
        {same, variable_assoc()} |
        {different, {tuple(),tuple()}, tokens(), tokens()}.
compare_tokens([{T0,_}|Terms1], [{T02,_}|Terms2], CompVars) when T0 =:= T02 ->
    compare_tokens(Terms1, Terms2, CompVars);
compare_tokens([{var,_,Var1}|Terms1], [{var,_,Var2}|Terms2], CompVars) ->
    case orddict:find(Var2, CompVars) of
        {ok, MaybeVar1} when MaybeVar1 =:= Var1 ->
            compare_tokens(Terms1, Terms2, CompVars);
        {ok, NotVar1} ->
            io:format("~n%% ~w ?= ~w => ~w %% ~w~n",[Var1,Var2,NotVar1,CompVars]),
            case orddict:filter(fun(_,Var1L) -> Var1L =:= Var1 end, CompVars) of
                [] -> {different, {Var1,NotVar1}, Terms1, Terms2};
                [Var1LFound] -> {different, {Var1LFound,Var2}, Terms1, Terms2}
            end;
        _ ->
            compare_tokens(Terms1, Terms2, orddict:store(Var2, Var1, CompVars))
    end;
compare_tokens([{T0,_,T2}|Terms1], [{T02,_,T22}|Terms2], CompVars) when T0 =:= T02, T2 =:= T22 ->
    compare_tokens(Terms1, Terms2, CompVars);
compare_tokens([T0|Terms1], [T02|Terms2], _) ->
    {different, {T0,T02}, Terms1, Terms2};
compare_tokens([], [], CompVars) ->
    {same, CompVars};
compare_tokens([], [T02|Terms2], _) ->
    {different, {none,T02}, [], Terms2};
compare_tokens([T0|Terms1], [], _) ->
    {different, {T0,none}, Terms1, []}.


-spec compare_arg_assocs/3
    :: (tokens(), tokens(), variable_assoc()) ->
        {same, variable_assoc()} |
        {different, {tuple(),tuple()}, tokens(), tokens()}.
compare_arg_assocs([{T0, Tokens1}|Terms1], [{T02, Tokens2OrString}|Terms2], CompVars) when T0 =:= T02 ->
    case Tokens2OrString of
        {tokens_for, CompareString} ->
            {ok, Tokens2, _} = erl_scan:string(CompareString);
        _ ->
            Tokens2 = Tokens2OrString
    end,
    case compare_tokens(Tokens1, Tokens2, CompVars) of
        {same, _} -> compare_arg_assocs(Terms1, Terms2, CompVars);
        Different -> Different
    end;
compare_arg_assocs([T0|Terms1], [T02|Terms2], _) ->
    {different, {T0,T02}, Terms1, Terms2};
compare_arg_assocs([], [], CompVars) ->
    {same, CompVars}.


-spec compare_returned_tokens_udg/2
    :: ([tuple()], [tuple()]) -> same | {different, different_reason()}.
compare_returned_tokens_udg([{guard_disjunction,Guards1,ArgAssocs1}|Rest1],[CompareDisjunction|Rest2]) ->
    case CompareDisjunction of
        {guard_disjunction, {tokens_for, Guards2String},ArgAssocs2List} ->
            {ok, Guards2, _} = erl_scan:string(Guards2String),
            ArgAssocs2 = ArgAssocs2List;
        {guard_disjunction,Guards2TList,ArgAssocs2List} when is_list(Guards2TList) ->
            Guards2 = Guards2TList,
            ArgAssocs2 = ArgAssocs2List
    end,
    case compare_tokens(Guards1,Guards2, orddict:new()) of
        {same, CompVars} ->
            case compare_arg_assocs(ArgAssocs1,ArgAssocs2, CompVars) of
                {same,_} ->
                    compare_returned_tokens_udg(Rest1,Rest2);
                {different, DiffTerm, RTerms1, RTerms2} ->
                    {different, {pattern, DiffTerm, RTerms1, RTerms2, Rest1, Rest2}}
            end;
        {different, DiffTerm, RTerms1, RTerms2} ->
            {different, {guards, DiffTerm, RTerms1, RTerms2, Rest1, Rest2}}
    end;
compare_returned_tokens_udg([],[]) -> same;
compare_returned_tokens_udg([{pattern_disjunction,_,_}|_]=Disjunctions, ComparingTo) ->
    {different, {not_a_udg_function, Disjunctions, ComparingTo}};
compare_returned_tokens_udg(_,[{pattern_disjunction,_,_}|_]) ->
    error(test_file_is_comparing_a_udg_function_with_pattern_disjunctions);
compare_returned_tokens_udg([],TLeft2) -> {different, {missing_disjunctions, [], TLeft2}};
compare_returned_tokens_udg(TLeft1,[]) -> {different, {extra_disjunctions, TLeft1, []}}.


-spec compare_returned_tokens_pattern_template/2
    :: ([tuple()],[tuple()]) -> same | {different, different_reason()}.
compare_returned_tokens_pattern_template([{pattern_disjunction,Pattern1,AddGuards1}|Rest1],[CompareDisjunction|Rest2]) ->
    case CompareDisjunction of
        {pattern_disjunction,{tokens_for, CompareString},AddGuards2Compare} ->
            {ok, Pattern2, _} = erl_scan:string(CompareString),
            case AddGuards2Compare of
                {tokens_for, AddGuards2CompareString} -> {ok, AddGuards2, _} = erl_scan:string(AddGuards2CompareString);
                TList when is_list(TList) -> AddGuards2 = AddGuards2Compare
            end;
        {pattern_disjunction,Pattern2TList,AddGuards2Compare} when is_list(Pattern2TList) ->
            Pattern2 = Pattern2TList,
            case AddGuards2Compare of
                {tokens_for, AddGuards2CompareString} -> {ok, AddGuards2, _} = erl_scan:string(AddGuards2CompareString);
                TList when is_list(TList) -> AddGuards2 = AddGuards2Compare
            end
    end,
    case compare_tokens(Pattern1,Pattern2, orddict:new()) of
        {same, CompVars} ->
            case compare_tokens(AddGuards1,AddGuards2, CompVars) of
                {same, _} ->
                    compare_returned_tokens_pattern_template(Rest1,Rest2);
                {different, DiffTerm, RTerms1, RTerms2} -> 
                    {different, {guards, DiffTerm, RTerms1, RTerms2, Rest1, Rest2}}
            end;
        {different, DiffTerm, RTerms1, RTerms2} ->
            {different, {pattern, DiffTerm, RTerms1, RTerms2, Rest1, Rest2}}
        
    end;
compare_returned_tokens_pattern_template([],[]) -> same;
compare_returned_tokens_pattern_template([{guard_disjunction,_,_}|_]=Disjunctions, ComparingTo) ->
    {different, {not_a_pattern_template, Disjunctions, ComparingTo}};
compare_returned_tokens_pattern_template(_,[{guard_disjunction,_,_}|_]) ->
    error(test_file_is_comparing_a_pattern_template_with_guard_disjunctions);
compare_returned_tokens_pattern_template([],TLeft2) -> {different, {missing_disjunctions, [], TLeft2}};
compare_returned_tokens_pattern_template(TLeft1,[]) -> {different, {extra_disjunctions, TLeft1, []}}.


-spec test_and_compare/3
    :: (integer(), tuple(), erg_definitions()) ->
        same | {different, different_reason()}.
test_and_compare(Num0, {Num, test_udg, FunctionName, Args, ExpectedList}, ERGDefinitions) when Num0 =:= Num ->
    Arity = length(Args),
    UDGFunction = {udg,FunctionName,Arity},
    {ok, ReturnedList, _InlineContext} = udg_erg:get_udg_function(UDGFunction, Args, ERGDefinitions),
    compare_returned_tokens_udg(ReturnedList, ExpectedList);
test_and_compare(Num0, {Num, test_pattern_template, FunctionName, Args, ExpectedList}, ERGDefinitions) when Num0 =:= Num ->
    Arity = length(Args),
    UDGFunction = {pattern_template,FunctionName,Arity},
    {ok, ReturnedList, _InlineContext} = udg_erg:get_udg_pattern_template(UDGFunction, Args, ERGDefinitions),
    compare_returned_tokens_pattern_template(ReturnedList, ExpectedList).


-spec tests_from_term_file/4
    :: (integer(), integer(), [integer()], erg_definitions()) ->
        pass | {no_pass, integer()}.
tests_from_term_file(_, _, [], _) -> pass;
tests_from_term_file(TestNumber, TestTermNumber, [TestTerm|Terms], ERGDefinitions) ->
    case test_and_compare(TestTermNumber, TestTerm, ERGDefinitions) of
        same -> tests_from_term_file(TestNumber, TestTermNumber+1, Terms, ERGDefinitions);
        {different, DiffKind} ->
            case DiffKind of
                {Part, DiffTerm, RT1, RT2, Rest1, Rest2} ->
                    io:format("tests_2_~w (~w): Term in ~w part is different: ~w~n~n", [TestNumber, TestTermNumber, Part, DiffTerm]),
                    io:format("tests_2_~w (~w): Rest of result: ~w~n~n",  [TestNumber, TestTermNumber, RT1]),
                    io:format("tests_2_~w (~w): Rest of expected: ~w~n~n",  [TestNumber, TestTermNumber, RT2]),
                    io:format("tests_2_~w (~w): Rest of result disjunctions: ~w~n~n",  [TestNumber, TestTermNumber, Rest1]),
                    io:format("tests_2_~w (~w): Rest of expected disjunctions: ~w~n~n",  [TestNumber, TestTermNumber, Rest2]);
                {missing_disjunctions, [], TLeft2} ->
                    io:format("tests_2_~w (~w): Result is missing disjunctions than expected~n~n", [TestNumber, TestTermNumber]),
                    io:format("tests_2_~w (~w): Missing disjunctions: ~w~n~n",  [TestNumber, TestTermNumber, TLeft2]);
                {extra_disjunctions, TLeft1, []} ->
                    io:format("tests_2_~w (~w): Result has more disjunctions than expected~n~n", [TestNumber, TestTermNumber]),
                    io:format("tests_2_~w (~w): Extra disjunctions: ~w~n~n",  [TestNumber, TestTermNumber, TLeft1])
            end,
            {no_pass, TestTermNumber}
    end.


-spec test/2 :: (string(), [integer()]) -> boolean().
test(_,[]) ->
    io:format("tests_2 complete~n"), true;
test(TestsDir,[Number|NextList]) when is_integer(Number) ->
    FileToProcess = filename:join([TestsDir, io_lib:format("tests_2_~w.erg", [Number])]),
    FileForTests = filename:join([TestsDir, io_lib:format("tests_2_~w_compare.erg_tests", [Number])]),
    case load_terms(FileForTests) of
        {ok, [{expected_definitions, {erg_file, '<erg-file-path>', ExpectedERGLoaded}}|TestingTerms]} ->
            {ok, {erg_file, _, ERGDefinitions}, Warnings} = udg_erg:read_udg_erg_file(FileToProcess),
            case ExpectedERGLoaded =:= ERGDefinitions of
                true ->
                    case tests_from_term_file(Number, 1, TestingTerms, ERGDefinitions) of
                        pass -> io:format("tests_2_~w: pass~n", [Number]), test(TestsDir, NextList);
                        {no_pass, AtTestTermNumber} ->
                            io:format("tests_2_~w: did not pass at test term ~w~n~n", [Number, AtTestTermNumber]),
                            false
                    end;
                _ ->
                    io:format("tests_2_~w: ERG loaded result and expected not the same.~n~n", [Number]),
                    io:format("tests_2_~w: ERG result  : ~w~n~n", [Number, ERGDefinitions]),
                    io:format("tests_2_~w: ERG expected: ~w~n~n", [Number, ExpectedERGLoaded]),
                    false
            end
    end.

t() ->
    TestsDir = determine_tests_dir(),
    test(TestsDir, load_tests_sequence(TestsDir)).
