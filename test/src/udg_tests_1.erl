
%%
%% udg_tests_1: Series of tests involving comparing Erlang source code before
%%              and after preprocessing.
%%
%% The test application looks for a "tests_1" directory, reads a one line
%% file named "tests_sequence" and preprocesses each file named "tests_1_N.erl"
%% in the "src" directory, using the user defined guards defined in the adjacent
%% "udg" directory, and compares the results with the tokens given in the file
%% named "tests_1_N_compare.erl", where N is an integer specified in the
%% "tests_sequence" file. Files are preprocessed sequentially in the same
%% sequence as in the "tests_sequence" file.
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
-module(udg_tests_1).
-description('User defined guard tests 1').
-author('Edward L. Blake < edwardlblake at gmail.com >').

-export([ t/0 ]).

-type tokens() :: [tuple()].


determine_tests_dir() ->
    hd(lists:dropwhile(fun(A) -> element(1,file:read_file_info(A)) /= ok end,
        ["tests_1", "../tests_1", "test/tests_1", "../test/tests_1"])).

load_tests_sequence(TestsDir) ->
    case file:consult(filename:join(TestsDir, "tests_sequence.txt")) of
        {ok, [TestsSequence]} when is_list(TestsSequence) ->
            lists:map(fun(E) when is_integer(E) -> E end, TestsSequence);
        _ ->
            io:format("Could open ~s file.~n",[filename:join(TestsDir, "tests_sequence.txt")]),
            error(cant_load_test_terms_to_compare_to)
    end.

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
compare_tokens([{T0,T1,T2}|Tokens1], [{T02,T12,T22}|Tokens2]) when T0 =:= T02, T1 =:= T12, T2 =:= T22 ->
    compare_tokens(Tokens1, Tokens2);
compare_tokens([{T0,T1}|Tokens1], [{T02,T12}|Tokens2]) when T0 =:= T02, T1 =:= T12 ->
    compare_tokens(Tokens1, Tokens2);
compare_tokens([Token1|_]=Tokens1, [Token2|_]=Tokens2) when is_tuple(Token1), is_tuple(Token2) ->
    compare_tokens_similar(Tokens1, Tokens2);
compare_tokens([], []) ->
    same.


-spec compare_tokens_similar/2
    :: (tokens(), tokens()) ->
        same | {different, {tuple() | atom(), tuple() | atom()}, {integer(), integer()}, tokens(), tokens()}.
compare_tokens_similar(Tokens1, Tokens2) ->
    compare_tokens_similar(Tokens1, Tokens2, orddict:new()).
compare_tokens_similar([{'var',T1,Var1}|Tokens1], [{'var',T12,Var2}|Tokens2], VarPairs) ->
    case orddict:find(Var1, VarPairs) of
        {ok, MaybeVar2} when Var2 =:= MaybeVar2 ->
            compare_tokens_similar(Tokens1, Tokens2, VarPairs);
        {ok, _} ->
            {different, {{'var', Var1},{'var',Var2}}, {T1,T12}, Tokens1, Tokens2};
        _ ->
            compare_tokens_similar(Tokens1, Tokens2, orddict:store(Var1, Var2, VarPairs))
    end;
compare_tokens_similar([{T0,T1,T2}|Tokens1], [{T02,T12,T22}|Tokens2], VarPairs) when T0 =:= T02, T2 =:= T22 ->
    compare_tokens_similar(Tokens1, Tokens2, VarPairs);
compare_tokens_similar([{T0,T1}|Tokens1], [{T02,T12}|Tokens2], VarPairs) when T0 =:= T02 ->
    compare_tokens_similar(Tokens1, Tokens2, VarPairs);
compare_tokens_similar([{T0,T1,T2}|Tokens1], [{T02,T12,T22}|Tokens2], _) ->
    {different, {{T0, T2},{T02,T22}}, {T1,T12}, Tokens1, Tokens2};
compare_tokens_similar([{T0,T1}|Tokens1], [{T02,T12}|Tokens2], _) ->
    {different, {T0,T02}, {T1,T12}, Tokens1, Tokens2};
compare_tokens_similar([], [], _) ->
    same.


-spec test/2 :: (string(), [integer()]) -> boolean().
test(_,[]) ->
    io:format("tests_1 complete~n"), true;
test(TestsDir, [Number|NextList]) when is_integer(Number) ->
    FileToProcess = filename:join([TestsDir, "src", io_lib:format("tests_1_~w.erl", [Number])]),
    FileToCompare = filename:join([TestsDir, "src", io_lib:format("tests_1_~w_compare.erl", [Number])]),
    {ok, _, ProcessedTokens} = udg:preprocess(FileToProcess),
    case compare_tokens(ProcessedTokens, load_tokens(FileToCompare)) of
        same -> io:format("tests_1_~w: same~n", [Number]), test(TestsDir, NextList);
        {different, T, L, R1, R2} ->
            io:format("tests_1_~w: Different Tokens=~w Lines=~w~n~n", [Number, T, L]),
            io:format("tests_1_~w: Rest of processed: ~w~n~n", [Number, R1]),
            io:format("tests_1_~w: Rest of compared: ~w~n~n",  [Number, R2]),
            false
    end.
    
t() ->
    TestsDir = determine_tests_dir(),
    test(TestsDir, load_tests_sequence(TestsDir)).
