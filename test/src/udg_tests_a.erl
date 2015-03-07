
%%
%% udg_tests_a: Test application for traversing the file system under a
%%              directory to preprocess and compare with the original Erlang
%%              source files.
%%
%% The test requires a directory which contains Erlang source files that
%% does not make use of any of the user defined guard syntax. Each file is
%% read, preprocessed and compared on a token basis with the original source
%% file before preprocessing.
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
-module(udg_tests_a).
-description('User defined guard tests any source verbatim').
-author('Edward L. Blake < edwardlblake at gmail.com >').

-export([ t/1, run/0 ]).

-type tokens() :: [tuple()].

run() ->
    case init:get_plain_arguments() of
        [] -> show_info();
        [InputDir] ->
            case file:read_file_info(InputDir) of
                {ok, {file_info,_,directory,_,_,_,_,_,_,_,_,_,_,_}} ->
                    t(InputDir)
            end
    end,
    erlang:halt().

show_info() ->
    io:format("erl -pa ../ebin -pa ebin -noshell -run udg_tests_a run -- <directory>~n",[]),
    io:format("~n",[]),
    io:format("  udg_tests_a traverses a directory for .erl files that do not use user~n",[]),
    io:format("  defined guards and passes them through the preprocessor to determine if~n",[]),
    io:format("  they are returned unchanged.~n",[]),
    io:format("~n",[]),
    io:format("  <directory> : Directory containing erlang source code that does not use~n", []),
    io:format("                user defined guards.~n",[]),
    io:format("~n",[]),
    io:format("~n",[]).

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
compare_tokens_similar([{T0,T1,T2}|Tokens1], [{T02,T12,T22}|Tokens2]) when T0 =:= T02, T2 =:= T22 ->
    compare_tokens_similar(Tokens1, Tokens2);
compare_tokens_similar([{T0,T1}|Tokens1], [{T02,T12}|Tokens2]) when T0 =:= T02 ->
    compare_tokens_similar(Tokens1, Tokens2);
compare_tokens_similar([{T0,T1,T2}|Tokens1], [{T02,T12,T22}|Tokens2]) ->
    {different, {{T0, T2},{T02,T22}}, {T1,T12}, Tokens1, Tokens2};
compare_tokens_similar([{T0,T1}|Tokens1], [{T02,T12}|Tokens2]) ->
    {different, {T0,T02}, {T1,T12}, Tokens1, Tokens2};
compare_tokens_similar([], []) ->
    same.


find(SearchDir) ->
    case find_rec(SearchDir) of
        true -> io:format("tests_a complete~n");
        _ ->    io:format("tests_a: some files did not pass~n"), error(not_passed)
    end.


-spec find_rec/1 :: (string()) -> boolean().
find_rec(SearchDir) ->
    io:format("tests_a entering: ~s~n",[SearchDir]),
    Result = case file:list_dir(SearchDir) of
        {ok, ListOfFilesAndDirs} ->
            each_file_or_dir(SearchDir, ListOfFilesAndDirs);
        _ -> true
    end,
    io:format("tests_a leaving: ~s~n",[SearchDir]),
    Result.
each_file_or_dir(SearchDir, [FileOrDir|ListOfFilesAndDirs]) ->
    FileOrDirFullPath = filename:join([SearchDir, FileOrDir]),
    case file:read_file_info(FileOrDirFullPath) of
        {ok,{file_info,_,directory,_,_,_,_,_,_,_,_,_,_,_}} ->
            find_rec(FileOrDirFullPath) andalso
                each_file_or_dir(SearchDir, ListOfFilesAndDirs);
        {ok,{file_info,_,regular,_,_,_,_,_,_,_,_,_,_,_}} ->
            case filename:extension(FileOrDir) of
                ".erl" ->
                    test(FileOrDirFullPath, FileOrDir) andalso
                        each_file_or_dir(SearchDir, ListOfFilesAndDirs);
                _ -> each_file_or_dir(SearchDir, ListOfFilesAndDirs)
            end;
        _ -> each_file_or_dir(SearchDir, ListOfFilesAndDirs)
    end;
each_file_or_dir(_,[]) -> true.


-spec test/2 :: (string(), string()) -> boolean().
test(FileToTest, FilenameOnly) ->
    {ok, _, ProcessedTokens} = udg:preprocess(FileToTest),
    case compare_tokens(ProcessedTokens, load_tokens(FileToTest)) of
        same -> io:format("tests_a: ~s: same~n", [FilenameOnly]), true;
        {different, T, L, R1, R2} ->
            io:format("tests_a: ~s: Different Tokens=~w Lines=~w~n~n", [FilenameOnly, T, L]),
            io:format("tests_a: ~s: Rest of processed: ~w~n~n", [FilenameOnly, R1]),
            io:format("tests_a: ~s: Rest of compared: ~w~n~n",  [FilenameOnly, R2]),
            false
    end.

t(SearchDir) ->
    find(SearchDir).
