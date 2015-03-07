
%%
%% erlc_udg: Preprocess an erl file with user defined guard syntax before
%%           compiling it with erlc.
%% 
%% Preprocesses an Erlang source file containing user defined guard syntax and
%% then invokes the Erlang compiler through compile:file.
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
-module(erlc_udg).
-description('Compile erl with udgs.').
-author('Edward L. Blake < edwardlblake at gmail.com >').

-export([ run/0, file/1 ]).

-type tokens() :: [tuple()].

run() ->
    case init:get_plain_arguments() of
        [] -> show_info();
        [InputFile] ->
            file(InputFile)
    end,
    erlang:halt().

show_info() ->
    io:format("erl -pa ../ebin -pa ebin -noshell -run erlc_udg run -- <file>~n",[]),
    io:format("~n",[]),
    io:format("  Preprocesses an Erlang source file containing user defined guard syntax and~n",[]),
    io:format("  then invokes the Erlang compiler through compile:file.~n",[]),
    io:format("~n",[]),
    io:format("  <file> : Erlang source file.~n",[]),
    io:format("~n",[]),
    io:format("~n",[]).

file(InputFile) ->
    case filename:extension(InputFile) of
        ".erl" ->
            InputFilePath = filename:absname(InputFile),
            {ok, _, ResultTokens} = udg:preprocess(InputFilePath),
            create_temporary_location(InputFilePath,
                fun(TempSourceFile) when TempSourceFile /= InputFilePath ->
                    {ok, GeneratedCode} = tokens_to_binary(ResultTokens),
                    ok = file:write_file(TempSourceFile, GeneratedCode),
                    case compile:file(TempSourceFile) of
                        {ok, _ModuleName} ->
                            {ok, CWD} = file:get_cwd(),
                            ProcessedERL = filename:join([CWD, filename:basename(filename:rootname(
                                InputFilePath, ".erl")) ++ ".processed_erl"]),
                            file:delete(ProcessedERL),
                            {ok,_} = file:copy(TempSourceFile, ProcessedERL),
                            case file:copy(
                                filename:rootname(TempSourceFile, ".erl") ++ ".beam",
                                filename:rootname(InputFilePath, ".erl") ++ ".beam") of
                                {ok,_} ->
                                    ok = file:delete(filename:rootname(TempSourceFile, ".erl") ++ ".beam");
                                _ -> ok
                            end,
                            ok = file:delete(TempSourceFile)
                    end
                end)
    end.
    
token_to_iodata('dot') -> <<".">>;
token_to_iodata(Reserved) when is_atom(Reserved) -> [atom_to_list(Reserved),<<" ">>].
token_to_iodata('var', Variable) -> [atom_to_list(Variable),<<" ">>];
token_to_iodata(_, StringAtomNumber) -> [io_lib:print(StringAtomNumber),<<" ">>].


-spec tokens_to_binary/1 :: (tokens()) -> {ok, binary()}.
tokens_to_binary(Tokens) ->
    tokens_to_binary(Tokens, 1, [], []).
tokens_to_binary([{_,L}|_]=List, CL, Lines, CurrLine) when CL < L ->
    tokens_to_binary(List, CL+1, [lists:reverse(CurrLine)|Lines], []);
tokens_to_binary([{_,L,_}|_]=List, CL, Lines, CurrLine) when CL < L ->
    tokens_to_binary(List, CL+1, [lists:reverse(CurrLine)|Lines], []);
tokens_to_binary([{T,_}|Rest], CL, Lines, CurrLine) ->
    tokens_to_binary(Rest, CL, Lines, [token_to_iodata(T)|CurrLine]);
tokens_to_binary([{T,_,E}|Rest], CL, Lines, CurrLine) ->
    tokens_to_binary(Rest, CL, Lines, [token_to_iodata(T,E)|CurrLine]);
tokens_to_binary([], _, Lines, CurrLine) ->
    {ok, iolist_to_binary(add_newlines_and_reverse([lists:reverse(CurrLine)|Lines]))}.
    
add_newlines_and_reverse(Lines0) ->
    add_newlines_and_reverse(Lines0, []).
add_newlines_and_reverse([L|Rest], Lines) ->
    add_newlines_and_reverse(Rest, [L,<<"\n">>|Lines]);
add_newlines_and_reverse([], Lines) ->
    Lines.


-spec create_temporary_location/2 :: (string(), fun((string()) -> any())) -> any().
create_temporary_location(InputPath, WithFun) when is_function(WithFun) ->
    case filename:extension(InputPath) of
        ".erl" ->
            TempRootDir = get_temp_dir(["TEMP","TMP"]),
            case filename:basename(InputPath) of
                ".erl" -> error(need_more_characters_in_filename);
                BaseName when BaseName /= "" ->
                    TempDir = lists:append(["UDG_", integer_to_list(random:uniform(100000000000))]),
                    file:make_dir(filename:join([TempRootDir, TempDir])),
                    WithFun(filename:join([TempRootDir, TempDir, BaseName])),
                    file:del_dir(filename:join([TempRootDir, TempDir]))
            end;
        _ -> error(input_path_not_erl_file)
    end.
get_temp_dir([]) -> guess_temp_dir();
get_temp_dir([Env|RestEnv]) ->
    case os:getenv(Env) of
        TempDir when is_list(TempDir) ->
            case file:read_file_info(TempDir) of
                {ok, _} -> TempDir;
                _ -> get_temp_dir(RestEnv)
            end;
        _ -> get_temp_dir(RestEnv)
    end.
guess_temp_dir() ->
    case os:type() of
        {win32, _} ->
            case file:read_file_info("\\Windows\\Temp") of
                {ok, _} -> "\\Windows\\Temp";
                _ -> error(no_temp_directory_found)
            end;
        _ ->
            case {file:read_file_info("/tmp"), file:read_file_info("/var/tmp")} of
                {{ok, _}, _} -> "/tmp";
                {_, {ok, _}} -> "/var/tmp";
                _ -> error(no_temp_directory_found)
            end
    end.
