
%%
%% udg: Preprocessor for user defined guards and pattern templates in Erlang
%%
%% This module provides a preprocessor for parsing and handling a form of user
%% defined guard as well as pattern template construct for organizing bundles 
%% of patterns and guards into reusable and composable elements.
%%
%% View the README.md file in the parent directory for an overview and details
%% of how the preprocessor works.
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
-module(udg).
-description('User defined guard preprocessor').
-author('Edward L. Blake < edwardlblake @ gmail.com >').

-export([ preprocess/1 ]).

-record(udg_state,
    {
        input_file = "" :: [char()],
        udg_search_path = [] :: [iolist()],
        udg_import_list = [] :: [{{udg|pattern_template, atom(), integer()}, atom()}],
        extern_udg_list = [] :: [{{udg|pattern_template, atom(), integer()}, atom()}],
        loaded_erg_files = [] :: [{atom(), any()}],
        default_udg_search_path = [] :: [iolist()],
        context_stack = [] :: [{{integer(),integer()}, any()}],
        erg_files_nesting = [] :: [binary()]
    }).

-type tokens() :: [tuple()].
-type udg_preprop_state() :: record(udg_state).
-type udg_definitions() :: tuple().
-type udg_fun_name() :: atom().
-type udg_arity() :: integer().
-type inline_context() :: tuple().
-type inline_context_token() ::
    {context_push_state | context_pop_state,
        integer(), inline_context(),
        {integer(), integer()},
        {binary(), {udg | pattern_template, udg_fun_name(), udg_arity()}}}.
-type guards_and_arg_assocs() :: [tuple()].
-type pattern_template_disjunctions() :: [tuple()].

preprocess(InputFile) ->
    DefaultUDGSearchPath = [filename:join(["..","udg"]),"udg"],
    State = #udg_state{
        input_file = InputFile,
        udg_search_path = DefaultUDGSearchPath,
        default_udg_search_path = DefaultUDGSearchPath
    },
    case preprocess_file(State, InputFile) of
        {ok, NewState, OutputFile} ->
            {ok, NewState, OutputFile};
        Error -> Error
    end.

%% Process Erlang source file.
%%

proc_source(State, Tokens) ->
    proc_source(State, Tokens, []).
proc_source(State, [{'-',_}=_Token1,{atom,_,extern_udg}=_Token2 | Rest], SoFar) ->
    case proc_source_extern_udg(Rest, State#udg_state.extern_udg_list) of
        {ok, NewExternUDGList, Rest2} ->
            proc_source(
                State#udg_state{extern_udg_list = NewExternUDGList},
                Rest2, SoFar)
    end;
proc_source(State, [{'-',_},{atom,_,no_default_udg_path} | Rest], SoFar) ->
    case Rest of
        [{'(',_},{')',_},{'dot',_} | Rest2] ->
            DefaultUDGSearchPath = State#udg_state.default_udg_search_path,
            case State#udg_state.udg_search_path of
                UDGSearchPath when UDGSearchPath =:= DefaultUDGSearchPath ->
                    proc_source(
                        State#udg_state{udg_search_path = []},
                        Rest2, SoFar);
                _ ->
                    error(no_default_udg_path_has_to_be_before_adda_and_addz_path_directives)
            end;
        _ -> error(no_default_udg_path_not_formed_right)
    end;
proc_source(State, [{'-',_},{atom,_,adda_udg_path} | Rest], SoFar) ->
    case Rest of
        [{'(',_},{'string',_,UDGPath},{')',_},{dot,_}|Rest2] ->
            NewUDGSearchPath = [UDGPath|State#udg_state.udg_search_path],
            proc_source(
                State#udg_state{udg_search_path = NewUDGSearchPath},
                Rest2, SoFar);
        _ -> error(paren_missing_after_adda_udg_path_directive)
    end;
proc_source(State, [{'-',_},{atom,_,addz_udg_path} | Rest], SoFar) ->
    case Rest of
        [{'(',_},{'string',_,UDGPath},{')',_},{dot,_}|Rest2] ->
            NewUDGSearchPath = State#udg_state.udg_search_path ++ [UDGPath],
            proc_source(
                State#udg_state{udg_search_path = NewUDGSearchPath},
                Rest2, SoFar);
        _ -> error(paren_missing_after_addz_udg_path_directive)
    end;
proc_source(State, [{'-',_},{atom,_,include_udg} | Rest], SoFar) ->
    case udg_erg_includes:proc_source_include_udg(Rest, State#udg_state.udg_import_list) of
        {ok, NewUDGImportList, Rest2} ->
            proc_source(
                State#udg_state{udg_import_list = NewUDGImportList},
                Rest2, SoFar)
    end;
proc_source(State, [{'-',_},{atom,_,auto_include_udg} | Rest], SoFar) ->
    case udg_erg_includes:proc_source_auto_include_udg(Rest, State#udg_state.udg_import_list) of
        {ok, NewUDGImportList, Rest2} ->
            proc_source(
                State#udg_state{udg_import_list = NewUDGImportList},
                Rest2, SoFar)
    end;

proc_source(State, [{'-',_}=Token1 | Rest], SoFar) ->
    proc_source_skip_up_to_dot(State,Rest,[Token1|SoFar]);
proc_source(State, [{'?',_}=Token1 | Rest], SoFar) ->
    io:format("NOTE: Encountered ?...(...) as a definition, skipping to dot.~n",[]),
    proc_source_skip_up_to_dot(State,Rest,[Token1|SoFar]);
proc_source(State, [{'atom',_,_FunctionName}=_Token1,{'(',_}=_Token2 | _]=DefinitionsStart, SoFar) ->
    {ok, NewState, FunctionDisjunctions, DotToken, Rest2} = proc_fun_defs_up_to_dot(State, DefinitionsStart),
    {NewState2, FunctionDisjunctions2} = expand_user_defined_guards(NewState, FunctionDisjunctions),
    proc_source(NewState2, Rest2, lists:reverse(proc_combine_body_defs(FunctionDisjunctions2, DotToken)) ++ SoFar);
proc_source(State, [], SoFar) ->
    {ok, State, lists:reverse(SoFar)}.


proc_source_skip_up_to_dot(State,[{'dot',_}=T|Rest],SoFar) ->
    proc_source(State, Rest, [T|SoFar]);
proc_source_skip_up_to_dot(State,[T|Rest],SoFar) ->
    proc_source_skip_up_to_dot(State, Rest, [T|SoFar]).

tokens_up_to_dot([{'dot',_}|_]=Rest,SoFar) ->
    {ok, Rest, lists:reverse(SoFar)};
tokens_up_to_dot([T|Rest],SoFar) ->
    tokens_up_to_dot(Rest, [T|SoFar]).

proc_fun_defs_up_to_dot(State, Tokens) ->
    proc_fun_defs_up_to_dot(State, Tokens, []).
proc_fun_defs_up_to_dot(State, [{'?',_}|_]=Tokens, Defs) ->
    io:format("NOTE: Encountered ?...(...) mid-definitions, skipping to dot.~n",[]),
    {ok, [{'dot',_}=DotToken|Rest], TokensUpToDot} = tokens_up_to_dot(Tokens,[]),
    {ok, State, lists:reverse([{{unprocessed}, TokensUpToDot}|Defs]), DotToken, Rest};
proc_fun_defs_up_to_dot(State, Tokens, Defs) ->
    case proc_fun_head(Tokens) of
        {ok, FunctionName, BeforeArgs, Arguments, AfterArgs, Guards, AfterGuards, Rest} ->
            {ok, NewState, Body, Rest3} = proc_whole_disjunction_body(State, 'dot', Rest),
            Defn = {FunctionName, BeforeArgs,
                without_context_nesting_tokens(Arguments), AfterArgs,
                without_context_nesting_tokens(Guards), AfterGuards, Body},
            case Rest3 of
                [{'dot',_}=DotToken|Rest4] -> {ok, NewState, lists:reverse([Defn|Defs]), DotToken, Rest4};
                [{';',_}|Rest4] -> proc_fun_defs_up_to_dot(NewState, Rest4, [Defn|Defs])
            end;
        encountered_mid_def_macro ->
            io:format("NOTE: Encountered ?...(...) mid-definitions, skipping to dot.~n",[]),
            {ok, [{'dot',_}=DotToken|Rest], TokensUpToDot} = tokens_up_to_dot(Tokens,[]),
            {ok, State, lists:reverse([{{unprocessed}, TokensUpToDot}|Defs]), DotToken, Rest}
    end.

proc_fun_head([{'atom',_,FunctionName}=T1,{'(',_}=T2|Rest]) ->
    {ok, Arguments, [{')',_}=T3|Rest2]} = proc_fun_head_arguments(Rest),
    case Rest2 of
        [{'when',_}=T4|Rest3] -> 
            {ok, Guards, [{'->',_}=T5|Rest4]} = proc_source_up_to_arrow(Rest3),
            {ok, FunctionName, [T1,T2], Arguments, [T3,T4], Guards, [T5], Rest4};
        [{'->',_}=T5|Rest3] -> 
            {ok, FunctionName, [T1,T2], Arguments, [T3], none, [T5], Rest3};
        [{'?',_}|_] ->
            encountered_mid_def_macro
    end.

proc_combine_body_defs(FunctionDisjunctions, LastToken) ->
    Combining =
        lists:map(
            fun({FunName, BeforeArgs, Args, AfterArgs, Guards, [{'->',_}=AfterGuards], Body}) when is_atom(FunName) ->
                GuardsT = case Guards of none -> []; _ -> Guards end,
                lists:append([BeforeArgs, Args, AfterArgs, GuardsT, [AfterGuards|Body]]);
               ({{unprocessed}, Tokens}) -> Tokens
            end, FunctionDisjunctions),
    DisjInBetween = lists:foldl(
        fun (FDisj,[]) -> [FDisj];
            (FDisj,Combining2) ->
                LineNum2 = case FDisj of [{_,Ln2}|_] -> Ln2; [{_,Ln2,_}|_] -> Ln2 end,
                [FDisj,[{';',LineNum2}]|Combining2]
        end, [], Combining),
    lists:append(lists:reverse([[LastToken]|DisjInBetween])).

proc_fun_head_arguments(Tokens) ->
    proc_fun_head_arguments(Tokens,[],[]).
proc_fun_head_arguments([{')', _}=T | Rest], SoFar, []) ->
    {ok, lists:reverse(SoFar), [T|Rest]};
proc_fun_head_arguments([{C, _}=T | Rest], SoFar, [O|H])
when O =:= '(', C =:= ')'; O =:= '[', C =:= ']'; O =:= '{', C =:= '}'; O =:= '<<', C =:= '>>' ->
    proc_fun_head_arguments(Rest, [T|SoFar], H);
proc_fun_head_arguments([{O, _}=T | Rest], SoFar, H)
when O =:= '('; O =:= '['; O =:= '{'; O =:= '<<' ->
    proc_fun_head_arguments(Rest, [T|SoFar], [O|H]);
proc_fun_head_arguments([Token|Rest], SoFar, N) ->
    proc_fun_head_arguments(Rest, [Token|SoFar], N).

proc_fun_arguments_of(Tokens) ->
    proc_fun_arguments_of(Tokens,[],[]).
proc_fun_arguments_of([{'when',_}=T|Rest],SoFar,[]) ->
    {ok, [], lists:reverse(SoFar), [T], true, Rest};
proc_fun_arguments_of([{'->',_}|_]=Tokens,SoFar,[]) ->
    {ok, [], lists:reverse(SoFar), [], false, Tokens};
proc_fun_arguments_of([{C, _}=T | Rest], SoFar, [O|H])
when O =:= '(', C =:= ')'; O =:= '[', C =:= ']'; O =:= '{', C =:= '}'; O =:= '<<', C =:= '>>' ->
    proc_fun_arguments_of(Rest, [T|SoFar], H);
proc_fun_arguments_of([{O, _}=T | Rest], SoFar, H)
when O =:= '('; O =:= '['; O =:= '{'; O =:= '<<' ->
    proc_fun_arguments_of(Rest, [T|SoFar], [O|H]);
proc_fun_arguments_of([T|Rest],SoFar,H) ->
    proc_fun_arguments_of(Rest,[T|SoFar],H).

proc_fun_arguments_fun([{'(', _}=T | Rest]) ->
    {ok, Arguments, [{')',_}=TE|Rest2]} = proc_fun_head_arguments(Rest),
    case Rest2 of
        [{'when',_}=TW|Rest3] -> {ok, [T], Arguments, [TE,TW], true, Rest3};
        [{'->',_}|_] -> {ok, [T], Arguments, [TE], false, Rest2}
    end.

proc_fun_arguments_fun_with_self([{var,_,_}=SelfT,{'(', _}=T | Rest]) ->
    {ok, Arguments, [{')',_}=TE|Rest2]} = proc_fun_head_arguments(Rest),
    case Rest2 of
        [{'when',_}=TW|Rest3] -> {ok, [SelfT,T], Arguments, [TE,TW], true, Rest3};
        [{'->',_}|_] -> {ok, [SelfT,T], Arguments, [TE], false, Rest2}
    end.

proc_fun_arguments_if(Tokens) ->
    {ok, [], [], [], true, Tokens}.

proc_fun_arguments_catch(Tokens) ->
    proc_fun_arguments_of(Tokens).

proc_whole_disjunction_body(State, LastTokenType, Tokens) when
        LastTokenType =:= 'end';
        LastTokenType =:= 'dot';
        LastTokenType =:= 'of';
        LastTokenType =:= 'after';
        LastTokenType =:= 'catch' ->
    case Tokens of
        [{'catch',_}=T|Tokens2] ->
            proc_whole_disjunction_body_catches(State, LastTokenType, Tokens2, [T]);
        _ ->
            proc_whole_disjunction_body(State, LastTokenType, Tokens, [], [])
    end.

proc_whole_disjunction_body_catches(State, LastTokenType, [{'catch',_}=T|Tokens], CatchTokens) ->
    proc_whole_disjunction_body_catches(State, LastTokenType, Tokens, [T|CatchTokens]);
proc_whole_disjunction_body_catches(State, LastTokenType, [T|Tokens], CatchTokens) ->
    proc_whole_disjunction_body(State, LastTokenType, [T|Tokens], CatchTokens, []).

%% LTT: Last Token Type
proc_whole_disjunction_body(State, LTT, [{'case', _}=Token | Rest], SoFar, H) ->
    {ok, State2, ExprBody, [{'of',_}=Token2|Rest2]} = proc_whole_disjunction_body(State, 'of', Rest),
    {ok, NewState, Disjunctions, EndToken, Rest3} = proc_enter_expr_block(State2, Token, 'end', fun proc_fun_arguments_of/1, Rest2),
    {NewState2, Disjunctions2} = expand_user_defined_guards(NewState, Disjunctions),
    proc_whole_disjunction_body(NewState2, LTT, Rest3,
        lists:append([
            lists:reverse(proc_combine_body_defs(Disjunctions2, EndToken)),
            [Token2|lists:reverse(ExprBody)],
            [Token|SoFar]
        ]), H);
proc_whole_disjunction_body(State, LTT, [{'fun', _}=Token,{'(',_}=ParenToken | RestWithoutParen], SoFar, H) ->
    Rest = [ParenToken|RestWithoutParen],
    {ok, NewState, Disjunctions, EndToken, Rest2} = proc_enter_expr_block(State, Token, 'end', fun proc_fun_arguments_fun/1, Rest),
    {NewState2, Disjunctions2} = expand_user_defined_guards(NewState, Disjunctions),
    proc_whole_disjunction_body(NewState2, LTT, Rest2,
        lists:append([
            lists:reverse(proc_combine_body_defs(Disjunctions2, EndToken)),
            [Token|SoFar]
        ]), H);
proc_whole_disjunction_body(State, LTT, [{'fun', _}=Token,{var,_,_}=SelfToken,{'(',_}=ParenToken | RestWithoutSelfAndParen], SoFar, H) ->
    Rest = [SelfToken,ParenToken|RestWithoutSelfAndParen],
    {ok, NewState, Disjunctions, EndToken, Rest2} = proc_enter_expr_block(State, Token, 'end', fun proc_fun_arguments_fun_with_self/1, Rest),
    {NewState2, Disjunctions2} = expand_user_defined_guards(NewState, Disjunctions),
    proc_whole_disjunction_body(NewState2, LTT, Rest2,
        lists:append([
            lists:reverse(proc_combine_body_defs(Disjunctions2, EndToken)),
            [Token|SoFar]
        ]), H);
proc_whole_disjunction_body(State, LTT, [{'if', _}=Token | Rest], SoFar, H) ->
    {ok, NewState, Disjunctions, EndToken, Rest2} = proc_enter_expr_block(State, Token, 'end', fun proc_fun_arguments_if/1, Rest),
    %% There is no patterns in if block, just use expand_simple_u_guards on Guards
    Disjunctions2 = lists:map(
        fun({'-', [], [], [], [], AfterGuards, Body}) ->
            {'-', [], [], [], [], AfterGuards, Body};
           ({'-', [], [], [], Guards, AfterGuards, Body}) ->
            {_, NewGuards} = expand_simple_u_guards(State, Guards),
            {'-', [], [], [], without_context_nesting_tokens(NewGuards), AfterGuards, Body}
        end, Disjunctions),
    proc_whole_disjunction_body(NewState, LTT, Rest2,
        lists:append([
            lists:reverse(proc_combine_body_defs(Disjunctions2, EndToken)),
            [Token|SoFar]
        ]), H);
proc_whole_disjunction_body(State, LTT, [{'receive', _}=Token | Rest], SoFar, H) ->
    {ok, State2, Disjunctions, Token2, Rest2} = proc_enter_expr_block(State, Token, 'after', fun proc_fun_arguments_of/1, Rest),
    {State3, Disjunctions2} = expand_user_defined_guards(State2, Disjunctions),
    case Token2 of
        {'after',_} ->
            {ok, State4, ExprBody, [{'end',_}=Token3|Rest3]} = proc_whole_disjunction_body(State3, 'end', Rest2),
            proc_whole_disjunction_body(State4, LTT, Rest3,
                lists:append([
                    [Token3|lists:reverse(ExprBody)],
                    lists:reverse(proc_combine_body_defs(Disjunctions2, Token2)),
                    [Token|SoFar]
                ]), H);
        {'end',_} ->
            proc_whole_disjunction_body(State3, LTT, Rest2,
                lists:append([
                    lists:reverse(proc_combine_body_defs(Disjunctions2, Token2)),
                    [Token|SoFar]
                ]), H)
    end;

proc_whole_disjunction_body(State, LTT, [{'try', _}=Token | Rest], SoFar, H) ->
    {ok, State2, ExprBody, Token2AndRest2} = proc_whole_disjunction_body(State, 'catch', Rest),
    case Token2AndRest2 of
        [{'end',_}=Token2|Rest2] ->
            proc_whole_disjunction_body(State2, LTT, Rest2, [Token2|lists:reverse(ExprBody)] ++ [Token|SoFar], H);
        [{'after',_}=Token2|Rest2] ->
            {ok, State3, ExprBodyAfter, [{'end',_}=EndToken2|Rest3]} = proc_whole_disjunction_body(State2, 'end', Rest2),
            proc_whole_disjunction_body(State3, LTT, Rest3,
                lists:append([
                    [EndToken2|lists:reverse(ExprBodyAfter)],
                    [Token2|lists:reverse(ExprBody)],
                    [Token|SoFar]
                ]), H);
        [{'catch',_}=Token2|Rest2] ->
            {ok, State3, DisjunctionsCatch, EndToken1, Rest3} = proc_enter_expr_block(State2, Token, 'after', fun proc_fun_arguments_catch/1, Rest2),
            {State4, DisjunctionsCatch2} = expand_user_defined_guards(State3, DisjunctionsCatch),
            case EndToken1 of
                {'after',_} ->
                    {ok, State5, ExprBodyAfter, [{'end',_}=EndToken2|Rest4]} = proc_whole_disjunction_body(State4, 'end', Rest3),
                    proc_whole_disjunction_body(State5, LTT, Rest4,
                        lists:append([
                            [EndToken2|lists:reverse(ExprBodyAfter)],
                            lists:reverse(proc_combine_body_defs(DisjunctionsCatch2, EndToken1)),
                            [Token2|lists:reverse(ExprBody)],
                            [Token|SoFar]
                        ]), H);
                {'end',_} ->
                    proc_whole_disjunction_body(State4, LTT, Rest3,
                        lists:append([
                            lists:reverse(proc_combine_body_defs(DisjunctionsCatch2, EndToken1)),
                            [Token2|lists:reverse(ExprBody)],
                            [Token|SoFar]
                        ]), H)
            end;
        [{'of',_}=Token2|Rest2] ->
            {ok, State3, DisjunctionsOf, Token3, Rest3} = proc_enter_expr_block(State2, Token, 'catch', fun proc_fun_arguments_of/1, Rest2),
            {State4, Disjunctions2Of} = expand_user_defined_guards(State3, DisjunctionsOf),
            case Token3 of
                {'end',_} ->
                    proc_whole_disjunction_body(State4, LTT, Rest3,
                        lists:append([
                            lists:reverse(proc_combine_body_defs(Disjunctions2Of, Token3)),
                            [Token2|lists:reverse(ExprBody)],
                            [Token|SoFar]
                        ]), H);
                {'after',_} ->
                    {ok, State5, ExprBodyAfter, [{'end',_}=EndToken|Rest4]} = proc_whole_disjunction_body(State4, 'end', Rest3),
                    proc_whole_disjunction_body(State5, LTT, Rest4,
                        lists:append([
                            [EndToken|lists:reverse(ExprBodyAfter)],
                            lists:reverse(proc_combine_body_defs(Disjunctions2Of, Token3)),
                            [Token2|lists:reverse(ExprBody)],
                            [Token|SoFar]
                        ]), H);
                {'catch',_} ->
                    {ok, State5, DisjunctionsCatch, Token4, Rest4} = proc_enter_expr_block(State4, Token, 'after', fun proc_fun_arguments_catch/1, Rest3),
                    {State6, Disjunctions2Catch} = expand_user_defined_guards(State5, DisjunctionsCatch),
                    case Token4 of
                        {'after',_} ->
                            {ok, State7, ExprBodyAfter, [{'end',_}=EndToken|Rest5]} = proc_whole_disjunction_body(State6, 'end', Rest4),
                            proc_whole_disjunction_body(State7, LTT, Rest5,
                                lists:append([
                                    [EndToken|lists:reverse(ExprBodyAfter)],
                                    lists:reverse(proc_combine_body_defs(Disjunctions2Catch, Token4)),
                                    lists:reverse(proc_combine_body_defs(Disjunctions2Of, Token3)),
                                    [Token2|lists:reverse(ExprBody)],
                                    [Token|SoFar]
                                ]), H);
                        {'end',_} ->
                            proc_whole_disjunction_body(State6, LTT, Rest4,
                                lists:append([
                                    lists:reverse(proc_combine_body_defs(Disjunctions2Catch, Token4)),
                                    lists:reverse(proc_combine_body_defs(Disjunctions2Of, Token3)),
                                    [Token2|lists:reverse(ExprBody)],
                                    [Token|SoFar]
                                ]), H)
                    end
            end
    end;
proc_whole_disjunction_body(State, LTT, [{'begin', _}=Token | Rest], SoFar, H) ->
    {ok, NewState, ExprBody, [{'end',_}=Token2|Rest2]} = proc_whole_disjunction_body(State, 'end', Rest),
    proc_whole_disjunction_body(NewState, LTT, Rest2,
        lists:append([
            [Token2|lists:reverse(ExprBody)],
            [Token|SoFar]
        ]), H);


proc_whole_disjunction_body(State, LTT, [{',', _}=T|[{'catch',_}|_]=Rest], SoFar, []) ->
    proc_whole_disjunction_body_catches(State, LTT, Rest, [T|SoFar]);
proc_whole_disjunction_body(State, _LTT, [{';', _}|_]=Rest, SoFar, []) ->
    {ok, State, lists:reverse(SoFar), Rest};
proc_whole_disjunction_body(State, 'dot', [{'dot', _}|_]=Rest, SoFar, []) ->
    {ok, State, lists:reverse(SoFar), Rest};
proc_whole_disjunction_body(State, 'end', [{'end', _}|_]=Rest, SoFar, []) ->
    {ok, State, lists:reverse(SoFar), Rest};
proc_whole_disjunction_body(State, 'of', [{'of', _}|_]=Rest, SoFar, []) ->
    {ok, State, lists:reverse(SoFar), Rest};
proc_whole_disjunction_body(State, 'after', [{'after', _}|_]=Rest, SoFar, []) ->
    {ok, State, lists:reverse(SoFar), Rest};
proc_whole_disjunction_body(State, 'after', [{'end', _}|_]=Rest, SoFar, []) ->
    {ok, State, lists:reverse(SoFar), Rest};
proc_whole_disjunction_body(State, 'catch', [{'of', _}|_]=Rest, SoFar, []) ->
    {ok, State, lists:reverse(SoFar), Rest};
proc_whole_disjunction_body(State, 'catch', [{'catch', _}|_]=Rest, SoFar, []) ->
    {ok, State, lists:reverse(SoFar), Rest};
proc_whole_disjunction_body(State, 'catch', [{'after', _}|_]=Rest, SoFar, []) ->
    {ok, State, lists:reverse(SoFar), Rest};
proc_whole_disjunction_body(State, 'catch', [{'end', _}|_]=Rest, SoFar, []) ->
    {ok, State, lists:reverse(SoFar), Rest};
    
proc_whole_disjunction_body(State, LTT, [{O,_}=T|Rest], SoFar, H) when O =:= '('; O =:= '['; O =:= '{'; O =:= '<<' ->
    proc_whole_disjunction_body(State, LTT, Rest, [T|SoFar], [O|H]);
proc_whole_disjunction_body(State, LTT, [{C,_}=T|Rest], SoFar, [O|H])
    when O =:= '(', C =:= ')'; O =:= '[', C =:= ']';
         O =:= '{', C =:= '}'; O =:= '<<', C =:= '>>' ->
    proc_whole_disjunction_body(State, LTT, Rest, [T|SoFar], H);
proc_whole_disjunction_body(State, LTT, [T|Rest], SoFar, H) ->
    proc_whole_disjunction_body(State, LTT, Rest, [T|SoFar], H).


proc_enter_expr_block(State, Token, LTT, ArgumentsAndWhenKeywordFun, Rest) ->
    proc_enter_expr_block(State, Token, LTT, ArgumentsAndWhenKeywordFun, Rest, []).
proc_enter_expr_block(State, Token, LTT, ArgumentsAndWhenKeywordFun, Rest, Defs) ->
    {ok, BeforeArgs, Arguments, AfterArgs, _HasGuards, Rest1} = ArgumentsAndWhenKeywordFun(Rest),
    {ok, Guards, [{'->',_}=TA|Rest2]} = proc_source_up_to_arrow(Rest1),
    {ok, NewState, Body, Rest3} = proc_whole_disjunction_body(State, LTT, Rest2),
    Defn = {'-', BeforeArgs,
        without_context_nesting_tokens(Arguments), AfterArgs,
        without_context_nesting_tokens(Guards), [TA], Body},
    case {LTT,Rest3} of
        {'end',[{'end',_}=EndToken|Rest4]} -> {ok, NewState, lists:reverse([Defn|Defs]), EndToken, Rest4};
        %% Removed: {'of',[{'of',_}=EndToken|Rest4]} -> {ok, NewState, lists:reverse([Defn|Defs]), EndToken, Rest4};
        {'after',[{'after',_}=EndToken|Rest4]} -> {ok, NewState, lists:reverse([Defn|Defs]), EndToken, Rest4};
        {'after',[{'end',_}=EndToken|Rest4]} -> {ok, NewState, lists:reverse([Defn|Defs]), EndToken, Rest4};
        {'catch',[{'of',_}=EndToken|Rest4]} -> {ok, NewState, lists:reverse([Defn|Defs]), EndToken, Rest4};
        {'catch',[{'catch',_}=EndToken|Rest4]} -> {ok, NewState, lists:reverse([Defn|Defs]), EndToken, Rest4};
        {'catch',[{'after',_}=EndToken|Rest4]} -> {ok, NewState, lists:reverse([Defn|Defs]), EndToken, Rest4};
        {'catch',[{'end',_}=EndToken|Rest4]} -> {ok, NewState, lists:reverse([Defn|Defs]), EndToken, Rest4};
        {_,[{';',_}|Rest4]} -> proc_enter_expr_block(NewState, Token, LTT, ArgumentsAndWhenKeywordFun, Rest4, [Defn|Defs])
    end.

without_context_nesting_tokens(none) -> none;
without_context_nesting_tokens(Arguments) when is_list(Arguments) ->
    without_context_nesting_tokens(Arguments, []).
without_context_nesting_tokens([], SoFar) ->
    lists:reverse(SoFar);
without_context_nesting_tokens([{context_push_state,_,_,_,_}|Arguments], SoFar) ->
    without_context_nesting_tokens(Arguments, SoFar);
without_context_nesting_tokens([{context_pop_state,_,_,_,_}|Arguments], SoFar) ->
    without_context_nesting_tokens(Arguments, SoFar);
without_context_nesting_tokens([T|Arguments], SoFar) ->
    without_context_nesting_tokens(Arguments, [T|SoFar]).

proc_source_up_to_arrow(Tokens) ->
    proc_source_up_to_arrow(Tokens, []).
proc_source_up_to_arrow([{'->', _}=Token|Rest], Before) ->
    {ok, lists:reverse(Before), [Token|Rest]};
proc_source_up_to_arrow([Token|Rest], Before) when element(1,Token) /= '->' ->
    proc_source_up_to_arrow(Rest, [Token|Before]).

proc_split_along_disjunctions(Tokens) when is_list(Tokens) ->
    proc_split_along_disjunctions(Tokens, [], []);
proc_split_along_disjunctions(none) -> none.
proc_split_along_disjunctions([{';', _}=_Token|Rest], Before, Lists) ->
    proc_split_along_disjunctions(Rest, [], [lists:reverse(Before)|Lists]);
proc_split_along_disjunctions([Token|Rest], Before, Lists) when element(1,Token) /= ';' ->
    proc_split_along_disjunctions(Rest, [Token|Before], Lists);
proc_split_along_disjunctions([], Before, Lists) ->
    {ok_reversed, [lists:reverse(Before)|Lists]}.


expand_user_defined_guards(State, Disjunctions) ->
    {NewState, Disjunctions2} = lists:foldl(fun proc_pattern_disjunctions/2, {State, []}, Disjunctions),
    Disjunctions3 = lists:map(
        fun({FunctionName, BeforeArgs, Arguments, AfterArgs, [], AfterGuards, Body}) ->
            {FunctionName, BeforeArgs, Arguments, AfterArgs, [], AfterGuards, Body};
           ({FunctionName, BeforeArgs, Arguments, AfterArgs, Guards, AfterGuards, Body}) ->
            {_, NewGuards} = expand_simple_u_guards(State, Guards),
            {FunctionName, BeforeArgs, without_context_nesting_tokens(Arguments), AfterArgs, without_context_nesting_tokens(NewGuards), AfterGuards, Body};
           ({{unprocessed}, Rest}) -> {{unprocessed}, Rest}
        end, Disjunctions2),
    NewDisjunctions = lists:reverse(Disjunctions3),
    {NewState, NewDisjunctions}.

proc_pattern_disjunctions(DisjPattern, {State, CurrDisjs}) ->
    case proc_pattern_guards(State, DisjPattern) of
        {subst, NewState, DisjPatterns2} ->
            {NewState2, DisjPatterns3} = lists:foldl(fun proc_pattern_disjunctions/2, {NewState,[]}, DisjPatterns2),
            {NewState2, DisjPatterns3 ++ CurrDisjs}; % lists:reverse(DisjPatterns3)
        no_subst ->
            {State, [DisjPattern|CurrDisjs]}
    end.

proc_pattern_guards(_State, {{unprocessed}, _}) -> no_subst;
proc_pattern_guards(State, {FunctionName, BeforeArgs, Arguments, AfterArgs, Guards, AfterGuards, Body}) ->
    case proc_pattern_guards_pattern_template(State, Arguments) of
        {subst, NewState, ArgumentsLeftSide, NewArgumentsAndGuards, NewInlineContext, ArgumentsRightSide} ->
            {ok, {StartContext, FinishContext}} = token_context_tuples(NewInlineContext),
            {subst, NewState,
                lists:map(
                    fun({pattern_disjunction, ExtraPattern, ExtraGuards}) ->
                        {ok, NewAfterArgs, NewGuards, NewAfterGuards} =
                            add_extra_guards(AfterArgs, [ExtraGuards], Guards, AfterGuards, {StartContext, FinishContext}),
                        {FunctionName, BeforeArgs, ArgumentsLeftSide ++ [{'=',0},StartContext|ExtraPattern] ++ [FinishContext|ArgumentsRightSide], NewAfterArgs, NewGuards, NewAfterGuards, Body}
                    end,
                    NewArgumentsAndGuards)};
        no_subst ->
            case proc_pattern_guards_is_up_udg_function(State, Guards) of
                {subst_with_context, NewState, LeftDisjunctions, NewGuardsAndArgumentAssocs, RightGuardTokens, ContextTuples} ->
                    {subst, NewState, lists:append(
                        case LeftDisjunctions of
                            [] -> [];
                            [[]] -> [];
                            _ -> [[{FunctionName, BeforeArgs, Arguments, AfterArgs, proc_combine_with_disjunctions(lists:reverse(LeftDisjunctions)), AfterGuards, Body}]]
                        end ++
                        [lists:map(
                            fun({guard_disjunction_with_context, NewGuards, ArgumentAssocs}) ->
                                {ok, Arguments2, _AssocsSet} = add_extra_argument_patterns(Arguments, ArgumentAssocs, ContextTuples),
                                
                                {FunctionName, BeforeArgs, Arguments2, AfterArgs, NewGuards, AfterGuards, Body}
                            end,
                            NewGuardsAndArgumentAssocs)] ++
                        case RightGuardTokens of
                            [] -> [];
                            [[]] -> [];
                            _ -> [[{FunctionName, BeforeArgs, Arguments, AfterArgs, RightGuardTokens, AfterGuards, Body}]]
                        end
                        )};
                no_subst ->
                    no_subst
            end
    end.


-spec add_extra_guards/5
    :: (tokens(), [tokens()], tokens() | none, tokens(), _) ->
        {ok, tokens(), tokens(), tokens()}.
add_extra_guards(AfterArgs, [], Guards, AfterGuards, _ContextTuples) ->
    {ok, AfterArgs, Guards, AfterGuards};
add_extra_guards(AfterArgs, [[]], Guards, AfterGuards, _ContextTuples) ->
    {ok, AfterArgs, Guards, AfterGuards};
add_extra_guards(AfterArgs, ExtraGuards, none, AfterGuards, ContextTuples) when ExtraGuards /= [] ->
    add_extra_guards(AfterArgs, ExtraGuards, [], AfterGuards, ContextTuples);
add_extra_guards(AfterArgs, [ExtraGuards|MoreExtraGuards], [T|_]=Guards, AfterGuards, {StartContext, FinishContext}=ContextTuples) when ExtraGuards /= [] ->
    LnNum = case T of {_,L0} -> L0; {_,L0,_} -> L0; {_,L0,_,_,_} -> L0 end,
    {ok_reversed, DisjunctionsL} = proc_split_along_disjunctions(ExtraGuards),
    {ok_reversed, DisjunctionsR} = proc_split_along_disjunctions(Guards),
    Disjunctions2 = [[StartContext|L] ++ [FinishContext,{',',LnNum} | R] || L <- DisjunctionsL, R <- DisjunctionsR],
    NewGuardTokens = proc_combine_with_disjunctions(lists:reverse(Disjunctions2)),
    add_extra_guards(AfterArgs, MoreExtraGuards, NewGuardTokens, AfterGuards, ContextTuples);
add_extra_guards([{')',LnNum}=TE], [NewGuards|ExtraGuards], [], AfterGuards, ContextTuples) ->
    add_extra_guards([TE,{'when',LnNum}], ExtraGuards, NewGuards, AfterGuards, ContextTuples).


-spec add_extra_argument_patterns/3
    :: (tokens(), [{any(),tokens()}], _) ->
        {ok, tokens(), [{any(),tokens()}]}.
add_extra_argument_patterns(Arguments, ArgumentAssocs, ContextTuples) ->
    add_extra_argument_patterns(Arguments, ArgumentAssocs, [], [], ContextTuples).
add_extra_argument_patterns([{'var',L,Var}=T|Rest], ArgumentAssocs, SoFar, AssocsSet,  {StartContext, FinishContext}=ContextTuples) ->
    case orddict:find(Var, ArgumentAssocs) of
        {ok, Pattern} ->
            add_extra_argument_patterns(Rest, ArgumentAssocs, [FinishContext|add_line_info_and_reverse(L, Pattern)] ++ [StartContext,{'=',L},T|SoFar], [Var|AssocsSet], ContextTuples);
        _ ->
            add_extra_argument_patterns(Rest, ArgumentAssocs, [T|SoFar], AssocsSet, ContextTuples)
    end;
add_extra_argument_patterns([T|Rest], ArgumentAssocs, SoFar, AssocsSet, ContextTuples) ->
    add_extra_argument_patterns(Rest, ArgumentAssocs, [T|SoFar], AssocsSet, ContextTuples);
add_extra_argument_patterns([], _, SoFar, AssocsSet, _ContextTuples) ->
    {ok, lists:reverse(SoFar), AssocsSet}.
add_line_info_and_reverse(L, Toks) ->
    add_line_info_and_reverse(L, Toks, []).
add_line_info_and_reverse(L, [{T1,Z,T2}|Toks], SoFar) when is_integer(Z) ->
    add_line_info_and_reverse(L, Toks, [{T1,L,T2}|SoFar]);
add_line_info_and_reverse(L, [{T0,Z}|Toks], SoFar) when is_atom(T0), is_integer(Z) ->
    add_line_info_and_reverse(L, Toks, [{T0,L}|SoFar]);
add_line_info_and_reverse(_, [], SoFar) ->
    SoFar.


proc_pattern_guards_pattern_template(State, Arguments) ->
    proc_pattern_guards_pattern_template(State, Arguments, no_subst, []).
proc_pattern_guards_pattern_template(State, [{'\\',_},{'\\',_},{atom,_,PattTemplate},{'(',_}|Rest], no_subst, SoFar) ->
    {ok, Args, [{')',_}|Rest2]} = proc_pattern_template_arguments(Rest),
    Arity = length(Args),
    %% io:format("Pattern template: ~w/~w ~w~n",[PattTemplate, length(Args), Args]),
    {ok, NewState, Disjunctions, NewInlineContext} = template_pattern_to_subst_list(State, PattTemplate, Arity, Args),
    proc_pattern_guards_pattern_template(NewState, Rest2, {subst, lists:reverse(SoFar), Disjunctions, NewInlineContext}, []);
proc_pattern_guards_pattern_template(State, [{context_push_state,_,InlineContext,Identifier,ERGFile}=ContextToken|Rest], St, SoFar) -> 
    % io:format("proc_pattern_guards_pattern_template :: context_push_state~n",[]),
    NewContextStack = [{Identifier, InlineContext} | State#udg_state.context_stack],
    case find_circular_reference_in_erg_nesting(ERGFile, State#udg_state.erg_files_nesting) of
        false -> 
            NewFileNesting = [ERGFile | State#udg_state.erg_files_nesting],
            NewState = State#udg_state{
                context_stack = NewContextStack,
                erg_files_nesting = NewFileNesting },
            proc_pattern_guards_pattern_template(NewState, Rest, St, [ContextToken|SoFar]);
        true ->
            error({circular_or_recursive_references_in_erg_source_files, ERGFile, State#udg_state.erg_files_nesting})
    end;
proc_pattern_guards_pattern_template(State, [{context_pop_state,_,_InlineContext,Identifier,ERGFile}=ContextToken|Rest], St, SoFar) ->
    % io:format("context_pop_state 3 ~w~n",[Identifier]),
    [{PrevIdentifier,_}|RestOfContextStack] = State#udg_state.context_stack,
    [PrevERGFile|RestOfERGFilesNesting] = State#udg_state.erg_files_nesting,
    if PrevIdentifier =:= Identifier andalso PrevERGFile =:= ERGFile ->
        NewState = State#udg_state{
            context_stack = RestOfContextStack,
            erg_files_nesting = RestOfERGFilesNesting },
        proc_pattern_guards_pattern_template(NewState, Rest, St, [ContextToken|SoFar])
    end;
proc_pattern_guards_pattern_template(State, [T|Rest], St, SoFar) ->
    proc_pattern_guards_pattern_template(State, Rest, St, [T|SoFar]);
proc_pattern_guards_pattern_template(State, [], St, SoFar) ->
    case St of
        {subst, LeftSide, PatternDisjunctions, NewInlineContext} ->
            {subst, State, LeftSide, PatternDisjunctions, NewInlineContext, lists:reverse(SoFar)};
        no_subst -> no_subst
    end.

find_circular_reference_in_erg_nesting(_ERGFile, []) -> false;
find_circular_reference_in_erg_nesting({FileName,_}=ERGSource, ERGFilesNesting) when is_binary(FileName) ->
    lists:any(fun(A) -> A =:= ERGSource end, ERGFilesNesting).

proc_pattern_template_arguments(Tokens) ->
    proc_pattern_template_arguments(Tokens,[],[],[]).
proc_pattern_template_arguments([{',', _} | Rest], SoFar, [], Args) ->
    proc_pattern_template_arguments(Rest, [], [], [lists:reverse(SoFar)|Args]);
proc_pattern_template_arguments([{')', _}=T | Rest], SoFar, [], Args) ->
    {ok, lists:reverse([lists:reverse(SoFar)|Args]), [T|Rest]};
proc_pattern_template_arguments([{C, _}=T | Rest], SoFar, [O|H], Args)
when O =:= '(', C =:= ')'; O =:= '[', C =:= ']'; O =:= '{', C =:= '}'; O =:= '<<', C =:= '>>' ->
    proc_pattern_template_arguments(Rest, [T|SoFar], H, Args);
proc_pattern_template_arguments([{O, _}=T | Rest], SoFar, H, Args)
when O =:= '('; O =:= '['; O =:= '{'; O =:= '<<' ->
    proc_pattern_template_arguments(Rest, [T|SoFar], [O|H], Args);
proc_pattern_template_arguments([Token|Rest], SoFar, N, Args) ->
    proc_pattern_template_arguments(Rest, [Token|SoFar], N, Args).


proc_udg_function_not_nested(Rest2, SoFar) ->
    case next_token_without_context_nesting(SoFar) of
        [{',',_}|_] -> true;
        [{';',_}|_] -> true;
        [] -> true;
        _ -> false
    end andalso case next_token_without_context_nesting(Rest2) of
        [{',',_}|_] -> true;
        [{';',_}|_] -> true;
        [{'->',_}|_] -> true;
        [] -> true;
        _ -> false
    end.

next_token_without_context_nesting([{context_pop_state,_,_,_,_}|Rest]) ->
    next_token_without_context_nesting(Rest);
next_token_without_context_nesting([{context_push_state,_,_,_,_}|Rest]) ->
    next_token_without_context_nesting(Rest);
next_token_without_context_nesting(Rest) ->
    Rest.

proc_udg_function_in_boolean_expression(Rest2, SoFar) ->
    proc_udg_function_in_boolean_expression_left(SoFar) andalso
    proc_udg_function_in_boolean_expression_right(Rest2).

proc_udg_function_in_boolean_expression_left(Tokens) ->
    proc_udg_function_in_boolean_expression_left(Tokens, 1, []).
proc_udg_function_in_boolean_expression_left([], _, []) -> true;
proc_udg_function_in_boolean_expression_left([{T,_}|_], _, []) when T =:= ','; T =:= ';' -> true;
proc_udg_function_in_boolean_expression_left([{'orelse',_}|Tokens], _, []) ->
    proc_udg_function_in_boolean_expression_left(Tokens, 2, []);
proc_udg_function_in_boolean_expression_left([{'andalso',_}|Tokens], _, []) ->
    proc_udg_function_in_boolean_expression_left(Tokens, 2, []);
proc_udg_function_in_boolean_expression_left([{O,_}|Tokens], _, []) when O =:= '(' ->
    proc_udg_function_in_boolean_expression_left(Tokens, 1, []);
proc_udg_function_in_boolean_expression_left([{C,_}|Tokens], 2, H) when C =:= ')'; C =:= ']'; C =:= '}'; C =:= '>>' ->
    proc_udg_function_in_boolean_expression_left(Tokens, 2, [C|H]);
proc_udg_function_in_boolean_expression_left([{O,_}|Tokens], 2, [C|H]) when O =:= '(', C =:= ')'; O =:= '[', C =:= ']'; O =:= '{', C =:= '}'; O =:= '<<', C =:= '>>' ->
    proc_udg_function_in_boolean_expression_left(Tokens, 2, H);
proc_udg_function_in_boolean_expression_left([_|Tokens], 2, H) ->
    proc_udg_function_in_boolean_expression_left(Tokens, 2, H);
proc_udg_function_in_boolean_expression_left([{context_push_state,_,_,_,_}|Tokens], A, H) ->
    proc_udg_function_in_boolean_expression_left(Tokens, A, H);
proc_udg_function_in_boolean_expression_left([_T|_Tokens], 1, []) -> false.

proc_udg_function_in_boolean_expression_right(Tokens) ->
    proc_udg_function_in_boolean_expression_right(Tokens, 1, []).
proc_udg_function_in_boolean_expression_right([{T,_}|_], _, []) when T =:= ','; T =:= ';'; T =:= '->' -> true;
proc_udg_function_in_boolean_expression_right([{'orelse',_}|Tokens], _, []) ->
    proc_udg_function_in_boolean_expression_right(Tokens, 2, []);
proc_udg_function_in_boolean_expression_right([{'andalso',_}|Tokens], _, []) ->
    proc_udg_function_in_boolean_expression_right(Tokens, 2, []);
proc_udg_function_in_boolean_expression_right([{C,_}|Tokens], _, []) when C =:= ')' ->
    proc_udg_function_in_boolean_expression_right(Tokens, 1, []);
proc_udg_function_in_boolean_expression_right([{O,_}|Tokens], 2, H) when O =:= '('; O =:= '['; O =:= '{'; O =:= '<<' ->
    proc_udg_function_in_boolean_expression_right(Tokens, 2, [O|H]);
proc_udg_function_in_boolean_expression_right([{C,_}|Tokens], 2, [O|H]) when O =:= '(', C =:= ')'; O =:= '[', C =:= ']'; O =:= '{', C =:= '}'; O =:= '<<', C =:= '>>' ->
    proc_udg_function_in_boolean_expression_right(Tokens, 2, H);
proc_udg_function_in_boolean_expression_right([_|Tokens], 2, H) ->
    proc_udg_function_in_boolean_expression_right(Tokens, 2, H);
proc_udg_function_in_boolean_expression_right([{context_pop_state,_,_,_,_}|Tokens], A, H) ->
    proc_udg_function_in_boolean_expression_right(Tokens, A, H);
proc_udg_function_in_boolean_expression_right([_T|_Tokens], 1, []) -> false.

proc_pattern_guards_is_up_udg_function_have_function(UDGKind, PerhapsGuardFun, State, Line, Rest, LeftDisj, SoFar) ->
    {ok, Inside, Rest2} = proc_up_to_close_paren(Rest),
    case proc_udg_function_not_nested(Rest2, SoFar) of
        false when UDGKind =:= 'up' ->
            error({a_non_nested_udg_is_being_nested_in_an_expression, PerhapsGuardFun});
        NotNested ->
            {ok, Arity, Args} = arity_and_arguments(Inside),
            case NotNested orelse proc_udg_function_in_boolean_expression(Rest2, SoFar) of
                false ->
                    error(a_uep_udg_function_needs_to_be_in_andalso_orelse_expression);
                _ ->
                    %% io:format("proc ~w/~w ~w~n",[PerhapsGuardFun, Arity, Args]),
                    {ok, NewState, NewGuardsAndArgAssocs, NewInlineContext} = udg_function_to_subst_list(State, PerhapsGuardFun, Arity, Args),
                    proc_pattern_guards_is_up_udg_function(NewState, Rest2,
                        {subst, 
                            case NotNested of
                                true -> NewGuardsAndArgAssocs;
                                false -> lists:map(
                                    fun({guard_disjunction,NewGuards,ArgAssocs}) ->
                                        {guard_disjunction, [{'(',Line}|NewGuards] ++ [{')',Line}], ArgAssocs}
                                    end, NewGuardsAndArgAssocs) ++
                                    [{guard_disjunction,[{atom,Line,false}],[]}]
                            end,
                            lists:reverse(SoFar), NewInlineContext}, LeftDisj, [])
            end
    end.

proc_pattern_guards_is_up_udg_function(State, GuardTokens) when is_list(GuardTokens) ->
    proc_pattern_guards_is_up_udg_function(State, GuardTokens, no_subst, [], []);
proc_pattern_guards_is_up_udg_function(_State, none) -> no_subst.
proc_pattern_guards_is_up_udg_function(State, [{';',_}|Rest], no_subst, LeftDisj,SoFar) ->
    proc_pattern_guards_is_up_udg_function(State, Rest, no_subst, [lists:reverse(SoFar)|LeftDisj],[]);
proc_pattern_guards_is_up_udg_function(State, [{';',_}|Rest], {subst, NewGuardsAndArgAssocs, LeftGuardTokens, NewInlineContext}, LeftDisj,SoFar) ->
    proc_pattern_guards_is_up_udg_function(State, Rest, {subst_disj, NewGuardsAndArgAssocs, LeftGuardTokens, NewInlineContext, lists:reverse(SoFar)}, LeftDisj,[]);
proc_pattern_guards_is_up_udg_function(State, [{atom,_,PerhapsGuardFun}=Token1,{'(', Line}=Token2|Rest], no_subst, LeftDisj, SoFar) when is_atom(PerhapsGuardFun) ->
    case atom_to_list(PerhapsGuardFun) of
        %% is_u_ and is_ue_ are handled by a later stage.
        [$i,$s,$_,$u,$p,$_|_] ->
            proc_pattern_guards_is_up_udg_function_have_function(up, PerhapsGuardFun, State, Line, Rest, LeftDisj, SoFar);
        [$i,$s,$_,$u,$e,$p,$_|_] ->
            proc_pattern_guards_is_up_udg_function_have_function(uep, PerhapsGuardFun, State, Line, Rest, LeftDisj, SoFar);
        _ ->
            proc_pattern_guards_is_up_udg_function(State, Rest, no_subst, LeftDisj, [Token2, Token1 | SoFar])
    end;
proc_pattern_guards_is_up_udg_function(State, [{context_push_state,_,InlineContext,Identifier,ERGFile}=ContextToken|GuardTokens], St, LeftDisj, SoFar) ->
    % io:format("proc_pattern_guards_is_up_udg_function context_push_state~n",[]),
    NewContextStack = [{Identifier, InlineContext} | State#udg_state.context_stack],
    case find_circular_reference_in_erg_nesting(ERGFile, State#udg_state.erg_files_nesting) of
        false ->
            NewFileNesting = [ERGFile | State#udg_state.erg_files_nesting],
            NewState = State#udg_state{
                context_stack = NewContextStack,
                erg_files_nesting = NewFileNesting },
            proc_pattern_guards_is_up_udg_function(NewState, GuardTokens, St, LeftDisj, [ContextToken|SoFar]);
        true ->
            error({circular_or_recursive_references_in_erg_source_files, ERGFile, State#udg_state.erg_files_nesting})
    end;
proc_pattern_guards_is_up_udg_function(State, [{context_pop_state,_,_InlineContext,Identifier,ERGFile}=ContextToken|GuardTokens], St, LeftDisj, SoFar) ->
    % io:format("context_pop_state 2 ~w~n",[Identifier]),
    [{PrevIdentifier, _} | RestOfContextStack] = State#udg_state.context_stack,
    [PrevERGFile | RestOfERGFilesNesting] = State#udg_state.erg_files_nesting,
    if PrevIdentifier =:= Identifier andalso PrevERGFile =:= ERGFile ->
        NewState = State#udg_state{
            context_stack = RestOfContextStack,
            erg_files_nesting = RestOfERGFilesNesting },
        proc_pattern_guards_is_up_udg_function(NewState, GuardTokens, St, LeftDisj, [ContextToken|SoFar])
    end;
proc_pattern_guards_is_up_udg_function(State, [T|GuardTokens], St, LeftDisj, SoFar) ->
    proc_pattern_guards_is_up_udg_function(State, GuardTokens, St, LeftDisj, [T|SoFar]);
proc_pattern_guards_is_up_udg_function(State, [], St, LeftDisj, SoFar) ->
    case St of
        no_subst -> no_subst;
        {subst_disj, NewGuardTokensAndArgumentAssocs, LeftTokens, NewInlineContext, RightTokens} when is_list(NewGuardTokensAndArgumentAssocs) ->
            {ok, {StartContext, FinishContext}} = token_context_tuples(NewInlineContext),
            RestTokens = lists:reverse(SoFar),
            {subst_with_context, State, LeftDisj, lists:map(
                fun({guard_disjunction, NewGuardTokens, ArgumentAssocs}) ->
                    {guard_disjunction_with_context, lists:append([
                        LeftTokens, [StartContext], NewGuardTokens, [FinishContext], RightTokens]), ArgumentAssocs}
                end, NewGuardTokensAndArgumentAssocs), RestTokens, {StartContext, FinishContext}};
        {subst, NewGuardTokensAndArgumentAssocs, LeftTokens, NewInlineContext} when is_list(NewGuardTokensAndArgumentAssocs) ->
            {ok, {StartContext, FinishContext}} = token_context_tuples(NewInlineContext),
            RightTokens = lists:reverse(SoFar),
            {subst_with_context, State, LeftDisj, lists:map(
                fun({guard_disjunction, NewGuardTokens, ArgumentAssocs}) ->
                    {guard_disjunction_with_context, lists:append([
                        LeftTokens, [StartContext], NewGuardTokens, [FinishContext], RightTokens]), ArgumentAssocs}
                end, NewGuardTokensAndArgumentAssocs), [], {StartContext, FinishContext}}
    end.


expand_simple_u_guards(State, Guards) ->
    case proc_split_along_disjunctions(Guards) of
        {ok_reversed, Disjunctions} ->
            {NewState, Disjunctions2} = lists:foldl(fun proc_conjunctions/2, {State, []}, Disjunctions),
            NewGuardTokens = proc_combine_with_disjunctions(Disjunctions2),
            {NewState, NewGuardTokens};
        none ->
            {State, none}
    end.
    
proc_conjunctions(CL, {State, CurrDisjs}) ->
    case proc_guards(State, CL) of
        {subst, NewState, Disjunctions} ->
            {NewState2, Disjunctions2} = lists:foldl(fun proc_conjunctions/2, {NewState,[]}, Disjunctions),
            {NewState2, lists:reverse(Disjunctions2) ++ CurrDisjs};
        no_subst ->
            {State, [CL|CurrDisjs]}
    end.
    
proc_guards_have_function(UDGKind, State, PerhapsGuardFun, Line, Rest, SoFar) ->
    {ok, Inside, Rest2} = proc_up_to_close_paren(Rest),
    case proc_udg_function_not_nested(Rest2, SoFar) of
        false when UDGKind =:= 'u' ->
            error({a_non_nested_udg_is_being_nested_in_an_expression, PerhapsGuardFun});
        NotNested ->
            {ok, Arity, Args} = arity_and_arguments(Inside),
            %% io:format("proc ~w/~w ~w~n",[PerhapsGuardFun, Arity, Args]),
            {ok, NewState, NewGuardsAndArgAssocs, NewInlineContext} = udg_function_to_subst_list(State, PerhapsGuardFun, Arity, Args),
            NewGuardTokens = lists:reverse(lists:foldl(
                fun({guard_disjunction,GuardTokens,[]},List) ->
                    [case NotNested of
                        true -> GuardTokens;
                        false -> [{'(',Line}|GuardTokens] ++ [{')',Line}]
                     end | List]
                end, [], NewGuardsAndArgAssocs)),
            proc_guards(NewState, Rest2, {subst, NewGuardTokens, lists:reverse(SoFar), NewInlineContext}, [])
    end.
proc_guards(State, GuardTokens) ->
    proc_guards(State, GuardTokens, no_subst, []).
proc_guards(State, [{atom,_,PerhapsGuardFun}=Token1,{'(', Line}=Token2|Rest], no_subst, SoFar) when is_atom(PerhapsGuardFun) ->
    case atom_to_list(PerhapsGuardFun) of
        [$i,$s,$_,$u,$e,$p,$_|_] ->
            error(unexpected_appearance_of_up_or_uep_function_at_this_stage);
        [$i,$s,$_,$u,$p,$_|_] ->
            error(unexpected_appearance_of_up_or_uep_function_at_this_stage);
        [$i,$s,$_,$u,$e,$_|_] ->
            proc_guards_have_function(ue, State, PerhapsGuardFun, Line, Rest, SoFar);
        [$i,$s,$_,$u,$_|_] ->
            proc_guards_have_function(u, State, PerhapsGuardFun, Line, Rest, SoFar);
        _ ->
            proc_guards(State, Rest, no_subst, [Token2, Token1 | SoFar])
    end;
proc_guards(State, [{context_push_state,_,InlineContext,Identifier,ERGFile}=ContextToken|Rest], St, SoFar) ->
    % io:format("proc_guards context_push_state~n",[]),
    NewContextStack = [{Identifier, InlineContext} | State#udg_state.context_stack],
    case find_circular_reference_in_erg_nesting(ERGFile, State#udg_state.erg_files_nesting) of
        false ->
            NewFileNesting = [ERGFile | State#udg_state.erg_files_nesting],
            NewState = State#udg_state{
                context_stack = NewContextStack,
                erg_files_nesting = NewFileNesting },
            proc_guards(NewState, Rest, St, [ContextToken|SoFar]);
        true ->
            error({circular_or_recursive_references_in_erg_source_files, ERGFile, State#udg_state.erg_files_nesting})
    end;
proc_guards(State, [{context_pop_state,_,_InlineContext,Identifier,ERGFile}=ContextToken|Rest], St, SoFar) ->
    % io:format("context_pop_state 1 ~w~n",[Identifier]),
    [{PrevIdentifier,_} | RestOfContextStack] = State#udg_state.context_stack,
    [PrevERGFile | RestOfERGFilesNesting] = State#udg_state.erg_files_nesting,
    if PrevIdentifier =:= Identifier andalso PrevERGFile =:= ERGFile ->
        NewState = State#udg_state{
            context_stack = RestOfContextStack,
            erg_files_nesting = RestOfERGFilesNesting },
        proc_guards(NewState, Rest, St, [ContextToken|SoFar])
    end;
proc_guards(State, [Token|Rest], St, SoFar) ->
    proc_guards(State, Rest, St, [Token|SoFar]);
proc_guards(State, [], St, SoFar) ->
    case St of
        no_subst -> no_subst;
        {subst, NewGuardTokens, LeftTokens, NewInlineContext} when is_list(NewGuardTokens) ->
            {ok, {StartContext, FinishContext}} = token_context_tuples(NewInlineContext),
            RightTokens = lists:reverse(SoFar),
            {subst, State, lists:map(
                fun(NewDisjunction) ->
                    lists:append([LeftTokens, [StartContext], NewDisjunction, [FinishContext], RightTokens])
                end, NewGuardTokens)}
    end.


-spec proc_up_to_close_paren/1 :: (tokens()) -> {ok, tokens(), tokens()}.
proc_up_to_close_paren(Tokens) ->
    proc_up_to_close_paren(Tokens, 1, []).
proc_up_to_close_paren([{')', _}|Rest], 1, Before) ->
    {ok, lists:reverse(Before), Rest};
proc_up_to_close_paren([{')', _}=Token|Rest], N, Before) when N > 1 ->
    proc_up_to_close_paren(Rest, N-1, [Token|Before]);
proc_up_to_close_paren([{'(', _}=Token|Rest], N, Before) ->
    proc_up_to_close_paren(Rest, N+1, [Token|Before]);
proc_up_to_close_paren([Token|Rest], N, Before) when element(1,Token) /= ')', element(1,Token) /= '(' ->
    proc_up_to_close_paren(Rest, N, [Token|Before]).

proc_combine_with_disjunctions([List|Rest]) ->
    LineNum = case List of
        [{_,N}|_] -> N;
        [{_,N,_}|_] -> N;
        [{_,N,_,_,_}|_] -> N
    end,
    proc_combine_with_disjunctions(LineNum,Rest,[List]).
proc_combine_with_disjunctions(LineNum,[List|Rest], AccList) ->
    LineNum2 = case List of
        [{_,N}|_] -> N;
        [{_,N,_}|_] -> N;
        [{_,N,_,_,_}|_] -> N
    end,
    proc_combine_with_disjunctions(LineNum,Rest,[List,[{';',LineNum2}]|AccList]);
proc_combine_with_disjunctions(_,[],AccList) ->
    lists:append(lists:reverse(AccList)).

%% Preprocessor functions
%%

preprocess_file(State, InputFile) ->
    case file:read_file(InputFile) of
        {ok, InputString} -> preprocess_string(State, binary_to_list(InputString));
        Error -> Error
    end.
    
preprocess_string(State, InputString) ->
    sequence(
        [ fun tokens/2,
          fun proc_source/2 ],
        State,
        InputString).

%%preprocess_string_guard_expansion(State, Tokens) ->
%%    proc_source(State, Tokens).


sequence([], State, Input) -> {ok, State, Input};
sequence([Fun|Rest], State, Input) when is_function(Fun) ->
    case Fun(State, Input) of
        {ok, NewState, Result} -> sequence(Rest, NewState, Result);
        Error -> Error
    end.
    
tokens(State, InputString) ->
    case erl_scan:string(InputString) of
        {ok, List, _} ->
            {ok, State, List};
        Error -> Error
    end.

%% Functions
%%

-spec arity_and_arguments/1 :: (tokens()) -> {ok, udg_arity(), [tokens()]}.
arity_and_arguments(Tokens) ->
    arity_and_arguments(Tokens, [], [], {0,0,0}).
arity_and_arguments([{',',_}|Rest], Arg, ArgList, {0,0,0}) ->
    arity_and_arguments(Rest, [], [lists:reverse(Arg)|ArgList], {0,0,0});
arity_and_arguments([{'(',_}=Token|Rest], Arg, ArgList, {P,B,C}) ->
    arity_and_arguments(Rest, [Token|Arg], ArgList, {P+1,B,C});
arity_and_arguments([{'[',_}=Token|Rest], Arg, ArgList, {P,B,C}) ->
    arity_and_arguments(Rest, [Token|Arg], ArgList, {P,B+1,C});
arity_and_arguments([{'{',_}=Token|Rest], Arg, ArgList, {P,B,C}) ->
    arity_and_arguments(Rest, [Token|Arg], ArgList, {P,B,C+1});
arity_and_arguments([{')',_}=Token|Rest], Arg, ArgList, {P,B,C}) when P > 0 ->
    arity_and_arguments(Rest, [Token|Arg], ArgList, {P-1,B,C});
arity_and_arguments([{']',_}=Token|Rest], Arg, ArgList, {P,B,C}) when B > 0 ->
    arity_and_arguments(Rest, [Token|Arg], ArgList, {P,B-1,C});
arity_and_arguments([{'}',_}=Token|Rest], Arg, ArgList, {P,B,C}) when C > 0 ->
    arity_and_arguments(Rest, [Token|Arg], ArgList, {P,B,C-1});
arity_and_arguments([Token|Rest], Arg, ArgList, Nesting) when element(1, Token) /= ',' ->
    arity_and_arguments(Rest, [Token|Arg], ArgList, Nesting);
arity_and_arguments([], Arg, ArgList0, {0,0,0}) ->
    ArgList = lists:reverse([lists:reverse(Arg)|ArgList0]),
    {ok, length(ArgList), ArgList}.


-spec get_erg_file_for_udg/2
    :: (udg_preprop_state(), {udg, _, _} | {pattern_template, _, _}) -> 
        {ok, string()}.
get_erg_file_for_udg(State, UDG) ->
    {UDGImportList, ERGSourceFile} = 
        case State#udg_state.context_stack of
            [] -> { State#udg_state.udg_import_list, inline_erl_source };
            [{_,InlineContext}|_] ->
                { udg_erg:erg_include_list_from_inline_context(InlineContext),
                  udg_erg:erg_source_file_from_inline_context(InlineContext) }
        end,
    case orddict:find(UDG, UDGImportList) of
        {ok, Found} -> {ok, udg_erg_includes:no_slashes(atom_to_list(Found))};
        error -> error({erg_file_for_udg_function_not_specified, UDG, ERGSourceFile})
    end.

-spec udg_function_to_subst_list/4
    :: (udg_preprop_state(), udg_fun_name(), udg_arity(), [tokens()]) ->
        {ok, udg_preprop_state(), guards_and_arg_assocs(), inline_context()}.
udg_function_to_subst_list(State, UDGAtom, Arity, Arguments) when is_atom(UDGAtom), is_list(Arguments), is_integer(Arity) ->
    case orddict:find({UDGAtom, Arity}, State#udg_state.extern_udg_list) of
        {ok, ApplicationNameAtom} ->
            FileNameNoExt = udg_erg_includes:erg_filename_from_udg_name(UDGAtom),
            ApplicationName = udg_erg_includes:no_slashes(atom_to_list(ApplicationNameAtom)),
            case find_udg_file(State, ["extern", ApplicationName], FileNameNoExt) of
                {ok, NewState, UDGDefinitions} ->
                    {ok, Disjunctions, NewInlineContext} = udg_erg:get_udg_function({udg,UDGAtom,Arity}, Arguments, UDGDefinitions),
                    {ok, NewState, Disjunctions, NewInlineContext};
                {error, _} ->
                    case find_udg_file(State, ["extern"], lists:append([ApplicationName, "-", FileNameNoExt])) of
                        {ok, NewState, UDGDefinitions} ->
                            {ok, Disjunctions, NewInlineContext} = udg_erg:get_udg_function({udg,UDGAtom,Arity}, Arguments, UDGDefinitions),
                            {ok, NewState, Disjunctions, NewInlineContext};
                        {error, _} ->
                            error({could_not_find_extern_udg_file, FileNameNoExt, {udg, UDGAtom, Arity}})
                            %% NewInlineContext = no_context_needed,
                            %% {ok, State, [{guard_disjunction, [{'atom',0,'true'}], []}], NewInlineContext}
                    end
            end;
        _ ->
            {ok, FileNameNoExt} = get_erg_file_for_udg(State, {udg, UDGAtom, Arity}),
            case find_udg_file(State, [], FileNameNoExt) of
                {ok, NewState, UDGDefinitions} ->
                    {ok, Disjunctions, NewInlineContext} = udg_erg:get_udg_function({udg,UDGAtom,Arity}, Arguments, UDGDefinitions),
                    {ok, NewState, Disjunctions, NewInlineContext};
                {error, _} ->
                    error({could_not_find_udg_erg_file, FileNameNoExt, {udg, UDGAtom, Arity}})
            end
    end.


-spec template_pattern_to_subst_list/4
    :: (udg_preprop_state(), udg_fun_name(), udg_arity(), [tokens()]) ->
        {ok, udg_preprop_state(), pattern_template_disjunctions(), inline_context()}.
template_pattern_to_subst_list(State, TemplatePatternName, Arity, Arguments) when is_atom(TemplatePatternName), is_list(Arguments), is_integer(Arity) ->
    {ok, FileNameNoExt} = get_erg_file_for_udg(State, {pattern_template, TemplatePatternName, Arity}),
    %%FileNameNoExt = udg_erg_includes:no_slashes(atom_to_list(TemplatePatternName)),
    case find_udg_file(State, [], FileNameNoExt) of
        {ok, NewState, UDGDefinitions} ->
            {ok, Disjunctions, NewInlineContext} = udg_erg:get_udg_pattern_template({pattern_template,TemplatePatternName,Arity}, Arguments, UDGDefinitions),
            {ok, NewState, Disjunctions, NewInlineContext};
        {error, _} ->
            error({could_not_find_pattern_template_erg_file, FileNameNoExt, {pattern_template, TemplatePatternName, Arity}})
    end.


-spec find_udg_file/3
    :: (udg_preprop_state(), [string()], string()) ->
        {ok, udg_preprop_state(), udg_definitions()} | {error, any()}.
find_udg_file(State, SuffixDirs, Name) ->
    SourceFile = State#udg_state.input_file,
    FileName = lists:append([Name, ".erg"]),
    SourceDir = filename:dirname(SourceFile),
    AtomFileName = list_to_atom(filename:join(SuffixDirs ++ [FileName])),
    case orddict:find(AtomFileName, State#udg_state.loaded_erg_files) of
        {ok, UDGDefinitions} ->
            {ok, State, UDGDefinitions};
        _ ->
            find_udg_file(State, SuffixDirs, AtomFileName, FileName, SourceDir, State#udg_state.udg_search_path)
    end.
find_udg_file(State, SuffixDirs, AtomFileName, FileName, SourceDir, [RelPath|RestRelPaths]) ->
    InputFile = filename:join([SourceDir,RelPath] ++ SuffixDirs ++ [FileName]),
    case file:read_file_info(InputFile) of
        {ok, _} ->
            case udg_erg:read_udg_erg_file(InputFile) of
                {ok, UDGDefinitions, Warnings} ->
                    NewLoadedERG = orddict:store(AtomFileName, UDGDefinitions, State#udg_state.loaded_erg_files),
                    {ok, State#udg_state{loaded_erg_files = NewLoadedERG}, UDGDefinitions};
                {errors, Errors} ->
                    {error, Errors}
            end;
        _ ->
            find_udg_file(State, SuffixDirs, AtomFileName, FileName, SourceDir, RestRelPaths)
    end;
find_udg_file(_State, _SuffixDirs, _AtomFileName, _FileName, _SourceDir, []) ->
    {error,could_not_find_udg}.


-spec token_context_tuples/1 :: (inline_context()) -> {ok, {inline_context_token(), inline_context_token()}}.
token_context_tuples(InlineContext) ->
    Identifier = {erlang:phash2(InlineContext), random:uniform(1000000)},
    ERGSource = { iolist_to_binary(filename:basename(filename:rootname(
                      udg_erg:erg_source_file_from_inline_context(InlineContext)))),
                  udg_erg:erg_source_function_from_inline_context(InlineContext) },
    StartContext = {context_push_state,0,InlineContext,Identifier,ERGSource},
    FinishContext = {context_pop_state,0,InlineContext,Identifier,ERGSource},
    {ok, {StartContext, FinishContext}}.


proc_source_extern_udg(Rest, ExternUDGList) ->
    case Rest of
        [{'(',_}=_Token3|Rest2] ->
            {ok, Imports, [{dot,_} | Rest3]} = proc_up_to_close_paren(Rest2),
            {ok, ImportList} = parse_extern_list(Imports),
            NewExternUDGList = to_udg_module_tuple_pairs(ImportList) ++ ExternUDGList,
            {ok, NewExternUDGList, Rest3};
        _ -> error(paren_missing_after_extern_udg_directive)
    end.
to_udg_module_tuple_pairs([{Mod,UDGList}|Rest]) ->
    to_udg_module_tuple_pairs([{Mod,UDGList}|Rest], []).
to_udg_module_tuple_pairs([{Mod,UDGList}|Rest], Acc) when is_list(UDGList) ->
    to_udg_module_tuple_pairs(Rest, [lists:map(fun(UDG) -> {UDG, Mod} end, UDGList)|Acc]);
to_udg_module_tuple_pairs([], Acc) ->
    lists:reverse(Acc).
parse_extern_list([{'[',_}|Rest]) ->
    parse_extern_list(Rest, notset, notset, [], []).
parse_extern_list([{T,_}|Rest], {set, ModuleName}, {set, UDGList}, [], ImportsList) when T =:= ','; T =:= ']', Rest =:= [] ->
    NewImportsList = [{ModuleName, UDGList}|ImportsList],
    case T of
        ',' -> parse_extern_list(Rest, notset, notset, [], NewImportsList);
        ']' -> {ok, lists:reverse(NewImportsList)}
    end;
parse_extern_list([{atom,_,ModuleName},{'(',_},{'[',_}|Rest], notset, notset, [], ImportsList) ->
    parse_extern_list(Rest, {list_entry, ModuleName}, notset, [], ImportsList);
parse_extern_list([{atom,_,UDG},{'/',_},{integer,_,Arity}|Rest], {list_entry, ModuleName}, notset, UDGList, ImportsList) ->
    parse_extern_list(Rest, {after_entry, ModuleName}, notset, [{UDG,Arity}|UDGList], ImportsList);
parse_extern_list([{',',_}|Rest], {after_entry, ModuleName}, notset, UDGList, ImportsList) ->
    parse_extern_list(Rest, {list_entry, ModuleName}, notset, UDGList, ImportsList);
parse_extern_list([{']',_},{')',_}|Rest], {after_entry, ModuleName}, notset, UDGList, ImportsList) ->
    parse_extern_list(Rest, {set, ModuleName}, {set, lists:reverse(UDGList)}, [], ImportsList).
