
%%
%% udg_erg: ERG file loading and parsing of user defined guard and pattern
%%          template definitions.
%%
%% This module loads and parses user defined guards and pattern templates that
%% are defined in ERG source files to return a UDG definitions data structure.
%% The UDG definitions can then be cached by the preprocessor and can call
%% functions in this module to unify arguments from the Erlang source file with
%% the definitions to return the tokens to substitute.
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
-module(udg_erg).
-description('User defined guard erg preprocessor').
-author('Edward L. Blake < edwardlblake @ gmail.com >').

-export([ read_udg_erg_file/1, get_udg_function/3, get_udg_pattern_template/3,
    erg_include_list_from_inline_context/1,
    erg_source_file_from_inline_context/1,
    erg_source_function_from_inline_context/1 ]).

-type tokens() :: [tuple()].
-type inline_context() :: tuple().
-type udg_definitions() :: tuple().
-type variable_set() :: [atom()].
-type argument_actions() :: [ subst_guard | arg_assoc ].
-type guard_disjunction() :: {guard_disjunction, [any()], [any()]}.
-type conjunction_expression() :: {conjunction_list,[ any() ]}.
-type variable_assoc() :: [{atom(),atom()}].
-type pattern_argument_actions() :: [ direct_subst | pattern_subst ].
-type pattern_disjunction() :: {pattern_disjunction, tokens(), tokens()}.

erg_include_list_from_inline_context(InlineContext) ->
    case InlineContext of
        {context_includes, UDGIncludes, SelfIncludes, _, _} ->
            orddict:merge(included_and_self_included_fun(InlineContext),
                UDGIncludes, SelfIncludes)
    end.

included_and_self_included_fun(_InlineContext) ->
    fun (_NameAndArity, UDGIncludeValue, _SelfIncludedValue) ->
        UDGIncludeValue
    end.

erg_source_file_from_inline_context(InlineContext) ->
    case InlineContext of
        {context_includes, _, _, ERGSourceFile, _} ->
            ERGSourceFile
    end.

erg_source_function_from_inline_context(InlineContext) ->
    case InlineContext of
        {context_includes, _, _, _, NameAndArity} ->
            NameAndArity
    end.
    

%%
%% ERG reading and definition handling.

read_udg_erg_file(InputFile) ->
    case file:read_file(InputFile) of
        {ok, InputString} ->
            case read_udg_erg_string(binary_to_list(InputString)) of
                {ok, UDGDefinitionsList, Warnings} ->
                    {ok, _} = erg_definitions(UDGDefinitionsList, InputFile),
                    {ok, {erg_file, InputFile, UDGDefinitionsList}, Warnings};
                {errors, Errors} -> {errors, Errors}
            end;
        Error -> Error
    end.

erg_definitions({udg_definitions, UDGDefinitions}, ERGSourceFile) when is_list(UDGDefinitions) ->
    erg_definitions(UDGDefinitions, ERGSourceFile, []).
erg_definitions([{user_defined_guard,ERGName,_UDGList}|UDGDefinitions], ERGSourceFile, State) ->
    ERGFileName = filename:basename(filename:rootname(ERGSourceFile)),
    case atom_to_list(ERGName) of
        ERGNameStr when ERGNameStr =:= ERGFileName ->
            erg_definitions(UDGDefinitions, ERGSourceFile, State);
        ERGNameStr ->
            error({the_erg_filename_and_user_defined_guard_source_name_differ, ERGFileName, ERGNameStr})
    end;
erg_definitions([{erg_includes, _}|UDGDefinitions], ERGSourceFile, State) ->
    erg_definitions(UDGDefinitions, ERGSourceFile, State);
erg_definitions([{defs,_}|UDGDefinitions], ERGSourceFile, State) ->
    erg_definitions(UDGDefinitions, ERGSourceFile, State);
erg_definitions([], _ERGSourceFile, State) ->
    {ok, State}.


-spec get_arg_types/1 :: ([tokens()]) -> [ simple | expr_or_pattern ].
get_arg_types(Args) ->
    lists:map(fun([{var,_,_Var}]) -> simple; ([_|_Rest]) -> expr_or_pattern end, Args).
get_arg_types_across_disjunctions([{_, _TPattern1, Args1, _Guards1}|RestOfDisjunctions]) ->
    lists:foldl(
        fun({_, _TPattern, Args, _Guards}, PrevTypes) ->
            get_arg_types_merge_types(PrevTypes, get_arg_types(Args))
        end, get_arg_types(Args1), RestOfDisjunctions).
get_arg_types_merge_types(ArgTypes1, ArgTypes2) ->
    get_arg_types_merge_types(ArgTypes1, ArgTypes2, []).
get_arg_types_merge_types([expr_or_pattern|ArgTypes1], [_|ArgTypes2], ArgsAcc) ->
    get_arg_types_merge_types(ArgTypes1, ArgTypes2, [expr_or_pattern|ArgsAcc]);
get_arg_types_merge_types([_|ArgTypes1], [expr_or_pattern|ArgTypes2], ArgsAcc) ->
    get_arg_types_merge_types(ArgTypes1, ArgTypes2, [expr_or_pattern|ArgsAcc]);
get_arg_types_merge_types([simple|ArgTypes1], [simple|ArgTypes2], ArgsAcc) ->
    get_arg_types_merge_types(ArgTypes1, ArgTypes2, [simple|ArgsAcc]);
get_arg_types_merge_types([], [], ArgsAcc) ->
    lists:reverse(ArgsAcc).


-spec get_arg_actions/2
    :: ([ simple | expr_or_pattern ],[ simple | expr_or_pattern ]) ->
        {ok, argument_actions()} | {error, integer()}.
get_arg_actions(Args1, Args2) ->
    get_arg_actions(Args1, Args2, 1, []).
get_arg_actions([expr_or_pattern|Args1], [simple|Args2], N, Actions) ->
    get_arg_actions(Args1, Args2, N+1, [subst_guard|Actions]);
get_arg_actions([simple|Args1], [expr_or_pattern|Args2], N, Actions) ->
    get_arg_actions(Args1, Args2, N+1, [arg_assoc|Actions]);
get_arg_actions([simple|Args1], [simple|Args2], N, Actions) ->
    get_arg_actions(Args1, Args2, N+1, [subst_guard|Actions]);
get_arg_actions([],[],_,Actions) -> {ok, lists:reverse(Actions)};
get_arg_actions(_Args1,_Args2, N, _Actions) ->
    {error, N}.


-spec get_arg_actions_for_pattern_template/2
    :: ([ simple | expr_or_pattern ], [ simple | expr_or_pattern ]) ->
        {ok, pattern_argument_actions()} | {error, integer()}.
get_arg_actions_for_pattern_template(Args1, Args2) ->
    get_arg_actions_for_pattern_template(Args1, Args2, 1, []).
get_arg_actions_for_pattern_template([expr_or_pattern|Args1], [simple|Args2], N, Actions) ->
    get_arg_actions_for_pattern_template(Args1, Args2, N+1, [pattern_subst|Actions]);
get_arg_actions_for_pattern_template([simple|Args1], [simple|Args2], N, Actions) ->
    get_arg_actions_for_pattern_template(Args1, Args2, N+1, [direct_subst|Actions]);
get_arg_actions_for_pattern_template([],[],_,Actions) -> {ok, lists:reverse(Actions)};
get_arg_actions_for_pattern_template(_Args1,_Args2, N, _Actions) ->
    {error, N}.
    

-spec get_udg_function/3
    :: ({udg, atom(), integer()}, [tokens()], udg_definitions()) ->
        {ok, [any()], inline_context()} | {error, any()}.
get_udg_function({udg,_,_}=NameAndArity, InpugTokenArgs, UDGDefinitions) ->
    VarSet = ordsets:new(),
    case find_udg_definitions(NameAndArity, UDGDefinitions) of
        {ok, Disjunctions, UDGArgTypes, UDGIncludes, SelfIncludes, ERGSourceFile} ->
            InputArgTypes = get_arg_types(InpugTokenArgs),
            case get_arg_actions(InputArgTypes, UDGArgTypes) of
                {ok, ArgActions} ->
                    NewInlineContext =
                        {context_includes, orddict:from_list(UDGIncludes),
                            orddict:from_list(SelfIncludes), ERGSourceFile, NameAndArity},
                    {ok, lists:map(
                        fun({none, Args, Guards}) ->
                            get_udg_function_tokens(VarSet,InpugTokenArgs,ArgActions,Args,Guards)
                        end, Disjunctions), NewInlineContext};
                {error, ArgNumber} ->
                    {error, {cannot_have_expressions_on_this_argument, ArgNumber}}
            end;
        {error, Error} -> {error, Error}
    end.

    
-spec get_udg_pattern_template/3
    :: ({pattern_template, atom(), integer()}, [tokens()], udg_definitions()) ->
        {ok, [any()], inline_context()} | {error, any()}.
get_udg_pattern_template({pattern_template,_,_}=NameAndArity, InpugTokenArgs, UDGDefinitions) ->
    VarSet = ordsets:new(),
    case find_udg_definitions(NameAndArity, UDGDefinitions) of
        {ok, Disjunctions, UDGArgTypes, UDGIncludes, SelfIncludes, ERGSourceFile} ->
            InputArgTypes = get_arg_types(InpugTokenArgs),
            case get_arg_actions_for_pattern_template(InputArgTypes, UDGArgTypes) of
                {ok, ArgActions} ->
                    NewInlineContext =
                        {context_includes, orddict:from_list(UDGIncludes),
                            orddict:from_list(SelfIncludes), ERGSourceFile, NameAndArity},
                    {ok, lists:map(
                        fun({TPattern, Args, Guards}) ->
                            get_udg_pattern_template_tokens(VarSet,TPattern,InpugTokenArgs,ArgActions,Args,Guards)
                        end, Disjunctions), NewInlineContext};
                {error, ArgNumber} ->
                    {error, {cannot_have_expressions_on_this_argument, ArgNumber}}
            end;
        {error, Error} -> {error, Error}
    end.


-spec rewrite_vars_to_ivars/1 :: (tokens()) -> tokens().
rewrite_vars_to_ivars(TPattern) ->
    rewrite_vars_to_ivars(TPattern, []).
rewrite_vars_to_ivars([{var,L,Var}|TPattern], SoFar) when Var /= '_' ->
    rewrite_vars_to_ivars(TPattern, [{ivar,L,Var}|SoFar]);
rewrite_vars_to_ivars([T|TPattern], SoFar) ->
    rewrite_vars_to_ivars(TPattern, [T|SoFar]);
rewrite_vars_to_ivars([], SoFar) ->
    lists:reverse(SoFar).

    
-spec error_on_remaining_ivariables/1 :: (tokens()) -> tokens().
error_on_remaining_ivariables(Tokens) ->
    error_on_remaining_ivariables(Tokens, Tokens).
error_on_remaining_ivariables([{ivar,_,_}|_]=Rest, _) ->
    error({an_ivar_remained_in_the_processed_pattern_template, Rest});
error_on_remaining_ivariables([_|Rest], Tokens) ->
    error_on_remaining_ivariables(Rest, Tokens);
error_on_remaining_ivariables([],Tokens) -> Tokens.


-spec substitute_guard_ivariable_to_tokens/3
    :: (tokens(), atom(), tokens()) ->
        {ok, tokens()}.
substitute_guard_ivariable_to_tokens(Guards, PlaceVar, TArgs) when PlaceVar /= '_' ->
    substitute_guard_ivariable_to_tokens(Guards, PlaceVar, TArgs, []).
substitute_guard_ivariable_to_tokens([{ivar,L,Var}|Guards], PlaceVar, TArgs, SoFar) when Var =:= PlaceVar ->
    case TArgs of
        [{var,_,_}=TVal] ->
            substitute_guard_ivariable_to_tokens(Guards, PlaceVar, TArgs, [TVal|SoFar]);
        %%[{var,_,_}=TVal,{'=',_}|_] ->
        %%    substitute_guard_ivariable_to_tokens(Guards, PlaceVar, TArgs, [TVal|SoFar]);
        _ ->
            substitute_guard_ivariable_to_tokens(Guards, PlaceVar, TArgs, [{')',L}|lists:reverse(TArgs)] ++ [{'(',L}|SoFar])
    end;
substitute_guard_ivariable_to_tokens([T|Guards], PlaceVar, TArgs, SoFar) ->
    substitute_guard_ivariable_to_tokens(Guards, PlaceVar, TArgs, [T|SoFar]);
substitute_guard_ivariable_to_tokens([], _, _, SoFar) ->
    {ok, lists:reverse(SoFar)}.


-spec find_exemptable_variables_in_udg_args/1
    :: (tokens()) ->
        {ok, tokens(), variable_set()} |
        {simple, variable_set()}.
find_exemptable_variables_in_udg_args(ATokens) ->
    case ATokens of
        [{'ivar',_,Var}] ->
            {simple,ordsets:add_element(Var, ordsets:new())};
        [{'ivar',_,Var},{'=',_}|RestATokens] ->
            {ok,RestATokens,ordsets:add_element(Var, ordsets:new())};
        _ ->
            case lists:reverse(ATokens) of
                [{'ivar',_,Var},{'=',_}|RestATokens] ->
                    {ok,lists:reverse(RestATokens),ordsets:add_element(Var, ordsets:new())};
                _ -> {ok,ATokens,ordsets:new()}
            end
    end.


-spec get_udg_function_tokens/5
    :: (variable_set(), [tokens()], argument_actions(), [tokens()], conjunction_expression() | none) ->
        guard_disjunction().
get_udg_function_tokens(VarSet,InpugTArgs,ArgActions,FunTArgs,Guards) ->
    LineNum = case InpugTArgs of [] -> 0; [[]|_] -> 0; [[{_,L0}|_]|_] -> L0; [[{_,L0,_}|_]|_] -> L0 end,
    {ok, Guards2} = conjunctions_list_to_tokens(LineNum, Guards),
    get_udg_function_tokens(VarSet,InpugTArgs,ArgActions,FunTArgs,
        rewrite_vars_to_ivars(Guards2),[],orddict:new()).
get_udg_function_tokens(VarSet,[TArgs|InpugTArgs],[subst_guard|ArgActions],[[{var,_,PlaceVar}]|FunTArgs],Guards,ArgAssocs,AVars) ->
    {ok, NewGuards} = substitute_guard_ivariable_to_tokens(Guards, PlaceVar, TArgs),
    get_udg_function_tokens(VarSet,InpugTArgs,ArgActions,FunTArgs,NewGuards,ArgAssocs,AVars);
get_udg_function_tokens(VarSet,[[{var,_,Var}]=TArg|InpugTArgs],[arg_assoc|ArgActions],[ATokens0|FunTArgs],Guards,ArgAssocs,AVars) ->
    ATokens = rewrite_vars_to_ivars(ATokens0),
    case find_exemptable_variables_in_udg_args(ATokens) of
        {simple, [PlaceVar]} ->
            {ok, NewGuards} = substitute_guard_ivariable_to_tokens(Guards, PlaceVar, TArg),
            get_udg_function_tokens(VarSet,InpugTArgs,ArgActions,FunTArgs,NewGuards,ArgAssocs,AVars);
        ArgAssociated ->
            {ok, NewGuards} =
                case ArgAssociated of
                    {ok, ATokens2, [PlaceVar]=ExemptVars} ->
                        {ok, VarSet2, NewATokens, AVars2, AVars3} = substitute_to_auto_variables(VarSet, ATokens2, ExemptVars, AVars),
                        {ok, Guards2} = substitute_to_existing_auto_variables(Guards, AVars3),
                        substitute_guard_ivariable_to_tokens(Guards2, PlaceVar, TArg);
                    _ ->
                        {ok, VarSet2, NewATokens, AVars2, AVars3} = substitute_to_auto_variables(VarSet, ATokens, ordsets:new(), AVars),
                        substitute_to_existing_auto_variables(Guards, AVars3)
                end,
            case orddict:find(Var, ArgAssocs) of
                {ok, PrevArgAssocs} ->
                    NewArgAssocs = orddict:store(Var,
                        error_on_remaining_ivariables(NewATokens) ++ [{'=',0}|PrevArgAssocs], ArgAssocs),
                    get_udg_function_tokens(VarSet2,InpugTArgs,ArgActions,FunTArgs,NewGuards,NewArgAssocs,AVars2);
                _ ->
                    NewArgAssocs = orddict:store(Var, error_on_remaining_ivariables(NewATokens), ArgAssocs),
                    get_udg_function_tokens(VarSet2,InpugTArgs,ArgActions,FunTArgs,NewGuards,NewArgAssocs,AVars2)
            end
        end;
get_udg_function_tokens(_,[],[],[],Guards,ArgAssocs,_AVars) ->
    {guard_disjunction,
        error_on_remaining_ivariables(Guards),
        ArgAssocs}.


-spec variables_from_arguments/1 :: ([tokens()]) -> {ok, [atom()]}.
variables_from_arguments(Args) ->
    variables_from_arguments(Args,[]).
variables_from_arguments([[{var,_,Var}]|Args],SoFar) ->
    variables_from_arguments(Args, [Var|SoFar]);
variables_from_arguments([],SoFar) ->
    {ok, lists:reverse(SoFar)}.


-spec non_argument_variables_in_tokens/4
    :: (variable_set(), tokens(), [atom()], variable_assoc()) ->
        {ok, variable_set(), tokens(), variable_assoc()}.
non_argument_variables_in_tokens(VarSet, Tokens, ArgVars, AVars) ->
    non_argument_variables_in_tokens(VarSet, Tokens, ArgVars, AVars, []).
non_argument_variables_in_tokens(VarSet, [{ivar,L,Var}=T|Tokens], ArgVars, AVars, SoFar) when Var /= '_' ->
    case lists:filter(fun(A) -> A =:= Var end, ArgVars) of
        [] ->
            case orddict:find(Var, AVars) of
                {ok, UsedVar} ->
                    non_argument_variables_in_tokens(VarSet, Tokens, ArgVars, AVars, [{var,L,UsedVar}|SoFar]);
                _ ->
                    UsedVar = create_new_auto_variable(
                        auto_var_uniqueness_fun_from_avars_orddict_and_varset(AVars, VarSet)),
                    NewAVars = orddict:store(Var,UsedVar,AVars),
                    VarSet2 = ordsets:add_element(UsedVar, VarSet),
                    non_argument_variables_in_tokens(VarSet2, Tokens, ArgVars, NewAVars, [{var,L,UsedVar}|SoFar])
            end;
        _ ->
            non_argument_variables_in_tokens(VarSet, Tokens, ArgVars, AVars, [T|SoFar])
    end;
non_argument_variables_in_tokens(VarSet, [T|Tokens], ArgVars, AVars, SoFar) ->
    non_argument_variables_in_tokens(VarSet, Tokens, ArgVars, AVars, [T|SoFar]);
non_argument_variables_in_tokens(VarSet, [], _, AVars, SoFar) ->
    {ok, VarSet, lists:reverse(SoFar), AVars}.


-spec substitute_ivariable_to_variable/3
    :: (tokens(), atom(), atom()) ->
        {ok, tokens()}.
substitute_ivariable_to_variable(Tokens, PlaceVar, NewVar) when PlaceVar /= '_' ->
    substitute_ivariable_to_variable(Tokens, PlaceVar, NewVar, []).
substitute_ivariable_to_variable([{ivar,L,Var}|Tokens], PlaceVar, NewVar, SoFar) when Var =:= PlaceVar ->
    substitute_ivariable_to_variable(Tokens, PlaceVar, NewVar, [{'var',L,NewVar}|SoFar]);
substitute_ivariable_to_variable([T|Tokens], PlaceVar, NewVar, SoFar) ->
    substitute_ivariable_to_variable(Tokens, PlaceVar, NewVar, [T|SoFar]);
substitute_ivariable_to_variable([], _, _, SoFar) ->
    {ok, lists:reverse(SoFar)}.


-spec add_pattern_tokens_after_ivariable/3
    :: (tokens(), atom(), tokens()) ->
        {ok, tokens()}.
add_pattern_tokens_after_ivariable(TPattern, PlaceVar, InputPattern) when PlaceVar /= '_' ->
    add_pattern_tokens_after_ivariable(TPattern, PlaceVar, InputPattern, []).
add_pattern_tokens_after_ivariable([{ivar,L,Var}|TPattern], PlaceVar, InputPattern, SoFar) when Var =:= PlaceVar ->
    add_pattern_tokens_after_ivariable(TPattern, PlaceVar, InputPattern, lists:reverse(InputPattern) ++ [{'=',L}|SoFar]);
add_pattern_tokens_after_ivariable([T|TPattern], PlaceVar, InputPattern, SoFar) ->
    add_pattern_tokens_after_ivariable(TPattern, PlaceVar, InputPattern, [T|SoFar]);
add_pattern_tokens_after_ivariable([], _, _, SoFar) ->
    {ok, lists:reverse(SoFar)}.


-spec get_udg_pattern_template_tokens/6
    :: (variable_set(), tokens(), [tokens()], pattern_argument_actions(), [tokens()], conjunction_expression() | none) ->
        pattern_disjunction().
get_udg_pattern_template_tokens(VarSet,TPattern,InpugTokenArgs,ArgActions,Args,Guards) ->
    LineNum = 0,
    AVars = orddict:new(),
    {ok, Guards2} = conjunctions_list_to_tokens(LineNum, Guards),
    {ok, ArgVars} = variables_from_arguments(Args),
    {ok, VarSet2, NewTPattern, AVars2} = non_argument_variables_in_tokens(VarSet, rewrite_vars_to_ivars(TPattern), ArgVars, AVars),
    {ok, VarSet2, NewGuards, AVars2} = non_argument_variables_in_tokens(VarSet2, rewrite_vars_to_ivars(Guards2), ArgVars, AVars2),
    get_udg_pattern_template_tokens(
        VarSet,NewTPattern,InpugTokenArgs,ArgActions,Args,NewGuards,AVars).
get_udg_pattern_template_tokens(VarSet,TPattern,[[{var,_,DirectVar}]|InpugTokenArgs],[direct_subst|ArgActions],[[{var,_,PlaceVar}]|Args],Guards,AVars) ->
    {ok, NewTPattern} = substitute_ivariable_to_variable(TPattern, PlaceVar, DirectVar),
    {ok, NewGuards} = substitute_ivariable_to_variable(Guards, PlaceVar, DirectVar),
    get_udg_pattern_template_tokens(VarSet,NewTPattern,InpugTokenArgs,ArgActions,Args,NewGuards,AVars);
get_udg_pattern_template_tokens(VarSet,TPattern,[InputPattern|InpugTokenArgs],[pattern_subst|ArgActions],[[{var,_,PlaceVar}]|Args],Guards,AVars) ->
    NewLocalVar = create_new_auto_variable(
        auto_var_uniqueness_fun_from_avars_orddict_and_varset(AVars, VarSet)),
    NewAVars = orddict:store(PlaceVar,NewLocalVar,AVars),
    VarSet2 = ordsets:add_element(NewLocalVar, VarSet),
    {ok, TPattern2} = add_pattern_tokens_after_ivariable(TPattern, PlaceVar, InputPattern),
    {ok, NewTPattern} = substitute_ivariable_to_variable(TPattern2, PlaceVar, NewLocalVar),
    {ok, NewGuards} = substitute_ivariable_to_variable(Guards, PlaceVar, NewLocalVar),
    get_udg_pattern_template_tokens(VarSet2,NewTPattern,InpugTokenArgs,ArgActions,Args,NewGuards,NewAVars);
get_udg_pattern_template_tokens(_,TPattern,[],[],[],Guards,_) ->
    {pattern_disjunction,
        error_on_remaining_ivariables(TPattern),
        error_on_remaining_ivariables(Guards)}.
    

-spec create_new_auto_variable/1 :: (fun((atom()) -> boolean())) -> atom().
create_new_auto_variable(AVarUniqueFun) when is_function(AVarUniqueFun) ->
    create_new_auto_variable(AVarUniqueFun, 25).
create_new_auto_variable(AVarUniqueFun, N) when N > 0 ->
    NewVar = list_to_atom(lists:concat([
        "TMP__UDG_", integer_to_list(random:uniform(100000000000000)),
                "_", integer_to_list(random:uniform(100000000000000)),
                "_", integer_to_list(random:uniform(100000000000000))])),
    case AVarUniqueFun(NewVar) of
        true -> NewVar;
        false ->
            io:format("Retrying...~n",[]),
            create_new_auto_variable(AVarUniqueFun, N-1)
    end;
create_new_auto_variable(_, 0) ->
    error(cant_generate_variable).


-spec auto_var_uniqueness_fun_from_avars_orddict_and_varset/2
    :: (variable_assoc(), variable_set()) -> fun((atom()) -> boolean()).
auto_var_uniqueness_fun_from_avars_orddict_and_varset(AVars, VarSet) ->
    fun(NewVar) ->
        orddict:filter(
            fun (_Key,V) when V =:= NewVar -> true;
                (_Key,_) -> false
            end,
            AVars) =:= [] andalso not ordsets:is_element(NewVar, VarSet)
    end.


-spec substitute_to_auto_variables/4
    :: (variable_set(), tokens(), [atom()], variable_assoc()) ->
        {ok, variable_set(), tokens(), variable_assoc(), variable_assoc()}.
substitute_to_auto_variables(VarSet, Tokens, ExemptVars, AVars1) ->
    substitute_to_auto_variables(VarSet, Tokens, [], ExemptVars, AVars1, orddict:new()).
substitute_to_auto_variables(VarSet, [{ivar,L,Var}=T|Rest], SoFar, ExemptVars, AVars1,AVars2) when Var /= '_' ->
    case ordsets:is_element(Var, ExemptVars) of
        true ->
            substitute_to_auto_variables(VarSet, Rest, [T|SoFar], ExemptVars, AVars1,AVars2);
        _ ->
            case orddict:find(Var, AVars1) of
                {ok, UsedVar} ->
                    substitute_to_auto_variables(VarSet, Rest,[{var,L,UsedVar}|SoFar], ExemptVars,AVars1,AVars2);
                _ ->
                    UsedVar = create_new_auto_variable(
                        auto_var_uniqueness_fun_from_avars_orddict_and_varset(AVars1, VarSet)),
                    NewAVars1 = orddict:store(Var,UsedVar,AVars1),
                    NewAVars2 = orddict:store(Var,UsedVar,AVars2),
                    VarSet2 = ordsets:add_element(UsedVar,VarSet),
                    substitute_to_auto_variables(VarSet2, Rest,[{var,L,UsedVar}|SoFar], ExemptVars,NewAVars1,NewAVars2)
            end
    end;
substitute_to_auto_variables(VarSet, [T|Rest], SoFar, ExemptVars, AVars1,AVars2) ->
    substitute_to_auto_variables(VarSet, Rest, [T|SoFar], ExemptVars, AVars1,AVars2);
substitute_to_auto_variables(VarSet, [], SoFar, _ExemptVars, AVars1,AVars2) ->
    {ok, VarSet, lists:reverse(SoFar), AVars1, AVars2}.


-spec substitute_to_existing_auto_variables/2
    :: (tokens(), variable_assoc()) -> {ok, tokens()}.
substitute_to_existing_auto_variables(Tokens, AVars) ->
    substitute_to_existing_auto_variables(Tokens, [], AVars).
substitute_to_existing_auto_variables([{ivar,L,Var}=T|Rest], SoFar, AVars) when Var /= '_' ->
    case orddict:find(Var, AVars) of
        {ok, UsedVar} ->
            substitute_to_existing_auto_variables(Rest,[{var,L,UsedVar}|SoFar],AVars);
        _ ->
            substitute_to_existing_auto_variables(Rest,[T|SoFar],AVars)
    end;
substitute_to_existing_auto_variables([T|Rest], SoFar, AVars) ->
    substitute_to_existing_auto_variables(Rest, [T|SoFar], AVars);
substitute_to_existing_auto_variables([], SoFar, _) ->
    {ok, lists:reverse(SoFar)}.


-spec find_udg_definitions/2
    :: ({udg, atom(), integer()} | {pattern_template, atom(), arity()}, udg_definitions()) ->
        {ok, [any()], [ _ ], [ _ ], [ _ ], string()} | {error, any()}.
find_udg_definitions(NameAndArity, {udg_definitions, UDGDefinitions}) when is_list(UDGDefinitions) ->
    find_udg_definitions(NameAndArity, UDGDefinitions, [], false, [], [], no_erg_file);
find_udg_definitions(NameAndArity, {erg_file, ERGSourceFile, {udg_definitions, UDGDefinitions}}) when is_list(UDGDefinitions) ->
    find_udg_definitions(NameAndArity, UDGDefinitions, [], false, [], [], ERGSourceFile).
find_udg_definitions({Type1, Name1, Arity1}=NameAndArity, [{user_defined_guard,ERGName,UDGList}|UDGDefinitions], Defs, Exported, UDGIncludes, SelfIncludes, ERGSourceFile) ->
    %% user_defined_guard source file name and filename comparison is 
    %% done once with erg_definitions/2
    %%
    Found =
        lists:foldl(
            fun ({Type2, Name2, Arity2},_Found) when Type1 =:= Type2, Name1 =:= Name2, Arity1 =:= Arity2 -> true;
                (_,Found0) -> Found0
            end, Exported, UDGList),
    NewSelfIncludes = udg_erg_includes:erg_filenames_for_each_include_udg(ERGName, UDGList),
    find_udg_definitions(NameAndArity, UDGDefinitions, Defs, Found, UDGIncludes, [NewSelfIncludes|SelfIncludes], ERGSourceFile);
find_udg_definitions({Type1, Name1, Arity1}=NameAndArity, [{erg_includes, NewUDGImportList}|UDGDefinitions], Defs, Exported, UDGIncludes, SelfIncludes, ERGSourceFile) ->
    find_udg_definitions(NameAndArity, UDGDefinitions, Defs, Exported, [NewUDGImportList|UDGIncludes], SelfIncludes, ERGSourceFile);
find_udg_definitions({Type1, Name1, Arity1}=NameAndArity, [{defs,DList}|UDGDefinitions], Defs, Exported, UDGIncludes, SelfIncludes, ERGSourceFile) ->
    [{TNA, _TPattern, _Args, _Guards}|_] = DList,
    case TNA of
        {Type2, Name2, Arity2} when Type2 =:= Type1, Name2 =:= Name1, Arity2 =:= Arity1 ->
            find_udg_definitions(NameAndArity, UDGDefinitions, [DList|Defs], Exported, UDGIncludes, SelfIncludes, ERGSourceFile);
        _ ->
            find_udg_definitions(NameAndArity, UDGDefinitions, Defs, Exported, UDGIncludes, SelfIncludes, ERGSourceFile)
    end;
find_udg_definitions(_NameAndArity, [], [Defs], true, UDGIncludes, SelfIncludes, ERGSourceFile) when is_list(Defs) ->
    UDGArgTypes = get_arg_types_across_disjunctions(Defs),
    {ok, lists:map(
        fun({_, TPattern, Args, Guards}) ->
            {TPattern, Args, Guards}
        end, Defs), UDGArgTypes,
        lists:append(lists:reverse(UDGIncludes)),
        lists:append(lists:reverse(SelfIncludes)), ERGSourceFile};
find_udg_definitions(_NameAndArity, [], [_First|_Rest]=ManyDefs, true, _, _, ERGSourceFile) -> {error, {more_than_one_definition, ManyDefs, ERGSourceFile}};
find_udg_definitions(_NameAndArity, [], _Defs, false, _, _, ERGSourceFile) -> {error, {udg_not_exported, ERGSourceFile}};
find_udg_definitions(_NameAndArity, [], [], true, _, _, ERGSourceFile) -> {error, {udg_exported_but_not_defined, ERGSourceFile}}.


sequence_no_state([], Input) -> {ok, Input};
sequence_no_state([Fun|Rest], Input) when is_function(Fun) ->
    case Fun(Input) of
        {ok, Result} -> sequence_no_state(Rest, Result);
        Error -> Error
    end.

read_udg_erg_string(InputString) ->
    case sequence_no_state(
            [ fun read_udg_erg_string_to_tokens/1,
              fun read_udg_erg_string_parse/1 ],
            InputString)
    of
        {ok, UDGDefinitions} ->
            case find_errors(UDGDefinitions) of
                [] ->
                    Warnings = find_warnings(UDGDefinitions),
                    {ok, UDGDefinitions2, MoreWarnings} = make_udg_argument_variables_single_position(UDGDefinitions),
                    {ok, {udg_definitions, UDGDefinitions2}, MoreWarnings ++ Warnings};
                Errors -> {errors, Errors}
            end
    end.

read_udg_erg_string_to_tokens(InputString) ->
    case erl_scan:string(InputString) of
        {ok, List, _} ->
            {ok, List};
        Error -> Error
    end.
read_udg_erg_string_parse(B) ->
    {ok, Parsed} = erg_parse(B),
    {ok, Parsed}.


-spec make_udg_argument_variables_single_position/1 :: ([{defs, _ }]) -> {ok, tokens(), [ _ ]}.
make_udg_argument_variables_single_position(UDGDefinitions) ->
    make_udg_argument_variables_single_position(UDGDefinitions, [], []).
make_udg_argument_variables_single_position([{defs, Disjunctions}=T|Rest], SoFar, Warnings) ->
    case make_udg_argument_variables_single_position_disjunctions(Disjunctions) of
        {subst, NewDisjunctions, NewWarnings} ->
            make_udg_argument_variables_single_position(
                Rest, [{defs, NewDisjunctions}|SoFar], NewWarnings ++ Warnings);
        single_positions ->
            make_udg_argument_variables_single_position(Rest, [T|SoFar], Warnings)
    end;
make_udg_argument_variables_single_position([T|Rest], SoFar, Warnings) ->
    make_udg_argument_variables_single_position(Rest, [T|SoFar], Warnings);
make_udg_argument_variables_single_position([], SoFar, MoreWarnings) ->
    {ok, lists:reverse(SoFar), MoreWarnings}.


make_udg_argument_variables_single_position_disjunctions(Disjunctions) ->
    VarSet = ordsets:new(),
    make_udg_argument_variables_single_position_disjunctions(Disjunctions, [], none, [], VarSet).
make_udg_argument_variables_single_position_disjunctions([Disjunction|Rest], SoFar, St, Warnings, VarSet) ->
    case Disjunction of
        {{udg,FunName,Arity}=UDG,none,Args,Conjunctions} ->
            case substitute_variable_argument_positioning(VarSet, Args) of
                {subst, NewArgs, NewWarnings, NewConjunctions, VarSet2} ->
                    make_udg_argument_variables_single_position_disjunctions(
                        Rest, [{UDG,none,NewArgs,add_conjunctions(NewConjunctions, FunName, Conjunctions)}|SoFar], subst, NewWarnings ++ Warnings, VarSet2);
                single_positions ->
                    make_udg_argument_variables_single_position_disjunctions(
                        Rest, [Disjunction|SoFar], St, Warnings, VarSet)
            end;
        _ ->
            make_udg_argument_variables_single_position_disjunctions(Rest, [Disjunction|SoFar], St, Warnings, VarSet)
    end;
make_udg_argument_variables_single_position_disjunctions([], SoFar, St, Warnings, _) ->
    case St of
        subst -> {subst, lists:reverse(SoFar), Warnings};
        none -> single_positions
    end.

    
add_conjunctions([], _, none) ->
    none;
add_conjunctions(NewConjunctions, FunName, none) when is_list(NewConjunctions) ->
    {conjunction_list, conjunctions_list_for_udg_function_kind(FunName, NewConjunctions)};
add_conjunctions(NewConjunctions, FunName, {conjunction_list, Conjunctions}) when is_list(NewConjunctions), is_list(Conjunctions) ->
    {conjunction_list, conjunctions_list_for_udg_function_kind(FunName, NewConjunctions ++ Conjunctions)}.

conjunctions_list_for_udg_function_kind(_FunName, [Expression]) ->
    Expression;
conjunctions_list_for_udg_function_kind(FunName, ConjunctionsList) ->
    case get_udg_function_kind_from_atom(FunName) of
        {ok, u} -> ConjunctionsList;
        {ok, up} -> ConjunctionsList;
        {ok, UDGKind} when UDGKind =:= ue orelse UDGKind =:= uep ->
            {'andalso_list', lists:map(fun(Expr) -> {parenthesis, Expr} end, ConjunctionsList)}
    end.


substitute_variable_argument_positioning(VarSet, Args) ->
    substitute_variable_argument_positioning(VarSet, Args, [], none, ordsets:new(), [], []).
substitute_variable_argument_positioning(VarSet, [Arg|Rest], SoFar, St, ArgSet, Warnings, NewConjunctions) ->
    {ok, VarSet2, NewArg, St2, NewArgSet, NewWarnings, NewConjunctions2} =
        substitute_argument_variables_from_argpositioning(VarSet, Arg, ArgSet, Warnings, NewConjunctions, St),
    substitute_variable_argument_positioning(VarSet2, Rest, [NewArg|SoFar], St2, NewArgSet, NewWarnings, NewConjunctions2);
substitute_variable_argument_positioning(VarSet, [], SoFar, St, _ArgSet, Warnings, NewConjunctions) ->
    case St of
        none -> single_positions;
        subst ->
            {subst, lists:reverse(SoFar), lists:reverse(Warnings), lists:reverse(NewConjunctions), VarSet}
    end.


substitute_argument_variables_from_argpositioning(VarSet, Arg, ArgSet, Warnings, NewConjunctions, St) ->
    substitute_argument_variables_from_argpositioning(VarSet, Arg, ArgSet, Warnings, NewConjunctions, [], St, ordsets:new()).
substitute_argument_variables_from_argpositioning(VarSet, [{'var',Line,Var}=Arg|Rest], ArgSet, Warnings, NewConjunctions, SoFar, St, ThisArgSet) ->
    case ordsets:is_element(Arg, ArgSet) of
        true ->
            NewVar = create_new_auto_variable(fun(AVar) -> ordsets:is_element(AVar, VarSet) end),
            ThisArgSet2 = ordsets:add_element(NewVar, ThisArgSet),
            VarSet2 = ordsets:add_element(NewVar, VarSet),
            EqConjunction = {'comp_ops', [{'var',Var}, '=:=', {'var',NewVar}]},
            W = {'warning', {multiple_position_variable_moved_to_guard_constraint, Var}},
            substitute_argument_variables_from_argpositioning(
                VarSet2, Rest, ArgSet, [W|Warnings], [EqConjunction|NewConjunctions],
                [{'var',Line,NewVar}|SoFar], subst, ThisArgSet2);
        false ->
            ThisArgSet2 = ordsets:add_element(Var, ThisArgSet),
            substitute_argument_variables_from_argpositioning(
                VarSet, Rest, ArgSet, Warnings, NewConjunctions, [Arg|SoFar], St, ThisArgSet2)
    end;
substitute_argument_variables_from_argpositioning(VarSet, [Arg|Rest], ArgSet, Warnings, NewConjunctions, SoFar, St, ThisArgSet) ->
    substitute_argument_variables_from_argpositioning(VarSet, Rest, ArgSet, Warnings, NewConjunctions, [Arg|SoFar], St, ThisArgSet);
substitute_argument_variables_from_argpositioning(VarSet, [], ArgSet, Warnings, NewConjunctions, SoFar, St, ThisArgSet) ->
    {ok, VarSet, lists:reverse(SoFar), St, ordsets:union(ArgSet,ThisArgSet), Warnings, NewConjunctions}.

    
%%
%% ERG parsing code below

erg_parse_directive_udg([{']',_},{')',_},{dot,_}|Rest]) ->
    {ok, [], Rest};
erg_parse_directive_udg(Tokens) ->
    erg_parse_directive_udg(Tokens, 1, []).
erg_parse_directive_udg([{atom,_,UDGName},{'/',_},{integer,_,Arity}|Rest], 1, List) ->
    erg_parse_directive_udg(Rest, 2, [{udg, UDGName, Arity}|List]);
erg_parse_directive_udg([{'\\',_},{'\\',_},{atom,_,PattTemplateName},{'/',_},{integer,_,Arity}|Rest], 1, List) ->
    erg_parse_directive_udg(Rest, 2, [{pattern_template, PattTemplateName, Arity}|List]);
erg_parse_directive_udg([{',',_}|Rest], 2, List) ->
    erg_parse_directive_udg(Rest, 1, List);
erg_parse_directive_udg([{']',_},{')',_},{dot,_}|Rest], 2, List) ->
    {ok, lists:reverse(List), Rest}.

erg_parse_directive_available([{']',_},{')',_},{dot,_}|Rest]) ->
    {ok, [], Rest};
erg_parse_directive_available(Tokens) ->
    erg_parse_directive_available(Tokens, 1, []).
erg_parse_directive_available([{atom,_,ModuleName}|Rest], 1, List) ->
    erg_parse_directive_available(Rest, 2, [ModuleName|List]);
erg_parse_directive_available([{',',_}|Rest], 2, List) ->
    erg_parse_directive_available(Rest, 1, List);
erg_parse_directive_available([{']',_},{')',_},{dot,_}|Rest], 2, List) ->
    {ok, lists:reverse(List), Rest}.

erg_parse_skip_directive(Tokens) ->
    erg_parse_skip_directive(Tokens, []).
erg_parse_skip_directive([{O,_}|Tokens], H) when O =:= '('; O =:= '['; O =:= '{' ->
    erg_parse_skip_directive(Tokens, [O|H]);
erg_parse_skip_directive([{C,_}|Tokens], [O|H])
when O =:= '(', C =:= ')'; O =:= '[', C =:= ']'; O =:= '{', C =:= '}' ->
    erg_parse_skip_directive(Tokens, H);
erg_parse_skip_directive([{dot,_}|Tokens], []) ->
    {ok, Tokens}.

erg_parse(Tokens) ->
    erg_parse(Tokens, []).
erg_parse([], L) ->
    {ok, lists:reverse(L)};
erg_parse([{'-',_},{atom,_,for_application},{'(',_},{atom,_,ApplicationName},{')',_},{dot,_}|Rest], L) ->
    erg_parse(Rest, [{application, ApplicationName}|L]);
erg_parse([{'-',_},{atom,_,user_defined_guard}|Rest],L) ->
    case Rest of
        [{'(',_},{atom,_,ERGName},{',',_},{'[',_}|Rest1] ->
            {ok, UDGs, Rest2} = erg_parse_directive_udg(Rest1),
            erg_parse(Rest2,[{user_defined_guard, ERGName, UDGs}|L]);
        _ ->
            error({user_defined_guard_form_not_formed_correctly})
    end;
erg_parse([{'-',_},{atom,_,available_to},{'(',_},{'[',_}|Rest],L) ->
    {ok, AvailableTo, Rest2} = erg_parse_directive_available(Rest),
    erg_parse(Rest2,[{available_to, AvailableTo}|L]);

erg_parse([{'-',_},{atom,_,include_udg}|Rest],L) ->
    case udg_erg_includes:proc_source_include_udg(Rest, []) of
        {ok, NewUDGImportList, Rest2} ->
            erg_parse(Rest2,[{erg_includes, NewUDGImportList}|L])
    end;
erg_parse([{'-',_},{atom,_,auto_include_udg}|Rest],L) ->
    case udg_erg_includes:proc_source_auto_include_udg(Rest, []) of
        {ok, NewUDGImportList, Rest2} ->
            erg_parse(Rest2,[{erg_includes, NewUDGImportList}|L])
    end;

erg_parse([{'-',_},{atom,_,X},{'(',_}|Rest],L) when X /= user_defined_guard, X /= for_application ->
    {ok, Rest2} = erg_parse_skip_directive(Rest),
    erg_parse(Rest2,L);
erg_parse([{atom,_,_UDGName}=T1,{'(',_}=T2|Rest], L) ->
    {ok, Defs, Rest2} = erg_parse_disj_clauses([T1,T2|Rest], []),
    erg_parse(Rest2,[{defs, Defs}|L]);
erg_parse([{'(',_}=T1|Rest], L) ->
    {ok, Defs, Rest2} = erg_parse_disj_clauses([T1|Rest], []),
    erg_parse(Rest2,[{defs, Defs}|L]).

erg_parse_disj_clauses(Tokens, Defs) ->
    case erg_parse_clause(Tokens) of
        {';', Def, Rest2} -> erg_parse_disj_clauses(Rest2, [Def|Defs]);
        {dot, Def, Rest2} -> {ok, lists:reverse([Def|Defs]), Rest2}
    end.

erg_parse_clause([{'(',_}|Rest]) ->
    {ok, PattTemplate, Rest2} = erg_parse_pattern_to_op(Rest),
    case Rest2 of
        [{')',_},{'\\',_},{'\\',_},{atom,_,PattTemplateName},{'(',_}|Rest3] ->
            {ok, Args, Rest4} = erg_parse_arguments(Rest3),
            Arity = length(Args),
            NameAndArity = {pattern_template, PattTemplateName, Arity},
            case Rest4 of
                [{';',_}|Rest5] -> {';', {NameAndArity,PattTemplate,Args,none}, Rest5};
                [{dot,_}|Rest5] -> {dot, {NameAndArity,PattTemplate,Args,none}, Rest5};
                [{'when',_}|Rest5] ->
                    case erg_parse_clause_when(Rest5, []) of
                        {ok, _Exprs, [{';',_}|_Rest6]} -> error(need_tail_guard_expression_for_disjunction); % {';', {NameAndArity,none,Args,Exprs}, Rest6};
                        {ok, Exprs, [{dot,_}|Rest6]} -> {dot, {NameAndArity,PattTemplate,Args,Exprs}, Rest6};
                        {ok, Exprs, [{':=',_}|Rest6]} ->
                            {conjunction_list, CLExprs} = Exprs,
                            case erg_parse_clause_tail_expr(Rest6) of
                                {ok, TExpr, [{';',_}|Rest7]} -> {';', {NameAndArity,PattTemplate,Args,{conjunction_list, CLExprs++[TExpr]}}, Rest7};
                                {ok, TExpr, [{dot,_}|Rest7]} -> {dot, {NameAndArity,PattTemplate,Args,{conjunction_list, CLExprs++[TExpr]}}, Rest7}
                            end
                    end;
                [{':=',_}|Rest5] ->
                    case erg_parse_clause_tail_expr(Rest5) of
                        {ok, TExpr, [{';',_}|Rest6]} -> {';', {NameAndArity,PattTemplate,Args,{conjunction_list, [TExpr]}}, Rest6};
                        {ok, TExpr, [{dot,_}|Rest6]} -> {dot, {NameAndArity,PattTemplate,Args,{conjunction_list, [TExpr]}}, Rest6}
                    end
            end
    end;
erg_parse_clause([{atom,_,UDGName},{'(',_}|Rest]) ->
    {ok, Args, Rest2} = erg_parse_arguments(Rest),
    Arity = length(Args),
    NameAndArity = {udg, UDGName, Arity},
    case Rest2 of
        [{';',_}|Rest3] -> {';', {NameAndArity,none,Args,none}, Rest3};
        [{dot,_}|Rest3] -> {dot, {NameAndArity,none,Args,none}, Rest3};
        [{'when',_}|Rest3] ->
            case erg_parse_clause_when(Rest3, []) of
                {ok, _Exprs, [{';',_}|_Rest4]} -> error(need_tail_guard_expression_for_disjunction); % {';', {NameAndArity,none,Args,Exprs}, Rest4};
                {ok, Exprs, [{dot,_}|Rest4]} -> {dot, {NameAndArity,none,Args,Exprs}, Rest4};
                {ok, Exprs, [{':=',_}|Rest4]} ->
                    {conjunction_list, CLExprs} = Exprs,
                    case erg_parse_clause_tail_expr(Rest4) of
                        {ok, TExpr, [{';',_}|Rest5]} -> {';', {NameAndArity,none,Args,{conjunction_list, CLExprs++[TExpr]}}, Rest5};
                        {ok, TExpr, [{dot,_}|Rest5]} -> {dot, {NameAndArity,none,Args,{conjunction_list, CLExprs++[TExpr]}}, Rest5}
                    end
            end;
        [{':=',_}|Rest3] ->
            case erg_parse_clause_tail_expr(Rest3) of
                {ok, TExpr, [{';',_}|Rest4]} -> {';', {NameAndArity,none,Args,{conjunction_list, [TExpr]}}, Rest4};
                {ok, TExpr, [{dot,_}|Rest4]} -> {dot, {NameAndArity,none,Args,{conjunction_list, [TExpr]}}, Rest4}
            end
    end.


erg_parse_pattern_to_op(Tokens) ->
    erg_parse_pattern_to_op(Tokens,[],[]).
erg_parse_pattern_to_op([{')',_}=T1,{'\\',_}=T2,{'\\',_}=T3|Rest],SoFar,[]) ->
    {ok, lists:reverse(SoFar), [T1,T2,T3|Rest]};
erg_parse_pattern_to_op([{O,_}=T|Rest],SoFar,H) when O =:= '('; O =:= '[' ; O =:= '{'; O =:= '<<' ->
    erg_parse_pattern_to_op(Rest,[T|SoFar],[O|H]);
erg_parse_pattern_to_op([{C,_}=T|Rest],SoFar,[O|H]) when O =:= '(', C =:= ')'; O =:= '[', C =:= ']' ; O =:= '{', C =:= '}'; O =:= '<<', C =:= '>>' ->
    erg_parse_pattern_to_op(Rest,[T|SoFar],H);
erg_parse_pattern_to_op([T|Rest],SoFar,H) ->
    erg_parse_pattern_to_op(Rest,[T|SoFar],H).


erg_parse_arguments([{')',_}|Rest]) ->
    {ok, [], Rest};
erg_parse_arguments(Tokens) ->
    erg_parse_arguments(Tokens, []).
erg_parse_arguments([_TA|_Rest]=Tokens, VarList) ->
    case erg_parse_argument_pattern_tokens(Tokens) of
        {ok, Arg, Rest2} ->
            case Rest2 of
                [{')',_}|Rest3] -> {ok, lists:reverse([Arg|VarList]), Rest3};
                [{',',_}|Rest3] -> erg_parse_arguments(Rest3, [Arg|VarList])
            end
    end.

erg_parse_argument_pattern_tokens(Tokens) ->
    erg_parse_argument_pattern_tokens(Tokens,[],[]).
erg_parse_argument_pattern_tokens([{T0,_}=T|Rest],SoFar,[]) when T0 =:= ','; T0 =:= ')' ->
    {ok, lists:reverse(SoFar), [T|Rest]};
erg_parse_argument_pattern_tokens([{O,_}=T|Rest],SoFar,H) when O =:= '('; O =:= '[' ; O =:= '{'; O =:= '<<' ->
    erg_parse_argument_pattern_tokens(Rest,[T|SoFar],[O|H]);
erg_parse_argument_pattern_tokens([{C,_}=T|Rest],SoFar,[O|H]) when O =:= '(', C =:= ')'; O =:= '[', C =:= ']' ; O =:= '{', C =:= '}'; O =:= '<<', C =:= '>>' ->
    erg_parse_argument_pattern_tokens(Rest,[T|SoFar],H);
erg_parse_argument_pattern_tokens([T|Rest],SoFar,H) ->
    erg_parse_argument_pattern_tokens(Rest,[T|SoFar],H).


erg_parse_clause_when(Tokens, ExprList) ->
    case expr_orelses(Tokens) of
        {ok, Expr, Rest} -> 
            case Rest of
                [{',',_}|Rest2] -> erg_parse_clause_when(Rest2, [Expr|ExprList]);
                [{';',_}=_T1|_Rest2] -> error(should_have_tail_expression_for_disjunction); %{ok, lists:reverse([Expr|ExprList]), [T1|Rest2]};
                [{T0,_}=T1|Rest2] when T0 =:= 'dot' ; T0 =:= ':=' ->
                    {ok, {'conjunction_list', lists:reverse([Expr|ExprList])}, [T1|Rest2]}
            end
        % ; no_expr -> error(no_expr_in_guard)
    end.

erg_parse_clause_tail_expr(Tokens) ->
    case expr_orelses(Tokens) of
        {ok, Expr, Rest} -> 
            case Rest of
                [{',',_}|_] -> error(no_conjunction_in_tail_expression);
                [{':=',_}|_] -> error(should_have_only_one_tail_expression);
                [{T0,_}=T1|Rest2] when T0 =:= 'dot' ; T0 =:= ';' ->
                    {ok, Expr, [T1|Rest2]}
            end
        % ; no_expr -> error(no_expr_in_tail_expression)
    end.

expr_orelses(Tokens) ->
    expr_orelses(Tokens, []).
expr_orelses(Tokens, ExprList) ->
    case expr_andalsos(Tokens) of
        {ok, Expr, Rest} -> 
            case Rest of
                [{'orelse',_}=_T1|Rest2] -> expr_orelses(Rest2, [Expr|ExprList]);
                [{T0,_}=T1|Rest2] when T0 =:= ',' ; T0 =:= 'dot' ; T0 =:= ';' ; T0 =:= ':='; T0 =:= ')'  ->
                    case [Expr|ExprList] of
                        [Expr] -> {ok, Expr, [T1|Rest2]};
                        _ -> {ok, {'orelse_list', lists:reverse([Expr|ExprList])}, [T1|Rest2]}
                    end
            end
        % ; no_expr -> error(no_expr_in_guard)
    end.

expr_andalsos(Tokens) ->
    expr_andalsos(Tokens, []).
expr_andalsos(Tokens, ExprList) ->
    case expr_comp(Tokens) of
        {ok, Expr, Rest} -> 
            case Rest of
                [{'andalso',_}=_T1|Rest2] -> expr_andalsos(Rest2, [Expr|ExprList]);
                [{T0,_}=T1|Rest2] when T0 =:= 'orelse' ; T0 =:= ',' ; T0 =:= 'dot' ; T0 =:= ';' ; T0 =:= ':='; T0 =:= ')'  ->
                    case [Expr|ExprList] of
                        [Expr] -> {ok, Expr, [T1|Rest2]};
                        _ -> {ok, {'andalso_list', lists:reverse([Expr|ExprList])}, [T1|Rest2]}
                    end
            end
        % ; no_expr -> error(no_expr_in_guard)
    end.

expr_comp(Tokens) ->
    expr_comp(Tokens, []).
expr_comp(Tokens, ExprList) ->
    case expr_concat(Tokens) of
        {ok, Expr, Rest} -> 
            case Rest of
                [{'==',_}=_T1|Rest2]  -> expr_comp(Rest2, ['==',Expr|ExprList]);
                [{'/=',_}=_T1|Rest2]  -> expr_comp(Rest2, ['/=',Expr|ExprList]);
                [{'=<',_}=_T1|Rest2]  -> expr_comp(Rest2, ['=<',Expr|ExprList]);
                [{'>=',_}=_T1|Rest2]  -> expr_comp(Rest2, ['>=',Expr|ExprList]);
                [{'<',_}=_T1|Rest2]   -> expr_comp(Rest2, ['<',Expr|ExprList]);
                [{'>',_}=_T1|Rest2]   -> expr_comp(Rest2, ['>',Expr|ExprList]);
                [{'=:=',_}=_T1|Rest2] -> expr_comp(Rest2, ['=:=',Expr|ExprList]);
                [{'=/=',_}=_T1|Rest2] -> expr_comp(Rest2, ['=/=',Expr|ExprList]);
                [{T0,_}=T1|Rest2] when T0 =:= 'andalso' ; T0 =:= 'orelse' ; T0 =:= ',' ; T0 =:= 'dot' ; T0 =:= ';' ; T0 =:= ':='; T0 =:= ')'  ->
                    case [Expr|ExprList] of
                        [Expr] -> {ok, Expr, [T1|Rest2]};
                        _ -> {ok, {'comp_ops', lists:reverse([Expr|ExprList])}, [T1|Rest2]}
                    end
            end
        % ; no_expr -> error(no_expr_in_guard)
    end.

%% ++ and -- don't seem to be available in guard expressions.
expr_concat(Tokens) -> expr_add(Tokens).

expr_add(Tokens) ->
    expr_add(Tokens, []).
expr_add(Tokens, ExprList) ->
    case expr_mul(Tokens) of
        {ok, Expr, Rest} -> 
            case Rest of
                [{'+',_}=_T1|Rest2]    -> expr_add(Rest2, ['+',Expr|ExprList]);
                [{'-',_}=_T1|Rest2]    -> expr_add(Rest2, ['-',Expr|ExprList]);
                [{'bor',_}=_T1|Rest2]  -> expr_add(Rest2, ['bor',Expr|ExprList]);
                [{'bxor',_}=_T1|Rest2] -> expr_add(Rest2, ['bxor',Expr|ExprList]);
                [{'bsl',_}=_T1|Rest2]  -> expr_add(Rest2, ['bsl',Expr|ExprList]);
                [{'bsr',_}=_T1|Rest2]  -> expr_add(Rest2, ['bsr',Expr|ExprList]);
                [{'or',_}=_T1|Rest2]   -> expr_add(Rest2, ['or',Expr|ExprList]);
                [{'xor',_}=_T1|Rest2]  -> expr_add(Rest2, ['xor',Expr|ExprList]);
                [{T0,_}=T1|Rest2] when 
                    T0 =:= '==' ; T0 =:= '/=' ; T0 =:= '=<' ; T0 =:= '>=' ; T0 =:= '<' ; T0 =:= '>' ; T0 =:= '=:=' ; T0 =:= '=/=' ;
                    T0 =:= 'andalso' ; T0 =:= 'orelse' ; T0 =:= ',' ; T0 =:= 'dot' ; T0 =:= ';' ; T0 =:= ':='; T0 =:= ')'  ->
                    case [Expr|ExprList] of
                        [Expr] -> {ok, Expr, [T1|Rest2]};
                        _ -> {ok, {'add_ops', lists:reverse([Expr|ExprList])}, [T1|Rest2]}
                    end
            end
        % ; no_expr -> error(no_expr_in_guard)
    end.

expr_mul(Tokens) ->
    expr_mul(Tokens, []).
expr_mul(Tokens, ExprList) ->
    case expr_unary(Tokens) of
        {ok, Expr, Rest} -> 
            case Rest of
                [{'/',_}=_T1|Rest2]    -> expr_mul(Rest2, [Expr,'/'|ExprList]);
                [{'*',_}=_T1|Rest2]    -> expr_mul(Rest2, [Expr,'*'|ExprList]);
                [{'div',_}=_T1|Rest2]  -> expr_mul(Rest2, [Expr,'div'|ExprList]);
                [{'rem',_}=_T1|Rest2]  -> expr_mul(Rest2, [Expr,'rem'|ExprList]);
                [{'band',_}=_T1|Rest2] -> expr_mul(Rest2, [Expr,'band'|ExprList]);
                [{'and',_}=_T1|Rest2]  -> expr_mul(Rest2, [Expr,'and'|ExprList]);
                [{T0,_}=T1|Rest2] when 
                    T0 =:= '+' ; T0 =:= '-' ; T0 =:= 'bor' ; T0 =:= 'bxor' ; T0 =:= 'bsl' ; T0 =:= 'bsr' ; T0 =:= 'or' ; T0 =:= 'xor' ;
                    T0 =:= '==' ; T0 =:= '/=' ; T0 =:= '=<' ; T0 =:= '>=' ; T0 =:= '<' ; T0 =:= '>' ; T0 =:= '=:=' ; T0 =:= '=/=' ;
                    T0 =:= 'andalso' ; T0 =:= 'orelse' ; T0 =:= ',' ; T0 =:= 'dot' ; T0 =:= ';' ; T0 =:= ':='; T0 =:= ')'  ->
                    case [Expr|ExprList] of
                        [Expr] -> {ok, Expr, [T1|Rest2]};
                        _ -> {ok, {'mul_ops', lists:reverse([Expr|ExprList])}, [T1|Rest2]}
                    end
            end
        % ; no_expr -> error(no_expr_in_guard)
    end.

expr_unary([{'not',_} | Tokens]) ->
    {ok, Expr, Rest} = expr_individual(Tokens),
    {ok, {'unary_not', Expr}, Rest};
expr_unary([{'+',_} | Tokens]) ->
    {ok, Expr, Rest} = expr_individual(Tokens),
    {ok, {'unary_+', Expr}, Rest};
expr_unary([{'-',_} | Tokens]) ->
    {ok, Expr, Rest} = expr_individual(Tokens),
    {ok, {'unary_-', Expr}, Rest};
expr_unary([{'bnot',_} | Tokens]) ->
    {ok, Expr, Rest} = expr_individual(Tokens),
    {ok, {'unary_bnot', Expr}, Rest};
expr_unary(Tokens) -> expr_individual(Tokens).


expr_individual([{atom,_,Fun},{'(',_}|Rest]) ->
    {ok, Args, Rest2} = expr_fun(Rest),
    {ok, {f, Fun, Args}, Rest2};
expr_individual([{var,_,Arg}|Rest]) ->
    {ok, {'var',Arg}, Rest};
expr_individual([{integer,_,Num}|Rest]) ->
    {ok, {'integer',Num}, Rest};
expr_individual([{'#',_}|Rest]) -> todo;
expr_individual([{'<<',_}|Rest]) -> 
    {ok, BExpr, Rest2} = expr_bits(Rest),
    {ok, {bitexpr, BExpr}, Rest2};
expr_individual([{atom,_,Atom}|Rest]) ->
    {ok, {'atom',Atom}, Rest};
expr_individual([{'(',_}|Rest]) ->
    {ok, Inside, Rest2} = expr_parenthesis(Rest),
    {ok, {parenthesis, Inside}, Rest2};
expr_individual([{'[',_}|Rest]) ->
    {ok, List, TListExpr, Rest2} = expr_list(Rest),
    {ok, {list, List, TListExpr}, Rest2};
expr_individual([{'{',_}|Rest]) ->
    {ok, Tuple, Rest2} = expr_tuple(Rest),
    {ok, {tuple, Tuple}, Rest2}.


expr_bits([{'>>',_}|Rest]) ->
    {ok, [], Rest};
expr_bits(Rest) ->
    expr_bits(Rest, []).
expr_bits(Rest, BExprs) ->
    {ok, ExprInBits, Rest2} = expr_in_bits(Rest),
    case Rest2 of
        [{'>>',_}|Rest3] -> {ok, lists:reverse([ExprInBits|BExprs]), Rest3};
        [{',',_}|Rest3] -> expr_bits(Rest3, [ExprInBits|BExprs])
    end.

expr_in_bits([{'<<',_}|Rest]) ->
    {ok, BExpr, Rest2} = expr_bits(Rest),
    {ok, Size, TL, Rest3} = expr_in_bits_size_and_typelist(Rest2),
    {ok, {bitexpr, BExpr, Size, TL}, Rest3};
expr_in_bits([{'integer',_,N}|Rest]) ->
    {ok, Size, TL, Rest2} = expr_in_bits_size_and_typelist(Rest),
    {ok, {integer, N, Size, TL}, Rest2};
expr_in_bits([{'string',_,S}|Rest]) ->
    {ok, Size, TL, Rest2} = expr_in_bits_size_and_typelist(Rest),
    {ok, {string, S, Size, TL}, Rest2};
expr_in_bits([{'var',_,Var}|Rest]) ->
    {ok, Size, TL, Rest2} = expr_in_bits_size_and_typelist(Rest),
    {ok, {bitvar, Var, Size, TL}, Rest2}.

expr_in_bits_size_and_typelist(Rest) ->
    case Rest of
        [{':',_},{'integer',_,Size},{'/',_}|Rest2] -> 
            {ok, TL, Rest3} = expr_in_bits_typelist(Rest2),
            {ok, Size, TL, Rest3};
        [{':',_},{'integer',_,Size}|Rest2] ->
            {ok, Size, none, Rest2};
        [{'/',_}|Rest2] -> 
            {ok, TL, Rest3} = expr_in_bits_typelist(Rest2),
            {ok, none, TL, Rest3};
        _ ->
            {ok, none, none, Rest}
    end.

expr_in_bits_typelist(Tokens) ->
    expr_in_bits_typelist(Tokens,[]).
expr_in_bits_typelist([{'atom',_,'unit'}=T1,{':',_}=T2,{'integer',_,_}=T3|Rest], TL) ->
    case Rest of
        [{'-',_}=T4|Rest2] -> expr_in_bits_typelist(Rest2, [T4,T3,T2,T1|TL]);
        _ -> {ok, lists:reverse([T3,T2,T1|TL]), Rest}
    end;
expr_in_bits_typelist([{'atom',_,_}=T|Rest], TL) ->
    case Rest of
        [{'-',_}=T2|Rest2] -> expr_in_bits_typelist(Rest2, [T2,T|TL]);
        _ -> {ok, lists:reverse([T|TL]), Rest}
    end.


expr_fun([{')',_}|Rest]) ->
    {ok, [], Rest};
expr_fun(Tokens) ->
    expr_fun(Tokens, []).
expr_fun(Tokens, Args) ->
    case expr_orelses(Tokens) of
        {ok, Expr, Rest} ->
            case Rest of
                [{',',_}|Rest2] -> expr_fun(Rest2, [Expr|Args]);
                [{')',_}|Rest2] -> {ok, lists:reverse([Expr|Args]), Rest2}
            end
        % ; no_expr -> error(no_expr)
    end.

expr_parenthesis(Tokens) ->
    case expr_orelses(Tokens) of
        {ok, Expr, Rest} ->
            case Rest of
                [{')',_}|Rest2] -> {ok, Expr, Rest2}
            end
        % ; no_expr -> error(no_expr)
    end.

expr_list(Tokens) ->
    expr_list(Tokens, []).
expr_list(Tokens, Elements) ->
    case expr_orelses(Tokens) of
        {ok, Expr, Rest} ->
            case Rest of
                [{'|',_}|Rest2] ->
                    case expr_orelses(Rest2) of
                        {ok, TExpr, Rest3} ->
                            case Rest3 of
                                [{']',_}|Rest4] ->
                                    {ok, lists:reverse([Expr|Elements]), TExpr, Rest4}
                            end
                    end;
                [{',',_}|Rest2] -> expr_list(Rest2, [Expr|Elements]);
                [{']',_}|Rest2] -> {ok, lists:reverse([Expr|Elements]), none, Rest2}
            end
        % ; no_expr -> error(no_expr)
    end.

expr_tuple(Tokens) ->
    expr_tuple(Tokens, []).
expr_tuple(Tokens, Elements) ->
    case expr_orelses(Tokens) of
        {ok, Expr, Rest} ->
            case Rest of
                [{',',_}|Rest2] -> expr_tuple(Rest2, [Expr|Elements]);
                [{'}',_}|Rest2] -> {ok, lists:reverse([Expr|Elements]), Rest2}
            end
        % ; no_expr -> error(no_expr)
    end.
    
%%
%% Combine back to tokens.

conjunctions_list_to_tokens(_LineNum,none) -> {ok, []};
conjunctions_list_to_tokens(LineNum,{conjunction_list,Conjunctions}) ->
    conjunctions_list_to_tokens(LineNum,Conjunctions,[]).
conjunctions_list_to_tokens(LineNum,[C],SoFar) ->
    Tokens = lists:append(lists:reverse([to_tokens(LineNum,C)|SoFar])),
    lists:map(fun(A) when is_tuple(A) -> A end, Tokens),
    {ok, Tokens};
conjunctions_list_to_tokens(LineNum,[C|Rest],SoFar) ->
    conjunctions_list_to_tokens(LineNum,Rest,[[{',',LineNum}],to_tokens(LineNum,C)|SoFar]).
    
to_tokens(LineNum, {atom,Atom}) -> [{atom, LineNum, Atom}];

to_tokens(LineNum, {'orelse_list', ExprList}) ->
    combine_list_to_tokens_with_token(LineNum,ExprList,'orelse');
to_tokens(LineNum, {'andalso_list', ExprList}) ->
    combine_list_to_tokens_with_token(LineNum,ExprList,'andalso');

to_tokens(LineNum, {'comp_ops', ExprAndOpList}) ->
    combine_list_to_tokens_with_operators(LineNum,ExprAndOpList);
to_tokens(LineNum, {'add_ops', ExprAndOpList}) ->
    combine_list_to_tokens_with_operators(LineNum,ExprAndOpList);
to_tokens(LineNum, {'mul_ops', ExprAndOpList}) ->
    combine_list_to_tokens_with_operators(LineNum,ExprAndOpList);

to_tokens(LineNum, {'unary_not', Expr}) ->
    [{'not',LineNum}] ++ to_tokens(LineNum, Expr);
to_tokens(LineNum, {'unary_+', Expr}) ->
    [{'+',LineNum}] ++ to_tokens(LineNum, Expr);
to_tokens(LineNum, {'unary_-', Expr}) ->
    [{'-',LineNum}] ++ to_tokens(LineNum, Expr);
to_tokens(LineNum, {'unary_bnot', Expr}) ->
    [{'bnot',LineNum}] ++ to_tokens(LineNum, Expr);


to_tokens(LineNum, {f, Fun, Args}) ->
    [{'atom',LineNum,Fun},{'(',LineNum}] ++
        combine_list_to_tokens_with_token(LineNum,Args,',') ++ [{')',LineNum}];
to_tokens(LineNum, {'var',Var}) -> [{var, LineNum, Var}];
to_tokens(LineNum, {'integer',Num}) -> [{'integer', LineNum, Num}];
to_tokens(LineNum, {bitexpr, BExprs}) ->
    [{'<<',LineNum}] ++ to_tokens_bits(LineNum, BExprs) ++ [{'>>',LineNum}];
to_tokens(LineNum, {parenthesis, Inside}) ->
    [{'(',LineNum}] ++ to_tokens(LineNum, Inside) ++ [{')',LineNum}];
to_tokens(LineNum, {list, ExprList, none}) ->
    [{'[',LineNum}] ++ 
    combine_list_to_tokens_with_token(LineNum,ExprList,',') ++ [{']',LineNum}];
to_tokens(LineNum, {list, ExprList, TailExpr}) ->
    [{'[',LineNum}] ++ 
    combine_list_to_tokens_with_token(LineNum,ExprList,',') ++ [{'|',LineNum}] ++ to_tokens(LineNum, TailExpr) ++ [{']',LineNum}];
to_tokens(LineNum, {tuple, ExprList}) ->
    [{'{',LineNum}] ++ 
    combine_list_to_tokens_with_token(LineNum,ExprList,',') ++ [{'}',LineNum}].

to_tokens_bits(LineNum, BExprs) -> to_tokens_bits(LineNum, BExprs, []).
to_tokens_bits(LineNum, [], SoFar) -> lists:append(lists:reverse(SoFar));
to_tokens_bits(LineNum, [{bitexpr, BExprs, Size, TL}|Rest], SoFar) ->
    to_tokens_bits_next(LineNum, Rest, [lists:append(
        [[{'<<',LineNum}|to_tokens_bits(LineNum, BExprs)],
            [{'>>',LineNum}], to_tokens_bits_size_tl(LineNum, Size, TL)])|SoFar]);
to_tokens_bits(LineNum, [{integer, N, Size, TL}|Rest], SoFar) ->
    to_tokens_bits_next(LineNum, Rest,
        [[{'integer',LineNum,N}] ++ to_tokens_bits_size_tl(LineNum, Size, TL)|SoFar]);
to_tokens_bits(LineNum, [{string, S, Size, TL}|Rest], SoFar) ->
    to_tokens_bits_next(LineNum, Rest,
        [[{'string',LineNum,S}] ++ to_tokens_bits_size_tl(LineNum, Size, TL)|SoFar]);
to_tokens_bits(LineNum, [{bitvar, Var, Size, TL}|Rest], SoFar) ->
    to_tokens_bits_next(LineNum, Rest,
        [[{'var',LineNum,Var}] ++ to_tokens_bits_size_tl(LineNum, Size, TL)|SoFar]).
to_tokens_bits_next(LineNum, [], SoFar) -> to_tokens_bits(LineNum, [], SoFar);
to_tokens_bits_next(LineNum, Next, SoFar) -> to_tokens_bits(LineNum, Next, [[{',',LineNum}]|SoFar]).


to_tokens_bits_size_tl(_, none, none) -> [];
to_tokens_bits_size_tl(LineNum, Size, none) when is_integer(Size) ->
    [{':',LineNum},{'integer',LineNum,Size}];
to_tokens_bits_size_tl(LineNum, none, TL) when is_list(TL) ->
    [{'/',LineNum}|TL];
to_tokens_bits_size_tl(LineNum, Size, TL) when is_list(TL), is_integer(Size) ->
    [{':',LineNum},{'integer',LineNum,Size},{'/',LineNum}|TL].

combine_list_to_tokens_with_token(LineNum,ExprList,Op) ->
    combine_list_to_tokens_with_token(LineNum,ExprList,Op,[]).
combine_list_to_tokens_with_token(LineNum,[C],_,SoFar) ->
    lists:append(lists:reverse([to_tokens(LineNum,C)|SoFar]));
combine_list_to_tokens_with_token(LineNum,[C|Rest],Op,SoFar) ->
    combine_list_to_tokens_with_token(LineNum,Rest,Op,[[{Op,LineNum}],to_tokens(LineNum,C)|SoFar]).

combine_list_to_tokens_with_operators(LineNum,ExprAndOpList) ->
    combine_list_to_tokens_with_operators(LineNum,ExprAndOpList, []).
combine_list_to_tokens_with_operators(LineNum,[Expr],SoFar) ->
    lists:append(lists:reverse([to_tokens(LineNum,Expr)|SoFar]));
combine_list_to_tokens_with_operators(LineNum,[Expr, Op |ExprAndOpList], SoFar) when is_atom(Op) ->
    combine_list_to_tokens_with_operators(LineNum,ExprAndOpList, [[{Op,LineNum}],to_tokens(LineNum,Expr)|SoFar]).


%%
%% ERG Exception possibilities check

find_errors(UDGDefinitions) ->
    find_errors(UDGDefinitions, []).
find_errors([{defs,[{{udg,Name,Arity},none,Args,_}|_]=Definitions}|Rest], Errors) when is_list(Args) ->
    case {get_udg_function_kind_from_atom(Name), length(Definitions)} of
        {not_udg_name,_} ->
            find_errors(Rest, [{error,{udg,Name,Arity},head_name_not_of_a_udg_type}|Errors]);
        {{ok, ue}, N} when N > 1 ->
            find_errors(Rest, [{error,{udg,Name,Arity},udg_of_ue_type_cannot_have_disjunctions_to_allow_negation}|Errors]);
        {{ok, UDGKind}, _} ->
            {ok, NewErrors} =
                case UDGKind of
                    u -> find_errors_in_definition(udg,Name,Arity,any,u,Definitions);
                    ue -> find_errors_in_definition(udg,Name,Arity,1,ue,Definitions);
                    up -> find_errors_in_definition(udg,Name,Arity,any,up,Definitions);
                    uep -> find_errors_in_definition(udg,Name,Arity,1,uep,Definitions)
                end,
            find_errors(Rest, lists:reverse(NewErrors) ++ Errors)
    end;
find_errors([{defs,[{{pattern_template,Name,Arity},Pattern,Args,_}|_]=Definitions}|Rest], Errors) when is_list(Pattern), is_list(Args) ->
    {ok, NewErrors} = find_errors_in_definition(pattern_template,Name,Arity,any,pattern_template,Definitions),
    find_errors(Rest, lists:reverse(NewErrors) ++ Errors);
find_errors([_|Rest], Errors) ->
    find_errors(Rest, Errors);
find_errors([], Errors) ->
    lists:reverse(Errors).

get_udg_function_kind_from_atom(Name) ->
    case atom_to_list(Name) of
        [$i,$s,$_,$u,$_|_] -> {ok, u};
        [$i,$s,$_,$u,$e,$_|_] -> {ok, ue};
        [$i,$s,$_,$u,$p,$_|_] -> {ok, up};
        [$i,$s,$_,$u,$e,$p,$_|_] -> {ok, uep};
        _ -> not_udg_name
    end.

find_errors_in_definition(Type,Name,Arity,ConjunctionLimit,UDGKind,Definitions) ->
    find_errors_in_definition(Type,Name,Arity,ConjunctionLimit,UDGKind,Definitions,[]).
find_errors_in_definition(Type0,Name0,Arity0,ConjunctionLimit,UDGKind,[Definition|Rest],Errors) ->
    Name0AndArity0 = {Type0,Name0,Arity0},
    NewErrors =
        case Definition of
            {{udg,Name1,Arity1},none,Args1,Body1} when udg =:= Type0, Name1 =:= Name0, Arity1 =:= Arity0 ->
                case Body1 of
                    {conjunctions_list, L} when ConjunctionLimit /= any, length(L) > ConjunctionLimit ->
                        [{error,Name0AndArity0,udg_function_kind_expects_no_conjunctions}];
                    _ ->
                        DefinedVars = ordsets:union(lists:foldl(
                            fun(A,L) -> [ord_list_of_variables_from_tokens(A)|L] end, [], Args1)),
                        find_undefined_variables(Name0AndArity0, DefinedVars, Body1)
                end;
            {{pattern_template,Name1,Arity1},Pattern1,Args1,Body1} when pattern_template =:= Type0, Name1 =:= Name0, Arity1 =:= Arity0 ->
                DefinedVars = ord_list_of_variables_from_tokens(Pattern1),
                Errors1 = find_errors_with_pattern_template_arguments(Args1),
                Errors2 = lists:append(lists:foldl(fun(A,L) -> [find_undefined_variables(Name0AndArity0, DefinedVars, {argument_tokens, A})|L] end, [], Args1)),
                Errors3 = find_undefined_variables(Name0AndArity0, DefinedVars, Body1),
                lists:append([Errors1, Errors2, Errors3]);
            {{Type1,_,_},_,_,_} when Type1 /= Type0 -> [{error,Name0AndArity0,head_mismatch_change_in_udg_kind}];
            {{_,Name1,_},_,_,_} when Name1 /= Name0 -> [{error,Name0AndArity0,head_mismatch_change_in_function_name}];
            {{_,_,Arity1},_,_,_} when Arity1 /= Arity0 -> [{error,Name0AndArity0,head_mismatch_change_in_function_arity}]
        end,
    find_errors_in_definition(Type0,Name0,Arity0,ConjunctionLimit,UDGKind,Rest,lists:reverse(NewErrors) ++ Errors);
find_errors_in_definition(_Type0,_Name0,_Arity0,_ConjunctionLimit,_UDGKind,[],Errors) ->
    {ok, lists:reverse(Errors)}.

find_errors_with_pattern_template_arguments(Args1) ->
    find_errors_with_pattern_template_arguments(Args1, ordsets:new(), []).
find_errors_with_pattern_template_arguments([[{'var',_Line,Var}]|Args1], ArgSet, Errors) ->
    case ordsets:is_element(Var, ArgSet) of
        true ->
            ArgError = {'error', {pattern_template_can_only_have_simple_single_position_arguments, Var}},
            find_errors_with_pattern_template_arguments(Args1, ArgSet, [ArgError|Errors]);
        false ->
            find_errors_with_pattern_template_arguments(Args1, ordsets:add_element(Var, ArgSet), Errors)
    end;
find_errors_with_pattern_template_arguments([_Etc|Args1], ArgSet, Errors) ->
    find_errors_with_pattern_template_arguments(Args1, ArgSet, Errors);
find_errors_with_pattern_template_arguments([], _ArgSet, Errors) ->
    lists:reverse(Errors).


find_undefined_variables(Name0AndArity0, DefinedVars, {argument_tokens, Tokens}) ->
    find_undefined_variables(Name0AndArity0, DefinedVars, Tokens, []);
find_undefined_variables(Name0AndArity0, DefinedVars, Body) when Body =:= none orelse is_tuple(Body) ->
    {ok, Tokens} = conjunctions_list_to_tokens(1,Body),
    find_undefined_variables(Name0AndArity0, DefinedVars, Tokens, []).
find_undefined_variables(Name0AndArity0, DefinedVars, [{var,_,Var}|Body], Errors) ->
    case ordsets:is_element(Var, DefinedVars) of
        true -> find_undefined_variables(Name0AndArity0, DefinedVars, Body, Errors);
        _ -> find_undefined_variables(Name0AndArity0, DefinedVars, Body,
                [{error,Name0AndArity0,{variable_not_found, Var}}|Errors])
    end;
find_undefined_variables(Name0AndArity0, DefinedVars, [_|Body], Errors) ->
    find_undefined_variables(Name0AndArity0, DefinedVars, Body, Errors);
find_undefined_variables(_Name0AndArity0, _DefinedVars, [], Errors) ->
    lists:reverse(Errors).

ord_list_of_variables_from_tokens(Tokens) ->
    ord_list_of_variables_from_tokens(Tokens,[]).
ord_list_of_variables_from_tokens([{var,_,'_'}|Rest],Vars) ->
    ord_list_of_variables_from_tokens(Rest,Vars);
ord_list_of_variables_from_tokens([{var,_,V}|Rest],Vars) ->
    ord_list_of_variables_from_tokens(Rest,[V|Vars]);
ord_list_of_variables_from_tokens([_|Rest],Vars) ->
    ord_list_of_variables_from_tokens(Rest,Vars);
ord_list_of_variables_from_tokens([],Vars) ->
    ordsets:from_list(Vars).


%%
%% ERG Exception possibilities check.

find_warnings(UDGDefinitions) ->
    find_warnings(UDGDefinitions, []).

find_warnings([{defs,[{NameAndArity,_,_,_}|_]=Definitions}|Rest], Warnings) ->
    {ok, NewErrors} = find_warnings_in_definition(NameAndArity,Definitions),
    find_warnings(Rest, lists:reverse(NewErrors) ++ Warnings);
find_warnings([_|Rest], Warnings) ->
    find_warnings(Rest, Warnings);
find_warnings([], Warnings) ->
    lists:reverse(Warnings).

find_warnings_in_definition(NameAndArity,Definitions) ->
    find_warnings_in_definition(NameAndArity,Definitions,[]).
find_warnings_in_definition(Name0AndArity0,[{_,Pattern,_Args1,Body1}|Rest],Warnings) when is_list(Pattern) ->
    TState = possible_types_from_pattern(Pattern),
    NewWarnings = find_exception_prone_conjunctions(TState,Body1),
    find_warnings_in_definition(Name0AndArity0,Rest,lists:reverse(NewWarnings) ++ Warnings);
find_warnings_in_definition(Name0AndArity0,[{_,none,Args1,Body1}|Rest],Warnings) ->
    TState = possible_types_from_arguments(Args1),
    NewWarnings = find_exception_prone_conjunctions(TState,Body1),
    find_warnings_in_definition(Name0AndArity0,Rest,lists:reverse(NewWarnings) ++ Warnings);
find_warnings_in_definition(_Name0AndArity0,[],Warnings) ->
    {ok, lists:reverse(Warnings)}.

possible_types_from_pattern(_Pattern) ->
    todo.

possible_types_from_arguments(_Arguments) ->
    todo.

find_exception_prone_conjunctions(_TState,none) -> [];
find_exception_prone_conjunctions(TState,{conjunction_list,Conjunctions}) ->
    conjunctions_list_to_exn_warnings(TState,Conjunctions,[]).
conjunctions_list_to_exn_warnings(_TState,[],Warnings) ->
    lists:reverse(Warnings);
conjunctions_list_to_exn_warnings(TState,[C|Rest],Warnings) ->
    {TState2, Warnings} = to_warnings(TState,C),
    conjunctions_list_to_exn_warnings(TState2,Rest,lists:reverse(Warnings) ++ Warnings).

to_warnings(TState, {atom, _Atom}) -> {TState, []};
to_warnings(TState, {'orelse_list', _ExprList}) -> {TState, []};
to_warnings(TState, {'andalso_list', _ExprList}) -> {TState, []};
to_warnings(TState, {'comp_ops', _ExprAndOpList}) -> {TState, []};
to_warnings(TState, {'add_ops', _ExprAndOpList}) -> {TState, []};
to_warnings(TState, {'mul_ops', _ExprAndOpList}) -> {TState, []};
to_warnings(TState, {'unary_not', _Expr}) -> {TState, []};
to_warnings(TState, {'unary_+', _Expr}) -> {TState, []};
to_warnings(TState, {'unary_-', _Expr}) -> {TState, []};
to_warnings(TState, {'unary_bnot', _Expr}) -> {TState, []};
to_warnings(TState, {f, _Fun, _Args}) -> {TState, []};
to_warnings(TState, {parenthesis, Inside}) ->
    to_warnings(TState, Inside);
to_warnings(TState, {'var', _}) -> {TState, []};
to_warnings(TState, {'integer', _}) -> {TState, []};
to_warnings(TState, {bitexpr, _})  -> {TState, []};
to_warnings(TState, {list, _, _}) -> {TState, []};
to_warnings(TState, {tuple, _})   -> {TState, []}.

