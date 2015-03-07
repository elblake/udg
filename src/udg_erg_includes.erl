
%%
%% udg_erg_includes: Functions pertaining to parsing tokens and determining
%%                   data structures for including user defined guards and
%%                   pattern templates from an ERG source file.
%%
%% Functions that are used in common in between udg.erl and udg_erg.erl for
%% parsing -include_udg(...) and -auto_include_udg(...).
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
-module(udg_erg_includes).
-description('User defined guard ERG includes').
-author('Edward L. Blake < edwardlblake @ gmail.com >').

-export([ proc_source_include_udg/2,
          proc_source_auto_include_udg/2,
          erg_filename_from_udg_name/1,
          erg_filenames_for_each_include_udg/2,
          no_slashes/1 ]).

-type tokens() :: [tuple()].
-type udg_name_arity() :: {udg, atom(), integer()} | {pattern_template, atom(), integer()}.
-type udg_include_list() :: [{udg_name_arity(), atom()}].

-spec proc_source_include_udg/2 :: (tokens(), udg_include_list()) -> {ok, udg_include_list(), tokens()}.
proc_source_include_udg(Tokens, ExistingIncludeList) ->
    case Tokens of
        [{'(',_},{'atom',_,ERGFile},{',',_},{'[',_} | Rest] ->
            case proc_udg_include_list(Rest) of
                {ok, ListOfImportedUDG, [{dot,_}|Rest2]} ->
                    NewUDGImportList = orddict:merge(fun included_twice/3, ExistingIncludeList,
                        orddict:from_list(erg_filenames_for_each_include_udg(ERGFile, ListOfImportedUDG))),
                    {ok, NewUDGImportList, Rest2}
            end;
        _ -> error(include_udg_directive_not_formed_right)
    end.

-spec proc_source_auto_include_udg/2 :: (tokens(), udg_include_list()) -> {ok, udg_include_list(), tokens()}.
proc_source_auto_include_udg(Tokens, ExistingIncludeList) ->
    case Tokens of
        [{'(',_},{'[',_} | Rest] ->
            case proc_udg_include_list(Rest) of
                {ok, ListOfUDGs, [{dot,_}|Rest2]} ->
                    NewUDGImportList = orddict:merge(fun included_twice/3, ExistingIncludeList,
                        orddict:from_list(erg_filenames_from_auto_include_udg_list(ListOfUDGs))),
                {ok, NewUDGImportList, Rest2}
            end;
        _ -> error(auto_include_udg_directive_not_formed_right)
    end.

included_twice(_, Val1, Val2) ->
    if Val1 =:= Val2 -> Val1;
       true -> error(udg_function_with_same_name_and_arity_included_from_different_files)
    end.


-spec proc_udg_include_list/1 :: (tokens()) -> {ok, [udg_name_arity()], tokens()}.
proc_udg_include_list([{']',_},{')',_} | Rest]) ->
    {ok, [], Rest};
proc_udg_include_list(Tokens) ->
    proc_udg_include_list(Tokens, []).
proc_udg_include_list([{atom,_,UDGName}, {'/',_}, {'integer',_,Arity} | Rest], ListOfUDGs) ->
    NewListOfUDGs = [{udg,UDGName,Arity} | ListOfUDGs],
    case Rest of
        [{']',_},{')',_} | Rest2] -> {ok, NewListOfUDGs, Rest2};
        [{',',_} | Rest2] -> proc_udg_include_list(Rest2, NewListOfUDGs)
    end;
proc_udg_include_list([{'\\',_},{'\\',_},{atom,_,PattTemplateName}, {'/',_}, {'integer',_,Arity} | Rest], ListOfUDGs) ->
    NewListOfUDGs = [{pattern_template,PattTemplateName,Arity} | ListOfUDGs],
    case Rest of
        [{']',_},{')',_} | Rest2] -> {ok, NewListOfUDGs, Rest2};
        [{',',_} | Rest2] -> proc_udg_include_list(Rest2, NewListOfUDGs)
    end.


-spec erg_filenames_from_auto_include_udg_list/1 :: ([udg_name_arity()]) -> udg_include_list().
erg_filenames_from_auto_include_udg_list(UDGList) ->
    erg_filenames_from_auto_include_udg_list(UDGList, []).
erg_filenames_from_auto_include_udg_list([{udg, UDGAtomName, _}=UDG|Rest], ListWithERGFilenames) ->
    ERGEntry = {UDG, list_to_atom(erg_filename_from_udg_name(UDGAtomName))},
    erg_filenames_from_auto_include_udg_list(Rest, [ERGEntry | ListWithERGFilenames]);
erg_filenames_from_auto_include_udg_list([{pattern_template, PattTemplateAtomName, _}=UDG | Rest], ListWithERGFilenames) ->
    ERGEntry = {UDG, PattTemplateAtomName},
    erg_filenames_from_auto_include_udg_list(Rest, [ERGEntry | ListWithERGFilenames]);
erg_filenames_from_auto_include_udg_list([], ListWithERGFilenames) ->
    lists:reverse(ListWithERGFilenames).


-spec erg_filenames_for_each_include_udg/2 :: (atom(), [udg_name_arity()]) -> udg_include_list().
erg_filenames_for_each_include_udg(ERGFileName, UDGList) when is_atom(ERGFileName) ->
    erg_filenames_for_each_include_udg(ERGFileName, UDGList, []).
erg_filenames_for_each_include_udg(ERGFileName, [UDG | Rest], ListWithERGFilenames) ->
    erg_filenames_for_each_include_udg(ERGFileName, Rest, [{UDG,ERGFileName} | ListWithERGFilenames]);
erg_filenames_for_each_include_udg(_, [], ListWithERGFilenames) ->
    lists:reverse(ListWithERGFilenames).


-spec erg_filename_from_udg_name/1 :: (atom()) -> string().
erg_filename_from_udg_name(UDGAtom) when is_atom(UDGAtom) ->
    no_slashes(
        case atom_to_list(UDGAtom) of
            [$i,$s,$_,$u,$_ | Name] -> Name;
            [$i,$s,$_,$u,$p,$_ | Name] -> Name;
            [$i,$s,$_,$u,$e,$_ | Name] -> Name;
            [$i,$s,$_,$u,$e,$p,$_ | Name] -> Name
        end).


-spec no_slashes/1 :: (string()) -> string().
no_slashes(String) -> case has_no_slashes(String) of true -> String end.
has_no_slashes([]) -> true;
has_no_slashes([$/|_]) -> error(unexpected_slashes_in_udg_atom);
has_no_slashes([$\\|_]) -> error(unexpected_slashes_in_udg_atom);
has_no_slashes([_|Rest]) -> has_no_slashes(Rest).
