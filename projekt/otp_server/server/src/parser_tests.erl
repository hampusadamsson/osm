-module(parser_tests).

%% To use EUnit:
-include_lib("eunit/include/eunit.hrl").

-export([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% Tests for removeNewLine %%%%%%%%%%%%%%%%%
removeNewLine_test() ->
    ?assertEqual([], parser:removeNewLine([])),
    ?assertEqual("",parser:removeNewLine("")),
    ?assertEqual(" ",parser:removeNewLine(" ")),
    ?assertEqual("  ",parser:removeNewLine("  ")),
    ?assertEqual("Hej",parser:removeNewLine("Hej")),
    ?assertEqual("Hej ",parser:removeNewLine("Hej ")),
    ?assertEqual("Hej",parser:removeNewLine("Hej\n")),
    ?assertEqual("Hej",parser:removeNewLine("Hej\n" )),
    ?assertEqual("Hej\nVad görs?",parser:removeNewLine("Hej\nVad görs?")),
    ?assertEqual("En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng",
		 parser:removeNewLine("En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng\n")).



%%%%%%%%%%%%%%%%%%% Tests for getParts  %%%%%%%%%%%%%%%%%%%
getParts_test() ->
    ?assertEqual({[],[],[]},parser:getParts("")),
    ?assertEqual({[],[],[]},parser:getParts(" ")),
    ?assertEqual({[],[],[]},parser:getParts("  ")),
    ?assertEqual({[],[],[]},parser:getParts("   ")),
    ?assertEqual({[],[],[]},parser:getParts("    ")),
    ?assertEqual({"asd",[],[]},parser:getParts("asd")),
    ?assertEqual({"tjena","tjofräs",[]},parser:getParts("tjena tjofräs")),
    ?assertEqual({"hallå","hej","köttbullar"},parser:getParts("hallå hej köttbullar")),
    ?assertEqual({"hallå","hej","köttbullar"},parser:getParts("hallå hej köttbullar kladdkaka med grädde")),
    ?assertEqual({"asd","qwe",true},parser:getParts("asd qwe true")),
    ?assertEqual({"hallaballo","tratt",false},parser:getParts("hallaballo tratt false")),
    ?assertEqual({"asd","qwe",true},parser:getParts("asd qwe true jag har kul")),
    ?assertEqual({"hallaballo","tratt",false},parser:getParts("hallaballo tratt false jag är glad")),
    ?assertEqual({"massor","av","nonsens"},parser:getParts("massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens")).

