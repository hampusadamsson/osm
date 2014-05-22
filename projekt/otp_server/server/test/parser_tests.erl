-module(parser_tests).

%% To use EUnit:
-include_lib("eunit/include/eunit.hrl").

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


%%%%%%%%%%%%%%%%%%% Tests for getString %%%%%%%%%%%%%%%%%%%
getString_test() ->
    S1 = parser:getString("global du är en tratt",s1,[{"global",[{s3,"john3"},{s2,"john2"},{s1,"john1"}],false}]),
    S2 = parser:getString("global du är en tratt",s2,[{"global",[{s3,"john3"},{s2,"john2"},{s1,"john1"}],false}]),
    S3 = parser:getString("global du är en tratt",s3,[{"global",[{s3,"john3"},{s2,"john2"},{s1,"john1"}],false}]),
    S4 = parser:getString("global Glass är gott",s1,[{"global",[{s3,"john3"},{s2,"john2"},{s1,"john1"}],true}]),
    S5 = parser:getString("global Glass är gott",s2,[{"global",[{s3,"john3"},{s2,"john2"},{s1,"john1"}],true}]),
    S6 = parser:getString("global Glass är gott",s3,[{"global",[{s3,"john3"},{s2,"john2"},{s1,"john1"}],true}]), 
    S7 = parser:getString("global Stockholm är en förort till Uppsala",s1,[{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},{s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},{s5,"pelle"}],false}]),
    S8 = parser:getString("global Stockholm är en förort till Uppsala",s2,[{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},{s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},{s5,"pelle"}],false}]),
    S9 = parser:getString("global Stockholm är en förort till Uppsala",s3,[{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},{s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},{s5,"pelle"}],false}]),
    S10 = parser:getString("global Stockholm är en förort till Uppsala",s4,[{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},{s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},{s5,"pelle"}],false}]),
    S11 = parser:getString("global Stockholm är en förort till Uppsala",s5,[{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},{s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},{s5,"pelle"}],false}]),
    S12 = parser:getString("global Stockholm är en förort till Uppsala",s6,[{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},{s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},{s5,"pelle"}],false}]),
    S13 = parser:getString("coolroom Stockholm är en förort till Uppsala",s4,[{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},{s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},{s5,"pelle"}],false}]),
    S14 = parser:getString("coolroom Stockholm är en förort till Uppsala",s5,[{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},{s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},{s5,"pelle"}],false}]),
    S15 = parser:getString("coolroom Stockholm är en förort till Uppsala",s6,[{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},{s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},{s5,"pelle"}],false}]),
    
    ?assertEqual(S1,"global john1>  du är en tratt"),
    ?assertEqual(S2,"global john2>  du är en tratt"),
    ?assertEqual(S3,"global john3>  du är en tratt"),
    ?assertEqual(S4,"global john1>  Glass är gott"),
    ?assertEqual(S5,"global john2>  Glass är gott"),
    ?assertEqual(S6,"global john3>  Glass är gott"),
    ?assertEqual(S7,"global pa>  Stockholm är en förort till Uppsala"),
    ?assertEqual(S8,"global erik>  Stockholm är en förort till Uppsala"),
    ?assertEqual(S9,"global john>  Stockholm är en förort till Uppsala"),
    ?assertEqual(S10,"global hampus>  Stockholm är en förort till Uppsala"),
    ?assertEqual(S11,"global pelle>  Stockholm är en förort till Uppsala"),
    ?assertEqual(S12,"global maria>  Stockholm är en förort till Uppsala"),
    ?assertEqual(S13,"coolroom hampus>  Stockholm är en förort till Uppsala"),
    ?assertEqual(S14,"coolroom pelle>  Stockholm är en förort till Uppsala"),
    ?assertEqual(S15,"coolroom maria>  Stockholm är en förort till Uppsala").
