-module(parser_tests).

%% To use EUnit:
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% Tests for remove_new_line %%%%%%%%%%%%%%%%%
remove_new_line_test() ->
    ?assertEqual("", parser:remove_new_line("\n")),
    ?assertEqual([], parser:remove_new_line([])),
    ?assertEqual("",parser:remove_new_line("")),
    ?assertEqual(" ",parser:remove_new_line(" ")),
    ?assertEqual("  ",parser:remove_new_line("  ")),
    ?assertEqual("Hej",parser:remove_new_line("Hej")),
    ?assertEqual("Hej ",parser:remove_new_line("Hej ")),
    ?assertEqual("Hej",parser:remove_new_line("Hej\n")),
    ?assertEqual("Hej",parser:remove_new_line("Hej\n" )),
    ?assertEqual("Hej\nVad görs?",parser:remove_new_line("Hej\nVad görs?")),
    LongStr1 = "En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng\n",
    LongStr2 = "En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng En lite längre sträng",
    ?assertEqual(LongStr2, parser:remove_new_line(LongStr1)).


%%%%%%%%%%%%%%%%%%% Tests for get_parts  %%%%%%%%%%%%%%%%%%%
get_parts_test() ->
    ?assertEqual({[],[],[]},parser:get_parts("")),
    ?assertEqual({[],[],[]},parser:get_parts(" ")),
    ?assertEqual({[],[],[]},parser:get_parts("  ")),
    ?assertEqual({[],[],[]},parser:get_parts("   ")),
    ?assertEqual({[],[],[]},parser:get_parts("    ")),
    ?assertEqual({"asd",[],[]},parser:get_parts("asd")),
    ?assertEqual({"tjena","tjofräs",[]},parser:get_parts("tjena tjofräs")),
    ?assertEqual({"tjena","tjofräs",[]},parser:get_parts("tjena tjofräs\n")),
    ?assertEqual({"hallå","hej",false},parser:get_parts("hallå hej köttbullar")),
    ?assertEqual({"hallå","hej",false},parser:get_parts("hallå hej köttbullar\n")),
    ?assertEqual({"hallå","hej",false},parser:get_parts("hallå hej köttbullar kladdkaka med grädde")),
    ?assertEqual({"asd","qwe",true},parser:get_parts("asd qwe private")),
    ?assertEqual({"hallaballo","tratt",false},parser:get_parts("hallaballo tratt false")),
    ?assertEqual({"asd","qwe",true},parser:get_parts("asd qwe private jag har kul")),
    ?assertEqual({"hallaballo","tratt",false},parser:get_parts("hallaballo tratt false jag är glad")),
    ?assertEqual({"massor","av",false},parser:get_parts("massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens massor av nonsens")).


%%%%%%%%%%%%%%%%%%% Tests for get_string %%%%%%%%%%%%%%%%%%%
get_string_test() ->
    S1 = parser:get_string("global du är en tratt",s1,[{"global",[{s3,"john3"},
        {s2,"john2"},{s1,"john1"}],false}]),
    S2 = parser:get_string("global du är en tratt",s2,[{"global",[{s3,"john3"},
        {s2,"john2"},{s1,"john1"}],false}]),
    S3 = parser:get_string("global du är en tratt",s3,[{"global",[{s3,"john3"},
        {s2,"john2"},{s1,"john1"}],false}]),
    S4 = parser:get_string("global Glass är gott",s1,[{"global",[{s3,"john3"},
        {s2,"john2"},{s1,"john1"}],true}]),
    S5 = parser:get_string("global Glass är gott",s2,[{"global",[{s3,"john3"},
        {s2,"john2"},{s1,"john1"}],true}]),
    S6 = parser:get_string("global Glass är gott",s3,[{"global",[{s3,"john3"},
        {s2,"john2"},{s1,"john1"}],true}]), 
    S7 = parser:get_string("global Stockholm är en förort till Uppsala",s1,
        [{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},
        {s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},
        {s5,"pelle"}],false}]),
    S8 = parser:get_string("global Stockholm är en förort till Uppsala",s2,
        [{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},
        {s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},
        {s5,"pelle"}],false}]),
    S9 = parser:get_string("global Stockholm är en förort till Uppsala",s3,
        [{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},
        {s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},
        {s5,"pelle"}],false}]),
    S10 = parser:get_string("global Stockholm är en förort till Uppsala",s4,
        [{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},
        {s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},
        {s5,"pelle"}],false}]),
    S11 = parser:get_string("global Stockholm är en förort till Uppsala",s5,
        [{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},
        {s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},
        {s5,"pelle"}],false}]),
    S12 = parser:get_string("global Stockholm är en förort till Uppsala",s6,
        [{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},
        {s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},
        {s5,"pelle"}],false}]),
    S13 = parser:get_string("coolroom Stockholm är en förort till Uppsala",s4,
        [{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},
        {s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},
        {s5,"pelle"}],false}]),
    S14 = parser:get_string("coolroom Stockholm är en förort till Uppsala",s5,
        [{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},
        {s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},
        {s5,"pelle"}],false}]),
    S15 = parser:get_string("coolroom Stockholm är en förort till Uppsala",s6,
        [{"global",[{s6,"maria"},{s5,"pelle"},{s4,"hampus"},{s3,"john"},
        {s2,"erik"},{s1,"pa"}],false},{"coolroom",[{s4,"hampus"},{s6,"maria"},
        {s5,"pelle"}],false}]),
    
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
