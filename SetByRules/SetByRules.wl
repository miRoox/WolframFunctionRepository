(* ::Package:: *)

SetAttributes[quote,HoldAll];
release[expr_]:=expr/.{quote[e_]:>e}
replace[Rule[lhs_,rhs_]]:=quote@Set[lhs,rhs]
replace[RuleDelayed[lhs_,rhs_]]:=quote@SetDelayed[lhs,rhs]
replace[rule:{(_Rule|_RuleDelayed)}]:=replace@First[rule]
replace[rules:{(_Rule|_RuleDelayed)..}]:=(ResourceFunction["ResourceFunctionMessage"][SetByRules::conflict,rules];replace@First[rules])
replace[expr_]:=(ResourceFunction["ResourceFunctionMessage"][SetByRules::rules,expr];$Failed)

merge[rules:{(_Rule|_RuleDelayed)..},f_]:=f/@Gather[rules,patternEquivalentQ]
patternEquivalentQ[a_,b_]:=MatchQ[Internal`ComparePatterns[a,b],"Identical"|"Equivalent"]

Default[SetByRules]=First;
SetByRules::rules="`1` is neither a list of replacement rules nor a valid dispatch table.";
SetByRules::conflict="`1` contains conflict rules, only the first rule will be applied.";
SetByRules[rule:(_Rule|_RuleDelayed),_.]:=replace[rule]//release
SetByRules[rules:(_List|_Dispatch|_Association),f_.]:=replace/@merge[Flatten@Normal@rules,f]//release
SetByRules[expr_,_.]:=(ResourceFunction["ResourceFunctionMessage"][SetByRules::rules,expr];$Failed)



