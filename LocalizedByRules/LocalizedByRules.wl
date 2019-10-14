(* ::Package:: *)

replaceRules[rules:{(_Rule|_RuleDelayed)...}]:=HoldComplete[rules]/.{
  Rule[lhs_Symbol,rhs_]:>Set[lhs,rhs],
  RuleDelayed[lhs_Symbol,rhs_]:>SetDelayed[lhs,rhs],
  Rule[Verbatim[HoldPattern][lhs_Symbol],rhs_]:>Set[lhs,rhs],
  RuleDelayed[Verbatim[HoldPattern][lhs_Symbol],rhs_]:>SetDelayed[lhs,rhs],
  rule:(Rule|RuleDelayed)[lhs_,_]:>Block[{},ResourceFunction["ResourceFunctionMessage"][LocalizedByRules::lvrule,lhs,rule];Nothing/;True]
}//DeleteCases[#,Nothing,Infinity]&

SetAttributes[LocalizedByRules,HoldRest];
Options[LocalizedByRules]={Method->With};
LocalizedByRules::lvrule="`1` in the rule `2` is not a symbol, such rule will be ignored";
LocalizedByRules[rules:{(_Rule|_RuleDelayed)...},body_,OptionsPattern[]]:= 
  With[{vars=Unevaluated@@replaceRules[rules],scope=OptionValue[Method]},
    scope[vars,body] /;FreeQ[vars,replaceRules]
  ]
LocalizedByRules[rule:(_Rule|_RuleDelayed),body_,o:OptionsPattern[]]:=
  LocalizedByRules[{rule},body,o]
LocalizedByRules[rules:{__List},body_,o:OptionsPattern[]]:=
  LocalizedByRules[#,body,o]&/@rules
LocalizedByRules[rules:(_Dispatch|_Association),body_,o:OptionsPattern[]]:=
  LocalizedByRules[Normal@rules,body,o]



