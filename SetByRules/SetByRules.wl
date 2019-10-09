(* ::Package:: *)

iSetByRules[rules:{(_Rule|_RuleDelayed)...}]:=HoldComplete[rules]/.{Rule->Set,RuleDelayed->SetDelayed}

SetByRules[rule:(_Rule|_RuleDelayed)]:=First@SetByRules[{rule}]
SetByRules[rules:{(_Rule|_RuleDelayed)...}]:=Identity@@iSetByRules[rules]
SetByRules[rules:{__List}]:=SetByRules/@rules
SetByRules[rules:(_Dispatch|_Association)]:=SetByRules[Normal@rules]

SetByRules/:(scope:With|Block|Module)[SetByRules[rules_/;MatchQ[rules,{(_Rule|_RuleDelayed)...}]],body_]:= 
  With[{vars=Unevaluated@@iSetByRules[rules]},
    scope[vars,body] /;FreeQ[vars,iSetByRules]
  ]
SetByRules/:(scope:With|Block|Module)[SetByRules[rule_/;MatchQ[rule,_Rule|_RuleDelayed]],body_]:=
  scope[SetByRules[{rule}],body]
SetByRules/:(scope:With|Block|Module)[SetByRules[rules_/;MatchQ[rules,{__List}]],body_]:=
  scope[SetByRules[#],body]&/@rules
SetByRules/:(scope:With|Block|Module)[SetByRules[rules_/;MatchQ[rules,_Dispatch|_Association]],body_]:=
  scope[SetByRules[Normal@rules],body]



