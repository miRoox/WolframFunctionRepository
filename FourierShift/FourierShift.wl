(* ::Package:: *)

FourierShift[data_?ArrayQ]:=FourierShift[data,All]
FourierShift[data_?ArrayQ,dim_]:=Block[
  {n=ConstantArray[0,ArrayDepth@data]},
  n[[dim]]=Check[
      Ceiling[Dimensions[data]/2][[dim]],
      Return[data,Block]
  ];
  RotateLeft[data,n]
]



