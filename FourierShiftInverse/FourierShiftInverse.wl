(* ::Package:: *)

FourierShiftInverse[data_?ArrayQ]:=FourierShiftInverse[data,All]
FourierShiftInverse[data_?ArrayQ,dim_]:=Block[
  {n=ConstantArray[0,ArrayDepth@data]},
  n[[dim]]=Check[
      Ceiling[Dimensions[data]/2][[dim]],
      Return[data,Block]
  ];
  RotateRight[data,n]
]



