(* ::Package:: *)

BeginPackage["CubeColors`"]


CharToColor::usage = ""
ColorToChar::usage = ""


CubeStringToColorList::usage = ""


Begin["`Private`"]


(* ::Section:: *)
(*Definizione colori*)


colorScheme1 = <| "R" -> Red, "O" -> Orange, "G" -> Green, "B"-> Blue, "W" -> White, "Y" -> Yellow, "T" -> Transparent|>;
currentColorScheme = colorScheme1;


(* ::Section:: *)
(*Map tra lettere e colori*)


(* ::Subsubsection:: *)
(*Map delle facce di un cubo risolto a colore*)


cubeColors = <| "L" ->  Orange,"R" ->Red,"F"->Green, "B"-> Blue,"U"-> White, "D"->Yellow |>;


(* ::Subsubsection:: *)
(*Map da char a colore*)


(* ::Text:: *)
(*TODO: I colori devono essere parametrici (Opzione Daltonismo)*)


CharToColor[char_] := Module[{},
	Return[currentColorScheme[[char]]]
];


ColorToChar[col_] := Module[{},
	Return[Position[currentColorScheme,col][[1,1,1]]]
];


(* ::Subsubsection:: *)
(*Map della stringa a lista di colori*)


CubeStringToColorList[cube_] :=
	Table[CharToColor[c], {c, Characters[cube]}];


End[]

EndPackage[]
