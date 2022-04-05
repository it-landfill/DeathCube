(* ::Package:: *)

BeginPackage["CubeColors`"]


CharToColor::usage = ""


CubeStringToColorList::usage = ""


Begin["`Private`"]


(* ::Subsection:: *)
(*Map da lettere a colori*)


(* ::Subsubsection:: *)
(*Map delle facce di un cubo risolto a colore*)


cubeColors = <| "L" ->  Orange,"R" ->Red,"F"->Green, "B"-> Blue,"U"-> White, "D"->Yellow |>;


(* ::Subsubsection:: *)
(*Map da char a colore*)


(* ::Text:: *)
(*TODO: I colori devono essere parametrici (Opzione Daltonismo)*)


CharToColor[char_] := Switch[char,"W",White,"R", Red, "B", Blue, "O", Orange, "G", Green, "Y", Yellow, _, Black];


(* ::Subsubsection:: *)
(*Map della stringa a lista di colori*)


CubeStringToColorList[cube_] :=
	Table[CharToColor[c], {c, Characters[cube]}];


End[]

EndPackage[]
