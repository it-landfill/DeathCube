(* ::Package:: *)

BeginPackage["CubeCore`"]


(* ::Section:: *)
(*Definizione usage*)


GetPieces::usage = ""
GetFaces::usage = ""
GetEdges::usage = ""
GetCorners::usage = ""


RotatePiece::usage = ""
RotateFace::usage = ""
RotateAllPieces::usage = ""


RotateX::usage = ""
RotateXi::usage = ""
RotateY::usage = ""
RotateYi::usage = ""
RotateZ::usage = ""
RotateZi::usage = ""


RotateL::usage = ""
RotateLi::usage = ""
RotateR::usage = ""
RotateRi::usage = ""
RotateU::usage = ""
RotateUi::usage = ""
RotateD::usage = ""
RotateDi::usage = ""
RotateF::usage = ""
RotateFi::usage = ""
RotateB::usage = ""
RotateBi::usage = ""


XAXIS::usage = ""
YAXIS::usage = ""
ZAXIS::usage = ""
RIGHT::usage = ""
LEFT::usage = ""
UP::usage = ""
DOWN::usage = ""
FRONT::usage = ""
BACK::usage = ""


(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Section:: *)
(*Definizione vettori e matrici*)


(* ::Subsection:: *)
(*Assi*)


XAXIS={1,0,0};
YAXIS={0,1,0};
ZAXIS={0,0,1};


(* ::Subsection:: *)
(*Facce*)


RIGHT={1,0,0};
LEFT={-1,0,0};
UP={0,1,0};
DOWN={0,-1,0};
FRONT={0,0,1};
BACK={0,0,-1};


(* ::Subsection:: *)
(*Rotazioni*)


(* ::Subsubsection:: *)
(*Piano XY*)


ROTXYCW={{0,1,0},{-1,0,0},{0,0,1}};
ROTXYCC={{0,-1,0},{1,0,0},{0,0,1}};


(* ::Subsubsection:: *)
(*Piano XZ*)


ROTXZCW={{0,0,-1},{0,1,0},{1,0,0}};
ROTXZCC={{0,0,1},{0,1,0},{-1,0,0}};


(* ::Subsubsection:: *)
(*Piano YZ*)


ROTYZCW={{1,0,0},{0,0,1},{0,-1,0}};
ROTYZCC={{1,0,0},{0,0,-1},{0,1,0}};


(* ::Section:: *)
(*Generazione componenti*)


(* ::Subsection:: *)
(*Generazione facce (cubi con 1 colore)*)


GetFaces[cubeStr_] :=
	Module[{},
		{<|"pos" -> RIGHT, "colors" -> {StringPart[cubeStr, 29], None, None
			}|>, <|"pos" -> LEFT, "colors" -> {StringPart[cubeStr, 23], None, None
			}|>, <|"pos" -> UP, "colors" -> {None, StringPart[cubeStr, 5], None}|>,
			 <|"pos" -> DOWN, "colors" -> {None, StringPart[cubeStr, 50], None}|>,
			 <|"pos" -> FRONT, "colors" -> {None, None, StringPart[cubeStr, 26]}|>,
			 <|"pos" -> BACK, "colors" -> {None, None, StringPart[cubeStr, 32]}|>
			}
	];


(* ::Subsection:: *)
(*Generazione lati (cubi con 2 colori)*)


GetEdges[cubeStr_] :=
	Module[{},
		{<|"pos" -> RIGHT + UP, "colors" -> {StringPart[cubeStr, 17], StringPart[
			cubeStr, 6], None}|>, <|"pos" -> RIGHT + DOWN, "colors" -> {StringPart[
			cubeStr, 41], StringPart[cubeStr, 51], None}|>, <|"pos" -> RIGHT + FRONT,
			 "colors" -> {StringPart[cubeStr, 28], None, StringPart[cubeStr, 27]}
			|>, <|"pos" -> RIGHT + BACK, "colors" -> {StringPart[cubeStr, 30], None,
			 StringPart[cubeStr, 31]}|>, <|"pos" -> LEFT + UP, "colors" -> {StringPart[
			cubeStr, 11], StringPart[cubeStr, 4], None}|>, <|"pos" -> LEFT + DOWN,
			 "colors" -> {StringPart[cubeStr, 35], StringPart[cubeStr, 49], None}
			|>, <|"pos" -> LEFT + FRONT, "colors" -> {StringPart[cubeStr, 24], None,
			 StringPart[cubeStr, 25]}|>, <|"pos" -> LEFT + BACK, "colors" -> {StringPart[
			cubeStr, 22], None, StringPart[cubeStr, 33]}|>, <|"pos" -> UP + FRONT,
			 "colors" -> {None, StringPart[cubeStr, 8], StringPart[cubeStr, 14]}|>,
			 <|"pos" -> UP + BACK, "colors" -> {None, StringPart[cubeStr, 2], StringPart[
			cubeStr, 20]}|>, <|"pos" -> DOWN + FRONT, "colors" -> {None, StringPart[
			cubeStr, 47], StringPart[cubeStr, 38]}|>, <|"pos" -> DOWN + BACK, "colors"
			 -> {None, StringPart[cubeStr, 53], StringPart[cubeStr, 44]}|>}
	];


(* ::Subsection:: *)
(*Generazione angoli  (cubi con 3 colori)*)


GetCorners[cubeStr_] :=
	Module[{},
		{<|"pos" -> RIGHT + UP + FRONT, "colors" -> {StringPart[cubeStr, 16
			], StringPart[cubeStr, 9], StringPart[cubeStr, 15]}|>, <|"pos" -> RIGHT
			 + UP + BACK, "colors" -> {StringPart[cubeStr, 18], StringPart[cubeStr,
			 3], StringPart[cubeStr, 19]}|>, <|"pos" -> RIGHT + DOWN + FRONT, "colors"
			 -> {StringPart[cubeStr, 40], StringPart[cubeStr, 48], StringPart[cubeStr,
			 39]}|>, <|"pos" -> RIGHT + DOWN + BACK, "colors" -> {StringPart[cubeStr,
			 42], StringPart[cubeStr, 54], StringPart[cubeStr, 43]}|>, <|"pos" ->
			 LEFT + UP + FRONT, "colors" -> {StringPart[cubeStr, 12], StringPart[
			cubeStr, 7], StringPart[cubeStr, 13]}|>, <|"pos" -> LEFT + UP + BACK,
			 "colors" -> {StringPart[cubeStr, 10], StringPart[cubeStr, 1], StringPart[
			cubeStr, 21]}|>, <|"pos" -> LEFT + DOWN + FRONT, "colors" -> {StringPart[
			cubeStr, 36], StringPart[cubeStr, 46], StringPart[cubeStr, 37]}|>, <|
			"pos" -> LEFT + DOWN + BACK, "colors" -> {StringPart[cubeStr, 34], StringPart[
			cubeStr, 52], StringPart[cubeStr, 45]}|>}
	];


(* ::Subsection:: *)
(*Generazione cubo completo (centri + lati + angoli)*)


GetPieces[cubeStr_] :=
	Module[{},
		Join[GetFaces[cubeStr], GetEdges[cubeStr], GetCorners[cubeStr]]
	];


(* ::Section:: *)
(*Estrazione componenti*)


(* ::Subsection:: *)
(*Estrazione faccia*)


ExtractFace[cube_, face_] :=
	Module[{},
		Select[cube, Dot[#["pos"], face] > 0&]
	];


(* ::Subsection:: *)
(*Estrazione slice (TODO)*)


(* ::Text:: *)
(*TODO*)


(* ::Section:: *)
(*Operazioni sul cubo*)


(* ::Subsection:: *)
(*Operazioni ausiliarie*)


(* ::Subsubsection:: *)
(*Rotazione singolo sotto cubo*)


RotatePiece[piece_,matrix_] := Module[{},
	before = piece["pos"];
	col = piece["colors"];
	(* Eseguo prodotto scalare *)
	pos = Dot[matrix,before];
	rot = pos-before;
	(*Gesisco caso nessuna rotazione (before == pos) (si ha nei blocchi centrali) *)
	If[SameQ[before,pos],Return[piece]];
	If[Count[rot,0]==2,rot +=Dot[matrix,rot]];
	(* A questo punto dovrei essere sicuro che in rot vi sia SOLO 1 elemento uguale a 0 *)
	(* Calcolo quale asse \[EGrave] 0 e swappo i colori negli altri 2 assi *)
	emptyAxe = Position[rot,0][[1]][[1]];
	swapIndex = Switch[emptyAxe,1,{2,3},2,{1,3},3,{1,2}];
	tmp =  col[[swapIndex[[1]]]];
	col[[swapIndex[[1]]]] = col[[swapIndex[[2]]]];
	col[[swapIndex[[2]]]] = tmp;

	(* Ricreo il piece e lo ritorno*)
	<|"pos"->pos,"colors" ->col|>
];


(* ::Subsubsection:: *)
(*Rotazione faccia*)


RotateFace[cube_, face_, matrix_] :=
	Map[
		If[Dot[#["pos"], face] > 0,
			RotatePiece[#, matrix]
			,
			#
		]&
		,
		cube
	];


(* ::Subsubsection:: *)
(*TODO: RotateFace con angolo custom*)


(* ::Subsubsection:: *)
(*Rotazione tutti sotto cubi*)


RotateAllPieces[cube_,matrix_] := Map[RotatePiece[#,matrix]&,cube];


(* ::Subsection:: *)
(*Operazioni standard*)


(* ::Subsubsection:: *)
(*Rotazioni intero cubo*)


RotateX[cube_]:=RotateAllPieces[cube,ROTYZCW];
RotateXi[cube_]:=RotateAllPieces[cube,ROTYZCC];
RotateY[cube_]:=RotateAllPieces[cube,ROTXZCW];
RotateYi[cube_]:=RotateAllPieces[cube,ROTXZCC];
RotateZ[cube_]:=RotateAllPieces[cube,ROTXYCW];
RotateZi[cube_]:=RotateAllPieces[cube,ROTXYCC];


(* ::Subsubsection:: *)
(*Rotazioni facce*)


RotateL[cube_] :=
	RotateFace[cube, LEFT, ROTYZCC];

RotateLi[cube_] :=
	RotateFace[cube, LEFT, ROTYZCW];

RotateR[cube_] :=
	RotateFace[cube, RIGHT, ROTYZCW];

RotateRi[cube_] :=
	RotateFace[cube, RIGHT, ROTYZCC];

RotateU[cube_] :=
	RotateFace[cube, UP, ROTXZCW];

RotateUi[cube_] :=
	RotateFace[cube, UP, ROTXZCC];

RotateD[cube_] :=
	RotateFace[cube, DOWN, ROTXZCC];

RotateDi[cube_] :=
	RotateFace[cube, DOWN, ROTXZCW];

RotateF[cube_] :=
	RotateFace[cube, FRONT, ROTXYCW];

RotateFi[cube_] :=
	RotateFace[cube, FRONT, ROTXYCC];

RotateB[cube_] :=
	RotateFace[cube, BACK, ROTXYCC];

RotateBi[cube_] :=
	RotateFace[cube, BACK, ROTXYCW];


(* ::Subsubsection:: *)
(*Rotazioni slice (TODO)*)


(* ::Text:: *)
(*TODO*)


(* ::Section:: *)
(*Fine package*)


End[]

EndPackage[]
