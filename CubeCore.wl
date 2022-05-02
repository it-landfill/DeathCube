(* ::Package:: *)

BeginPackage["CubeCore`"]


(* ::Section:: *)
(*Definizione usage*)


GetSolutionMoves::usage = ""
SetSolutionMoves::usage = ""
ResetSolutionMoves::usage = ""


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


ExtractFace::usage = ""
ExtractNotFace::usage = ""


cube3DPieces = ""
cube3D = ""


(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Section:: *)
(*Variabili private*)


solutionMoves = {};


(* ::Section:: *)
(*Getter e Setter*)


GetSolutionMoves[] := solutionMoves;
SetSolutionMoves[solMoves_] := Module[{},
	solutionMoves = solMoves;
];
ResetSolutionMoves[] := solutionMoves = {};


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


ExtractNotFace[cube_, face_] :=
	Module[{},
		Select[cube, Dot[#["pos"], face] <= 0&]
	];


(* ::Section:: *)
(*Definizione delle funzioni per la gestione del cubo di Rubik*)


(* ::Subsection:: *)
(*Operazioni ausiliarie*)


(* ::Text:: *)
(*Operazioni utilizzate nei notebook per operare nel cubo di Rubik.*)


(* ::Subsubsection:: *)
(*Rotazione singolo sotto cubo*)


(* 
	La funzione RotatePiece dato un singolo blocco (piece_) e una matrice di rotazione (matrix_) la funzione 
	applica la rotazione al blocco e ricalcola la posizione dei colori. 
*)
RotatePiece[piece_,matrix_] := Module[{},
	(* Posizione iniziale e colore del blocco. *)
	before = piece["pos"];
	col = piece["colors"];
	(* Applicazione della matrice di rotazione alle coordinate del blocco. *)
	pos = Dot[matrix, before];
	(* Calcolo della rotazione compiuta dal blocco. *)
	rot = pos-before;
	(* Gestisco caso nessuna rotazione (before == pos) (si ha nei blocchi centrali) *)
	If[SameQ[before,pos],Return[piece]];
	If[Count[rot,0]==2,rot +=Dot[matrix,rot]];
	(* A questo punto dovrei essere sicuro che in rot vi sia SOLO 1 elemento uguale a 0 *)
	If[Total[Mod[pos,1]]==0,
		(* Calcolo quale asse \[EGrave] 0 e swappo i colori negli altri 2 assi *)
		emptyAxe = Position[rot,0][[1]][[1]];
		swapIndex = Switch[emptyAxe,1,{2,3},2,{1,3},3,{1,2}];
		tmp =  col[[swapIndex[[1]]]];
		col[[swapIndex[[1]]]] = col[[swapIndex[[2]]]];
		col[[swapIndex[[2]]]] = tmp;
	,Null(*Non faccio nulla*)];

	(* Ricreo il piece e lo ritorno*)
	<|"pos"->pos,"colors" ->col|>
];


(* ::Subsubsection:: *)
(*Rotazione faccia*)


(* Applica la matrice di rotazione solo ai blocchi appartenenti alla faccia specificata. *)
RotateFace[cube_, face_, matrix_] :=
	Map[
		(* 
			Se il prodotto scalare tra le coordinate del blocco e il vettore faccia \[EGrave] maggiore di 0, allora il blocco 
			appartiene alla faccia e vi applico la rotazione.
		*)
		If[Dot[#["pos"], face] > 0,
			RotatePiece[#, matrix]
			,
			#
		]&
		,
		cube
	];


(* ::Subsubsection:: *)
(*Rotazione tutti sotto cubi*)


(* Applica la matrice di rotazione specificata a tutti i componenti del cubo. *)
RotateAllPieces[cube_, matrix_] :=
    Map[RotatePiece[#, matrix]&, cube];


(* ::Subsection:: *)
(*Operazioni standard*)


(* ::Text:: *)
(*Operazioni definite dalla guida del cubo di Rubik.*)


(* ::Subsubsection:: *)
(*Definizione delle funzioni di rotazioni per l'intero cubo di Rubik*)


(* 
	In base alla funzione richiamata, viene effettuata una rotazione completa del cubo lungo l'asse specificato.
*)
RotateX[cube_]:=Module[{},
	AppendTo[solutionMoves,"X"];
	RotateAllPieces[cube,ROTYZCW]
];
RotateXi[cube_]:=Module[{},
	AppendTo[solutionMoves,"Xi"];
	RotateAllPieces[cube,ROTYZCC]
];
RotateY[cube_]:=Module[{},
	AppendTo[solutionMoves,"Y"];
	RotateAllPieces[cube,ROTXZCW]
];
RotateYi[cube_]:=Module[{},
	AppendTo[solutionMoves,"Yi"];
	RotateAllPieces[cube,ROTXZCC]
];
RotateZ[cube_]:=Module[{},
	AppendTo[solutionMoves,"Z"];
	RotateAllPieces[cube,ROTXYCW]
];
RotateZi[cube_]:=Module[{},
	AppendTo[solutionMoves,"Zi"];
	RotateAllPieces[cube,ROTXYCC]
];


(* ::Subsubsection:: *)
(*Definizione delle funzioni di rotazioni per le facce del cubo di Rubik*)


(* 
	In base alla funzione richiamata, sul cubo viene applicata una differente rotazione ad una faccia 
*)

RotateL[cube_] := Module[{},
	AppendTo[solutionMoves,"L"];
	RotateFace[cube, LEFT, ROTYZCC]
];

RotateLi[cube_] := Module[{},
	AppendTo[solutionMoves,"Li"];
	RotateFace[cube, LEFT, ROTYZCW]
];

RotateR[cube_] := Module[{},
	AppendTo[solutionMoves,"R"];
	RotateFace[cube, RIGHT, ROTYZCW]
];

RotateRi[cube_] := Module[{},
	AppendTo[solutionMoves,"Ri"];
	RotateFace[cube, RIGHT, ROTYZCC]
];

RotateU[cube_] := Module[{},
	AppendTo[solutionMoves,"U"];
	RotateFace[cube, UP, ROTXZCW]
];

RotateUi[cube_] := Module[{},
	AppendTo[solutionMoves,"Ui"];
	RotateFace[cube, UP, ROTXZCC]
];

RotateD[cube_] := Module[{},
	AppendTo[solutionMoves,"D"];
	RotateFace[cube, DOWN, ROTXZCC]
];

RotateDi[cube_] := Module[{},
	AppendTo[solutionMoves,"Di"];
	RotateFace[cube, DOWN, ROTXZCW]
];

RotateF[cube_] := Module[{},
	AppendTo[solutionMoves,"F"];
	RotateFace[cube, FRONT, ROTXYCW]
];

RotateFi[cube_] := Module[{},
	AppendTo[solutionMoves,"Fi"];
	RotateFace[cube, FRONT, ROTXYCC]
];

RotateB[cube_] := Module[{},
	AppendTo[solutionMoves,"B"];
	RotateFace[cube, BACK, ROTXYCC]
];

RotateBi[cube_] := Module[{},
	AppendTo[solutionMoves,"Bi"];
	RotateFace[cube, BACK, ROTXYCW]
];


(* ::Section::Closed:: *)
(*Fine package*)


End[]

EndPackage[]
