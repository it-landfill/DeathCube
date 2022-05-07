(* ::Package:: *)

BeginPackage["CubeAnimate`"]


(* ::Section:: *)
(*Definizione usage*)


RubikNext::usage = ""
RubikPrev::usage = ""
SetResolutionMoves::usage = ""
HasPrevMove::usage = ""
HasNextMove::usage = ""


(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Section:: *)
(*Import dei package utilizzati*)


AppendTo[$Path, NotebookDirectory[]];
Get["CubeAcquire.wl"]
Get["CubeVisualize.wl"]
Get["CubeCore.wl"]


(* ::Section:: *)
(*Definizione delle matrici di rotazione del cubo di Rubik (Grafiche)*)


(* ::Subsection::Closed:: *)
(*Rotazioni rispetto il piano XZ*)


(* Matrice di rotazione da applicare quando viene letto il comando "U" -> RotateU o "Di" -> RotateDi *)
MatriceRotazioneXZCW[ang_] =
	Module[{},
		{{Cos[-ang], 0, Sin[-ang]}, {0, 1, 0}, {Sin[ang], 0, Cos[-ang]}}
	];
(* Matrice di rotazione da applicare quando viene letto il comando "Ui" -> RotateUi o "D" -> RotateDi *)
MatriceRotazioneXZCC[ang_] =
	Module[{},
		{{Cos[ang], 0, Sin[ang]}, {0, 1, 0}, {Sin[-ang], 0, Cos[ang]}}
	];


(* ::Subsection::Closed:: *)
(*Rotazioni rispetto il piano YZ*)


(* Matrice di rotazione da applicare quando viene letto il comando "Li" -> RotateLi o "R" -> RotateR *)
MatriceRotazioneYZCW[ang_] =
	Module[{},
		{{1, 0, 0}, {0, Cos[-ang], Sin[ang]}, {0, Sin[-ang], Cos[-ang]}}
	];
	
(* Matrice di rotazione da applicare quando viene letto il comando "L" -> RotateL o "Ri" -> RotateRi *)
MatriceRotazioneYZCC[ang_] =
	Module[{},
		{{1, 0, 0}, {0, Cos[ang], Sin[-ang]}, {0, Sin[ang], Cos[ang]}}
	];



(* ::Subsection::Closed:: *)
(*Rotazioni rispetto il piano XY*)


(* Matrice di rotazione da applicare quando viene letto il comando "F" -> RotateF o "Bi" -> RotateBi *)
MatriceRotazioneXYCW[ang_] =
	Module[{},
		{{Cos[-ang], Sin[ang], 0}, {Sin[-ang], Cos[-ang], 0}, {0, 0, 1}}
	];
	
(* Matrice di rotazione da applicare quando viene letto il comando "Fi" -> RotateFi o "B" -> RotateB *)
MatriceRotazioneXYCC[ang_] =
	Module[{},
		{{Cos[ang], Sin[-ang], 0}, {Sin[ang], Cos[ang], 0}, {0, 0, 1}}
	];


(* ::Section:: *)
(*Definizione delle funzione di rotazione del cubo di Rubik (Grafiche)*)


(* 
	La funzione AnimateMove rivcevendo una mossa permette di visualizzare la rotazione di una delle facce del cubo di Rubik. La
	faccia che viene ruotata dipende dalla mossa indicata.
*)
AnimateMove[move_, fps_ : 1.5] := DynamicModule[
	(* Ad ogni richiamo della funzione la variabile ang viene reimpostata a valore di default 0 *)
	{ang = 0},
	(* 
		Attraverso l'Animator viene modificato il valore dell'angolo da applicare alla matrice di rotazione per la mossa. 
		Il valore dell'angolo per ogni mossa parte da 0 ed aumenta fino a raggiugenre il valore di Pi/2 portando l'angolo compiere 
		un'angolo di 90\[Degree]. 
		I parametri utilizzati nell'Animator:
			- AnimationRate, permette di modificare la velocit\[AGrave] con cui varia la variabile oggetto di Animator.
			  NOTE: Velocit\[AGrave] a mio avviso consigliata \[EGrave] 1.5 di Default e massimo 5.5 (Il valore deve essere mappato in un 
			  "lingauggio comprensibile" per l'utente tipo [*1, *1.5, *2, *2.5, ...] )
			- AnimationRepetitions, permette di indicare quante volte viene effettuata l'animazione prima di terminare.
			- AppearanceElements, permette di modificare i controlli. Utilizzando "None" i controlli non vengono visualizzati.
		TODO: Stampa necessaria per l'animazione (Non riesco a spiegarmi come mai)
	 *)
	Print[Animator[Dynamic[ang], {0, Pi/2}, AnimationRate -> fps, AnimationRepetitions -> 1, AppearanceElements -> None]];
	(* In base alla mossa da eseguire viene applicata una differente matrice di rotazione *)
	cube3D = Switch[move, 
		"U",  Generate3DCube[cube3DPieces, UP,    MatriceRotazioneXZCW[Dynamic[ang]]],
		"Ui", Generate3DCube[cube3DPieces, UP,    MatriceRotazioneXZCC[Dynamic[ang]]],
		"D",  Generate3DCube[cube3DPieces, DOWN,  MatriceRotazioneXZCC[Dynamic[ang]]],
		"Di", Generate3DCube[cube3DPieces, DOWN,  MatriceRotazioneXZCW[Dynamic[ang]]],
		"L",  Generate3DCube[cube3DPieces, LEFT,  MatriceRotazioneYZCC[Dynamic[ang]]],
		"Li", Generate3DCube[cube3DPieces, LEFT,  MatriceRotazioneYZCW[Dynamic[ang]]],
		"R",  Generate3DCube[cube3DPieces, RIGHT, MatriceRotazioneYZCW[Dynamic[ang]]],
		"Ri", Generate3DCube[cube3DPieces, RIGHT, MatriceRotazioneYZCC[Dynamic[ang]]],
		"F",  Generate3DCube[cube3DPieces, FRONT, MatriceRotazioneXYCW[Dynamic[ang]]],
		"Fi", Generate3DCube[cube3DPieces, FRONT, MatriceRotazioneXYCC[Dynamic[ang]]],
		"B",  Generate3DCube[cube3DPieces, BACK,  MatriceRotazioneXYCC[Dynamic[ang]]],
		"Bi", Generate3DCube[cube3DPieces, BACK,  MatriceRotazioneXYCW[Dynamic[ang]]],
		"X", Generate3DCube[cube3DPieces, None,  MatriceRotazioneYZCW[Dynamic[ang]]],
		"Xi", Generate3DCube[cube3DPieces, None,  MatriceRotazioneYZCC[Dynamic[ang]]],
		"Y", Generate3DCube[cube3DPieces, None,  MatriceRotazioneXZCW[Dynamic[ang]]],
		"Yi", Generate3DCube[cube3DPieces, None,  MatriceRotazioneXZCC[Dynamic[ang]]],
		"Z", Generate3DCube[cube3DPieces, None,  MatriceRotazioneXYCW[Dynamic[ang]]],
		"Zi", Generate3DCube[cube3DPieces, None,  MatriceRotazioneXYCC[Dynamic[ang]]]];
];


(* ::Section:: *)
(*Automatizzazione delle funzione di rotazione del cubo di Rubik*)


RubikMove[move_, fps_ : 1.5] := Module[
	{},
	AnimateMove[move, fps];
	cube3DPieces = Switch[move, 
		"U",  RotateU[cube3DPieces, False],
		"Ui", RotateUi[cube3DPieces, False],
		"D",  RotateD[cube3DPieces, False],
		"Di", RotateDi[cube3DPieces, False],
		"L",  RotateL[cube3DPieces, False],
		"Li", RotateLi[cube3DPieces, False],
		"R",  RotateR[cube3DPieces, False],
		"Ri", RotateRi[cube3DPieces, False],
		"F",  RotateF[cube3DPieces, False],
		"Fi", RotateFi[cube3DPieces, False],
		"B",  RotateB[cube3DPieces, False],
		"Bi", RotateBi[cube3DPieces, False],
		"X", RotateX[cube3DPieces, False],
		"Xi", RotateXi[cube3DPieces, False],
		"Y", RotateY[cube3DPieces, False],
		"Yi", RotateYi[cube3DPieces, False],
		"Z", RotateZ[cube3DPieces, False],
		"Zi", RotateZi[cube3DPieces, False]];
];


RubikNext[fps_ : 1.5] := Module[
	{},
	If[HasNextMove[],
		nextMove = nextMove + 1;
		move = resolutionMoves[[nextMove]];
		RubikMove[move, fps];
	, Print["Non ci sono mosse successive."]];
];


RubikPrev[fps_ : 1.5] := Module[
	{},
	If[HasPrevMove[],
		move = resolutionMoves[[nextMove]];
		If[StringLength[move] == 2, move = StringTake[move, 1];, move = StringInsert[move, "i", 2];];
		nextMove = nextMove - 1;
		RubikMove[move, fps];
	, Print["Non ci sono mosse precedenti."]];
];


(* ::Section:: *)
(*Funzioni utili a set e get*)


resolutionMoves = {};
nextMove = 0;


SetResolutionMoves[list_ : {}] := Module[
	{},
	resolutionMoves = {};
	nextMove = 0;
	
	resolutionMoves = list;
];


HasNextMove[]:= Module[
	{},
	Return[nextMove < Length[resolutionMoves]]
];


HasPrevMove[]:= Module[
	{},
	Return[!Equal[nextMove, 0]]
];


(* ::Section:: *)
(*Fine package*)


End[]

EndPackage[]
