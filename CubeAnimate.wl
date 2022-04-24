(* ::Package:: *)

BeginPackage["CubeVisualize`"]


(* ::Section::Closed:: *)
(*Definizione usage*)


AnimateMove::usage = ""


(* ::Section::Closed:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Import dei package utilizzati*)


AppendTo[$Path, NotebookDirectory[]];
Get["CubeAcquire.wl"]
Get["CubeVisualize.wl"]
Get["CubeCore.wl"]


(* ::Section::Closed:: *)
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
		"Bi", Generate3DCube[cube3DPieces, BACK,  MatriceRotazioneXYCW[Dynamic[ang]]]];
];


(* ::Section::Closed:: *)
(*Fine package*)


End[]

EndPackage[]
