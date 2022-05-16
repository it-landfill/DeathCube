(* ::Package:: *)

BeginPackage["CubeAnimate`"]


(* ::Section:: *)
(*Definizione usage*)


(* Metodi *)
SetResolutionMoves::usage = "Permette di caricare nella variabile privata resolutionMoves le mosse necessarie a risolvere il cubo di Rubik e resettare il contatore della mossa da eseguire."
RubikNext::usage = "Permette di eseguire la mossa M+1 per la risoluzione del cubo di Rubik. La funzione prima di eseguire la mossa verifica che ve ne siano."
RubikPrev::usage = "Permette di eseguire la mossa M-1 per la risoluzione del cubo di Rubik. La funzione prima di eseguire la mossa verifica che ve ne siano."
HasPrevMove::usage = "Permette di verificare se allo stato attuale vi sono ancora mosse da eseguire."
HasNextMove::usage = "Permette di verificare se allo stato attuale vi sono mosse precedenti."


(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Subsection:: *)
(*Import dei package utilizzati*)


AppendTo[$Path, NotebookDirectory[]];
Get["CubeCore.wl"]
Get["CubeAcquire.wl"]
Get["CubeVisualize.wl"]


(* ::Section:: *)
(*Definizione matrici di rotazione del cubo di Rubik 3D*)


(* ::Subsection:: *)
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


(* ::Subsection:: *)
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


(* ::Subsection:: *)
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
(*Definizione funzioni di rotazione del cubo di Rubik 3D*)


(* ::Subsection:: *)
(*Funzioni di controllo del  cubo di Rubik 3D*)


(* 
	La funzione RubikNext permette di eseguire la mossa M+1 per la risoluzione del cubo di Rubik. 
	La funzione prima di eseguire la mossa verifica che ve ne siano.
*)
RubikNext[fps_ : 1.5] := Module[
	{},
	If[HasNextMove[],
		nextMove = nextMove + 1;
		(* Estrazione della mossa da eseguire *)
		move = resolutionMoves[[nextMove]];
		(* Richiamo alla funzione di aggiornamento/animazione del cubo *)
		RubikMove[move, fps];
	, Print["Non ci sono mosse successive."]];
];


(* 
	La funzione RubikPrev permette di eseguire la mossa M-1 per la risoluzione del cubo di Rubik. 
	La funzione prima di eseguire la mossa verifica che ve ne siano.
*)
RubikPrev[fps_ : 1.5] := Module[
	{},
	If[HasPrevMove[],
		(* Estrazione della mossa da eseguire *)
		move = resolutionMoves[[nextMove]];
		nextMove = nextMove - 1;
		(* 
			In base all'elemento passato nell'If viene identificata una mossa che appartiene ad uno dei seguneti insiemi:
			{Ui, Di, Li, Ri, Fi, Bi, Xi, Yi, Zi} o {U, D, L, R, F, B, X, Y, Z}.
			Per eseguire la mossa M-1 \[EGrave] necessario invertire la mossa passata e per ottenere tale risultato viene o 
			estratta la testa o viene aggiunta la lettera "i" in coda.
		*)
		If[StringLength[move] == 2, move = StringTake[move, 1];, move = StringInsert[move, "i", 2];];
		(* Richiamo alla funzione di aggiornamento/animazione del cubo. *)
		RubikMove[move, fps];
	, Print["Non ci sono mosse precedenti."]];
];


(* ::Subsection:: *)
(*Aggiornamento e animazioni del cubo di Rubik 3D*)


(* 
	La funzione RubikMove permette di aggiornare il cubo di Rubik in base alla mossa passata in input e 
	richiamare la funzione per l'animazione del cubo di Rubik 3D.
*)
RubikMove[move_, fps_ : 1.5] := Module[
	{},
	(* Richiamo alla funzione di animazione del cubo. *)
	AnimateMove[move, fps];
	(* Aggiornamento del cubo in base alla mossa indicata. *)
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


(* 
	La funzione AnimateMove ricevendo una mossa permette di visualizzare la rotazione di una delle facce del cubo di Rubik. 
	La faccia che viene ruotata dipende dalla mossa indicata.
*)
AnimateMove[move_, fps_ : 1.5] := DynamicModule[
	(* Ad ogni richiamo della funzione la variabile ang viene reimpostata a valore di default 0 *)
	{ang = 0},
	(* 
		Attraverso l'Animator viene modificato il valore dell'angolo da applicare alla matrice di rotazione per la mossa. 
		Il valore dell'angolo per ogni mossa parte da 0 ed aumenta fino a raggiugenre il valore di Pi/2 portando l'angolo compiere 
		una rotazione di 90\[Degree]. 
		I parametri utilizzati nell'Animator:
			- AnimationRate, permette di modificare la velocit\[AGrave] con cui varia la variabile oggetto di Animator.
			  NOTE: Velocit\[AGrave] a mio avviso consigliata \[EGrave] 1.5 di Default e massimo 5.5 
			        (Il valore deve essere mappato in un  "lingauggio comprensibile" per l'utente tipo [*1, *1.5, *2, *2.5, ...])
			- AnimationRepetitions, permette di indicare quante volte viene effettuata l'animazione prima di terminare.
			- AppearanceElements, permette di modificare i controlli dell'Animator. Utilizzando il modificatore "None" i 
			  controlli non vengono visualizzati.
		
		La stampa risulta necessaria per visualizzare l'animazione. Senza essa la variabile Dynamic non aggiorna il suo valore, 
		impedendo di fatto l'animazione.
	 *)
	Print[Animator[Dynamic[ang], {0, Pi/2}, AnimationRate -> fps, AnimationRepetitions -> 1, AppearanceElements -> None]];
	(* 
		In base alla mossa da eseguire viene applicata una differente matrice di rotazione, senza modificare lo stato di cube3DPieces. 
		La modifica effettiva di cube3DPieces sar\[AGrave] effettuata dalla funzione RubikMove.
	*)
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
		"Zi", Generate3DCube[cube3DPieces, None,  MatriceRotazioneXYCC[Dynamic[ang]]]
	];
];


(* ::Section:: *)
(*Definizione funzioni di gestione delle mosse di risoluzione del cubo di Rubik 3D*)


(* Variabili *)
resolutionMoves = {};
nextMove = 0;


(* 
	La funzione SetResolutionMoves permette di caricare nella variabile privata resolutionMoves le mosse necessarie a risolvere il cubo di Rubik
	e resettare il contatore della mossa da eseguire.
*)
SetResolutionMoves[list_ : {}] := Module[
	{},
	(* Reset delle variabili private *)
	resolutionMoves = {};
	nextMove = 0;
	(* Caricamento delle mosse necessarie a risolvere il cubo di Rubik *)
	resolutionMoves = list;
];


(* 
	La funzione HasNextMove permette di verificare se allo stato attuale vi sono ancora mosse da eseguire.
*)
HasNextMove[]:= Module[
	{},
	Return[nextMove < Length[resolutionMoves]]
];


(* 
	La funzione HasPrevMove permette di verificare se allo stato attuale vi sono mosse precedenti.
*)
HasPrevMove[]:= Module[
	{},
	Return[!Equal[nextMove, 0]]
];


(* ::Section:: *)
(*Fine package*)


End[]

EndPackage[]
