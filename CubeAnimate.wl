(* ::Package:: *)

BeginPackage["CubeVisualize`"]


AnimateMove::usage = ""


cube3DPieces = ""
cube3D = ""


Begin["`Private`"]


AppendTo[$Path, NotebookDirectory[]];
Get["CubeAcquire.wl"]
Get["CubeVisualize.wl"]
Get["CubeCore.wl"]


(* Piano XZ *)
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


(* Piano YZ *)
	
(* Matrice di rotazione da applicare quando viene letto il comando "Li" -> RotateLi o "R" -> RotateR *)
MatriceRotazioneYZCW[ang_] =
	Module[{},
		{{1, 0, 0}, {0, Cos[ang], Sin[-ang]}, {0, Sin[ang], Cos[ang]}}
	];
	
(* Matrice di rotazione da applicare quando viene letto il comando "L" -> RotateL o "Ri" -> RotateRi *)
MatriceRotazioneYZCC[ang_] =
	Module[{},
		{{1, 0, 0}, {0, Cos[-ang], Sin[ang]}, {0, Sin[-ang], Cos[-ang]}}
	];



(* Piano XY *)

(* Matrice di rotazione da applicare quando viene letto il comando "F" -> RotateF o "Bi" -> RotateBi *)
MatriceRotazioneXYCW[ang_] =
	Module[{},
		{{Cos[ang], Sin[-ang], 0}, {Sin[ang], Cos[ang], 0}, {0, 0, 1}}
	];
	
(* Matrice di rotazione da applicare quando viene letto il comando "Fi" -> RotateFi o "B" -> RotateB *)
MatriceRotazioneXYCC[ang_] =
	Module[{},
		{{Cos[-ang], Sin[ang], 0}, {Sin[-ang], Cos[-ang], 0}, {0, 0, 1}}
	];


AnimateMove[move_,fps_] := Module[{},
	ang = 0;
	Print[Animator[Dynamic[ang], {0, Pi/2},fps, AnimationRepetitions->1]];
	matr = Switch[move, 
		"U", MatriceRotazioneXZCW[Dynamic[ang]],
		"Ui", MatriceRotazioneXZCC[Dynamic[ang]],
		"D", MatriceRotazioneXZCC[Dynamic[ang]],
		"Di", MatriceRotazioneXZCW[Dynamic[ang]],
		"L", MatriceRotazioneYZCC[Dynamic[ang]],
		"Li", MatriceRotazioneYZCW[Dynamic[ang]],
		"R", MatriceRotazioneYZCW[Dynamic[ang]],
		"Ri", MatriceRotazioneYZCC[Dynamic[ang]],
		"F", MatriceRotazioneXYCW[Dynamic[ang]],
		"Fi", MatriceRotazioneXYCC[Dynamic[ang]],
		"B", MatriceRotazioneXYCC[Dynamic[ang]],
		"Bi", MatriceRotazioneXYCW[Dynamic[ang]]];
	(* TODO: Switch della face selezionata in base alla mossa letta*)
	cube3D = Generate3DCube[cube3DPieces,UP,matr];
];


End[]

EndPackage[]
