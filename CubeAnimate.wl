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
		{{1, 0, 0}, {0, Cos[-ang], Sin[ang]}, {0, Sin[-ang], Cos[-ang]}}
	];
	
(* Matrice di rotazione da applicare quando viene letto il comando "L" -> RotateL o "Ri" -> RotateRi *)
MatriceRotazioneYZCC[ang_] =
	Module[{},
		{{1, 0, 0}, {0, Cos[ang], Sin[-ang]}, {0, Sin[ang], Cos[ang]}}
	];



(* Piano XY *)

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


AnimateMove[move_,fps_] := DynamicModule[{ang = 0},
	Print[Animator[Dynamic[ang], {0, Pi/2}, fps, AnimationRepetitions->1]];
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


End[]

EndPackage[]
