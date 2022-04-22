(* ::Package:: *)

BeginPackage["CubeVisualize`"]


MatriceRotazioneUpCW::usage = ""


AnimateMove::usage = ""


cube3DPieces = ""
cube3D = ""


Begin["`Private`"]


AppendTo[$Path, NotebookDirectory[]];
Get["CubeAcquire.wl"]
Get["CubeVisualize.wl"]
Get["CubeCore.wl"]


MatriceRotazioneUpCW[ang_] =
	Module[{},
		{{Cos[-ang], 0, Sin[-ang]}, {0, 1, 0}, {Sin[ang], 0, Cos[-ang]}}
	];


AnimateMove[move_,fps_] := Module[{},
	ang = 0;
	Print[Animator[Dynamic[ang], { 0, Pi/2},fps, AnimationRepetitions->1]];
	matr=MatriceRotazioneUpCW[Dynamic[ang]];
	cube3D = Generate3DCube[cube3DPieces,UP,matr];
];


End[]

EndPackage[]
