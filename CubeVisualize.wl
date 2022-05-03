(* ::Package:: *)

BeginPackage["CubeVisualize`"]


(* ::Section:: *)
(*Definizione usage (TODO)*)


Generate2DCube::usage = ""


Visualize2DCube::usage = ""


Generate3DCube::usage = ""


Visualize3DCube::usage = ""


ControlsGlobal::usage = ""


ControlsRotation::usage = ""


ControlsMoves::usage = ""


GetGraphicPiece::usage = ""


vp = Options[Plot3D,ViewPoint][[1,2]];


(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


AppendTo[$Path, NotebookDirectory[]];
Get["CubeColors.wl"]
Get["CubeCore.wl"]


(* ::Section:: *)
(*Rappresentazione 2D*)


(* ::Subsection:: *)
(*Applico style a rettangoli*)


ApplyStyle2D[rect_, col_] := Module[{constStyleStr = {EdgeForm[Directive[Thick,Black]]}, styleStr},
	styleStr = Append[constStyleStr,col];
	Style[rect,styleStr]
];


(* ::Subsection:: *)
(*Genero lista di oggetti 2D*)


Generate2DCube[cubeString_] := Module[{points = {}, colorList},
	(* Genero i rettangoli del mio cubo aperto *)
	points=Join[points,Table[Rectangle[{x,8}],{x,3,5}]];
	points=Join[points,Table[Rectangle[{x,7}],{x,3,5}]];
	points=Join[points,Table[Rectangle[{x,6}],{x,3,5}]];
	points=Join[points,Table[Rectangle[{x,5}],{x,0,11}]];
	points=Join[points,Table[Rectangle[{x,4}],{x,0,11}]];
	points=Join[points,Table[Rectangle[{x,3}],{x,0,11}]];
	points=Join[points,Table[Rectangle[{x,2}],{x,3,5}]];
	points=Join[points,Table[Rectangle[{x,1}],{x,3,5}]];
	points=Join[points,Table[Rectangle[{x,0}],{x,3,5}]];
	
	(* Genero una lista di colori *)
	colorList = CubeStringToColorList[cubeString];

	(* Mappo i colori con i rettangoli generati *)
	MapThread[ApplyStyle2D,{points,colorList}]
];


(* ::Subsection:: *)
(*Visualizzo il cubo 2D*)


Visualize2DCube[cube_] := Module[
{cubeLabels = Style[{Text["F",{4.5,4.5}],Text["L",{1.5,4.5}],Text["R",{7.5,4.5}],Text["U",{4.5,7.5}],Text["D",{4.5,1.5}],Text["B",{10.5,4.5}]},15]},
Print[Graphics[{cube,cubeLabels}]];
];


(* ::Section:: *)
(*Rappresentazione cubo 3D*)


(* ::Subsection:: *)
(*Generazione componenti grafiche*)


(* ::Subsubsection:: *)
(*Generazione singoli sotto cubi*)


GetGraphicPiece[piece_, mat_:None] := Module[{pos, col},
	pos = piece["pos"];
	col = piece["colors"];
	polygons = {};

	(* Facce in YZ *)
	If[SameQ[col[[1]],None],
		(*Se \[EGrave] null non faccio nulla*)
		Null,
		offset = pos[[1]]/2;
		polyVert = {pos+{offset,-1/2,-1/2},pos+{offset,-1/2,1/2},pos+{offset,1/2,1/2},pos+{offset,1/2,-1/2}};
		If[SameQ[mat,None],
			Null,
			polyVert=Map[mat . #&,polyVert];
		];
		tmp = Polygon[polyVert];
		tmp = Style[tmp,{CharToColor[col[[1]]],EdgeForm[{Thick,Black}]}];
		AppendTo[polygons,tmp];
	];

	(* Facce in XZ *)
	If[SameQ[col[[2]],None],
		(*Se \[EGrave] null non faccio nulla*)
		Null,
		offset = pos[[2]]/2;
		polyVert = {pos+{-1/2,offset,-1/2},pos+{-1/2,offset,1/2},pos+{1/2,offset,1/2},pos+{1/2,offset,-1/2}};
		If[SameQ[mat,None],
			Null,
			polyVert=Map[mat . #&,polyVert];
		];
		tmp = Polygon[polyVert];
		tmp = Style[tmp,{CharToColor[col[[2]]],EdgeForm[{Thick,Black}]}];
		AppendTo[polygons,Style[tmp,CharToColor[col[[2]]]]];
	];

	(* Facce in XY *)
	If[SameQ[col[[3]],None],
		(*Se \[EGrave] null non faccio nulla*)
		Null,
		offset = pos[[3]]/2;polyVert = {pos+{-1/2,-1/2,offset},pos+{-1/2,1/2,offset},pos+{1/2,1/2,offset},pos+{1/2,-1/2,offset}};
		If[SameQ[mat,None],
			Null,
			polyVert=Map[mat . #&,polyVert];
		];
		tmp = Polygon[polyVert];
		tmp = Style[tmp,CharToColor[col[[3]]]];
		AppendTo[polygons,Style[tmp,{CharToColor[col[[3]]],EdgeForm[{Thick,Black}]}]];
	];
	
	polygons
];


(* ::Subsubsection:: *)
(*Generazione componenti grafiche*)


Generate3DCube[cube_,face_:None,matrix_:None] := Module[{f,nF,fC,nFC, ret},
	If[SameQ[face,None],
	ret = Map[GetGraphicPiece,cube];,
	f = ExtractFace[cube,face];
	nF = ExtractNotFace[cube, face];
	fC = Map[GetGraphicPiece[#,matrix]&,f];
	nFC = Map[GetGraphicPiece,nF];
	ret = Join[nFC,fC];
	];
	Return[ret];
];


(* ::Subsubsection:: *)
(*Rappresentazione cubo in ambiente 3D*)


Visualize3DCube[cube_] :=
	Module[{graphichOptions = {Lighting -> {{"Ambient", GrayLevel[1]}}, Axes
		 -> True, Ticks -> Automatic, AxesLabel -> {"x", "y", "z"},PlotRange->{{-2.2,2.2},{-2.2,2.2},{-2.2,2.2}},ViewPoint->Dynamic[vp]}},
		Print[Style[Graphics3D[cube, graphichOptions],RenderingOptions->{"3DRenderingOptions"->"Mesa"}]];
	];


(* ::Section:: *)
(*Fine Package*)


End[]

EndPackage[]
