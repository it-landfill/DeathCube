(* ::Package:: *)

BeginPackage["CubeVisualize`"]


(* ::Section:: *)
(*Definizione usage*)


(* Metodi *)
Generate2DCube::usage = ""
Visualize2DCube::usage = ""
Generate3DCube::usage = ""
Visualize3DCube::usage = ""
ControlsGlobal::usage = ""
ControlsRotation::usage = ""
ControlsMoves::usage = ""
GetGraphicPiece::usage = ""

(* Variabili *)
vp = Options[Plot3D, ViewPoint][[1,2]];


(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Subsection:: *)
(*Import dei package utilizzati*)


AppendTo[$Path, NotebookDirectory[]];
Get["CubeCore.wl"]
Get["CubeColors.wl"]


(* ::Section:: *)
(*Definizione del cubo di Rubik 2D*)


(* ::Subsection:: *)
(*Generazione componenti grafiche 2D*)


(* 
	La funzione ApplyStyle2D permette di applicare lo style desiderato al rettangolo passato in input.
*)
ApplyStyle2D[rect_, col_] := Module[
	{constStyleStr = {EdgeForm[Directive[Thick,Black]]}, styleStr},
	styleStr = Append[constStyleStr, col];
	Style[rect, styleStr]
];


(* 
	La funzione Generate2DCube permette di generare una lista di oggetti 2D.
*)
Generate2DCube[cubeString_] := Module[{points = {}, colorList},
	(* Generazione dei rettangoli del cubo 2D *)
	points = Join[points, Table[Rectangle[{x,8}],{x,3,5}]];
	points = Join[points, Table[Rectangle[{x,7}],{x,3,5}]];
	points = Join[points, Table[Rectangle[{x,6}],{x,3,5}]];
	points = Join[points, Table[Rectangle[{x,5}],{x,0,11}]];
	points = Join[points, Table[Rectangle[{x,4}],{x,0,11}]];
	points = Join[points, Table[Rectangle[{x,3}],{x,0,11}]];
	points = Join[points, Table[Rectangle[{x,2}],{x,3,5}]];
	points = Join[points, Table[Rectangle[{x,1}],{x,3,5}]];
	points = Join[points, Table[Rectangle[{x,0}],{x,3,5}]];
	(* Generazione della lista colori *)
	colorList = CubeStringToColorList[cubeString];
	(* Map tra i colori ed i rettangoli generati *)
	MapThread[ApplyStyle2D, {points,colorList}]
];


(* ::Subsection:: *)
(*Visualizzazione del cubo di Rubik 2D*)


(* 
	La funzione Visualize2DCube permette di visualizzare il cubo 2D.
*)
Visualize2DCube[cube_] := Module[
	(* 
		Generazione delle label e delle loro coordinate che saranno inserite nel cubo 2D in modo da aiutare l'utente 
		a comprendere la corrispondenza tra faccia e nominativo per le mosse.
	*)
	{cubeLabels = Style[{
		Text["F",{4.5,4.5}], 
		Text["L",{1.5,4.5}], 
		Text["R",{7.5,4.5}], 
		Text["U",{4.5,7.5}],
		Text["D",{4.5,1.5}],
		Text["B",{10.5,4.5}]},
		15]},
	(* Visualizzazione del cubo 2D *)
	Print[Graphics[{cube,cubeLabels}]];
];


(* ::Section:: *)
(*Definizione del cubo di Rubik 3D*)


(* ::Subsection:: *)
(*Generazione componenti grafiche 3D*)


(* 
	La funzione GetGraphicPiece permette di generare i singoli sotto cubi del cubo di Rubik 3D.
*)
GetGraphicPiece[piece_, mat_:None] := Module[
	{pos, col},
	pos = piece["pos"];
	col = piece["colors"];
	polygons = {};
	
	(* Generazione dei poligoni dei sotto cubi sul piano YZ *)
	If[SameQ[col[[1]],None],
		(*Se \[EGrave] null non faccio nulla*)
		Null,
		offset = pos[[1]]/2;
		(* Generazione dei vertici che compongono ogni poligono *)
		polyVert = {pos+{offset,-1/2,-1/2},pos+{offset,-1/2,1/2},pos+{offset,1/2,1/2},pos+{offset,1/2,-1/2}};
		If[SameQ[mat,None],
			Null,
			polyVert=Map[mat . #&,polyVert];
		];
		(* Assegnamento a tmp dei poligoni creati e dello stile grafico (colore e bordo) da applicare *)
		tmp = Polygon[polyVert];
		tmp = Style[tmp,{CharToColor[col[[1]]],EdgeForm[{Thick,Black}]}];
		AppendTo[polygons, tmp];
	];
	
	(* Generazione dei poligoni dei sotto cubi sul piano XZ *)
	If[SameQ[col[[2]],None],
		(*Se \[EGrave] null non faccio nulla*)
		Null,
		offset = pos[[2]]/2;
		(* Generazione dei vertici che compongono ogni poligono *)
		polyVert = {pos+{-1/2,offset,-1/2},pos+{-1/2,offset,1/2},pos+{1/2,offset,1/2},pos+{1/2,offset,-1/2}};
		If[SameQ[mat,None],
			Null,
			polyVert=Map[mat . #&,polyVert];
		];
		(* Assegnamento a tmp dei poligoni creati e dello stile grafico (colore e bordo) da applicare *)
		tmp = Polygon[polyVert];
		tmp = Style[tmp, {CharToColor[col[[2]]],EdgeForm[{Thick,Black}]}];
		AppendTo[polygons, Style[tmp, CharToColor[col[[2]]]]];
	];
	
	(* Generazione dei poligoni dei sotto cubi sul piano XY *)
	If[SameQ[col[[3]],None],
		(*Se \[EGrave] null non faccio nulla*)
		Null,
		offset = pos[[3]]/2;
		(* Generazione dei vertici che compongono ogni poligono *)
		polyVert = {pos+{-1/2,-1/2,offset},pos+{-1/2,1/2,offset},pos+{1/2,1/2,offset},pos+{1/2,-1/2,offset}};
		If[SameQ[mat,None],
			Null,
			polyVert=Map[mat . #&,polyVert];
		];
		(* Assegnamento a tmp dei poligoni creati e dello stile grafico (colore e bordo) da applicare *)
		tmp = Polygon[polyVert];
		tmp = Style[tmp,CharToColor[col[[3]]]];
		AppendTo[polygons, Style[tmp,{CharToColor[col[[3]]],EdgeForm[{Thick,Black}]}]];
	];
	
	polygons
];


(* 
	La funzione Generate3DCube ricevendo il cubo, l'indicatore della faccia e una matrice permette di applicare 
	la matrice ai cubi appartenenti alla faccia selezionata.
*)
Generate3DCube[cube_, face_:None, matrix_:None] := Module[
	{f, nF, fC, nFC},
	(* 
		In caso di rotazione rispetto "X, Xi, Y, Yi, Z, Zi" non viene selezionata una singola faccia, ma \[EGrave] necessario ruotare 
		tutti si sotto cubi. 
	*)
	If[SameQ[face, None],
		(* 
			Se non \[EGrave] associata alcuna matrice di rotazione viene ritornato il cubo attuale, altrimenti viene tornato il cubo a 
			cui \[EGrave] stata applicata la matrice di rotazione passata in input 
		*)
		If[SameQ[matrix, None],
			Return[Map[GetGraphicPiece, cube]];,
			Return[Map[GetGraphicPiece[#,matrix]&, cube]];
		];,
		(* 
			Attraverso le funzioni ExtractFace e ExtractNotFace vengono rispettivamente selezionati i sotto cubi a cui deve 
			essere applicata la matrice di rotazione e i sotto cubi che non necessitano di cambiamento. 
		*)
		f = ExtractFace[cube, face];
		nF = ExtractNotFace[cube, face];
		(* Applicazione della matrice di rotazione passata in input ai sotto cubi selezionati in precedenza. *)
		fC = Map[GetGraphicPiece[#, matrix]&,f];
		(* Estrazione dei sotto cubi che non hanno subito modifiche *)
		nFC = Map[GetGraphicPiece, nF];
		(* Generazione del nuovo cubo aggiornato dalla rotazione *)
		Return[Join[nFC, fC]];
	];
];


(* ::Subsection:: *)
(*Visualizzazione del cubo di Rubik 3D*)


(* 
	La funzione Visualize2DCube permette di visualizzare il cubo 3D.
*)
Visualize3DCube[cube_] := Module[
	(* 
		Generazione delle opzioni grafiche da applicare alla visualizzazione del cubo 3D.
		I parametri utilizzati nella graphichOptions:
			- Lighting, permette di modificare l'illuminazione dell'oggetto 3D.
			- Axes, permette di visualizzare gli assi utilizzati dall'oggetto 3D.
			- Ticks, permette di visualizzare i valori appartenenti agli assi dell'oggetto 3D.
			- AxesLabel, permette di inserire una label per i differenti assi.
			- PlotRange, permette di modificare (forzare) il range usato dagli assi.
			- ViewPoint, permette di impostare con quale viewpoint viene visualizzato di default l'oggeto 3D.
	*)
	{graphichOptions = {
		Lighting -> {{"Ambient", GrayLevel[1]}}, 
		Axes -> True, 
		Ticks -> Automatic, 
		AxesLabel -> {"x", "y", "z"}, 
		PlotRange->{{-2.2,2.2},{-2.2,2.2},{-2.2,2.2}}, 
		ViewPoint->Dynamic[vp]}},
		(* Visualizzazione del cubo 3D *)
		Print[Style[Graphics3D[cube, graphichOptions], RenderingOptions->{"3DRenderingOptions"->"Mesa"}]];
	];


(* ::Section:: *)
(*Fine Package*)


End[]

EndPackage[]
