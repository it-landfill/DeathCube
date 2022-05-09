(* ::Package:: *)

BeginPackage["CubeAcquisition`"]


(* ::Section:: *)
(*Definizione usage*)


VisualizeColorPickerBox::usage = ""
VisualizeInput2DCube::usage = ""


(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Section:: *)
(*Variabili*)


colors = {White, Green, Orange, Red, Blue, Yellow};
currentColor = Transparent;
cube;


(* ::Section:: *)
(*Generazione color picker*)


(* Genera i singoli bottoni con rettangolo di colore specificato *)
GenColorPickerColBox[coord_, color_] :=
	Module[{r},
		r = Rectangle[coord];
		r = Style[r, {color, EdgeForm[Thick]}];
		Button[r, currentColor = color]
	];


(* Genera il selettore di colori *)
GenColorPickerBox[colors] := Module[{colorBox = {}},
AppendTo[colorBox,GenColorPickerColBox[{0,0},colors[[1]]]];
AppendTo[colorBox,GenColorPickerColBox[{1,0},colors[[2]]]];
AppendTo[colorBox,GenColorPickerColBox[{2,0},colors[[3]]]];
AppendTo[colorBox,GenColorPickerColBox[{0,1},colors[[4]]]];
AppendTo[colorBox,GenColorPickerColBox[{1,1},colors[[5]]]];
AppendTo[colorBox,GenColorPickerColBox[{2,1},colors[[6]]]];
colorBox
];


(* Genero il visualizzatore del selettore *)
VisualizeColorPickerBox[] :=
	Module[{picker, pickerTitle, current, currentTitle, col1, col2},
		currentColor = Transparent;
		pickerTitle = "Color Picker";
		picker = GenColorPickerBox[colors];
		col1 = Column[{Style[pickerTitle, {25, Red, Bold}], Graphics[picker,
			 ImageSize -> Medium]}];
		currentTitle = "Current Color";
		current = {Dynamic[currentColor], EdgeForm[Thick], Rectangle[]};
		col2 = Column[{Style[currentTitle, {25, Red, Bold}], Graphics[current,
			 ImageSize -> Tiny]}];
		Print[Panel[Row[{col1, col2}]]]
	];


(* ::Section:: *)
(*Generazione del cubo 2D*)


ClickHandler[coord_] := Module[{}, 
(* Trovo l'idice dell'elemento con coordinate specificate nella lista cube *)
ind = Position[cube,coord][[1]][[1]];
(* Se il cubo \[EGrave] un centro, non lo modifico, altrimenti aggiorno il colore del centro *)
If[ContainsAny[{5,23,26,29,32,50},{ind}]==False,
cube[[ind]][[2]] = currentColor;
]
];


(* Genero un rettangolo di lato 1 con angolo in posizione coord e colore col *)
GenRect[rect_, col_] := Module[{},
Style[Rectangle[rect],{col,EdgeForm[Thick]}]
];


(* Genero il bottone wrapper al rettangolo. Quando premuto chiama ClickHandler con le coordinate del rettangolo *) 
GenBtn[rectStr_] := Module[{coord, col},
coord = rectStr[[1]];
col = rectStr[[2]];
(* Genero il rettangolo *)
r = GenRect[coord,col];
Button[r,ClickHandler[coord]]
];


(* Genero la lista di rettangoli composta da coordinate, colore *)
GenerateBaseCubeStruct[] := Module[{points = {}, defaultColor = Transparent},
	(* Genero i rettangoli del mio cubo aperto *)
	points=Join[points,Table[{{x,8},defaultColor},{x,3,5}]];
	points=Join[points,Table[{{x,7},defaultColor},{x,3,5}]];
	points=Join[points,Table[{{x,6},defaultColor},{x,3,5}]];
	points=Join[points,Table[{{x,5},defaultColor},{x,0,11}]];
	points=Join[points,Table[{{x,4},defaultColor},{x,0,11}]];
	points=Join[points,Table[{{x,3},defaultColor},{x,0,11}]];
	points=Join[points,Table[{{x,2},defaultColor},{x,3,5}]];
	points=Join[points,Table[{{x,1},defaultColor},{x,3,5}]];
	points=Join[points,Table[{{x,0},defaultColor},{x,3,5}]];

(* Imposto manualmente i colori dei centri:
 F = 26,
	B= 32,
	U = 5,
	D = 50,
	L = 23,
	R = 29	
 *)
points[[5]][[2]] =colors[[1]]; (* Up *)
points[[26]][[2]] =colors[[2]]; (* Front *)
points[[23]][[2]] =colors[[3]]; (* Left *)
points[[29]][[2]] =colors[[4]]; (* Right *)
points[[32]][[2]] =colors[[5]]; (* Back *)
points[[50]][[2]] =colors[[6]];(* Down *)
points
];


(* Visualizzo il cubo 2D di input *)
VisualizeInput2DCube[] := Module[{},
	(* Genero la struttura di base *)
	cube = GenerateBaseCubeStruct[];
	(* Stampo il cubo 2D *)
	Graphics[Dynamic[Map[GenBtn,cube]], Background->LightGray]
];


(* ::Section:: *)
(*Fine package*)


End[]

EndPackage[]
