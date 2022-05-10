(* ::Package:: *)

BeginPackage["CubeAcquire`"]


sampleStrings::usage = ""
VerifyCubeColorNumber::usage = ""
FindMatchingCubeCode::usage = ""


VisualizeColorPickerBox::usage = ""
VisualizeInput2DCube::usage = ""
cube;


(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Section:: *)
(*Variabili*)


colors = {White, Green, Orange, Red, Blue, Yellow};
currentColor = Transparent;
(* TODO: Replace con blind mode *)


(* ::Section:: *)
(*Acquisizione tramite stringa*)


(* ::Subsection:: *)
(*Stringhe di esempio*)


sampleStrings = <|"solved" -> "WWWWWWWWWOOOGGGRRRBBBOOOGGGRRRBBBOOOGGGRRRBBBYYYYYYYYY",
	 "start" -> "WOWBYRBYGOOYORWOWRBYBRORGBGWRBYGBOGGRYGWWBYOYYGROWBGWR"|>


(* ::Subsection:: *)
(*Verifica validit\[AGrave] della stringa*)


VerifyCubeColorNumber[cube_] :=
	Module[{characterList},
		characterList = CharacterCounts[cube];
		Max[characterList] == Min[characterList] == 9
	];


(* ::Section:: *)
(*Acquisizione grafica*)


(* ::Subsection:: *)
(*Data una lista di colori, ritorno il blocco corrispondente*)


(* ::Text:: *)
(*TODO: Mappare indici a quelli definiti su funzioni Get**)
(*TODO2: Mi serve a qualcosa?*)


FindMatchingCubeCode[cols_] := Module[{},
   (*Angoli*)
   corners = Association[{
		Sort[{White, Blue, Red}] -> 1,
		Sort[{White, Green, Red}] -> 2,
		Sort[{White, Blue, Orange}] -> 3,
		Sort[{White, Green, Orange}] -> 4,
		Sort[{Blue, Red, Yellow}] -> 5,
		Sort[{Green, Red, Yellow}] -> 6,
		Sort[{Yellow, Orange, Green}] -> 7,
		Sort[{Blue, Orange, Yellow}] -> 8
   }];
	(*Lati*)
	sides = Association[{
		Sort[{White, Blue}] -> 9,
		Sort[{White, Red}] -> 10,
		Sort[{White, Green}] -> 11,
		Sort[{White, Orange}] -> 12,
		Sort[{Red, Blue}] -> 13,
		Sort[{Red, Green}] -> 14,
		Sort[{Orange, Blue}] -> 15,
		Sort[{Orange, Green}] -> 16,
		Sort[{Green, Yellow}] -> 17,
		Sort[{Blue, Yellow}] -> 18,
		Sort[{Red, Yellow}] -> 19,
		Sort[{Yellow, Orange}] -> 20
	}];
	(*Centri*)
	centers = Association[{
		{White} -> 21,
		{Red} -> 22,
		{Orange} -> 23,
		{Blue} -> 24,
		{Yellow} -> 25,
		{Green} -> 26
	}];

	Switch[Length[cols], 1, centers[Sort[cols]], 2, sides[Sort[cols]], 3, corners[Sort[cols]], _, -1]
];


(* ::Subsection:: *)
(*Acquisizione con color picker*)


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
(*Validazione del cube*)


(*
1. Controllo che ogni colore sia ripetuto esattamente 9 volte.
2. Controllo che le combinazioni di colori abbiano senso
*)
ValidateCubeInput[] := Module[{},
	Null
];


(* ::Section:: *)
(*Fine package*)


End[]

EndPackage[]
