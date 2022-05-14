(* ::Package:: *)

BeginPackage["CubeAcquire`"]


(* ::Section:: *)
(*Definizione usage*)


(* Metodi *)
VerifyStringColorNumber::usage = "Verifica che il numero di caratteri uguali nella stringa sia pari a 9, ovvero che nella stringa ogni colore sia riportato esattamente 9 volte."
VisualizeColorPickerBox::usage = "Generazione del panel che contiene il selettore per il colore che l'utente vuole inserire."
VisualizeInput2DCube::usage = "Visualizzazione del cubo 2D di input."
ValidateCubeInput::usage = "Validazione del cubo salvato nella variabile cube."
Cube2DToString::usage = "Conversione del cubo2D in una stringa."

(* Variabili *)
sampleStrings::usage = "Stringhe di esempio"


(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Subsection:: *)
(*Import dei package utilizzati*)


AppendTo[$Path, NotebookDirectory[]];
Get["CubeCore.wl"]
Get["CubeColors.wl"]


(* ::Section:: *)
(*Definizione variabili*)


(* Set di colori per il cubo *)
colors = GetCurrentColorScheme[];
(* Colore attualmente selezionato tramite color picker *)
currentColor = Transparent;
(* Rappresentazione del cubo 2D *)
cube;


(* ::Section:: *)
(*Acquisizione tramite stringa*)


(* ::Subsection:: *)
(*Stringhe di esempio*)


sampleStrings = <|"solved" -> "WWWWWWWWWOOOGGGRRRBBBOOOGGGRRRBBBOOOGGGRRRBBBYYYYYYYYY",
	 "start" -> "WOWBYRBYGOOYORWOWRBYBRORGBGWRBYGBOGGRYGWWBYOYYGROWBGWR"|>


(* ::Subsection:: *)
(*Verifica validit\[AGrave] della stringa*)


(* Verifica che il numero di caratteri uguali nella stringa sia pari a 9, ovvero che nella stringa ogni colore sia riportato esattamente 9 volte. *)
VerifyStringColorNumber[cube_] := Module[{characterList},
		characterList = CharacterCounts[cube];
		Max[characterList] == Min[characterList] == 9
];


(* ::Section:: *)
(*Acquisizione grafica*)


(* ::Subsection:: *)
(*Color picker*)


(* ::Subsubsection:: *)
(*Generazione*)


(* Genera i singoli bottoni con rettangolo di colore specificato *)
GenColorPickerColBox[coord_, color_] :=
	Module[{r},
		r = Rectangle[coord];
		r = Style[r, {color, EdgeForm[Thick]}];
		Button[r, currentColor = color]
	];


(* Genera il selettore di colori, ovvero il rettangolo contenente i colori tra cui scegliere *)
GenColorPickerBox[colors] :=
	Module[{colorBox = {}},
		AppendTo[colorBox, GenColorPickerColBox[{0, 0}, colors[["R"]]]];
		AppendTo[colorBox, GenColorPickerColBox[{1, 0}, colors[["O"]]]];
		AppendTo[colorBox, GenColorPickerColBox[{2, 0}, colors[["G"]]]];
		AppendTo[colorBox, GenColorPickerColBox[{0, 1}, colors[["B"]]]];
		AppendTo[colorBox, GenColorPickerColBox[{1, 1}, colors[["W"]]]];
		AppendTo[colorBox, GenColorPickerColBox[{2, 1}, colors[["Y"]]]];
		Return[colorBox]
	];


(* ::Subsubsection:: *)
(*Visualizzazione*)


(* Generazione del panel che contiene il selettore per il colore che l'utente vuole inserire *)
VisualizeColorPickerBox[] :=
	Module[{picker, pickerTitle, current, currentTitle, col1, col2},
		(* Generazione della colonna del color picker *)
		pickerTitle = "Color Picker";
		picker = GenColorPickerBox[colors];
		col1 = Column[{Style[pickerTitle, {25, Red, Bold}], Graphics[picker,
			 ImageSize -> Medium]}];
			 
		(* Generazione della colonna del current color *)
		currentTitle = "Current Color";
		current = {Dynamic[currentColor], EdgeForm[Thick], Rectangle[]};
		col2 = Column[{Style[currentTitle, {25, Red, Bold}], Graphics[current,
			 ImageSize -> Tiny]}];
			 
		(* Stampa il panel composto dalle 2 colonne generate sopra *)
		Print[Panel[Row[{col1, col2}]]]
	];


(* ::Subsection:: *)
(*Cubo 2D*)


(* ::Subsubsection:: *)
(*Generazione*)


(* Generazione di un rettangolo di lato 1 con angolo in posizione coord e colore col *)
GenRect[rect_, col_] := Module[{},
	Style[Rectangle[rect],{col,EdgeForm[Thick]}]
];


(* 
	Funzione che verr\[AGrave] richiamata quando l'utente preme su uno dei cubi.
	La funzione su occupa di aggiornare il colore del cubo selezionato, ponendolo uguale al currentColor 
 *)
ClickHandler[coord_] := Module[{ind},
	(* Trovo l'indice dell'elemento con coordinate coord nella lista cube *)
	Quiet[ind = Position[cube,coord][[1]][[1]]];
	(* Se il cubo \[EGrave] un centro, non lo modifico, altrimenti aggiorno il colore del centro *)
	If[ContainsAny[{5,23,26,29,32,50},{ind}]==False,
		cube[[ind]][[2]] = currentColor;
	]
];


(* Genero il bottone wrapper al rettangolo. Quando premuto chiama ClickHandler con le coordinate del rettangolo *) 
GenBtn[rectStr_] := Module[{coord, col, r},
	coord = rectStr[[1]];
	col = rectStr[[2]];
	(* Genero il rettangolo *)
	r = GenRect[coord, col];
	(* Genero il bottone *)
	Button[r, ClickHandler[coord]]
];


(* Genero la lista di rettangoli composta da coordinata, colore *)
GenerateBaseCubeStruct[] := Module[{points = {}, defaultColor = Transparent},
	(* Genero una lista di rettangoli rappresentante il cubo aperto e di colore defaultColor *)
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
		Front = 26,
		Back  = 32,
		Up    = 5,
		Down  = 50,
		Left  = 23,
		Right = 29	
	*)
	points[[5]][[2]] =colors[["W"]];  (* Up *)
	points[[26]][[2]] =colors[["B"]]; (* Front *)
	points[[23]][[2]] =colors[["R"]]; (* Left *)
	points[[29]][[2]] =colors[["O"]]; (* Right *)
	points[[32]][[2]] =colors[["G"]]; (* Back *)
	points[[50]][[2]] =colors[["Y"]]; (* Down *)
	Return[points];
];


(* ::Subsubsection:: *)
(*Visualizzazione*)


(* Visualizzazione del cubo 2D di input *)
VisualizeInput2DCube[] := Module[{background = LightGray},
	(* Genero la struttura di base. cube \[EGrave] una variabile globale al modulo *)
	cube = GenerateBaseCubeStruct[];
	(* Stampo il cubo 2D *)
	Graphics[Dynamic[Map[GenBtn, cube]], Background->background]
];


(* ::Subsubsection:: *)
(*Validazione*)


(*
	Validazione del cubo salvato nella variabile cube.
	Per validare il cubo, confronto le triple di colori ordinate del cubo da validare con quelle del cubo risolto.
	Questa validazione non copre tutti i casi, controlla solo la correttezza dei singoli sotto cubi.
	I casi restanti vengono identificati tramite l'inabilit\[AGrave] di risolvere il cubo in un numero fissato di mosse massime.
*)
ValidateCubeInput[] := Module[{solvedCube, solvedCubeSorted, cube, cubeSorted},
	(* Cubo risolto di riferimento per la validazione *)
	solvedCube = GetPieces[sampleStrings[["solved"]]];
	(* Genero una lista ordinata contenente le triple contenenti i colori ordinati del cubo risolto *)
	solvedCubeSorted = Sort[Map[Sort[#[["colors"]]]&,solvedCube]];
	(* Cubo da validare *)
	cube = GetPieces[Cube2DToString[]];
	(* Genero una lista ordinata contenente le triple contenenti i colori ordinati del cubo da validare *)
	cubeSorted = Sort[Map[Sort[#[["colors"]]]&,cube]];
	
	(* Se le liste di triple corrispondono, allora il cubo \[EGrave] valido e ritorno True *)
	Return[SameQ[solvedCubeSorted,cubeSorted]];
];


(* ::Section:: *)
(*Utilities*)


(* ::Subsection:: *)
(*Conversione da Cubo 2D a stringa*)


(* Conversione del cubo2D in una stringa *)
Cube2DToString[] := Module[{cubeCols = {}, cubeString = ""},
(* Genero la stringa rappresentante il cubo *)
	AppendTo[cubeCols, Table[cube[[Position[cube,{x,8}][[1,1]],2]],{x,3,5}]];
	AppendTo[cubeCols, Table[cube[[Position[cube,{x,7}][[1,1]],2]],{x,3,5}]];
	AppendTo[cubeCols, Table[cube[[Position[cube,{x,6}][[1,1]],2]],{x,3,5}]];
	AppendTo[cubeCols, Table[cube[[Position[cube,{x,5}][[1,1]],2]],{x,0,11}]];
	AppendTo[cubeCols, Table[cube[[Position[cube,{x,4}][[1,1]],2]],{x,0,11}]];
	AppendTo[cubeCols, Table[cube[[Position[cube,{x,3}][[1,1]],2]],{x,0,11}]];
	AppendTo[cubeCols, Table[cube[[Position[cube,{x,2}][[1,1]],2]],{x,3,5}]];
	AppendTo[cubeCols, Table[cube[[Position[cube,{x,1}][[1,1]],2]],{x,3,5}]];
	AppendTo[cubeCols, Table[cube[[Position[cube,{x,0}][[1,1]],2]],{x,3,5}]];
	cubeCols = Flatten[cubeCols];
	(* Converto i colori nelle corrispettive lettere rappresentative *)
	cubeString = Map[ColorToChar, cubeCols];
	cubeString = StringJoin[cubeString];
	Return[cubeString];
];


(* ::Section:: *)
(*Fine package*)


End[]

EndPackage[]
