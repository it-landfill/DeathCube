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
Cube3DToString::usage = "Conversione del cubo3D in una stringa."

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


(* 
	La funzione VerifyStringColorNumber permette di verificare che il numero di caratteri uguali nella stringa 
	sia pari a 9, ovvero che nella stringa ogni colore sia riportato esattamente 9 volte.
*)
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


(* 
	La funzione GenColorPickerColBox permette di generare i singoli bottoni con rettangolo di colore 
	specificato.
*)
GenColorPickerColBox[coord_, color_] :=
	Module[{r},
		r = Rectangle[coord];
		r = Style[r, {color, EdgeForm[Thick]}];
		Button[r, currentColor = color]
	];


(* 
	La funzione GenColorPickerColBox permette di generare il selettore di colori, ovvero il rettangolo 
	contenente i colori tra cui scegliere.
*)
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


(* 
	La funzione GenColorPickerColBox permette di generare il panel che contiene il selettore 
	per il colore che l'utente vuole inserire.
*)
VisualizeColorPickerBox[fontSize_:25] :=
(* Generazione del panel che contiene il selettore per il colore che l'utente vuole inserire *)
	Module[{picker, pickerTitle, current, currentTitle, col1, col2},
		(* Generazione della colonna del color picker *)
		pickerTitle = "Color Picker";
		picker = GenColorPickerBox[colors];
		col1 = Column[{Style[pickerTitle, {fontSize, Red, Bold}], Graphics[picker,
			 ImageSize -> Medium]}];
			 
		(* Generazione della colonna del current color *)
		currentTitle = "Current Color";
		current = {Dynamic[currentColor], EdgeForm[Thick], Rectangle[]};
		col2 = Column[{Style[currentTitle, {fontSize, Red, Bold}], Graphics[current,
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


(* 
	La funzione Cube2DToString permette la visualizzazione del 2D di input.
*)
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
ValidateCubeInput[] := Module[
	{solvedCube, solvedCubeSorted, cube, cubeSorted},
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


(* 
	La funzione Cube2DToString permette la conversione del cubo2D 
	in una stringa.
*)
Cube2DToString[] := Module[
	{cubeCols = {}, cubeString = ""},
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

(* 
	La funzione Cube3DToString permette la conversione del cubo3D 
	in una stringa.
*)
Cube3DToString[cubePieces_] :=
	Module[
		{str = "", cols}
		,
		(*Generazione stringa vuota di lunghezza 54*) 
		Table[str = StringJoin[str, "-"], {i, 54}];
		(*Fill delle facce*)
		cols = ExtractCube[cubePieces, RIGHT][["colors"]];
		str = StringReplacePart[str, cols[[1]], {29, 29}];
		cols = ExtractCube[cubePieces, LEFT][["colors"]];
		str = StringReplacePart[str, cols[[1]], {23, 23}];
		cols = ExtractCube[cubePieces, UP][["colors"]];
		str = StringReplacePart[str, cols[[2]], {5, 5}];
		cols = ExtractCube[cubePieces, DOWN][["colors"]];
		str = StringReplacePart[str, cols[[2]], {50, 50}];
		cols = ExtractCube[cubePieces, FRONT][["colors"]];
		str = StringReplacePart[str, cols[[3]], {26, 26}];
		cols = ExtractCube[cubePieces, BACK][["colors"]];
		str = StringReplacePart[str, cols[[3]], {32, 32}];
		(*Fill dei lati*)
		cols = ExtractCube[cubePieces, RIGHT + UP][["colors"]];
		str = StringReplacePart[str, cols[[1]], {17, 17}];
		str = StringReplacePart[str, cols[[2]], {6, 6}];
		cols = ExtractCube[cubePieces, RIGHT + DOWN][["colors"]];
		str = StringReplacePart[str, cols[[1]], {41, 41}];
		str = StringReplacePart[str, cols[[2]], {51, 51}];
		cols = ExtractCube[cubePieces, RIGHT + FRONT][["colors"]];
		str = StringReplacePart[str, cols[[1]], {28, 28}];
		str = StringReplacePart[str, cols[[3]], {27, 27}];
		cols = ExtractCube[cubePieces, RIGHT + BACK][["colors"]];
		str = StringReplacePart[str, cols[[1]], {30, 30}];
		str = StringReplacePart[str, cols[[3]], {31, 31}];
		cols = ExtractCube[cubePieces, LEFT + UP][["colors"]];
		str = StringReplacePart[str, cols[[1]], {11, 11}];
		str = StringReplacePart[str, cols[[2]], {4, 4}];
		cols = ExtractCube[cubePieces, LEFT + DOWN][["colors"]];
		str = StringReplacePart[str, cols[[1]], {35, 35}];
		str = StringReplacePart[str, cols[[2]], {49, 49}];
		cols = ExtractCube[cubePieces, LEFT + FRONT][["colors"]];
		str = StringReplacePart[str, cols[[1]], {24, 24}];
		str = StringReplacePart[str, cols[[3]], {25, 25}];
		cols = ExtractCube[cubePieces, LEFT + BACK][["colors"]];
		str = StringReplacePart[str, cols[[1]], {22, 22}];
		str = StringReplacePart[str, cols[[3]], {33, 33}];
		cols = ExtractCube[cubePieces, UP + FRONT][["colors"]];
		str = StringReplacePart[str, cols[[2]], {8, 8}];
		str = StringReplacePart[str, cols[[3]], {14, 14}];
		cols = ExtractCube[cubePieces, UP + BACK][["colors"]];
		str = StringReplacePart[str, cols[[2]], {2, 2}];
		str = StringReplacePart[str, cols[[3]], {20, 20}];
		cols = ExtractCube[cubePieces, DOWN + FRONT][["colors"]];
		str = StringReplacePart[str, cols[[2]], {47, 47}];
		str = StringReplacePart[str, cols[[3]], {38, 38}];
		cols = ExtractCube[cubePieces, DOWN + BACK][["colors"]];
		str = StringReplacePart[str, cols[[2]], {53, 53}];
		str = StringReplacePart[str, cols[[3]], {44, 44}];
		(*Fill degli angoli*)
		cols = ExtractCube[cubePieces, RIGHT + UP + FRONT][["colors"]];
		str = StringReplacePart[str, cols[[1]], {16, 16}];
		str = StringReplacePart[str, cols[[2]], {9, 9}];
		str = StringReplacePart[str, cols[[3]], {15, 15}];
		cols = ExtractCube[cubePieces, RIGHT + UP + BACK][["colors"]];
		str = StringReplacePart[str, cols[[1]], {18, 18}];
		str = StringReplacePart[str, cols[[2]], {3, 3}];
		str = StringReplacePart[str, cols[[3]], {19, 19}];
		cols = ExtractCube[cubePieces, RIGHT + DOWN + FRONT][["colors"]];
		str = StringReplacePart[str, cols[[1]], {40, 40}];
		str = StringReplacePart[str, cols[[2]], {48, 48}];
		str = StringReplacePart[str, cols[[3]], {39, 39}];
		cols = ExtractCube[cubePieces, RIGHT + DOWN + BACK][["colors"]];
		str = StringReplacePart[str, cols[[1]], {42, 42}];
		str = StringReplacePart[str, cols[[2]], {54, 54}];
		str = StringReplacePart[str, cols[[3]], {43, 43}];
		cols = ExtractCube[cubePieces, LEFT + UP + FRONT][["colors"]];
		str = StringReplacePart[str, cols[[1]], {12, 12}];
		str = StringReplacePart[str, cols[[2]], {7, 7}];
		str = StringReplacePart[str, cols[[3]], {13, 13}];
		cols = ExtractCube[cubePieces, LEFT + UP + BACK][["colors"]];
		str = StringReplacePart[str, cols[[1]], {10, 10}];
		str = StringReplacePart[str, cols[[2]], {1, 1}];
		str = StringReplacePart[str, cols[[3]], {21, 21}];
		cols = ExtractCube[cubePieces, LEFT + DOWN + FRONT][["colors"]];
		str = StringReplacePart[str, cols[[1]], {36, 36}];
		str = StringReplacePart[str, cols[[2]], {46, 46}];
		str = StringReplacePart[str, cols[[3]], {37, 37}];
		cols = ExtractCube[cubePieces, LEFT + DOWN + BACK][["colors"]];
		str = StringReplacePart[str, cols[[1]], {34, 34}];
		str = StringReplacePart[str, cols[[2]], {52, 52}];
		str = StringReplacePart[str, cols[[3]], {45, 45}];
		Return[str]
	];


(* ::Section:: *)
(*Fine package*)


End[]

EndPackage[]
