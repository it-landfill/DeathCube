(* ::Package:: *)

BeginPackage["CubeSolver`"];
SolveCube::usage="";
WhiteCross::usage="";
PlaceWhiteCorner::usage="";
SecondLayer::usage="";
YellowCross::usage="";
YellowEdges::usage="";
YellowCorners::usage="";
YellowCornersOrientation::usage="";



(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Section:: *)
(*Import del core*)


AppendTo[$Path, NotebookDirectory[]];
Get["CubeCore.wl"]


(* ::Section:: *)
(*Ricerca cubi*)


(* ::Subsection:: *)
(*Restituzione dell'indice di un cubo a partire dall'elemento*)


getCube[total_,element_]:=(First[Position[total,element]]);


(* ::Subsection:: *)
(*Restituzione degli indici dei cubi che soddisfano un pattern di posizione*)


getPos[total_,elements_,pattern_]:=(
	idxs=Flatten[Position[Table[Flatten[Cases[{elements[[t]]["pos"]},pattern]],{t,1,Length[elements]}],pattern]];
	Return[Flatten[Table[getCube[total,elements[[idxs[[t]]]]],{t,1,Length[idxs]}]]]
);


(* ::Subsection:: *)
(*Restituzione degli indici dei cubi che soddisfano un pattern di colori	*)


getCol[total_,elements_,pattern_]:=(
	idxs=Flatten[Position[Table[Flatten[Cases[{elements[[t]]["colors"]},pattern]],{t,1,Length[elements]}],pattern]];
	Return[Flatten[Table[getCube[total,elements[[idxs[[t]]]]],{t,1,Length[idxs]}]]]
);


(* ::Subsection:: *)
(*Restituzione degli indici dei cubi che soddisfano un pattern di colori in qualsiasi ordine*)


getColSort[total_, elements_,col1_, col2_, col3_] := (
	Flatten[{
		getCol[total,elements,{col1,col2, col3}],getCol[total,elements,{col1,col3, col2}],getCol[total,elements,{col2,col1, col3}],
		getCol[total,elements,{col3,col1, col2}],getCol[total,elements,{col3,col2, col1}],getCol[total,elements,{col2,col3, col1}]}
	]
);


(* ::Subsection:: *)
(*Restituzione degli indici dei cubi che soddisfano un pattern di colori in qualsiasi ordine*)


(*getColSort[total_, elements_,col1_, col2_, col3_] := (
	Flatten[{
		getCol[total,elements,{col1,col2, col3}],getCol[total,elements,{col1,col3, col2}],getCol[total,elements,{col2,col1, col3}],
		getCol[total,elements,{col3,col1, col2}],getCol[total,elements,{col3,col2, col1}],getCol[total,elements,{col2,col3, col1}]}
	]
);*)


(* ::Section:: *)
(*Utilities*)


(* ::Subsection:: *)
(*Restituzione del terzo colore di un cubo, dati gli altri due*)


getSingleColor[element_, color1_, color2_]:=Intersection[Cases[element["colors"],Except[color1]], Cases[element["colors"], Except[color2]]];


(* ::Subsection:: *)
(*Restituzione delle facce orientate*)


frontFaceColor := First[cube3DPieces[[getPos[cube3DPieces,cube3DPieces,{0,0,1}]]]]["colors"][[3]];
rightFaceColor:= First[cube3DPieces[[getPos[cube3DPieces,cube3DPieces,{1,0,0}]]]]["colors"][[1]];
leftFaceColor:= First[cube3DPieces[[getPos[cube3DPieces,cube3DPieces,{-1,0,0}]]]]["colors"][[1]];
topFaceColor := First[cube3DPieces[[getPos[cube3DPieces,cube3DPieces,{0,1,0}]]]]["colors"][[2]];
backFaceColor := First[cube3DPieces[[getPos[cube3DPieces,cube3DPieces,{0,0,-1}]]]]["colors"][[3]]


(* ::Section:: *)
(*Solver*)


(* ::Subsection:: *)
(*Passo 1: margherita e croce bianca *)


(* ::Subsubsection:: *)
(*Orientazione corretta del cubo (Giallo sopra, Blu davanti)*)


OrientCube[] :=
	Module[{},
		While[frontFaceColor != "B" && frontFaceColor != "Y", cube3DPieces 
			= RotateY[cube3DPieces]];
		If[frontFaceColor == "B",
			While[topFaceColor != "Y", cube3DPieces = RotateZ[cube3DPieces]]
			,
			While[topFaceColor != "Y", cube3DPieces = RotateX[cube3DPieces]]
		];
		While[frontFaceColor != "B", cube3DPieces = RotateY[cube3DPieces]];
			
	];


(* ::Subsubsection:: *)
(*Calcolo del numero delle facce none di ogni cubo*)


numberOfNone:=Table[Count[cube3DPieces[[t]]["colors"],None],{t,1,Length[cube3DPieces]}];


(* ::Subsubsection:: *)
(*Ricerca degli edges bianchi*)


whiteEdges:=cube3DPieces[[getCol[cube3DPieces,cube3DPieces[[Flatten[Position[numberOfNone,1]]]],{___,"W",___}]]];


(* ::Subsubsection:: *)
(*Ricerca dei petali*)


idxPetali:=Flatten[{Flatten[getPos[cube3DPieces,cube3DPieces,{1|-1,1,0}]],Flatten[getPos[cube3DPieces,cube3DPieces,{0,1,1|-1}]]}];
petali := cube3DPieces[[Flatten[{Flatten[getPos[cube3DPieces,cube3DPieces,{1|-1,1,0}]],Flatten[getPos[cube3DPieces,cube3DPieces,{0,1,1|-1}]]}]]];


(* ::Subsubsection:: *)
(*Ricerca dei petali che sono al posto giusto*)


rightPetali := cube3DPieces[[getCol[cube3DPieces,petali,{_,"W",_}]]];


(* ::Subsubsection:: *)
(*Controllo della destinazione di un white edge*)


(* La funzione restituisce il cubo che dovr\[AGrave] essere sostituito da un white edge a seconda della posizione di questo *)

getWTop[element_] := Switch[element["pos"][[2]],
	1,  element, (*devo trasformarlo in un 2F ma il top \[EGrave] lui *)
	0, If[ MatchQ[element["colors"],{_,_,"W"}], (* Faccia bianca o su Front-Back o su Left-Right *)
		First[cube3DPieces[[ getPos[cube3DPieces,cube3DPieces,{element["pos"][[1]],1,0}]]]] ,(*FB mantengo X*)
		First[cube3DPieces[[ getPos[cube3DPieces,cube3DPieces,{0,1,element["pos"][[3]]}]]] ](*LR mantengo Z*)
		],
	-1, If[MatchQ[element["colors"],{_,"W",_}], (* Faccia bianca o su Up-Down o su Front-Back *)
		First[cube3DPieces[[ getPos[cube3DPieces,cube3DPieces,{element["pos"][[1]],1,element["pos"][[3]]}]]] ], (*UD mantengo Z e X*)
		First[cube3DPieces[[ getPos[cube3DPieces,cube3DPieces,{element["pos"][[1]],1,element["pos"][[3]]}]]]]  (*FB devo trasformarlo in un 2F prima, quindi controllo solo quella sopra, ma andr\[AGrave] fatto un doppio controllo*)
		]
	];


(* ::Subsubsection:: *)
(*Trasformazione di un edge in un second floor edge*)


(* Esegue la mossa corretta per trasformare un edge in un second floor edge a partire dalla posizione x, ed eventualmente z del cubo di riferimento *) 

makeSF[element_] := (
	While[getWTop[element]["colors"][[2]] == "W", cube3DPieces = RotateU[cube3DPieces]];
	Switch[element["pos"][[1]],
		0, If[element["pos"][[3]] == -1, cube3DPieces = RotateB[cube3DPieces], cube3DPieces = RotateF[cube3DPieces]], (* Se dietro, B, se davanti, F*)
		1, cube3DPieces = RotateR[cube3DPieces], (* se a dx, R *)
		-1, cube3DPieces = RotateL[cube3DPieces] (* se a sx, L *)
	]
);


(* ::Subsubsection:: *)
(*Restituzione degli edge ancora da inserire*)


leftEdges := Complement[whiteEdges, rightPetali];


(* ::Subsubsection:: *)
(*Inserimento dei second floor white edges *)


(* A seconda della posizione del second floor edge (4 possibilit\[AGrave]) e dalla posizione della faccia bianca su esso (2 possibilit\[AGrave]) 
viene eseguita la mossa corretta per posizionare tale edge sulla margherita *)

setSFDaisy[SFWhiteEdge_]:=(
	checkW=SFWhiteEdge["colors"][[1]]=="W"; (* True se la faccia bianca \[EGrave] sulla leftFace o sulla rightFace, False se \[EGrave] sulla backFace o frontFace *)
	Switch[SFWhiteEdge["pos"],
		{1,0,1},(If[checkW,cube3DPieces=RotateFi[cube3DPieces],cube3DPieces=RotateR[cube3DPieces]]),
		{1,0,-1},(If[checkW,cube3DPieces=RotateB[cube3DPieces],cube3DPieces=RotateRi[cube3DPieces]]),
		{-1,0,1},(If[checkW,cube3DPieces=RotateF[cube3DPieces],cube3DPieces=RotateLi[cube3DPieces]]),
		{-1,0,-1},(If[checkW,cube3DPieces=RotateBi[cube3DPieces],cube3DPieces=RotateL[cube3DPieces]])
	]
);


(* ::Subsubsection:: *)
(*Creazione della margherita e della croce bianca*)


(* 
	 Finch\[EAcute] ci sono dei leftEdges, mi salvo i colori del cubetto che sto considerando, per assegnargli un cubetto fisso. 
	 Poi, finch\[EAcute] questo cubetto non fa parte dei rightPetali, lo rendo un second floor, libero il cubo in alto da una faccia bianca facendo 
	 il numero necessario di RotateUp e faccio la mossa giusta per portarlo in alto (calcolata da setSFDaisy)
	 
	 Poi porto i petali correttamente sulla faccia bianca per ottenere la croce.
*)
WhiteCross[] := Module[{},
	OrientCube[];
	While[Length[leftEdges] != 0,
		{col1,col2,col3}= First[cube3DPieces[[getCube[cube3DPieces,First[leftEdges]]]]]["colors"];
		edge:= First[cube3DPieces[[getColSort[cube3DPieces,cube3DPieces,col1,col2,col3]]]];
		While[Intersection[List[edge],rightPetali] == {},
			If[edge["pos"][[2]] != 0,makeSF[edge]];
			While[getWTop[edge]["colors"][[2]] == "W", cube3DPieces = RotateU[cube3DPieces]];
			setSFDaisy[edge];
		];
	];
	(* Ottengo gli id dei centri di tutte le facce, tranne la gialla e la bianca *)
	idxCenters = Flatten[Position[numberOfNone,2]];
	centers = cube3DPieces[[idxCenters]];
	lateralCenters = Complement[centers, cube3DPieces[[getColSort[cube3DPieces,centers,None,None,"W"|"Y"]]]];

	(* Per ogni elemento del petalo... *)
	For[i=1, i<5, i++, (
		(* Trova il centro di riferimento *)
		currentCenter := lateralCenters[[i]];
		(* Trova il cubo da operare *)
		cubeAboveId := First[getPos[cube3DPieces, cube3DPieces, {currentCenter["pos"][[1]],currentCenter["pos"][[2]]+1,currentCenter["pos"][[3]]}]];
		(* Finch\[EGrave] il colore del cubo non combacia con quello di centro faccia ruota in senso orario il top layer *)
		While[getSingleColor[cube3DPieces[[cubeAboveId]],"W",None]!=getSingleColor[currentCenter, None, None],(cube3DPieces = RotateU[cube3DPieces];)];
		(* Ribalta la croce *)
		Switch[First[getSingleColor[cube3DPieces[[cubeAboveId]],"W",None]], 
			"O",(cube3DPieces=RotateL[RotateL[cube3DPieces]]),
			"B",(cube3DPieces=RotateF[RotateF[cube3DPieces]]),
			"G",(cube3DPieces=RotateB[RotateB[cube3DPieces]]),
			"R",(cube3DPieces=RotateR[RotateR[cube3DPieces]])
		]
	)];
	(* Visualizzazione della faccia bianca come top *)
	cube3DPieces=RotateZ[RotateZ[cube3DPieces]];
];





(* ::Subsection:: *)
(*Passo 2: white corners*)


(* ::Subsubsection:: *)
(*Ricerca dei corner da considerare*)


corners := cube3DPieces[[Flatten[Position[numberOfNone,0]]]];
whiteCorners := cube3DPieces[[getCol[cube3DPieces, cube3DPieces[[Flatten[Position[numberOfNone,0]]]],{___,"W",___}]]];
rightTopCorner := cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,1,1}]]]];
rightBottomCorner := cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,-1,1}]]]];
topCorners := cube3DPieces[[getPos[cube3DPieces,corners,{_,1,_}]]];

(* Il corner da posizionare \[EGrave] quello che deve finire in alto a destra, perci\[OGrave] ha i colori che corrispondono a quello davanti, in alto e a destra,
in un ordine non conosciuto in partenza *)
cornerToPlace := cube3DPieces[[First[getColSort[cube3DPieces,corners,frontFaceColor,rightFaceColor,topFaceColor]]]];


(* ::Subsubsection:: *)
(*Posizionamento dei white corner*)


(* \[EGrave] possibile posizionare un corner nel suo posto corretto, collocandolo inizialmente nella posizione in basso a sinistra rispetto alla faccia
frontale e poi eseguendo ciclicamente le mosse R'D'RD finch\[EAcute] questo non sar\[AGrave] al posto giusto *)

PlaceWhiteCorner[] := Module[{},
	For[i=1, i<5, i++, (
		If[cornerToPlace["pos"][[2]]==1, (* Se il corner da posizionare \[EGrave] in alto, faccio le mosse esatte per portarlo in basso*)
		Switch[cornerToPlace["pos"],
			{1,1,-1},cube3DPieces = RotateB[RotateDi[RotateBi[cube3DPieces]]],
			{1,1,1},cube3DPieces = RotateFi[RotateDi[RotateF[cube3DPieces]]],
			{-1,1,1},cube3DPieces = RotateF[RotateDi[RotateFi[cube3DPieces]]],
			{-1,1,-1}, cube3DPieces = RotateBi[RotateDi[RotateB[cube3DPieces]]]
		]];
		(* Ruoto poi la faccia in basso finch\[EAcute] il corner non \[EGrave] in basso a destra*)
		While[cornerToPlace != rightBottomCorner, cube3DPieces = RotateD[cube3DPieces]];
		(* Ed eseguo le mosse previste finch\[EAcute] non \[EGrave] in alto a destra con la faccia W rivolta verso l'alto *)
		While[cornerToPlace != rightTopCorner || rightTopCorner["colors"][[2]]!="W", cube3DPieces = RotateD[RotateR[RotateDi[RotateRi[cube3DPieces]]]]];
		(* Passo poi al corner successivo, girando il cubo *)
		cube3DPieces=RotateY[cube3DPieces] 
	)];
	(* Visualizzazione della faccia gialla come top *)
	cube3DPieces=RotateZ[RotateZ[cube3DPieces]];
];


(* ::Subsection:: *)
(*Passo 3: risoluzione second layer*)


(* ::Subsubsection:: *)
(*Ricerca degli edges*)


idxOfEdges := Flatten[Position[numberOfNone, 1]];
edges := cube3DPieces[[idxOfEdges]];


(* ::Subsubsection:: *)
(*Restituzione del colore dell'edge non sulla faccia superiore a partire da una lista di colori*)


getSideColor[colors_] := If[colors[[1]] == None, colors[[3]],colors[[1]]];


(* ::Subsubsection:: *)
(*Restituzione degli edge che vanno inseriti a sx e a dx rispetto alla faccia frontale*)


leftEdge := cube3DPieces[[First[getColSort[cube3DPieces,edges,frontFaceColor,leftFaceColor,None]]]];
rightEdge := cube3DPieces[[First[getColSort[cube3DPieces,edges,frontFaceColor,rightFaceColor,None]]]];


(* ::Subsubsection:: *)
(*Definizione della condizione di uscita dell'algoritmo*)


(* Lista di True False, basata sul posizionamento corretto di ogni "tripla" di cubetti per ogni faccia (centro-sx, centro-centro, centro-dx)*)

checkSF := {
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{0,0,1}]]]]["colors"][[3]] == 
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,0,1}]]]]["colors"][[3]] == 
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,0,1}]]]]["colors"][[3]],

	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{0,0,-1}]]]]["colors"][[3]] == 
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,0,-1}]]]]["colors"][[3]] == 
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,0,-1}]]]]["colors"][[3]],

	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,0,-1}]]]]["colors"][[1]] == 
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,0,0}]]]]["colors"][[1]] == 
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,0,1}]]]]["colors"][[1]],

	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,0,-1}]]]]["colors"][[1]] == 
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,0,0}]]]]["colors"][[1]] == 
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,0,1}]]]]["colors"][[1]]
};


(* ::Subsubsection:: *)
(*Algoritmo second layer*)


SecondLayer[] := Module[{},
	While[
		Total[Boole[checkSF]] != 4,
		(
		(* Proviamo a controllare se per ogni lato abbiamo nel layer pi\[UGrave] in alto un edge da inserire*)
		For[i=1, i<5, i++, (
			If[leftEdge["pos"][[2]]!=1 && rightEdge["pos"][[2]]!=1 ,cube3DPieces=RotateY[cube3DPieces]];
		)];

		topCube := cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{0,1,1}]]]];

		(* Se non ci sono edge da inserire, facciamo una mossa a vuoto per liberarne uno *)
		If[leftEdge["pos"][[2]]!=1 && rightEdge["pos"][[2]]!=1,
			cube3DPieces = RotateFi[RotateUi[RotateF[RotateU[RotateL[RotateU[RotateLi[RotateUi[cube3DPieces]]]]]]]],

		(* altrimenti o c'\[EGrave] un leftEdge, e salviamo i suoi colori *)
		If[leftEdge["pos"][[2]]==1 , 
			sideColor = getSideColor[leftEdge["colors"]]; (* colore della faccia che gestir\[AGrave] l'edge sinistro da inserire... *)
			otherColor = leftEdge["colors"][[2]], (* ...e quello che sta invece sopra*)
		(* o un rightEdge e salviamo i suoi *)
			sideColor = getSideColor[rightEdge["colors"]]; (* colore della faccia che gestir\[AGrave] l'edge destro da inserire... *)
			otherColor = rightEdge["colors"][[2]] (* ...e quello che sta invece sopra*)
		];

		While [frontFaceColor != sideColor, cube3DPieces = RotateY[cube3DPieces]]; (* Mettiamo di fronte la faccia su cui lavorare *)

		(* Ruotiamo il layer superiore finch\[EAcute] l'edge da inserire non \[EGrave] sopra il centro considerato *)
		While[ topCube["colors"][[3]]!=frontFaceColor||topCube["colors"][[2]]!=otherColor,cube3DPieces = RotateU[cube3DPieces]];

		(* Ora se l'edge considerato \[EGrave] un left, facciamo le relative mosse, altrimenti quelle dei right*)
		If[otherColor == leftFaceColor,
			cube3DPieces = RotateFi[RotateUi[RotateF[RotateU[RotateL[RotateU[RotateLi[RotateUi[cube3DPieces]]]]]]]],
			cube3DPieces = RotateF[RotateU[RotateFi[RotateUi[RotateRi[RotateUi[RotateR[RotateU[cube3DPieces]]]]]]]]]
		]
	)];
	
	(* Mettiamo di fronte la faccia blu *)
	While[frontFaceColor != "B", cube3DPieces=RotateY[cube3DPieces]];
];


(* ::Subsection:: *)
(*Passo 4: croce gialla*)


(* ::Subsubsection:: *)
(*Ricerca del numero di cubi ben posizionati nella croce*)


topFaceCross := Union[cube3DPieces[[getPos[cube3DPieces,cube3DPieces,{0,1,_}]]], cube3DPieces[[getPos[cube3DPieces,cube3DPieces,{_,1,0}]]]];
nYellow := Length[cube3DPieces[[getCol[cube3DPieces,topFaceCross,{_,"Y",_}]]]];


(* ::Subsubsection:: *)
(*Algoritmo croce gialla *)


YellowCross[] := Module[{}, 
	(* Caso singolo punto giallo *)
	If[nYellow<3, cube3DPieces = RotateFi[RotateUi[RotateRi[RotateU[RotateR[RotateF[cube3DPieces]]]]]]];
	(* Casi con 3 facce gialle, quindi o L o I *)
	If[nYellow == 3,
		(* Finch\[EAcute] non si arriva al caso della I orizzontale *)
		While[cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,1,0}]]]]["colors"][[2]] != "Y" || cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,1,0}]]]]["colors"][[2]] != "Y" ,
			(* Se \[EGrave] verticale, \[EGrave] sufficiente ruotarla*)
			If[cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{0,1,-1}]]]]["colors"][[2]] == "Y" && cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{0,1,1}]]]]["colors"][[2]] == "Y", 
				cube3DPieces = RotateU[cube3DPieces], 
				(* Altrimenti occorre ruotare la faccia superiore finch\[EAcute] non si ottiene la L nella posizione giusta e poi si esegue la mossa *)
				While[cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{0,1,-1}]]]]["colors"][[2]] != "Y" || cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,1,0}]]]]["colors"][[2]] != "Y",
					cube3DPieces = RotateU[cube3DPieces] ];
					cube3DPieces = RotateFi[RotateUi[RotateRi[RotateU[RotateR[RotateF[cube3DPieces]]]]]]
			]
		];
		(* Poi \[EGrave] possibile effettuare la mossa finale per arrivare alla croce gialla *)
		cube3DPieces = RotateFi[RotateUi[RotateRi[RotateU[RotateR[RotateF[cube3DPieces]]]]]];
	];
];


(* ::Subsection:: *)
(*Passo 5: Riposizionamento dei yellow edges*)


(* ::Subsubsection:: *)
(*Definizione della condizione di uscita dell'algoritmo*)


(* Lista di True o False basata sulla corrispondenza del colore in centro-alto e il centro-centro di ogni faccia *)

checkY := {
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{0,1,1}]]]]["colors"][[3]] == frontFaceColor,
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,1,0}]]]]["colors"][[1]] == rightFaceColor,
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{0,1,-1}]]]]["colors"][[3]] == backFaceColor,
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,1,0}]]]]["colors"][[1]] ==leftFaceColor
};


(* ::Subsubsection:: *)
(*Algoritmo dei yellow edges*)


(* Ruoto la faccia superiore finch\[EAcute] non ho due edge superiori nel posto giusto, poi quando ho a sinistra un edge sbagliato, faccio le mosse necessarie. 
Potrebbe essere necessario svolgere questo passaggio due volte *)

YellowEdges[] := Module[{},
	While[Total[Boole[checkY]] != 4,
		While[ Total[Boole[checkY]] != 2,
			cube3DPieces = RotateU[cube3DPieces]
		];

		While[
			cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,1,0}]]]]["colors"][[1]] == leftFaceColor,
			cube3DPieces = RotateY[cube3DPieces]
		];

		(* R U R' U R U U R' U *)
		cube3DPieces = RotateU[RotateRi[RotateU[RotateU[RotateR[RotateU[RotateRi[RotateU[RotateR[cube3DPieces]]]]]]]]];
	]
];


(* ::Subsection:: *)
(*Passo 6: Posizionamento dei yellow corners*)


(* ::Subsubsection:: *)
(*Definizione della condizione di uscita dell'algoritmo*)


(* Lista di True o False basata sulla corrispondenza dei colori dei corner in alto, con i relativi colori delle facce toccate *)

checkCorners := {
	Sort[cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,1,1}]]]]["colors"]] == Sort[{frontFaceColor, rightFaceColor,"Y"}],
	Sort[cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,1,1}]]]]["colors"]] == Sort[{frontFaceColor, leftFaceColor,"Y"}],
	Sort[cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,1,-1}]]]]["colors"]] == Sort[{backFaceColor, rightFaceColor,"Y"}],
	Sort[cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,1,-1}]]]]["colors"]] == Sort[{backFaceColor, leftFaceColor,"Y"}]
};


(* ::Subsubsection:: *)
(*Algoritmo yellow corners*)


(* La sequenza di mosse U R U' L' U R' U' L ruota fra loro gli angoli: davanti a sx, dietro a sx e dietro a dx. Questa va ripetuta finch\[EAcute] tutti e tre 
non si trovano nel posto corretto. Perci\[OGrave] \[EGrave] opportuno avere l'angolo davanti a dx gi\[AGrave] nel posto corretto.
Ripetiamo l'algoritmo quindi finch\[EAcute] non abbiamo 0 angoli nel posto corretto.*)

YellowCorners[] := Module[{},
	
	While[Total[Boole[checkCorners]] == 0,
		cube3DPieces = RotateL[RotateUi[RotateRi[RotateU[RotateLi[RotateUi[RotateR[RotateU[cube3DPieces]]]]]]]]
	];
	
	(* Dopodich\[EAcute] ruotiamo il cubo finch\[EAcute] non abbiamo un cubo corretto davanti a dx *)

	While[
		Sort[cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,1,1}]]]]["colors"]] != Sort[{frontFaceColor, rightFaceColor,"Y"}],
		cube3DPieces = RotateY[cube3DPieces]
	];
	
	(* Ed eseguiamo le mosse finch\[EAcute] non abbiamo tutti i cubi al posto giusto *)

	While[Total[Boole[checkCorners]] == 1,
		cube3DPieces = RotateL[RotateUi[RotateRi[RotateU[RotateLi[RotateUi[RotateR[RotateU[cube3DPieces]]]]]]]]
	]
];


(* ::Subsection:: *)
(*Passo 7: orientamento dei yellow corner*)


(* ::Subsubsection:: *)
(*Condizione di uscita dell'algoritmo*)


(* Lista di True o False basata sul corretto orientamento dei corner della faccia superiore *)

checkOrientation := {
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,1,1}]]]]["colors"] == {rightFaceColor, "Y", frontFaceColor},
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,1,1}]]]]["colors"] == {leftFaceColor, "Y", frontFaceColor},
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,1,-1}]]]]["colors"] == {rightFaceColor, "Y", backFaceColor},
	cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{-1,1,-1}]]]]["colors"] == {leftFaceColor, "Y", backFaceColor}
};


(* ::Subsubsection:: *)
(*Algoritmo orientamento yellow corner*)


(* La sequenza di mosse R' D' R D viene ripetuta su ogni top corner finch\[EAcute] questi non sono al posto esatto. Questo porter\[AGrave] il cubo ad essere 
completamente risolto *)

YellowCornersOrientation[] := Module[{},
	While[Total[Boole[checkOrientation]] != 4,
		While[
			cube3DPieces[[First[getPos[cube3DPieces,cube3DPieces,{1,1,1}]]]]["colors"] != {_, "Y", _},
			cube3DPieces = RotateD[RotateR[RotateDi[RotateRi[cube3DPieces]]]]
		];
		cube3DPieces = RotateU[cube3DPieces]
	]
];


(* ::Section:: *)
(*Main call del solver*)


SolveCube[] := Module[{},
	WhiteCross[];
	PlaceWhiteCorner[];
	SecondLayer[];
	YellowCross[];
	YellowEdges[];
	YellowCorners[];
	YellowCornersOrientation[];
];


(* ::Section:: *)
(*Chiusura del package*)


End[];
EndPackage[];
