(* ::Package:: *)

BeginPackage["CubeAcquire`"]


sampleStrings::usage = ""


VerifyCubeColorNumber::usage = ""


FindMatchingCubeCode::usage = ""


Begin["`Private`"]


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


End[]

EndPackage[]
