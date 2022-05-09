(* ::Package:: *)

BeginPackage["CubeColors`"]


CharToColor::usage = ""
CubeStringToColorList::usage = ""
SetTextureRubik::usage = ""
GetTextureRubik::usage = ""


Begin["`Private`"]


(* ::Subsection:: *)
(*Map da lettere a colori*)


(* ::Subsubsection:: *)
(*Map delle facce di un cubo risolto a colore*)


cubeColors = <| "L" ->  Orange,"R" ->Red,"F"->Green, "B"-> Blue,"U"-> White, "D"->Yellow |>;


(* ::Subsubsection:: *)
(*Map da char a colore*)


textureDim = 100;
selectedTexture = 1;

colorScheme1 = {
	Texture[Style[White, textureDim]], 
	Texture[Style[Red, textureDim]],  
	Texture[Style[Blue, textureDim]], 
	Texture[Style[Orange, textureDim]], 
	Texture[Style[Green, textureDim]], 
	Texture[Style[Yellow, textureDim]], 
	Texture[Style[Black, textureDim]]
}
colorScheme2 = {
	Texture[Style["1", textureDim]], 
	Texture[Style["2", textureDim]],  
	Texture[Style["3", textureDim]], 
	Texture[Style["4", textureDim]], 
	Texture[Style["5", textureDim]], 
	Texture[Style["6", textureDim]], 
	Texture[Style["7", textureDim]]
}
colorScheme3 = {
	Texture[Style["\:2726", textureDim]], 
	Texture[Style["\[FivePointedStar]", textureDim]],  
	Texture[Style["\[FilledDownTriangle]", textureDim]], 
	Texture[Style["\:2665\:fe0e", textureDim]], 
	Texture[Style["\:273f", textureDim]], 
	Texture[Style["\[ClubSuit]", textureDim]], 
	Texture[Style["\[FilledCircle]", textureDim]]
}
colorScheme4 = {
	Texture[Style["\|01f60e",textureDim]], 
	Texture[Style["\|01f60d", textureDim]],  
	Texture[Style["\|01f602", textureDim]], 
	Texture[Style["\|01f631", textureDim]], 
	Texture[Style["\|01f621", textureDim]], 
	Texture[Style["\|01f62a", textureDim]], 
	Texture[Style["\|01f921", textureDim]]
}




SetTextureRubik[selectTexture_ : "1"]:= Module[{},
	currentColorScheme = Switch[selectTexture, 
		1, colorScheme1,
		2, colorScheme2,
		3, colorScheme3,
		4, colorScheme4
	];
	selectedTexture = selectTexture;
];


GetTextureRubik[]:= Module[
	{},
	Return[selectedTexture]
];


CharToColor[char_] := Switch[char,
	"W",currentColorScheme[[1]],
	"R", currentColorScheme[[2]], 
	"B", currentColorScheme[[3]], 
	"O", currentColorScheme[[4]], 
	"G", currentColorScheme[[5]], 
	"Y", currentColorScheme[[6]], 
	_, currentColorScheme[[7]]
];


(* ::Subsubsection:: *)
(*Map della stringa a lista di colori*)


CubeStringToColorList[cube_] :=
	Table[CharToColor[c], {c, Characters[cube]}];


End[]
EndPackage[]
