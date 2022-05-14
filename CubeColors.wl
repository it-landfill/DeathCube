(* ::Package:: *)

BeginPackage["CubeColors`"]


(* Metodi *)
CharToColor::usage = ""
ColorToChar::usage = ""
CubeStringToColorList::usage = ""
GetCurrentColorScheme::usage = ""
VisualizeColorSchemePicker::usage=""


(* ::Section:: *)
(*Inizio package*)


Begin["`Private`"]


(* ::Section:: *)
(*Definizione schemi colori*)


(* Di seguito sono riportati gli schemi colori. *)
(* defaultColorScheme permette di visualizzare lo schema colori ufficiale del cubo di Rubik *)
defaultColorScheme = <| "R" -> Red, "O" -> Orange, "G" -> Green, "B"-> Blue, "W" -> White, "Y" -> Yellow, "T" -> Transparent|>;
(* 
	protanopiaColorScheme permette di visualizzare cubo di Rubik che utilizza uno schema colori adatto (teoricamente) ai
	soggetti daltonici affetti da protanopia.
	Note: Fonte per la generazione dello schema colore 
	"https://davidmathlogic.com/colorblind/#%23FF005F-%231AFF1A-%236ED0FF-%232121AB-%23E66100-%23FFFFFF"
*)
protanopiaColorScheme = <| "R" -> RGBColor[1,0,0.37], "O" -> RGBColor[0.9,0.38,0], "G" -> RGBColor[0.1,1,0.1], "B"-> RGBColor[0.13,0.13,0.67], "W" -> White, "Y" -> RGBColor[0.43,0.82,1], "T" -> Transparent|>;
(* 
	protanopiaColorScheme permette di visualizzare cubo di Rubik che utilizza uno schema colori adatto (teoricamente) ai
	soggetti daltonici affetti da deuteranopia.
	Note: Fonte per la generazione dello schema colore 
	"https://davidmathlogic.com/colorblind/#%23FFB200-%2342BCD2-%234FFFE4-%23FF7800-%231A85FF-%23FFFFFF"
*)
deuteranopiaColorScheme = <| "R" -> RGBColor[1,0.7,0], "O" -> RGBColor[0.26,0.74,0.82], "G" -> RGBColor[0.31,1,0.89], "B"-> RGBColor[1,0.47,0], "W" -> White, "Y" -> RGBColor[0.1,0.52,1], "T" -> Transparent|>;
(* 
	protanopiaColorScheme permette di visualizzare cubo di Rubik che utilizza uno schema colori adatto (teoricamente) ai
	soggetti daltonici affetti da tritanopia.
	Note: Fonte per la generazione dello schema colore 
	"https://davidmathlogic.com/colorblind/#%23FF005E-%2300FF00-%23F5E61E-%233D3DF7-%23B9D88E-%23FFFFFF"
*)
tritanopiaColorScheme = <| "R" -> RGBColor[1,0,0.37], "O" -> RGBColor[0.96,0.9,0.12], "G" -> RGBColor[0,1,0], "B"-> RGBColor[0.24,0.24,0.97], "W" -> White, "Y" -> RGBColor[0.73,0.85,0.56], "T" -> Transparent|>;
(* Di default lo schema colore utilizzato \[EGrave] quello per le persone non affette da disturbi visivi. *)
currentColorScheme = defaultColorScheme;


(* ::Section:: *)
(*Getter*)


GetCurrentColorScheme[] := Return[currentColorScheme];


(* ::Section:: *)
(*Color scheme picker*)


ChangeColorScheme[colScheme_] := Module[{},
	currentColorScheme = Switch[colScheme,"Default",defaultColorScheme,"Protanopia",protanopiaColorScheme,"Deuteranopia",deuteranopiaColorScheme,"Tritanopia",tritanopiaColorScheme];
];


VisualizeColorSchemePicker[] := DynamicModule[{row1,row2, title, colorSchemeNames={"Default","Protanopia","Deuteranopia","Tritanopia"},cScheme="Default"},
	row1 = Row[{Style["Color Scheme",20]}];
	row2 = Row[{PopupMenu[Dynamic[cScheme],colorSchemeNames],Button["Confirm", ChangeColorScheme[cScheme]]}];
	Panel[Column[{row1,row2}]]
];


(* ::Section:: *)
(*Map tra lettere e colori*)


(* ::Subsubsection:: *)
(*Map delle facce di un cubo risolto a colore*)


cubeColors = <| "L" ->  Orange,"R" ->Red,"F"->Green, "B"-> Blue,"U"-> White, "D"->Yellow |>;


(* ::Subsubsection:: *)
(*Map da char a colore*)


(* ::Text:: *)
(*TODO: I colori devono essere parametrici (Opzione Daltonismo)*)


CharToColor[char_] := Module[{},
	Return[currentColorScheme[[char]]]
];


ColorToChar[col_] := Module[{},
	Return[Position[currentColorScheme,col][[1,1,1]]]
];


(* ::Subsubsection:: *)
(*Map della stringa a lista di colori*)


CubeStringToColorList[cube_] :=
	Table[CharToColor[c], {c, Characters[cube]}];


(* ::Section:: *)
(*End Package*)


End[]

EndPackage[]
