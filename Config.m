(* Mathematica package *)


(* these settings determine what to load when FeynCalc starts *)
If[ !ValueQ[Global`$LoadFeynArts],
	Global`$LoadFeynArts = False
];
If[ !ValueQ[Global`$LoadPhi],
	Global`$LoadPhi = False
];
If[ !ValueQ[Global`$LoadTARCER],
	Global`$LoadTARCER = False
];
(* switch off startup messages by default: *)
If[ !ValueQ[Global`$FeynCalcStartupMessages],
	Global`$FeynCalcStartupMessages = False
];

If[ !ValueQ[Global`$FormLinkStartupMessages],
	Global`$FormLinkStartupMessages = False
];


(* this setting will set the FeynCalc global variable $LeviCivitaSign = -I, such that
FeynCalc uses the same convenction for traces involving Gamma[5]. *)
If[ !ValueQ[FeynCalcFormLink`$UseFormEpsConvention],
	FeynCalcFormLink`$UseFormEpsConvention = True
];

