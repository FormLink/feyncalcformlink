
Get @ FileNameJoin[{DirectoryName[FindFile[$Input]], "Config.m"}];

(* this is just for FeynCalc 8 : surpressing the startup messages *)
If[ Global`$FeynCalcStartupMessages === False,
    If[ OwnValues[System`FeynCalcCellPrint]==={},
        System`FeynCalcCellPrint = Hold;
        SetAttributes[System`FeynCalcCellPrint, Protected]
    ]
];
(* Created by the Wolfram Workbench 22.08.2012 *)

(* so FeynCalcFormLink loads FormLink and FeynCalc: *)

If[ FindFile["FeynCalc`"] =!= $Failed, (* for FC 9 *)
    Quiet @ BeginPackage["FeynCalcFormLink`", {"FormLink`", "FeynCalc`"}]
];
If[FindFile["HighEnergyPhysics`FeynCalc`"] =!= $Failed,
   Quiet @ BeginPackage["FeynCalcFormLink`", {"FormLink`", "HighEnergyPhysics`FeynCalc`"}]
];

If[ {FindFile["FeynCalc`"], FindFile["HighEnergyPhysics`FeynCalc`"] }=== {$Failed,$Failed},
    Print["FeynCalc  not found, please install FeynCalc first ! Exiting now "] ;
    Pause[1]; Quit[]
];

(* This can be set in Config.m *)


$UseFormEpsConvention::usage = "$UseFormEpsConvention is set to True by default and sets FeynCalc $LeviCivitaSign=-I , i.e., the
same convention for traces involving Gamma[5] as in FORM, when FeynCalcFormLink is loaded. $UseFormEpsConvention should be set
in the Config.m file only.";

Functions::usage = "Functions is an option to FeynCalcFormLink. If set to \"CFunctions\", then all non-System` functions,
except those present in $M2Form and some FeynCalc functions, are automatically declared CFunctions in FORM.
If Functions is set to \"Functions\", then they declare noncommutative functions \"Functions\" in FORM.";

ExtraDeclare::usage = "ExtraDeclare is an option to FeynCalcFormLink, a list
containing extra decalaration which will be sent to FORM."

FC2Form::usage = "FC2Form[expr] translates expr to FORM.";

FeynCalcFormLink::usage = "FeynCalcFormLink[expr] translates the FeynCalc expression expr to FORM, calculates it, pipes it back to Mathematica and translates it to FeynCalc syntax."

Form2FC::usage = "Form2FC[str] translates str to FeynCalc syntax. Form2FC has an option Replace.";

IDStatements::usage = "IDStatements is an option to FC2Form and can be set to a string or a list of strings like \"id k1.k1=mass^2\".";

(*
Multiply::usage="Multiply is an option to FC2Form."
*)

SetSF::usage = "SetSF set the default Format output type to StandardForm."

SetTF::usage = "SetTF sets the default Format output type to TraditionalForm (for FeynCalc formatting).";

(* this is not *necessary*, but helps for ToString ... *)
(* no need to give usage messages, since it is just for convenience of translating from M to FORM via ToString *)
e$;
i$;
d$;
g$;
gi$;
g5$;
g6$;
g7$;

Protect[e$, i$, d$, g$,gi$,g5$,g6$,g7$];


Begin["`Private`"]
(* Implementation of the package *)
(* ::Package:: *)

If[ FeynCalcFormLink`$UseFormEpsConvention === True,
    $LeviCivitaSign = -I
];

$thiscontext = Context[];

SetSF :=
    (SetOptions[#, "CommonDefaultFormatTypes" -> {"Input" -> StandardForm,
    "InputInline" -> StandardForm, "Output" -> StandardForm,
    "OutputInline" -> StandardForm, "Text" -> TextForm,
    "TextInline" -> TraditionalForm}] & /@ {$FrontEnd, $FrontEndSession});

SetTF := If[$FrontEnd =!=Null,
    SetOptions[$FrontEndSession, "CommonDefaultFormatTypes" -> {"Input" -> StandardForm,
    "InputInline" -> StandardForm, "Output" -> TraditionalForm,
    "OutputInline" -> StandardForm, "Text" -> TextForm,
    "TextInline" -> TraditionalForm}]];


If[ !ValueQ[FCI],
    MakeContext[Dimension, FCI, FCE, FreeQ2, MomentumExpand, DiracGammaExpand, ScalarProductExpand,
    LorentzIndex, Momentum, NonCommFreeQ, Eps, Pair, DiracGamma, DiracTrace, MTD, SUNSimplify, SUNT, TR, DotSimplify]
];

(* this if missing in some FeynCalc versions *)
 HighEnergyPhysics`FeynCalc`fctools`FeynCalcExternal`Private`eps = Eps;

 (* FeynCalc 8 has this PageWidth -> 137 for ToString. However, in Mathematica 9 this gives slight trouble *)
 SetOptions[ToString, PageWidth -> Infinity];

(* ::Section:: *)
(*FC2Form & Form2FC*)

asciicheck[w_Symbol] :=
    asciicheck[w] = Max[ToCharacterCode[ToString[w]]]<128;

Options[FC2Form] = {Functions -> "CFunctions",
                    Dimension -> Automatic,
                    ExtraDeclare->{},
                    IDStatements -> {},
(*                   Multiply -> 1,*)
                    Print -> True,
                    Replace->{}
                   };

FC2Form[exp_,OptionsPattern[]] :=
    Module[ {script = {},VF,fci,f2mrules,lor,mom,tmp,Rlo,Rmo,iRlo,iRmo,in = 0,imax = 0, formmom, formvecs, formlor, forminds, symbols, dim, allDlors, extravars, idstatements,  subscripts,
            extrasubsubst, extrasubstback, print, revrules, flo, dirmomto4, time, addgammaid,
            sym, Rsy, iRsy, formlors, specheads, cfuns(*, mult*)},
        dim = OptionValue[Dimension];
        f2mrules = {(*$thiscontext -> "", *)
                  " "->"",
                  "i$"->"i_",
                  "e$["->"e_(",
                  "d$["->"d_(",
                  "g$["->"g_(",
                  "gi$["->"gi_(",
                  "g5$["->"g5_(",
                  "g6$["->"g6_(",
                  "g7$["->"g7_(",
                  "["->"(",
                  "]"->")",
                  "**"->"*",
                  "\\"->""
                  (*,"\n"->""*)
        };
        (*]]]]]]]   these brackets are just for vi ... *)
        print = If[ OptionValue[Print],
                    print = Print,
                    print = Hold
                ];

        If[Global`$FLDebug,
           print["simple FeynCalc preparation start"];
        ];
        fci = exp // FCI // SUNSimplify // MomentumExpand // DiracGammaExpand // ScalarProductExpand;
        If[Global`$FLDebug,
           print["simple FeynCalc preparation done"];
        ];

        (* there can be Subscript[]'s *)
        subscripts = Cases[fci, _Subscript, -1] // DeleteDuplicates;
        extrasubsubst = Thread[ subscripts -> ( subscripts /. Subscript[a_, b_] :> ToExpression[ToString[a] <> "sub" <> ToString[b]] ) ];
        extrasubstback = Reverse /@ extrasubsubst;
        fci = fci /. extrasubsubst;
        (*
        gamma67opt = {(1-DiracGamma[5]) :> (2 DiracGamma[7]), (1+DiracGamma[5]) :> (2 DiracGamma[6])};
        fci = fci /. gamma67opt;
        *)
        idstatements = StringTrim /@ Flatten[OptionValue[IDStatements]/. s_String:>
                       StringTrim[StringReplace[s, {"\n"->"", "\[IndentingNewLine]"->"","\t"->""}]] /. s_String :> (#<>";"&/@StringSplit[s, ";"])];
        (* If[idstatements =!= {}, print["idstatements = ", idstatements//InputForm] ]; *)
        (* get extra variables from the right hand sides of id statements *)

        (* this is the list of LorentzIndex which will not get replaced by lor's *)
        formlor = Cases[fci, LorentzIndex[m_Symbol /; asciicheck[m],___],{0,Infinity}]//Union;
        forminds = formlor /. LorentzIndex[m_,___]:>m;

        (* in case someone entered Momentum[p] and Momentum[p,D], do this:*)
        dirmomto4[{a___, Momentum[pe_], b___, Momentum[pe_,de_Symbol], c___}] :=
            dirmomto4[{a,Momentum[pe],b,c}];
        dirmomto4[x_List] :=
            x;
        (* this is the list of Momenta which will not get replaced by mom's *)
        formmom = Union[Cases[fci, Momentum[p_Symbol /; asciicheck[p],___],{0,Infinity}]] // dirmomto4;
        formvecs = Union[ formmom /. Momentum[p_Symbol,___]:> p ];

        (* this is the list of Momenta which will get replaced by mom's *)
        mom = Complement[Cases[fci, _Momentum,{0,Infinity}]//Union, formmom];
        lor = Complement[Cases[fci, _LorentzIndex,{0,Infinity}]//Union, formlor];
        formlors = lor /. LorentzIndex[m_,___]:>m;
        (*print["formlors = ", formlors];*)
        flo = Join[formlor, lor];
        If[ dim ===Automatic,
            allDlors = Union[flo /. LorentzIndex[_]:> Sequence[] /. LorentzIndex[_,di_Symbol] :> di];
            If[ allDlors === {},
                dim = 4,
                dim = allDlors[[1]]
            ];
        ];

    (* find out which symbols are there *)
    (* this will give a list of symbols which are not in the System` context, which should probably be fine almost always *)
    (* this is the list of symbols which will not get replaced by sym's *)
        symbols = Complement[Select[Select[Cases[fci, _Symbol, -1], (Context[Evaluate[#]]=!="System`")&], asciicheck], formvecs, forminds];
        (* this is the list of symbols which will not get replaced by sym's, e.g., greek symbols *)
        If[ StringQ[OptionValue[Functions]],
            sym = Complement[Select[Select[Cases[fci, _Symbol, -1], (Context[Evaluate[#]]=!="System`")&], Function[x, Not[asciicheck[x]]]],
                                    formvecs, forminds, formlors];
            Rsy = Thread[ sym -> Array[Symbol["sym" <> ToString[#]] &, Length@sym]];
            iRsy = Reverse /@ Rsy;,
            sym = Rsy = iRsy  = {}
        ];

        (*get everything which is to be translated to CFunctions in FORM *)
        (* Eps, LorentzIndex, Momentum, DiracTrace and DiracGamma get special treatment
        *)
        specheads = Join[{LorentzIndex, Momentum, Eps, DiracTrace, DiracGamma, Pair}, $M2Form[[All,1]]];
        cfuns = Head /@ Cases[{(*mult,*)fci}, (h_Symbol /; (!MemberQ[specheads,h] && Context[h] =!= "System`"))[args__], -1];
        cfuns = Union[cfuns];
        extravars =
         Cases[ToExpression /@
           Select[
             Select[idstatements, StringMatchQ[#, "id*=*;"] &] /.
              s_String :> StringReplace[StringSplit[s, "="][[2]], ";" -> ""],
            SyntaxQ], _Symbol, -1];
        symbols = Union[symbols,extravars,Rsy[[All,2]]];
        If[ dim =!= 4,
            PrependTo[symbols,dim]
        ];
        If[Global`$FLDebug,
           print["symbols = ", symbols];
        ];
        Rlo = Thread[ lor -> Array[Symbol["lor" <> ToString[#]] &, Length@lor]];
        iRlo = Reverse /@ Rlo;
        If[ forminds=!={},
            iRlo = Join[iRlo, Thread[forminds -> formlor]]
        ];
        Rmo = Thread[mom -> Array[Symbol["mom" <> ToString[#]] &, Length@mom]];
        iRmo = Reverse /@ Rmo;
        If[ formvecs=!={},
            iRmo = Join[iRmo, Thread[formvecs -> formmom]]
        ];
        tmp = fci /. Eps[a___]:> (1/($LeviCivitaSign i$) e$[a]) /. Complex[a_,b_]:> a+b i$;
        tmp = tmp /. {Pair[a_Momentum,b_LorentzIndex | b_Momentum ]:>a[b],
                      Pair[a_LorentzIndex,b_LorentzIndex]:>d$[a,b],
        (* careful: different conventions in Form and FeynCalc form DiracGamma[6] and DiracGamma[7] *)
                      DiracGamma[5]->g5$[],DiracGamma[6]->(g6$[]/2),DiracGamma[7]->(g7$[]/2)
                     };
        tmp = tmp /. {DiracGamma[6] -> (2 DiracGamma[6]), DiracGamma[7] -> (2 DiracGamma[7])};
        tmp = tmp /. DiracGamma[z_,___] :> g$[z]/. Join[Rlo, Rmo, Rsy] /. {Momentum[a_Symbol,___] :> a, LorentzIndex[m_Symbol,___] :>m};
        tmp = Expand[tmp,_DiracTrace];
        tmp = Distribute[VF[tmp]];

        time = AbsoluteTime[];
        If[Global`$FLDebug,
           Global`TMPP = tmp];

        addgammaid[z_] :=
            Block[ {zplus,holdplus,plussubli},
                zplus = Select[
                   Cases[z, _Plus, -1] // Union, !FreeQ2[#, {_g$, _g5$,_g6$,_g7$}] &] /. {a__} :> (holdplus @@@ {a});
                If[ zplus === {},
                    z,
                    plussubli =
                     Thread[zplus -> (zplus /.
                          holdplus[a___, b_ /; FreeQ2[b, {_g$, _gi$, _g5$, _g6$, _g7$}], c___] :>
                           holdplus[a, b*gi$[], c])] /. Plus -> holdplus;
                    z /. Plus -> holdplus //. plussubli /. holdplus -> Plus
                ]
            ];
        tmp = FixedPoint[ addgammaid, tmp, 10];

        (* *)
        tmp = tmp /. DiracTrace[a__]^2 :> DiracTrace[a]**DiracTrace[a];
If[Global`$FLDebug, Global`TMPT1=tmp];
        tmp = tmp /. VF[vf_]:>(in = 0;
                               vf/. DiracTrace->Hold[DiracTrace] /.
                               Hold[DiracTrace][y_ /; FreeQ2[y,{_g$,_g5$,_g6$,_g7$,_gi$}]] :> ( in++;
                                                                                                If[ imax<in,
                                                                                                    imax = in
                                                                                                ];
                                                                                                (y * gi$[in])
                                                                                               ) /.
                               Hold[DiracTrace][x_]:>(in++;
                                                      If[ imax<in,
                                                          imax = in
                                                      ];
                                                      x/.(gx:(gi$|g$|g5$|g6$|g7$))[y___]:>gx[in,y]));
If[Global`$FLDebug, Global`TMPT2=tmp];

        tmp = tmp /. Dot->NonCommutativeMultiply;
        tmp = tmp/.OptionValue[Replace];
        tmp = ToString[tmp,InputForm, PageWidth -> FormLink`$FormPageWidth];
        tmp = StringReplace[tmp, Join[$M2Form, f2mrules]];
        If[ forminds =!= {},
            AppendTo[script, "Indices " <> Apply[StringJoin, Riffle[ToString/@forminds,","]]<>";"]
        ];
        If[ formvecs =!= {},
            AppendTo[script, "Vectors " <> Apply[StringJoin, Riffle[ToString/@formvecs,","]]<>";"]
        ];
        If[ dim =!= 4,
            PrependTo[script,"Dimension " <> ToString[dim] <>";"]
        ];
        If[ MatchQ[symbols, {_}],
            PrependTo[script, "Symbol " <> ToString[symbols[[1]] ] <>";"]
        ];
        If[ MatchQ[symbols,{_,__}],
            PrependTo[script, "Symbols " <> Apply[StringJoin, Riffle[ToString/@symbols,","]]<>";"]
        ];
        Scan[AppendTo[script,#]&,OptionValue[ExtraDeclare]];
        If[ mom =!= {},
            AppendTo[script,"AutoDeclare Vector mom;"]
        ];
        If[ lor =!= {},
            AppendTo[script,"AutoDeclare Index lor;"]
        ];
        If[ sym =!= {},
            AppendTo[script,"AutoDeclare Symbol sym;"]
        ];
        If[ cfuns =!= {},
            AppendTo[script,OptionValue[Functions]<> " " <> Apply[StringJoin, Riffle[ToString/@cfuns,","]]<>";"]
        ];
        AppendTo[script, "Format Mathematica;"];
        AppendTo[script,"L resFL = ("<>tmp<>");"];
        Table[AppendTo[script, StringJoin["trace", If[ dim ===4,
                                                       "4",
                                                       "n"
                                                   ], ",",ToString[in],";"]],{in,1,imax}];
        AppendTo[script,"contract 0;"];
        AppendTo[script,".sort;"];
        If[ idstatements =!= {},
            Scan[AppendTo[script,#]&,idstatements];
            AppendTo[script,".sort;"];
        ];
        AppendTo[script, "#call put(\"%E\", resFL)"];
        AppendTo[script, "#fromexternal"];
        (*AppendTo[script,".sort"];*)
        (*AppendTo[script,".end"];*)
        revrules = Flatten[{Table[Reverse[x],{x,OptionValue[Replace]}],iRlo,iRmo, iRsy}]//Union;
        {script,Join[extrasubstback, revrules]}
    ];

(* FCE -> True means that the result is converted to FeynCalcExternal format, i.e., no Pair's, just FV's and SP's etc. *)



(* needed for replacing [ ] the right way; see also in Config.m (of FormLinnk.m) :
      FormLink`$Form2M
*)
holdidentityrep = {
	(h_Symbol[Identity][x__]) :> (ToExpression[StringReplace[ToString[h], "Hold" -> ""]][
      x]) /; (StringLength[StringReplace[ToString[h], "Hold" -> ""]] > 0),
      Hold[Identity][x_] :> x(*, Hold[Identity][x_,y__] :> Hold[x,y]*)
};

Options[Form2FC] = {Replace -> {}, FCE -> True};
Form2FC[exp_String] := Form2FC[exp,{}];
Form2FC[exp_String, ReplaceBack:(_Rule|{___Rule}), OptionsPattern[] ] :=
    Module[ {rule, tmp, res, rep, finalreplacements, m2rulerev},
        rep = Flatten[{OptionValue[Replace]}];
        tmp = "("<> exp <>  ")";
        m2rulerev = $Form2M;
        rule = Join[ Select[rep, MatchQ[#, _String -> _String]&], m2rulerev ];
        tmp = StringReplace[tmp,rule];
        tmp = ToExpression[tmp,TraditionalForm, Hold] /. ReplaceBack;
        tmp = tmp /. { (*d$[z___]:>Pair[z], e$[a___]:>-I Eps[a], i$->I,*)
                       Dot[pe_,Power[qu_,n_Integer]]:>Dot[pe,qu]^n };
        tmp = tmp /. ReplaceBack;
        tmp = tmp /. Momentum[mo__][lo_LorentzIndex]:>Pair[Momentum[mo],lo];
        If[!FreeQ[tmp, Dot],
        	tmp = tmp /. Dot[pe_Momentum, qu_Momentum]:>Pair[pe,qu];
        ];
        finalreplacements = Select[rep, !MatchQ[#, _String -> _String]& ];
        (* the idea is to use DotSimplify later on, but we have to do this first: *)
        Block[{Times},
        tmp = ReleaseHold[tmp] /. holdidentityrep;
        If[!NonCommFreeQ[tmp], tmp= DotSimplify[tmp/. Times -> Dot]];
        ];
        If[ OptionValue[FCE]===True, finalreplacements = FCE[finalreplacements] ];
        If[ OptionValue[FCE]===True, tmp = FCE[tmp] ];
        If[!FreeQ[tmp, Dot], tmp= tmp/. FormLink`$DotPowerFix];
        res = tmp /. finalreplacements;
        res
    ];

Options[FeynCalcFormLink] = {
                             Functions -> "CFunctions", FCE->True,
                             FormSetup :> $FormSetup,
                             Form2FC -> Form2FC,
                             ExtraDeclare -> {},
                             IDStatements -> {},
                             Print -> True,
                             Replace -> {},
                             Style -> {Darker@Darker@N[Orange], FontFamily -> "Courier" }};
FeynCalcFormLink[exprin_, opts : OptionsPattern[]] :=
    Module[ {expr, (*fac = 1,*) fm1, fm2, frres, print, formtimestart,
    res, totaltimestart, cprint},
        cprint = Function[p, If[ p === False, Hold, p /. True -> CellPrint ]]@OptionValue[Print];
        totaltimestart = AbsoluteTime[];
        Catch[

        expr = FCI[exprin] /. DiracTrace[bla_ /; FreeQ2[bla,{DiracGamma, SUNT}] ] :> (bla DiracTrace[1]);
    (*
    somehow this whole factoring out and multiplying back is not easy to do. TODO: fix this sometime later
    If[(!FreeQ[expr, DiracTrace] ) &&  (!FreeQ[expr, DiracGamma]) &&
        (Head[expr] === Times), fac = Select[expr, FreeQ2[#,{ DiracGamma, LorentzIndex }]&] /. DiracTrace -> TR ; expr = expr/fac, fac = 1];
    *)
        {fm1, fm2} =
         FC2Form[expr, IDStatements -> OptionValue[IDStatements],
             Replace -> OptionValue[Replace],
        (*       Multiply -> fac,*) (* that seems not to be right ... *)
             Functions -> OptionValue[Functions],
          ExtraDeclare -> OptionValue[ExtraDeclare]];
        print = OptionValue[Print];
        If[ print === True,
            print = Print,
            print = Hold
        ];
        If[ Global`$FLDebug,
        	print["DEBUGF1 = ",fm1];
        	print["DEBUGF2 = ",fm2];
        	Global`DEBUGF1 = fm1;    Global`DEBUGF2 = fm2; ];
        (*
        If[fac =!= 1,
            print["The Form program generated by FC2Form is, up to the global factor  ", fac//TraditionalForm," : "],
            print["The Form program generated by FC2Form is: "];
        ];
        *)

        (* CellPrint here enables easy copy and paste in the FrontEnd ... *)

        If[$FrontEnd =!= Null,
        cprint@Cell[TextData[ExportString[fm1, "Text"]], FormLink`$FormOutputCellStyle, OptionValue[Style]],
        print@ExportString[fm1, "Text"]
        ];
        FormStart[Print -> OptionValue[Print]];
        formtimestart = AbsoluteTime[];
        print["Piping the script to FORM and running FORM"];
        FormWrite[StringReplace[fm1,"\n"->""]];
        frres = FormRead[];
        If[ !StringFreeQ[frres,{"gi_","g_"}],
            print["there are still gi_ or g_ expressions in the FORM output. Did you forget to put DiracTrace around the FeynCalcFormLink input?. Returning the input."];
            Throw[exprin]
        ];
        Uninstall[FormLink`$FormLink];
        print["Time needed by FORM : ", Round[(AbsoluteTime[]-formtimestart) 1000]/1000.," seconds. FORM finished. Got the result back to Mathematica as a string."];
        print["Start translation to Mathematica / FeynCalc syntax"];
        If[ Global`$FLDebug,
            Global`FRRES = frres;
            Global`FM2 = fm2
        ];
        If[ OptionValue[Form2FC]===Form2FC,
            res = Form2FC[frres, fm2, FCE -> OptionValue[FCE], Replace -> OptionValue[Replace]],
            res = OptionValue[Form2FC][frres]
        ];
        print["Total wall clock time used: ", Round[(AbsoluteTime[]-totaltimestart) 100]/100. ," seconds. Translation to Mathematica and FeynCalc finished."];
        If[ !FreeQ[res,NonCommutativeMultiply],
            res = DotSimplify[res/. NonCommutativeMultiply -> Dot]
        ];
        res = Switch[OptionValue[FCE], True, FCE[res], False, FCI[res],  _Symbol, OptionValue[FCE][res]];
        res
]
    ];

(* Think about this for the next FeynCalc version, but for now :*)
SetTF;

End[]

EndPackage[]
