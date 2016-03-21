(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: install															*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 2012-2016 Feng Feng
	Copyright (C) 2012-2016 Rolf Mertig
	Copyright (C) 2015-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Installs FeynCalcFormLink *)

(* ------------------------------------------------------------------------ *)

InstallFeynCalcFormLink::nofc =
"Looks like you don't have FeynCalc installed. FeynCalcFormLink cannot work without FeynCalc, so please \
install it first.";

InstallFeynCalcFormLink::nofl =
"Looks like you don't have FormLink installed. FeynCalcFormLink cannot work without FormLink, so please \
install it first.";

InstallFeynCalcFormLink::notcomp =
"Your Mathematica version is too old. FeynCalcFormLink requires at least Mathematica 8. Installation aborted!";

InstallFeynCalcFormLink::failed =
"Download of `1` failed. Installation aborted!";

AutoOverwriteFeynCalcFormLinkDirectory::usage="AutoOverwriteFeynCalcFormLinkDirectory is an option of InstallFeynCalcFormLink. If \
set to True, the existing FeynCalcFormLink directory will be deleted without any further notice. The default
value None means that the user will be asked by a dialog. False means that the directory will be overwritten.";

FeynCalcFormLinkDevelopmentVersionLink::usage="FeynCalcFormLinkDevelopmentVersionLink is an option of InstallFeynCalcFormLink. It specifies the url \
to the main repository of FeynCalcFormLink. This repository is used to install the development version of FeynCalcFormLink.";

FeynCalcFormLinkStableVersionLink::usage="FeynCalcFormLinkStableVersionLink is an option of InstallFeynCalcFormLink. It specifies the url \
to the latest stable release of FeynCalcFormLink.";

InstallFeynCalcFormLinkDevelopmentVersion::usage="InstallFeynCalcFormLinkDevelopmentVersion is an option of InstallFeynCalcFormLink. If \
set to True, the installer will download the latest development version of FeynCalcFormLink from the git repository. \
Otherwise it will install the latest stable version.";

InstallFeynCalcFormLinkTo::usage="InstallFeynCalcFormLinkTo is an option of InstallFeynCalcFormLink. It specifies, the full path \
to the directory where FeynCalcFormLink will be installed.";

If[  $VersionNumber == 8,
(*To use FetchURL in MMA8 we need to load URLTools first *)
Needs["Utilities`URLTools`"];
];

If [Needs["FeynCalc`"]===$Failed,
	Message[InstallFeynCalcFormLink::nofc];
	Abort[]
];

If [Needs["FormLink`"]===$Failed,
	Message[InstallFeynCalcFormLink::nofl];
	Abort[]
];

Options[InstallFeynCalcFormLink]={
	AutoOverwriteFeynCalcFormLinkDirectory->None,
	FeynCalcFormLinkDevelopmentVersionLink->"https://github.com/FormLink/feyncalcformlink/archive/master.zip",
	(*for the moment there is no stable branch!*)
	FeynCalcFormLinkStableVersionLink->"",
	InstallFeynCalcFormLinkDevelopmentVersion->True,
	InstallFeynCalcFormLinkTo->FileNameJoin[{$UserBaseDirectory, "Applications","FeynCalcFormLink"}]
};

InstallFeynCalcFormLink[OptionsPattern[]]:=
	Module[{	unzipDir, tmpzip, gitzip, packageName, packageDir,
				FCGetUrl, strOverwriteFCdit, zipDir},

	If[OptionValue[InstallFeynCalcFormLinkDevelopmentVersion],
		gitzip = OptionValue[FeynCalcFormLinkDevelopmentVersionLink];
		zipDir = "feyncalcformlink-master",
		gitzip = OptionValue[FeynCalcFormLinkStableVersionLink];
		zipDir = "feyncalcformlink-stable"
	];

	packageName = "FeynCalcFormLink";
	packageDir = OptionValue[InstallFeynCalcFormLinkTo];

strOverwriteFCdit="Looks like FeynCalcFormLink is already installed. Do you want to replace the content \
of " <> packageDir <> " with the downloaded version of FeynCalcFormLink? If you are using any custom configuration \
files or add-ons that are located in that directory, please backup them in advance.";

	If[$VersionNumber < 8,
		Message[InstallFeynCalcFormLink::notcomp];
		Abort[]
	];

	If[$VersionNumber == 8,
		(*To use FetchURL in MMA8 we need to load URLTools first *)
		FCGetUrl[x_]:= Utilities`URLTools`FetchURL[x],
		FCGetUrl[x_]:= URLSave[x,CreateTemporary[]]
	];


	(* If the package directory already exists, ask the user about overwriting *)
	If[ DirectoryQ[packageDir],

		If[ OptionValue[AutoOverwriteFeynCalcFormLinkDirectory],

			Quiet@DeleteDirectory[packageDir, DeleteContents -> True],

			Null,
			If[ ChoiceDialog[strOverwriteFCdit,{"Yes, overwrite the " <> packageName <>" directory"->True,
				"No! I need to do a backup first."->False}],
				Quiet@DeleteDirectory[packageDir, DeleteContents -> True],
				Abort[]
			]
		]
	];

	(* Download FeynCalcFormLink tarball	*)
	WriteString["stdout", "Downloading FeynCalcFormLink from ", gitzip," ..."];
	tmpzip=FCGetUrl[gitzip];
	unzipDir= tmpzip<>".dir";
	WriteString["stdout", "done! \n"];

	(* Extract to the content	*)
	WriteString["stdout", "FeynCalcFormLink zip file was saved to ", tmpzip,".\n"];
	WriteString["stdout", "Extracting FeynCalcFormLink zip file to ", unzipDir, " ..."];
	ExtractArchive[tmpzip, unzipDir];
	WriteString["stdout", "done! \n"];

	(* Delete the downloaded file	*)
	Quiet@DeleteFile[tmpzip];

	(* Move the files to the final destination	*)
	WriteString["stdout", "Copying "<>packageName<>" to ", packageDir, " ..."];
	Print[FileNameJoin[{unzipDir,zipDir}]];
	CopyDirectory[FileNameJoin[{unzipDir,zipDir}],packageDir];
	WriteString["stdout", "done! \n"];
	(* Delete the extracted archive *)
	Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];

	WriteString["stdout", "done! \n"];

	WriteString["stdout","\nInstallation complete! To load FeynCalcFormLink, restart Mathematica \
and evaluate \n\n <<FeynCalcFormLink`"];



];
