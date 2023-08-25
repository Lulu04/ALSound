# Introduction

ALSound offer an easy way to play, capture and mix sounds for your FreePascal/Lazarus applications. It use OpenAL-Soft and LibSndFile libraries.

OpenAL-Soft is an improved software implementation of OpenAL, actively maintained by Christopher Robinson (https://github.com/kcat/openal-soft).
LibSndFile is a C library for reading and writing audio files (https://github.com/libsndfile/libsndfile).
You hardly have to know these libraries to use ALSound.

Tested under Linux, Windows and MacOS (x86-64 only).
May work on other platforms with the appropriate compiled libraries.

# Contents

'source' -> the source of ALSound.

'example' -> contains some examples to show how to integrate ALSound in your FreePascal/Lazarus programs. After compilation, the example executable are placed in folder "binary", sub-folder "$(TargetCPU)-$(TargetOS)"

'docs' -> In progress. Contains a copy of OpenAL documentation.

'binary' -> contains sub-folder for each actual supported platforms. The compiled binaries of OpenAL-Soft and LibSndFile are provided for Windows and Linux both i386 and x86-64 and can be found in their respective sub-folder. If you prefers to compile these libraries by yourself, please go to their respective github repository and follow the guideline.  
# placing libraries in your Lazarus project

ALSound loads dynamically the two libraries at startup:

## Under Windows
The dlls must be named 'soft_oal.dll' and 'sndfile.dll' and copied in the same folder (or in a sub-folder) as your executable.

## Under Linux
The libraries must be named 'libopenal.so' and 'libsndfile.so' and copied in the same folder (or in a sub-folder) as your executable.

## Under MacOS
The libraries must be named 'libopenal.dylib' and 'libsndfile.dylib'.
- If your application don't use a bundle, put a copy of the libraries in the same folder (or in a sub-folder) as your executable and in your Lazarus->Project options->Application->**uncheck** 'use Application Bundle for running and debugging'.
- If your application use a bundle, put a copy of the libraries in the Resources folder of the bundle (or sub-folder and in your Lazarus->Project options->Application->**check** 'use Application Bundle for running and debugging'.

> **All platforms: if you use a sub-folder your application must call ALSManager.SetLibrariesSubFolder(OnlyTheNameOfTheSubfolder) before calling ALSManager.LoadLibraries.**

# Thanks
Thanks to Christopher Robinson, the author and maintainer of OpenAL-Soft, for its help.  
Thanks to the LibSndFile team.
(https://github.com/libsndfile/libsndfile)  

Thanks to Fred van Stappen, the author of United OpenLib of Sound (UOS) (https://github.com/fredvs/uos) who wrote pascal binding for LibSndFile and PortAudio. This inspired me to write pascal binding for OpenAL-Soft and LibSndFile used by ALSound.


