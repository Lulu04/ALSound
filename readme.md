Introduction
------------

Based on OpenAL-Soft and Libsndfile, ALSound offer an easy way to play, capture and mix sounds for your FreePascal/Lazarus applications.

OpenAL-Soft is an improved software implementation of OpenAL, actively maintained by Christopher Robinson (https://github.com/kcat/openal-soft).
LibSndFile is a C library for reading and writing audio files (https://github.com/libsndfile/libsndfile).
You hardly have to know these libraries to use ALSound.

ALSound is also compatible with old version of OpenAL but advanced features may not work.
If a feature is not available, the requested action is simply ignored and in some cases, an error message is returned (like the design of OpenAL). Not all new features of OpenAL-Soft are implemented.

Tested under Linux and Windows, i386 and x86-64.
May work on other platforms with the appropriate compiled libraries.

For FreePascal/Lazarus
lulu - 2022


Contents
--------

'source' -> the source of ALSound.

'example' -> contains some examples to show how to integrate ALSound in your FreePascal/Lazarus programs.

'docs' -> In progress. Contains a copy of OpenAL documentation.

'binary' -> contains sub-folder for each actual supported platforms. The compiled binaries of OpenAL-Soft and LibSndFile are provided for Windows and Linux both i386 and x86-64 and can be found in their respective sub-folder. If you prefers to compile these libraries by yourself, please go to their respective github repository and follow the guideline.
After compilation, all example's executables are placed in the appropriate sub-folder by Lazarus.


Dynamic linking libraries
-------------------------

ALSound loads and links automatically the libraries at startup. As under Windows, a copy of the libraries should be in the same folder as your executable.

Under Windows, ALSound try to load OpenAL-Soft using:
	1) Application_Location\soft_oal.dll -> try to find it in application folder
	2) soft_oal.dll -> try to find it in system folder
	3) OpenAL32.dll -> try to load legacy implementation of OpenAL.

Under Windows, ALSound try to load LibSndFile using:
	1) application_Location\sndfile.dll -> try to find it in application folder
	2) sndfile.dll -> try to find it in system folder


Under Linux, ALSound try to load OpenAL-Soft using:
	1) Application_Location/libopenal.so -> try to find it in application folder
	2) libopenal.so -> try to find an installed version
	3) libopenal.so.1 -> try to find an older installed version
	4) libopenal.so.0 -> try to find an older installed version

Under Linux, ALSound try to load LibSndFile using:
	1) Application_Location/libsndfile.so -> try to find it in application folder
	2)libsndfile.so -> try to find an installed version


!!NOT TESTED!! Under Mac, ALSound try to load OpenAL-Soft using:
	1) BundlePath+'/'+BundleName+'/Contents/Resources/libopenal.dylib' -> try to find it in Resources folder of application bundle.
	2) libopenal.dylib -> try to load without any path
	3) /System/Library/Frameworks/OpenAL.framework/OpenAL -> try to load desperately an old version on old machine


Building libraries from source
------------------------------

If you choose to build OpenAl-Soft and LibSndFile from source, please rename the obtained binaries to:
	soft_oal.dll and sndfile.dll            for Windows
	libopenal.so and libsndfile.so          for Linux

ALSound hasn't been tested yet on Mac. If you decide to do, after building the libraries, rename them to:
	libopenal.dylib and libsndfile.dylib
and put a copy in your application bundle, sub-folder Resources.


Thanks
------

Thanks to Christopher Robinson, the author and maintainer of OpenAL-Soft, for its help.

Thanks to Fred van Stappen, the author of United OpenLib of Sound (UOS) (https://github.com/fredvs/uos) who wrote pascal binding for LibSndFile and PortAudio. This inspired me to write pascal binding for OpenAL-Soft and LibSndFile used by ALSound.




