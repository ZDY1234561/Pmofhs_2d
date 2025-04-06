# Microsoft Developer Studio Project File - Name="Mgomain" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=Mgomain - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Mgomain.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Mgomain.mak" CFG="Mgomain - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Mgomain - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Mgomain - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Mgomain - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x804 /d "NDEBUG"
# ADD RSC /l 0x804 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "Mgomain - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x804 /d "_DEBUG"
# ADD RSC /l 0x804 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "Mgomain - Win32 Release"
# Name "Mgomain - Win32 Debug"
# Begin Source File

SOURCE=.\BAS1.FOR
# End Source File
# Begin Source File

SOURCE=.\BCF2.FOR
# End Source File
# Begin Source File

SOURCE=.\CHD1.FOR
# End Source File
# Begin Source File

SOURCE=.\DRN1.FOR
# End Source File
# Begin Source File

SOURCE=.\DTSSUBS.FOR
DEP_F90_DTSSU=\
	".\mgo.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\EVT1.FOR
# End Source File
# Begin Source File

SOURCE=.\FUNCTS.FOR
DEP_F90_FUNCT=\
	".\ga.inc"\
	".\mgo.inc"\
	".\modflow.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\GETMASS.FOR
DEP_F90_GETMA=\
	".\mgo.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\GHB1.FOR
# End Source File
# Begin Source File

SOURCE=.\HFB1.FOR
# End Source File
# Begin Source File

SOURCE=.\LKMT3.FOR
# End Source File
# Begin Source File

SOURCE=.\Mgomain.for
DEP_F90_MGOMA=\
	".\ga.inc"\
	".\mgo.inc"\
	".\modflow.inc"\
	{$(INCLUDE)}"imsl.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MODSUBS.FOR
DEP_F90_MODSU=\
	".\lkmt3.inc"\
	".\modflow.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\MT3DMS4.FOR
# End Source File
# Begin Source File

SOURCE=.\MT_ADV4.FOR
# End Source File
# Begin Source File

SOURCE=.\MT_BTN4.FOR
# End Source File
# Begin Source File

SOURCE=.\MT_DSP4.FOR
# End Source File
# Begin Source File

SOURCE=.\MT_FMI4.FOR
# End Source File
# Begin Source File

SOURCE=.\MT_GCG4.FOR
# End Source File
# Begin Source File

SOURCE=.\MT_RCT4.FOR
# End Source File
# Begin Source File

SOURCE=.\MT_SSM4.FOR
# End Source File
# Begin Source File

SOURCE=.\MT_UTL4.FOR
DEP_F90_MT_UT=\
	".\FILESPEC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\OPT2.FOR
DEP_F90_OPT2_=\
	".\ga.inc"\
	".\mgo.inc"\
	".\modflow.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\PCG2.FOR
# End Source File
# Begin Source File

SOURCE=.\pdflib.f
# End Source File
# Begin Source File

SOURCE=.\RCH1.FOR
# End Source File
# Begin Source File

SOURCE=.\RIV1.FOR
# End Source File
# Begin Source File

SOURCE=.\rnglib.f
# End Source File
# Begin Source File

SOURCE=.\SIP1.FOR
# End Source File
# Begin Source File

SOURCE=.\SOR1.FOR
# End Source File
# Begin Source File

SOURCE=.\STR1.FOR
# End Source File
# Begin Source File

SOURCE=.\UTL1.FOR
# End Source File
# Begin Source File

SOURCE=.\WEL1.FOR
# End Source File
# End Target
# End Project
