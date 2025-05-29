;fake gcc to add compiler flags and make c header includes available with inline c  
;Windows in the pb compliers folder rename gcc to gccreal and compile this as gcc.exe 
;note c errors won't be displayed after this just the exit code. 

;useage
;you can specify additional compiler flags on the command line as below 
;use an inline c comment !// and keyword gccflags followed by flags and end in ;  
;   !//gccflags -fno-schedule-insns -fno-schedule-insns2 ;
;if you need to add a header use inline c comment !// with #include keyword and header end in ;
;   !//#include /usr/include/portaudio.h ;  
; this will ensure that the macros and constants are availableto use from the header in c  
;if you want to compile with clang rather than gcc  
;   !//UseClang";
; windows 10,  windows xp

EnableExplicit 
OpenConsole() 

Global Flags.s,Fn,a,Command.s,Param.s,ParamCount,Find.s,CompilerHome.s,Pos,Pos1
Global Output.s,Gcc,Len,tCommand.s,error.s,usellvm,clangpath.s

clangpath="C:\llvm-mingw-20211002-msvcrt-x86_64\bin\clang.exe"

If ExamineEnvironmentVariables()
    
  CompilerHome = #PB_Compiler_Home
  
  If CompilerHome <> "" 
    ParamCount = CountProgramParameters()
    If ParamCount 
      For a = 0 To ParamCount-1 
        Param = ProgramParameter(a)
        Command + Param + " " 
      Next 
      Command + ProgramParameter(a) 
      
      If FileSize("purebasic.c")
        Fn = OpenFile(#PB_Any,"purebasic.c")  
        If Fn 
          Repeat 
            Flags.s = ReadString(Fn,#PB_UTF8)
            Pos =  FindString(Flags,"//gccflags",1) 
            Pos1 = FindString(Flags,"//#include",1) 
            usellvm = FindString(Flags,"//UseClang",1) 
            If Pos 
              tCommand.s = " " + Right(flags,Len(flags)-10) 
              pos = FindString(tCommand,";") 
              If pos   
                Command.s +  " " + Trim(Left(tCommand,pos-1))
              EndIf   
            EndIf   
            If Pos1
              tCommand = "-include "  + Right(flags,Len(flags)-10) 
              pos = FindString(tCommand,";") 
               If pos   
                Command.s + " " + Trim(Left(tCommand,pos-1))
              EndIf  
            EndIf
         Until Eof(Fn) 
         CloseFile(Fn)   
        EndIf 
      EndIf 
      
      MessageRequester("fake gcc",command + " " + GetCurrentDirectory()) 
      If usellvm
        Gcc = RunProgram(clangpath,command,GetCurrentDirectory(),#PB_Program_Open);
      Else   
        Gcc = RunProgram(CompilerHome + "Compilers\gccreal.exe",command,GetCurrentDirectory(),#PB_Program_Open);
      EndIf   
      If WaitProgram(Gcc) 
        End ProgramExitCode(Gcc)
      EndIf   
    EndIf 
  EndIf 
EndIf 



; IDE Options = PureBasic 6.00 Beta 4 (Windows - x64)
; CursorPosition = 3
; EnableXP
; Executable = ..\..\gcc.exe