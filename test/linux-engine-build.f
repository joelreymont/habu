\ linux-engine-build.f - `hb-build --target linux-aarch64` of the engine and of
\ a program with a callback, spawned from the tree's root because hb-build.f
\ reads its argv. Neither file it writes is run. A program whose top level binds
\ its callback is rejected: binding runs TASK's MAIN-BASE, a target word.

require src/lib/string.f
require src/lib/span.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fd.f
require src/lib/le.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/engine-candidate.f

package LINUX-ENGINE-BUILD-TEST
private

183 constant EM-AARCH64                  \ ELF e_machine, a u16 at byte 18

PATH-CAP BUFFER: DIR      variable DIR-U
PATH-CAP BUFFER: OUT-DIR  variable OUT-DIR-U
PATH-CAP BUFFER: EXE      variable EXE-U
PATH-CAP BUFFER: SRC      variable SRC-U
PATH-CAP BUFFER: PROG     variable PROG-U
20 SPAN-BUFFER: HEAD

: DIR$ ( -- str ) DIR DIR-U @ >str ;
: OUT-DIR$ ( -- str ) OUT-DIR OUT-DIR-U @ >str ;
: EXE$ ( -- str ) EXE EXE-U @ >str ;
: SRC$ ( -- str ) SRC SRC-U @ >str ;
: PROG$ ( -- str ) PROG PROG-U @ >str ;

: ELF-MAG ( -- str ) s\" \x7FELF" ;

: SHOW ( CMD:command n -- CMD:command )
   {: rc:n :}
   s" linux-engine-build: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

\ What to build, `--engine` or a source path, and the path it writes.
: BUILD ( CMD:command str str -- CMD:command n )
   {: w:str o:str :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+
   s" --target" CMD:ARG+  s" linux-aarch64" CMD:ARG+  w CMD:ARG+
   s" -o" CMD:ARG+  o CMD:ARG+
   s" HABU_OUT" OUT-DIR$ CMD:ENV+  s" TMPDIR" DIR$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

: HEAD-READ ( fd -- bool )
   HEAD FD:READ-EXACT MATCH FD:fill
      full OF true ENDOF
      eof OF drop false ENDOF
   ;MATCH ;

: READ-HEAD ( str -- bool )
   FS:OPEN-SIZED {: f:fd size:n found:bool :}
   found 0= if false exit then
   [: f HEAD-READ ;] [: f FD>N close ;] finally ;

: ENGINE-CASE ( CMD:command -- CMD:command )
   s" --engine" EXE$ BUILD {: rc:n :}
   rc 0<> if rc SHOW then
   s" the cross-build exits 0" T-LABEL  rc 0 T=
   rc 0<> if exit then
   s" the engine holds an ELF header's first 20 bytes" T-LABEL
   EXE$ READ-HEAD dup TTRUE 0= if exit then
   HEAD SPAN:$ STR:DATA {: h:^ :}
   s" the engine starts with the ELF magic" T-LABEL  h 4 >str ELF-MAG T$=
   s" the engine is for AArch64" T-LABEL  h 18 + LE:U16@ EM-AARCH64 T= ;

\ A callback's accessor compiles under the target's shadow only in a program:
\ the engine declares no CALLBACK:.
: PROGRAM-SOURCE$ ( -- str )
   S\" require src/lib/ffi-callback.f\nCALLBACK: CMP ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK\n: MAIN ( -- ) ;\n" ;

: PROGRAM-CASE ( CMD:command -- CMD:command )
   SRC$ PROGRAM-SOURCE$ FS:WRITE-ALL
   SRC$ PROG$ BUILD {: rc:n :}
   rc 0<> if rc SHOW then
   s" the callback program's cross-build exits 0" T-LABEL  rc 0 T=
   rc 0<> if exit then
   s" the program holds an ELF header's first 20 bytes" T-LABEL
   PROG$ READ-HEAD dup TTRUE 0= if exit then
   s" the program starts with the ELF magic" T-LABEL
   HEAD SPAN:$ STR:DATA 4 >str ELF-MAG T$= ;

\ The same program, binding its callback on line 3.
: BIND-SOURCE$ ( -- str )
   S\" require src/lib/ffi-callback.f\nCALLBACK: CMP ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK\nCMP TASK:SELF-CONTEXT FFI-CB:ENTRY drop\n: MAIN ( -- ) ;\n" ;

: BIND-CASE ( CMD:command -- CMD:command )
   SRC$ BIND-SOURCE$ FS:WRITE-ALL
   SRC$ PROG$ BUILD {: rc:n :}
   CMD:ERR$ s" error[E-TARGET-WORD]: a build cannot run target word `MAIN-BASE`"
      STR:CONTAINS? {: classed:bool :}
   CMD:ERR$ s" callback.f:3:23" STR:CONTAINS? {: placed:bool :}
   classed placed and  rc 0<> and 0= if rc SHOW then
   s" the binding program's cross-build fails" T-LABEL  rc 0<> TTRUE
   s" the rejection is E-TARGET-WORD" T-LABEL  classed TTRUE
   s" the rejection names MAIN-BASE on the binding's line" T-LABEL  placed TTRUE ;

: CHECKS ( -- ) [: ENGINE-CASE PROGRAM-CASE BIND-CASE ;] CMD:WITH-COMMAND ;

: REMOVE ( -- ) DIR$ FS:REMOVE-TREE ;

public

: RUN ( -- )
   T-RESET
   s" habu-linux-engine-build" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY  d STR:LEN DIR-U !
   DIR$ s" out" OUT-DIR FS:JOIN-PATH OUT-DIR-U !
   DIR$ s" hb" EXE FS:JOIN-PATH EXE-U !
   DIR$ s" callback.f" SRC FS:JOIN-PATH SRC-U !
   DIR$ s" callback" PROG FS:JOIN-PATH PROG-U !
   ['] CHECKS ['] REMOVE finally
   T-REPORT ;

;package

LINUX-ENGINE-BUILD-TEST:RUN
