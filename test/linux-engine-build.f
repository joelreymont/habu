\ linux-engine-build.f - `hb-build --target linux-aarch64` of the engine and of
\ a program with a callback, spawned from the tree's root because hb-build.f
\ reads its argv. Neither file it writes is run.

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

: DIR$ ( -- ptr u8 n ) DIR DIR-U @ ;
: OUT-DIR$ ( -- ptr u8 n ) OUT-DIR OUT-DIR-U @ ;
: EXE$ ( -- ptr u8 n ) EXE EXE-U @ ;
: SRC$ ( -- ptr u8 n ) SRC SRC-U @ ;
: PROG$ ( -- ptr u8 n ) PROG PROG-U @ ;

: ELF-MAG ( -- ptr u8 n ) s\" \x7FELF" ;

: SHOW ( CMD:command n -- CMD:command )
   {: rc:n :}
   s" linux-engine-build: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

\ What to build, `--engine` or a source path, and the path it writes.
: BUILD ( CMD:command ptr u8 n ptr u8 n -- CMD:command n )
   {: w:ptr wu:n o:ptr ou:n :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+
   s" --target" CMD:ARG+  s" linux-aarch64" CMD:ARG+  w wu CMD:ARG+
   s" -o" CMD:ARG+  o ou CMD:ARG+
   s" HABU_OUT" OUT-DIR$ CMD:ENV+  s" TMPDIR" DIR$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

: HEAD-READ ( fd -- bool )
   HEAD FD:READ-EXACT MATCH FD:fill
      full OF true ENDOF
      eof OF drop false ENDOF
   ;MATCH ;

: READ-HEAD ( ptr u8 n -- bool )
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
   HEAD SPAN:$ drop {: h:ptr :}
   s" the engine starts with the ELF magic" T-LABEL  h 4 ELF-MAG T$=
   s" the engine is for AArch64" T-LABEL  h 18 + LE:U16@ EM-AARCH64 T= ;

\ A callback's accessor compiles under the target's shadow only in a program:
\ the engine declares no CALLBACK:.
: PROGRAM-SOURCE$ ( -- ptr u8 n )
   S\" require src/lib/ffi-callback.f\nCALLBACK: CMP ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK\n: MAIN ( -- ) ;\n" ;

: PROGRAM-CASE ( CMD:command -- CMD:command )
   SRC$ PROGRAM-SOURCE$ FS:WRITE-ALL
   SRC$ PROG$ BUILD {: rc:n :}
   rc 0<> if rc SHOW then
   s" the callback program's cross-build exits 0" T-LABEL  rc 0 T=
   rc 0<> if exit then
   s" the program holds an ELF header's first 20 bytes" T-LABEL
   PROG$ READ-HEAD dup TTRUE 0= if exit then
   s" the program starts with the ELF magic" T-LABEL
   HEAD SPAN:$ drop 4 ELF-MAG T$= ;

: CHECKS ( -- ) [: ENGINE-CASE PROGRAM-CASE ;] CMD:WITH-COMMAND ;

: REMOVE ( -- ) DIR$ FS:REMOVE-TREE ;

public

: RUN ( -- )
   T-RESET
   s" habu-linux-engine-build" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   DIR$ s" out" OUT-DIR FS:JOIN-PATH OUT-DIR-U !
   DIR$ s" hb" EXE FS:JOIN-PATH EXE-U !
   DIR$ s" callback.f" SRC FS:JOIN-PATH SRC-U !
   DIR$ s" callback" PROG FS:JOIN-PATH PROG-U !
   ['] CHECKS ['] REMOVE finally
   T-REPORT ;

;package

LINUX-ENGINE-BUILD-TEST:RUN
