\ A saved production AOT maker builds a stripped application from a fresh
\ process; a second selection reuses the same runnable maker image.
\ Run: bin/hb --load tools/hb-build-aot-image-test.f

require tools/hb-build-test-lib.f
require tools/aot-maker-image.f
require src/lib/process-cwd.f

package HB-BUILD-CLI

create HBT-MAKER-IMAGE FS-PATH-CAP allot
variable HBT-MAKER-IMAGE-U
create HBT-MAKER-HB FS-PATH-CAP allot
create HBT-MAKER-TOOL FS-PATH-CAP allot
create HBT-MAKER-SHADOW FS-PATH-CAP allot
create HBT-MAKER-OLD FS-PATH-CAP allot
create HBT-MAKER-DIRECT-OUT FS-PATH-CAP allot
variable HBT-MAKER-HB-U
variable HBT-MAKER-TOOL-U
variable HBT-MAKER-SHADOW-U
variable HBT-MAKER-OLD-U
variable HBT-MAKER-DIRECT-OUT-U

: HBT-MAKER-IMAGE$ ( -- ptr u8 n )
   HBT-MAKER-IMAGE HBT-MAKER-IMAGE-U @ ;

: HBT-MAKER-HB$ ( -- ptr u8 n )
   HBT-MAKER-HB HBT-MAKER-HB-U @ ;

: HBT-MAKER-TOOL$ ( -- ptr u8 n )
   HBT-MAKER-TOOL HBT-MAKER-TOOL-U @ ;

: HBT-MAKER-SHADOW$ ( -- ptr u8 n )
   HBT-MAKER-SHADOW HBT-MAKER-SHADOW-U @ ;

: HBT-MAKER-OLD$ ( -- ptr u8 n )
   HBT-MAKER-OLD HBT-MAKER-OLD-U @ ;

: HBT-MAKER-DIRECT-OUT$ ( -- ptr u8 n )
   HBT-MAKER-DIRECT-OUT HBT-MAKER-DIRECT-OUT-U @ ;

: HBT-MAKER-PATH-CHILD ( -- )
   SCRIPT-ARGC 3 <> if E-TBL-BOUNDS throw then
   1 SCRIPT-ARGV$ BUILD-CACHE:ROOT!
   2 SCRIPT-ARGV$ BUILD-FIXPOINT:BF-ENGINE!
   HBB-MAKER-DEFAULT-MS AOT-MAKER-IMAGE:PATH$ type cr ;

: HBT-MAKER-CHILD-SETUP ( -- )
   SCRIPT-ARGC 4 <> if E-TBL-BOUNDS throw then
   HBB-RESET-OPTIONS
   1 SCRIPT-ARGV$ BUILD-CACHE:ROOT!
   1 SCRIPT-ARGV$ BUILD-FIXPOINT:BF-TMP!
   2 SCRIPT-ARGV$ BUILD-FIXPOINT:BF-ENGINE!
   3 SCRIPT-ARGV$ HBB-SRC!
   HBB-MAKER-DEFAULT-MS HBB-MAKER-TIMEOUT-MS ! ;

: HBT-MAKER-RUN-CHILD ( -- )
   HBT-MAKER-CHILD-SETUP
   HBB-RUN-MAKER-CMD HBT-MAKER-CAPTURE>N
      {: outn:n errn:n rc:n :}
   rc 0<> if HBB-OUT-BUF outn type HBB-ERR-BUF errn type rc throw then
   outn 0<> errn 0<> or if E-BUILD-PATH throw then
   s" hb-aot-got" BUILD-FIXPOINT:BF-A$ EXECUTABLE? 0= if E-BUILD-PATH throw then ;

: HBT-MAKER-COUNT-CHILD ( -- )
   HBT-MAKER-CHILD-SETUP
   HBB-MAKER-DEFAULT-MS AOT-MAKER-IMAGE:PATH$
      HBT-MAKER-IMAGE HBT-MAKER-IMAGE-U HBT-COPY!
   HBB-CMD-RESET
   s" --" >LEN PROC-ARGV+
   HBB-SRC$ >LEN PROC-ARGV+
   s" 0" >LEN PROC-ARGV+
   s" 1" >LEN PROC-ARGV+
   HBT-MAKER-IMAGE$ >LEN
   S\" 1 set-tier\nAOT-MAKER:OPEN\nAOT-MAKER:RUN\n" >LEN
   HBB-OUT-BUF HBB-CAPTURE-CAP >LEN HBB-ERR-BUF HBB-CAPTURE-CAP >LEN
   HBB-MAKER-DEFAULT-MS >MS RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
      HBT-MAKER-CAPTURE>N {: outn:n errn:n rc:n :}
   rc 0<> if HBB-OUT-BUF outn type HBB-ERR-BUF errn type rc throw then
   errn 0<> if E-BUILD-PATH throw then
   HBB-OUT-BUF outn s" hb-build: file unit hits" CONTAINS? 0= if E-BUILD-PATH throw then
   s" hb-aot-got" BUILD-FIXPOINT:BF-A$ EXECUTABLE? 0= if E-BUILD-PATH throw then ;

: HBT-MAKER-CHILD? ( -- bool )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s" path" STR= exit then
   false ;

: HBT-MAKER-SHADOW-SETUP ( -- )
   SOURCE-ROOT:ENGINE$ s" bin/hb" HBT-MAKER-HB JOIN-PATH HBT-MAKER-HB-U !
   SOURCE-ROOT:ENGINE$ s" tools/hb-build-aot-image-test.f"
      HBT-MAKER-TOOL JOIN-PATH HBT-MAKER-TOOL-U !
   HBT-ROOT s" tools" HBT-MAKER-SHADOW JOIN-PATH
      HBT-MAKER-SHADOW-U !
   HBT-MAKER-SHADOW$ MAKE-DIR
   HBT-ROOT s" tools/aot-maker.f" HBT-MAKER-SHADOW
      JOIN-PATH HBT-MAKER-SHADOW-U !
   SOURCE-ROOT:ENGINE$ s" tools/aot-maker.f"
      HBT-MAKER-OLD JOIN-PATH HBT-MAKER-OLD-U !
   HBT-MAKER-OLD$ HBT-MAKER-SHADOW$ COPY-FILE-STREAM ;

: HBT-MAKER-CHILD-RUN ( -- n )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-TMP >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   HBT-MAKER-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" path" >LEN PROC-ARGV+
   HBT-TMP >LEN PROC-ARGV+
   HBT-MAKER-HB$ >LEN PROC-ARGV+
   HBT-MAKER-HB$ >LEN HBT-ROOT >LEN
   HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
      HBT-CAPTURE>N {: outn:n errn:n rc:n :}
   rc 0<> if HBT-ERR errn type then
   rc 0 T= errn 0 T=
   outn 1 > TTRUE
   HBT-OUT outn 1- + c@ 10 T=
   outn 1- ;

: HBT-MAKER-ROOT-EDIT ( -- )
   HBT-MAKER-SHADOW-SETUP
   HBT-MAKER-CHILD-RUN {: oldu:n :}
   HBT-OUT oldu HBT-MAKER-OLD HBT-MAKER-OLD-U HBT-COPY!
   HBT-MAKER-OLD$ EXECUTABLE? TTRUE
   HBT-MAKER-SHADOW$ S\" \n\\ maker source changed\n" APPEND-FILE
   HBT-MAKER-CHILD-RUN {: newu:n :}
   HBT-OUT newu EXECUTABLE? TTRUE
   HBT-OUT newu HBT-MAKER-OLD$ STR= TFALSE ;

: HBT-MAKER-ROOTED-ON ( ptr u8 n -- )
   {: mode:ptr modeu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN HBT-MAKER-HB$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   HBT-MAKER-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   HBT-TMP >LEN PROC-ARGV+
   HBT-MAKER-HB$ >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   HBT-MAKER-HB$ >LEN HBT-ROOT >LEN
   HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
      HBT-CAPTURE>N {: outn:n errn:n rc:n :}
   rc 0<> if HBT-OUT outn type HBT-ERR errn type then
   rc 0 T= errn 0 T= ;

: HBT-MAKER-ROOTED-BUILD ( -- )
   HBT-AOT-SRC S\" require src/lib/json-read.f\nrequire src/lib/task.f\nvariable USER-CEILING\nTASK:#USER USER-CEILING !\n: MAIN ( -- ) TASK:#USER USER-CEILING @ <> if s\q maker user\q 74 die then s\q rooted\q type cr ;\n" WRITE-ALL
   HBT-REMOVE-AOT-OUT
   s" run" HBT-MAKER-ROOTED-ON
   HBT-TMP s" hb-aot-got" HBT-AOT-OUT-BUF JOIN-PATH HBT-AOT-OUT-U !
   HBT-AOT-OUT EXECUTABLE? TTRUE
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
      HBT-RUN-ERR HBT-CAPTURE-CAP >LEN HBT-TIMEOUT-MS >MS
      RUN-CAPTURE HBT-CAPTURE>N {: runn:n rune:n runrc:n :}
   runrc 0 T= rune 0 T=
   HBT-RUN-OUT runn s" rooted" CONTAINS? TTRUE
   s" counts" HBT-MAKER-ROOTED-ON ;

\ The maker producer loads process support before the application, then the
\ linker loads its own process support after the application. Temporary support
\ must not spend the application's task-user band. Exercise the saved maker
\ through hb-build and the compatibility maker directly on the public engine.
: HBT-MAKER-REPORT-BUILD ( bool -- )
   {: warm:bool :}
   HBT-ARGV-BASE
   s" --report-json" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 0<> if HBT-OUT outu type HBT-ERR erru type then
   rc 0 T= erru 0 T=
   HBB-RESET-OPTIONS
   warm if
      HBT-OUT outu JR:T-FALSE JR:T-FALSE JR:T-TRUE JR:T-FALSE JR:T-TRUE CHECK-REPORT
   else
      HBT-OUT outu JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-TRUE JR:T-TRUE CHECK-REPORT
   then ;

: HBT-MAKER-USER-RUN ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   HBT-AOT-OUT EXECUTABLE? TTRUE
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
      HBT-RUN-ERR HBT-CAPTURE-CAP >LEN HBT-TIMEOUT-MS >MS
      RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rc:n :}
   rc 0 T= errn 0 T=
   HBT-RUN-OUT outn want wantu T$= ;

: HBT-MAKER-DIRECT-RUN ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   HBT-TMP s" hb-aot-got" HBT-MAKER-DIRECT-OUT JOIN-PATH
      HBT-MAKER-DIRECT-OUT-U !
   HBT-MAKER-DIRECT-OUT$ HBT-REMOVE-FILE?
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-TMP >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" 0" >LEN PROC-ARGV+
   s" bin/hb" >LEN
   S\" require tools/aot-build-open.f\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n" >LEN
   HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
      HBT-MAKER-CAPTURE>N {: outn:n errn:n rc:n :}
   rc 0<> if HBT-OUT outn type HBT-ERR errn type rc throw then
   errn 0 T=
   HBT-MAKER-DIRECT-OUT$ EXECUTABLE? TTRUE
   HBT-MAKER-DIRECT-OUT$ >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
      HBT-RUN-ERR HBT-CAPTURE-CAP >LEN HBT-TIMEOUT-MS >MS
      RUN-CAPTURE HBT-CAPTURE>N {: runn:n rune:n runrc:n :}
   runrc 0 T= rune 0 T=
   HBT-RUN-OUT runn want wantu T$= ;

: HBT-MAKER-USER-BAND ( -- )
   HBT-AOT-SRC S\" require src/lib/task.f\nTASK:#USER 25 TASK:+USER APP-SLOT drop\n: MAIN ( -- ) APP-SLOT 0 swap ! s\" user-ok\" type cr ;\n" WRITE-ALL
   HBT-REMOVE-AOT-OUT
   true HBT-MAKER-REPORT-BUILD
   S\" user-ok\n" HBT-MAKER-USER-RUN
   S\" user-ok\n" HBT-MAKER-DIRECT-RUN
   HBT-AOT-SRC S\" require src/lib/zip.f\n: MAIN ( -- ) s\" zip-ok\" type cr ;\n" WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT-SOURCE HBT-HBB-BUILD-OUT
   S\" zip-ok\n" HBT-MAKER-USER-RUN
   S\" zip-ok\n" HBT-MAKER-DIRECT-RUN ;

public

: HBT-AOT-IMAGE-MAIN ( -- )
   HBT-MAKER-CHILD? if HBT-MAKER-PATH-CHILD exit then
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" run" STR= if HBT-MAKER-RUN-CHILD exit then
      0 SCRIPT-ARGV$ s" counts" STR= if HBT-MAKER-COUNT-CHILD exit then
   then
   T-RESET
   s" " s" " HBT-KEYED!
   HBT-PREPARE
   BUILD-FIXPOINT:BF-ENGINE-RESET
   false HBT-MAKER-REPORT-BUILD
   HBT-AOT-OUT EXECUTABLE? TTRUE
   HBT-RUN-AOT
   HBT-MAKER-USER-BAND
   HBT-MAKER-ROOT-EDIT
   HBT-MAKER-ROOTED-BUILD
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-aot-image-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-AOT-IMAGE-MAIN
