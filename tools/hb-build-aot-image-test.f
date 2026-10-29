\ A saved production AOT builder builds a stripped application from a fresh
\ process; a second selection reuses the same runnable maker image.
\ Run: bin/hb --load tools/hb-build-aot-image-test.f

require tools/hb-build-test-lib.f
require src/lib/string.f
require src/lib/process-cwd.f

package HB-BUILD-CLI

create HBT-MAKER-HB PATH-CAP allot
create HBT-MAKER-TOOL PATH-CAP allot
create HBT-MAKER-SHADOW PATH-CAP allot
create HBT-MAKER-OLD PATH-CAP allot
create HBT-MAKER-DIRECT-OUT PATH-CAP allot
create HBT-MAKER-OUT PATH-CAP allot
variable HBT-MAKER-HB-U
variable HBT-MAKER-TOOL-U
variable HBT-MAKER-SHADOW-U
variable HBT-MAKER-OLD-U
variable HBT-MAKER-DIRECT-OUT-U
variable HBT-MAKER-OUT-U
create HBT-MAKER-FSHA-CTX SHA256-FILE-CTX-BYTES allot
create HBT-MAKER-EXE-HEX 64 allot
create HBT-MAKER-MAP-HEX 64 allot
create HBT-MAKER-GOT-HEX 64 allot

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

\ Every child's out root: the saved maker it selects or builds stays private.
: HBT-MAKER-OUT$ ( -- ptr u8 n )
   HBT-MAKER-OUT HBT-MAKER-OUT-U @ ;

\ A child's argv and env so far: the private temp and out roots.
: HBT-MAKER-ENV ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" TMPDIR" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_OUT" >LEN HBT-MAKER-OUT$ >LEN PROC-ENV+ ;

\ Prints the saved maker image the engine selects, then hit or built.
: HBT-MAKER-PATH-CHILD ( -- )
   SCRIPT-ARGC 2 <> if E-TBL-BOUNDS throw then
   1 SCRIPT-ARGV$ BUILD-FIXPOINT:BF-ENGINE!
   HBB-MAKER-DEFAULT-MS HBB-MAKER-IMAGE$ type cr
   HBB-MAKER-HIT @ 0<> if s" hit" else s" built" then type cr ;

: HBT-MAKER-CHILD-SETUP ( -- )
   SCRIPT-ARGC 4 <> if E-TBL-BOUNDS throw then
   HBB-RESET-OPTIONS
   1 SCRIPT-ARGV$ BUILD-FIXPOINT:BF-TMP!
   2 SCRIPT-ARGV$ BUILD-FIXPOINT:BF-ENGINE!
   3 SCRIPT-ARGV$ HBB-SRC!
   HBB-MAKER-KEY!
   HBB-MAKER-DEFAULT-MS HBB-MAKER-TIMEOUT-MS ! ;

: HBT-MAKER-RUN-CHILD ( -- )
   HBT-MAKER-CHILD-SETUP
   HBB-RUN-MAKER-CMD HBT-MAKER-CAPTURE>N
      {: outn:n errn:n rc:n :}
   rc 0<> if HBB-OUT-BUF outn type HBB-ERR-BUF errn type rc throw then
   outn 0<> errn 0<> or if E-BUILD-PATH throw then
   s" hb-aot-got" BUILD-FIXPOINT:BF-A$ FS:EXECUTABLE? 0= if E-BUILD-PATH throw then ;

: HBT-MAKER-CHILD? ( -- bool )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s" path" STR= exit then
   false ;

: HBT-MAKER-SHADOW-SETUP ( -- )
   SOURCE-ROOT:ENGINE$ s" bin/hb" HBT-MAKER-HB FS:JOIN-PATH HBT-MAKER-HB-U !
   HBT-ROOT s" tools" HBT-MAKER-SHADOW FS:JOIN-PATH
      HBT-MAKER-SHADOW-U !
   HBT-MAKER-SHADOW$ FS:MAKE-DIR
   HBT-ROOT s" tools/aot-builder.f" HBT-MAKER-SHADOW
      FS:JOIN-PATH HBT-MAKER-SHADOW-U !
   SOURCE-ROOT:ENGINE$ s" tools/aot-builder.f"
      HBT-MAKER-OLD FS:JOIN-PATH HBT-MAKER-OLD-U !
   HBT-MAKER-OLD$ HBT-MAKER-SHADOW$ FS:COPY-FILE-STREAM ;

\ The saved maker a fresh process selects for an engine spelling from a working
\ directory: the image path's length in HBT-OUT and whether it was a hit.
: HBT-MAKER-PATH-RUN ( ptr u8 n ptr u8 n -- n bool )
   {: eng:ptr engu:n cwd:ptr cwdu:n :}
   HBT-MAKER-ENV
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   HBT-MAKER-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" path" >LEN PROC-ARGV+
   eng engu >LEN PROC-ARGV+
   HBT-MAKER-HB$ >LEN cwd cwdu >LEN
   HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
      HBT-CAPTURE>N {: outn:n errn:n rc:n :}
   rc 0<> if HBT-ERR errn type then
   rc 0 T= errn 0 T=
   HBT-OUT outn S\" \nhit\n" ENDS-WITH? if outn 5 - true exit then
   HBT-OUT outn S\" \nbuilt\n" ENDS-WITH? 0= if
      HBT-OUT outn type E-BUILD-PATH throw
   then
   outn 7 - false ;

\ The family's one image slot under the private out root, executable.
: HBT-MAKER-SLOT? ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path pathu HBT-MAKER-OUT$ STARTS-WITH? TTRUE
   path pathu s" /img/aot-builder" ENDS-WITH? TTRUE
   path pathu FS:EXECUTABLE? TTRUE ;

\ Editing the maker source under the root replaces the slot's image.
: HBT-MAKER-ROOT-EDIT ( -- )
   HBT-MAKER-SHADOW-SETUP
   HBT-MAKER-HB$ HBT-ROOT HBT-MAKER-PATH-RUN drop {: oldu:n :}
   HBT-OUT oldu HBT-MAKER-OLD HBT-MAKER-OLD-U HBT-COPY!
   HBT-MAKER-OLD$ HBT-MAKER-SLOT?
   HBT-MAKER-SHADOW$ S\" \n\\ maker source changed\n" FS:APPEND-FILE
   HBT-MAKER-HB$ HBT-ROOT HBT-MAKER-PATH-RUN TFALSE {: newu:n :}
   HBT-OUT newu HBT-MAKER-OLD$ T$=
   HBT-OUT newu HBT-MAKER-SLOT? ;

: HBT-MAKER-ROOTED-ON ( ptr u8 n -- )
   {: mode:ptr modeu:n :}
   HBT-MAKER-ENV
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
   HBT-AOT-SRC S\" require src/lib/json-read.f\nrequire src/lib/task.f\nvariable USER-CEILING\nTASK:#USER USER-CEILING !\n: MAIN ( -- ) TASK:#USER USER-CEILING @ <> if s\q maker user\q 74 die then s\q rooted\q type cr ;\n" FS:WRITE-ALL
   HBT-REMOVE-AOT-OUT
   s" run" HBT-MAKER-ROOTED-ON
   HBT-TMP s" hb-aot-got" HBT-AOT-OUT-BUF FS:JOIN-PATH HBT-AOT-OUT-U !
   HBT-AOT-OUT FS:EXECUTABLE? TTRUE
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
      HBT-RUN-ERR HBT-CAPTURE-CAP >LEN HBT-TIMEOUT-MS >MS
      RUN-CAPTURE HBT-CAPTURE>N {: runn:n rune:n runrc:n :}
   runrc 0 T= rune 0 T=
   HBT-RUN-OUT runn s" rooted" CONTAINS? TTRUE ;

\ The maker producer loads process support before the application, then the
\ linker loads its own process support after the application. Temporary support
\ must not spend the application's task-user band. Exercise the saved maker
\ through hb-build and the compatibility maker directly on the public engine.
: HBT-MAKER-REPORT-BUILD ( ptr u8 n bool -- )
   {: engine:ptr engineu:n warm:bool :}
   HBT-TMP engine engineu HBT-ARGV-CLI
   s" HABU_OUT" >LEN HBT-MAKER-OUT$ >LEN PROC-ENV-SET
   s" --report-json" >LEN PROC-ARGV+
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 0<> if HBT-OUT outu type HBT-ERR erru type then
   rc 0 T= erru 0 T=
   HBB-RESET-OPTIONS
   warm if
      HBT-OUT outu JR:T-TRUE JR:T-FALSE JR:T-TRUE CHECK-REPORT
   else
      HBT-OUT outu JR:T-FALSE JR:T-TRUE JR:T-TRUE CHECK-REPORT
   then ;

: HBT-MAKER-CACHE-PATH ( ptr u8 n -- ptr u8 n )
   SOURCE-ROOT:CWD$ HBT-MAKER-PATH-RUN TTRUE {: pathu:n :}
   HBT-OUT pathu HBT-MAKER-SLOT?
   HBT-OUT pathu ;

: HBT-MAKER-HASH ( ptr u8 n ptr u8 -- )
   {: path:ptr pathu:n hex:ptr :}
   HBT-MAKER-FSHA-CTX path pathu hex SHA256-FILE-HEX-IN 0 T= ;

: HBT-MAKER-USER-RUN ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   HBT-AOT-OUT FS:EXECUTABLE? TTRUE
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
      HBT-RUN-ERR HBT-CAPTURE-CAP >LEN HBT-TIMEOUT-MS >MS
      RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rc:n :}
   rc 0 T= errn 0 T=
   HBT-RUN-OUT outn want wantu T$= ;

: HBT-MAKER-DIRECT-RUN ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   HBT-TMP s" hb-aot-got" HBT-MAKER-DIRECT-OUT FS:JOIN-PATH
      HBT-MAKER-DIRECT-OUT-U !
   HBT-MAKER-DIRECT-OUT$ HBT-REMOVE-FILE?
   HBT-MAKER-ENV
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
   HBT-MAKER-DIRECT-OUT$ FS:EXECUTABLE? TTRUE
   HBT-MAKER-DIRECT-OUT$ >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
      HBT-RUN-ERR HBT-CAPTURE-CAP >LEN HBT-TIMEOUT-MS >MS
      RUN-CAPTURE HBT-CAPTURE>N {: runn:n rune:n runrc:n :}
   runrc 0 T= rune 0 T=
   HBT-RUN-OUT runn want wantu T$= ;

: HBT-MAKER-USER-BAND ( -- )
   \ ZIP must load before APP-SLOT: its task-local allocations afterward
   \ would round the user ceiling and hide the odd 25-byte regression.
   HBT-AOT-SRC S\" require src/lib/task.f\nrequire src/lib/zip.f\nTASK:#USER 25 TASK:+USER APP-SLOT drop\n: MAIN ( -- ) APP-SLOT 0 swap ! s\" user-ok\" type cr ;\n" FS:WRITE-ALL
   HBT-REMOVE-AOT-OUT
   s" bin/hb" false HBT-MAKER-REPORT-BUILD
   S\" user-ok\n" HBT-MAKER-USER-RUN
   HBT-AOT-OUT HBT-MAKER-EXE-HEX HBT-MAKER-HASH
   HBT-AOT-OUT HBB-MAP-DST!
   HBB-MAP-DST$ HBT-MAKER-MAP-HEX HBT-MAKER-HASH
   s" bin/hb" SOURCE-ROOT:CANON-OS 0= if E-BUILD-PATH throw then
      HBT-MAKER-HB HBT-MAKER-HB-U HBT-COPY!
   s" bin/hb" HBT-MAKER-CACHE-PATH
      HBT-MAKER-OLD HBT-MAKER-OLD-U HBT-COPY!
   HBT-MAKER-HB$ HBT-MAKER-CACHE-PATH HBT-MAKER-OLD$ T$=
   \ Keep the source fixed and require maker_ran: comparing a restored
   \ artifact would not exercise the absolute spelling's linker path.
   HBT-REMOVE-AOT-OUT
   HBT-MAKER-HB$ true HBT-MAKER-REPORT-BUILD
   HBT-AOT-OUT HBT-MAKER-GOT-HEX HBT-MAKER-HASH
   HBT-MAKER-GOT-HEX 64 HBT-MAKER-EXE-HEX 64 T$=
   HBT-AOT-OUT HBB-MAP-DST!
   HBB-MAP-DST$ HBT-MAKER-GOT-HEX HBT-MAKER-HASH
   HBT-MAKER-GOT-HEX 64 HBT-MAKER-MAP-HEX 64 T$=
   S\" user-ok\n" HBT-MAKER-USER-RUN
   S\" user-ok\n" HBT-MAKER-DIRECT-RUN
   HBT-AOT-SRC S\" require src/lib/zip.f\n: MAIN ( -- ) s\" zip-ok\" type cr ;\n" FS:WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-MAKER-HB$ true HBT-MAKER-REPORT-BUILD
   S\" zip-ok\n" HBT-MAKER-USER-RUN ;

public

: HBT-AOT-IMAGE-MAIN ( -- )
   HBT-MAKER-CHILD? if HBT-MAKER-PATH-CHILD exit then
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" run" STR= if HBT-MAKER-RUN-CHILD exit then
   then
   T-RESET
   NULL$ HBT-KEYED!
   HBT-PREPARE
   \ Private roots by claim: every child's out root is HBT-TMP/out, so the
   \ first report build's maker is built cold (maker_built), and
   \ HBT-MAKER-CACHE-PATH asserts the hit that child left.
   HBT-TMP s" out" HBT-MAKER-OUT HBT-MAKER-OUT-U HBT-PATH!
   SOURCE-ROOT:ENGINE$ s" tools/hb-build-aot-image-test.f"
      HBT-MAKER-TOOL FS:JOIN-PATH HBT-MAKER-TOOL-U !
   BUILD-FIXPOINT:BF-ENGINE-RESET
   HBT-MAKER-USER-BAND
   HBT-MAKER-ROOT-EDIT
   HBT-MAKER-ROOTED-BUILD
   FS:CLEANUP-RUN
   HBT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" hb-build-aot-image-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-AOT-IMAGE-MAIN
