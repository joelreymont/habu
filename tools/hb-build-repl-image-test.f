\ A cold REPL builder reports its own deadline through hb-build, then a saved
\ builder works from another working directory and rechecks its engine in-process.
\ Run: bin/hb --load tools/hb-build-repl-image-test.f

require tools/hb-build-test-lib.f
require src/lib/process-cwd.f

package HB-BUILD-CLI

create HBT-IMAGE-HB-BUF FS-PATH-CAP allot
create HBT-IMAGE-TOOL-BUF FS-PATH-CAP allot
create HBT-IMAGE-SHADOW-BUF FS-PATH-CAP allot
create HBT-IMAGE-SOURCE-BUF FS-PATH-CAP allot
variable HBT-IMAGE-HB-U
variable HBT-IMAGE-TOOL-U
variable HBT-IMAGE-SHADOW-U
variable HBT-IMAGE-SOURCE-U
variable HBT-IMAGE-SIZE
variable HBT-IMAGE-SEC
variable HBT-IMAGE-NS

: HBT-IMAGE-HB$ ( -- ptr u8 n ) HBT-IMAGE-HB-BUF HBT-IMAGE-HB-U @ ;
: HBT-IMAGE-TOOL$ ( -- ptr u8 n ) HBT-IMAGE-TOOL-BUF HBT-IMAGE-TOOL-U @ ;
: HBT-IMAGE-SHADOW$ ( -- ptr u8 n ) HBT-IMAGE-SHADOW-BUF HBT-IMAGE-SHADOW-U @ ;

: HBT-IMAGE-PATHS ( -- )
   SOURCE-ROOT:ENGINE$ s" bin/hb" HBT-IMAGE-HB-BUF JOIN-PATH HBT-IMAGE-HB-U !
   SOURCE-ROOT:ENGINE$ s" tools/hb-build.f" HBT-IMAGE-TOOL-BUF JOIN-PATH HBT-IMAGE-TOOL-U ! ;

: HBT-IMAGE-SHADOW-PATH ( -- )
   HBT-ROOT s" tools/app-build.f" HBT-IMAGE-SHADOW-BUF JOIN-PATH HBT-IMAGE-SHADOW-U ! ;

: HBT-IMAGE-ENV ( ptr u8 n -- ) {: limit:ptr limitu:n :}
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN HBT-IMAGE-HB$ >LEN PROC-ENV+
   s" HB_BUILD_TIMEOUT_MS" >LEN limit limitu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: HBT-IMAGE-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   HBT-IMAGE-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --repl" >LEN PROC-ARGV+ ;

: HBT-IMAGE-RUN-ON ( ptr u8 n -- n n n ) {: cwd:ptr cwdu:n :}
   HBT-IMAGE-HB$ >LEN cwd cwdu >LEN
   HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE HBT-CAPTURE>N ;

: HBT-COLD-REPL-TIMEOUT ( -- )
   s" 1" HBT-IMAGE-ENV
   HBT-IMAGE-ARGS
   s" --json-errors" >LEN PROC-ARGV+
   HBT-REPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-OUT >LEN PROC-ARGV+
   SOURCE-ROOT:CWD$ HBT-IMAGE-RUN-ON {: outu:n erru:n rc:n :}
   rc HBB-UNCAUGHT-RC T=
   outu 0 T=
   READER-STATE JR:STORAGE-BYTES HBT-ERR erru JR:INIT
   JR:NEXT JR:T-OBJ T=
   s" code" JR:FIND-KEY TTRUE s" E-PROC-TIMEOUT" REPORT-STRING=
   s" maker" JR:FIND-KEY TTRUE s" repl" REPORT-STRING=
   s" source" JR:FIND-KEY TTRUE HBT-REPL-SRC REPORT-STRING=
   s" timeout_ms" JR:FIND-KEY TTRUE JR:TOKEN JR:T-INT T= JR:INT 1 T=
   JR:CLOSE
   HBT-REPL-OUT EXISTS? TFALSE ;

\ A second cold attempt exercises the text form and leaves no output image.
: HBT-COLD-REPL-TIMEOUT-TEXT ( -- )
   s" 1" HBT-IMAGE-ENV
   HBT-IMAGE-ARGS
   HBT-REPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-OUT >LEN PROC-ARGV+
   SOURCE-ROOT:CWD$ HBT-IMAGE-RUN-ON {: outu:n erru:n rc:n :}
   rc HBB-UNCAUGHT-RC T=
   outu 0 T=
   HBT-ERR erru s" hb-build: code=E-PROC-TIMEOUT maker=repl" CONTAINS? TTRUE
   HBT-ERR erru HBT-REPL-SRC CONTAINS? TTRUE
   HBT-ERR erru s" timeout_ms=1" CONTAINS? TTRUE
   HBT-REPL-OUT EXISTS? TFALSE ;

: HBT-ROOTED-APP$ ( -- ptr u8 n )
   S\" require src/lib/json-read.f\n: MAIN ( -- ) s\q from-root\q type cr ;\n" ;

: HBT-ROOTED-BUILD ( -- )
   s" 600000" HBT-IMAGE-ENV
   HBT-IMAGE-ARGS
   HBT-REPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-OUT >LEN PROC-ARGV+
   HBT-ROOT HBT-IMAGE-RUN-ON {: outu:n erru:n rc:n :}
   rc 0 <> if HBT-OUT outu type HBT-ERR erru type then
   rc 0 T=
   erru 0 T=
   HBT-REPL-OUT EXECUTABLE? TTRUE ;

: HBT-ROOTED-RUN ( -- )
   HBT-REPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
   HBT-RUN-ERR HBT-CAPTURE-CAP >LEN HBT-TIMEOUT-MS >MS
   RUN-CAPTURE HBT-CAPTURE>N {: outu:n erru:n rc:n :}
   rc 0 T=
   HBT-RUN-OUT outu s" from-root" CONTAINS? TTRUE
   HBT-RUN-ERR erru HBT-EMPTY$ T$= ;

: HBT-ROOTED-MARK ( -- )
   HBT-REPL-OUT FILE-META {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   size HBT-IMAGE-SIZE ! sec HBT-IMAGE-SEC ! ns HBT-IMAGE-NS ! ;

: HBT-ROOTED-SAME? ( -- bool )
   HBT-REPL-OUT FILE-META {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   size HBT-IMAGE-SIZE @ = sec HBT-IMAGE-SEC @ = and
   ns HBT-IMAGE-NS @ = and ;

: HBT-ROOTED-COLD-WARM ( -- )
   HBT-REPL-SRC HBT-ROOTED-APP$ WRITE-ALL
   HBT-ROOTED-BUILD
   HBT-ROOTED-RUN
   HBT-ROOTED-MARK
   HBT-ROOTED-BUILD
   HBT-ROOTED-SAME? TTRUE
   HBT-ROOTED-RUN ;

\ A newly present CWD producer shadows the selected engine root's fallback
\ copy. The existing output must be rebuilt even though its old files match.
: HBT-ROOTED-PRODUCER-SHADOW ( -- )
   HBT-IMAGE-SHADOW-PATH
   HBT-IMAGE-SHADOW$ SOURCE-ROOT:DIRNAME MAKE-DIR
   SOURCE-ROOT:ENGINE$ s" tools/app-build.f"
      HBT-IMAGE-SOURCE-BUF JOIN-PATH HBT-IMAGE-SOURCE-U !
   HBT-IMAGE-SOURCE-BUF HBT-IMAGE-SOURCE-U @
      HBT-IMAGE-SHADOW$ COPY-FILE-STREAM
   HBT-ROOTED-BUILD
   HBT-ROOTED-SAME? TFALSE
   HBT-ROOTED-RUN
   HBT-ROOTED-MARK
   HBT-ROOTED-BUILD
   HBT-ROOTED-SAME? TTRUE ;

\ The observer stamps before discovery reads. Editing this private producer
\ after its stamp must leave the transcript stale; the next observation is fresh.
: HBT-IMAGE-STAMP-EDIT ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u HBB-REPL-STAMP-FILE
   a u HBT-IMAGE-SOURCE-BUF HBT-IMAGE-SOURCE-U @ STR= if
      a u S\" \n\\ producer changed after its stamp\n" APPEND-FILE
   then ;

: HBT-IMAGE-EDIT-PATH ( -- )
   NFILE-INPUTS:RESET
   ['] HBT-IMAGE-STAMP-EDIT 600000 APP-BUILD-IMAGE:PATH-EACH$
   EXECUTABLE? TTRUE
   [: NFILE-INPUTS:BYTES$ NFILE-INPUTS:CHECK ;] E-NUNIT-PROFILE TTHROWSQ
   NFILE-INPUTS:RESET
   ['] HBB-REPL-STAMP-FILE 600000 APP-BUILD-IMAGE:PATH-EACH$
   EXECUTABLE? TTRUE
   NFILE-INPUTS:BYTES$ NFILE-INPUTS:CHECK
   NFILE-INPUTS:CLOSE ;

: HBT-IMAGE-EDIT-CHILD ( -- )
   SCRIPT-ARGC 2 <> if E-TBL-BOUNDS throw then
   1 SCRIPT-ARGV$ SOURCE-ROOT:CANONICAL drop
   HBT-IMAGE-SOURCE-BUF HBT-IMAGE-SOURCE-U HBT-COPY!
   HBT-IMAGE-EDIT-PATH
   T-REPORT ;

: HBT-IMAGE-OBSERVER-EDIT ( -- )
   \ The producer resolver uses WITH-CWD, so the shadow's root must be the
   \ child's actual working directory, as it is for the ordinary CLI cases.
   s" 600000" HBT-IMAGE-ENV
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SOURCE-ROOT:ENGINE$ s" tools/hb-build-repl-image-test.f"
      HBT-IMAGE-TOOL-BUF JOIN-PATH HBT-IMAGE-TOOL-U !
   HBT-IMAGE-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" observer-edit" >LEN PROC-ARGV+
   HBT-IMAGE-SHADOW$ >LEN PROC-ARGV+
   HBT-ROOT HBT-IMAGE-RUN-ON {: outu:n erru:n rc:n :}
   rc 0<> if HBT-OUT outu type HBT-ERR erru type then
   rc 0 T= erru 0 T=
   HBT-IMAGE-PATHS
   HBT-ROOTED-BUILD
   HBT-ROOTED-RUN
   HBT-ROOTED-MARK
   HBT-ROOTED-BUILD
   HBT-ROOTED-SAME? TTRUE ;

: HBT-IMAGE-OBSERVER-THROW ( ptr u8 n -- )
   2drop E-FS-PATH throw ;

: HBT-IMAGE-OBSERVER-ERROR ( -- )
   SOURCE-ROOT:ENGINE$
   HBT-IMAGE-SOURCE-BUF HBT-IMAGE-SOURCE-U HBT-COPY!
   HBT-IMAGE-HB$ BUILD-FIXPOINT:BF-ENGINE!
   2 0 ?do
      [: ['] HBT-IMAGE-OBSERVER-THROW 600000
         APP-BUILD-IMAGE:PATH-EACH$ 2drop ;] E-FS-PATH TTHROWSQ
      SOURCE-ROOT:ENGINE$ HBT-IMAGE-SOURCE-BUF HBT-IMAGE-SOURCE-U @ T$=
   loop
   600000 APP-BUILD-IMAGE:PATH$ EXECUTABLE? TTRUE
   BUILD-FIXPOINT:BF-ENGINE-RESET ;

: HBT-ENGINE-RECHECK ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-IMAGE-HB$ BUILD-FIXPOINT:BF-ENGINE!
   600000 APP-BUILD-IMAGE:PATH$ 2dup EXECUTABLE? TTRUE 2drop
   s" /habu-no-such-repl-builder-engine" BUILD-FIXPOINT:BF-ENGINE!
   [: 600000 APP-BUILD-IMAGE:PATH$ 2drop ;] -1 TTHROWSQ
   BUILD-FIXPOINT:BF-ENGINE-RESET ;


: HBT-IMAGE-EDIT-MODE? ( -- bool )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s" observer-edit" STR= exit then
   0 0= 0= ;

public
: HBT-REPL-IMAGE-MAIN ( -- )
   T-RESET
   HBT-IMAGE-EDIT-MODE? if HBT-IMAGE-EDIT-CHILD exit then
   HBT-PREPARE
   HBT-IMAGE-PATHS
   HBT-COLD-REPL-TIMEOUT
   HBT-COLD-REPL-TIMEOUT-TEXT
   HBT-ROOTED-COLD-WARM
   HBT-ROOTED-PRODUCER-SHADOW
   HBT-IMAGE-OBSERVER-ERROR
   HBT-IMAGE-OBSERVER-EDIT
   HBT-ENGINE-RECHECK
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-repl-image-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-REPL-IMAGE-MAIN
