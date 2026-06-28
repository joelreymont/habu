\ hb-build-test.f - checked fixture for tools/hb-build-lib.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/source.f lib/build.f lib/codesign.f tools/build-fixpoint.f tools/warm-run.f tools/hb-build-lib.f tools/hb-build-test.f

65536 constant HBT-CAPTURE-CAP
120000 constant HBT-TIMEOUT-MS

variable HBT-ROOT-U
variable HBT-TMP-U
variable HBT-NEW-TMP-U
variable HBT-BAD-SRC-U
variable HBT-BAD-OUT-U
variable HBT-REPL-SRC-U
variable HBT-REPL-OUT-U
variable HBT-REPL-BAD-SRC-U
variable HBT-REPL-BAD-OUT-U

create HBT-ROOT-BUF FS-PATH-CAP allot
create HBT-TMP-BUF FS-PATH-CAP allot
create HBT-NEW-TMP-BUF FS-PATH-CAP allot
create HBT-BAD-SRC-BUF FS-PATH-CAP allot
create HBT-BAD-OUT-BUF FS-PATH-CAP allot
create HBT-REPL-SRC-BUF FS-PATH-CAP allot
create HBT-REPL-OUT-BUF FS-PATH-CAP allot
create HBT-REPL-BAD-SRC-BUF FS-PATH-CAP allot
create HBT-REPL-BAD-OUT-BUF FS-PATH-CAP allot
create HBT-OUT HBT-CAPTURE-CAP allot
create HBT-ERR HBT-CAPTURE-CAP allot
create HBT-RUN-OUT HBT-CAPTURE-CAP allot
create HBT-RUN-ERR HBT-CAPTURE-CAP allot

: HBT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: HBT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: HBT-ROOT ( -- ptr u8 n )
   HBT-ROOT-BUF HBT-ROOT-U @ ;

: HBT-TMP ( -- ptr u8 n )
   HBT-TMP-BUF HBT-TMP-U @ ;

: HBT-NEW-TMP ( -- ptr u8 n )
   HBT-NEW-TMP-BUF HBT-NEW-TMP-U @ ;

: HBT-CACHE-ENV$ ( -- ptr u8 n )
   s" HABU_BUILD_CACHE" GETENV dup 0= if
      2drop
      HBT-TMP
   then ;

: HBT-BAD-SRC ( -- ptr u8 n )
   HBT-BAD-SRC-BUF HBT-BAD-SRC-U @ ;

: HBT-BAD-OUT ( -- ptr u8 n )
   HBT-BAD-OUT-BUF HBT-BAD-OUT-U @ ;

: HBT-REPL-SRC ( -- ptr u8 n )
   HBT-REPL-SRC-BUF HBT-REPL-SRC-U @ ;

: HBT-REPL-OUT ( -- ptr u8 n )
   HBT-REPL-OUT-BUF HBT-REPL-OUT-U @ ;

: HBT-REPL-BAD-SRC ( -- ptr u8 n )
   HBT-REPL-BAD-SRC-BUF HBT-REPL-BAD-SRC-U @ ;

: HBT-REPL-BAD-OUT ( -- ptr u8 n )
   HBT-REPL-BAD-OUT-BUF HBT-REPL-BAD-OUT-U @ ;

: HBT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: HBT-BAD-SRC$ ( -- ptr u8 n )
   s" : MAIN ( -- ) here drop ;" ;

: HBT-REPL-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" 5 constant FIVE" SB-APPEND
   HBB-LF SB-APPEND-C
   s" create PAD 8 allot" SB-APPEND
   HBB-LF SB-APPEND-C
   s" variable SLOT" SB-APPEND
   HBB-LF SB-APPEND-C
   s" defer APPLY ( i64 -- i64 )" SB-APPEND
   HBB-LF SB-APPEND-C
   s" : SQ ( i64 -- i64 ) FIVE drop PAD drop SLOT drop dup * ;" SB-APPEND
   HBB-LF SB-APPEND-C
   s" : INC ( i64 -- i64 ) 1 + ;" SB-APPEND
   HBB-LF SB-APPEND-C
   s" : INSTALL-APPLY ( -- ) [: INC ;] is APPLY ;" SB-APPEND
   HBB-LF SB-APPEND-C
   s" EXPORT SQ" SB-APPEND
   HBB-LF SB-APPEND-C
   s" : SHOW-ARGS ( -- ) SCRIPT-ARGC 0 > if SCRIPT-ARGC . CR 0 SCRIPT-ARGV$ type cr then ;" SB-APPEND
   HBB-LF SB-APPEND-C
   s" INSTALL-APPLY" SB-APPEND
   HBB-LF SB-APPEND-C
   s" 9 APPLY . CR" SB-APPEND
   HBB-LF SB-APPEND-C
   s" 9 SQ . CR" SB-APPEND
   HBB-LF SB-APPEND-C
   s" SHOW-ARGS" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-REPL-BAD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : RBAD ( i64 -- i64 ) 0= ;" SB-APPEND
   HBB-LF SB-APPEND-C
   s" EXPORT RBAD" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-hb-build" TMPDIR-MKDIR {: a:ptr u :}
   a u HBT-ROOT-BUF HBT-ROOT-U HBT-COPY!
   HBT-ROOT CLEANUP-TREE+
   HBT-ROOT s" hbtmp" HBT-TMP-BUF HBT-TMP-U HBT-PATH!
   HBT-TMP MAKE-DIR
   HBT-ROOT s" hbtmp-new" HBT-NEW-TMP-BUF HBT-NEW-TMP-U HBT-PATH!
   HBT-ROOT s" bad.f" HBT-BAD-SRC-BUF HBT-BAD-SRC-U HBT-PATH!
   HBT-ROOT s" bad" HBT-BAD-OUT-BUF HBT-BAD-OUT-U HBT-PATH!
   HBT-ROOT s" repl.f" HBT-REPL-SRC-BUF HBT-REPL-SRC-U HBT-PATH!
   HBT-ROOT s" repl" HBT-REPL-OUT-BUF HBT-REPL-OUT-U HBT-PATH!
   HBT-ROOT s" repl-bad.f" HBT-REPL-BAD-SRC-BUF HBT-REPL-BAD-SRC-U HBT-PATH!
   HBT-ROOT s" repl-bad" HBT-REPL-BAD-OUT-BUF HBT-REPL-BAD-OUT-U HBT-PATH!
   HBT-BAD-SRC HBT-BAD-SRC$ WRITE-ALL
   HBT-REPL-SRC HBT-REPL-SRC$ WRITE-ALL
   HBT-REPL-BAD-SRC HBT-REPL-BAD-SRC$ WRITE-ALL ;

: HBT-ARGV-BASE-TMP ( ptr u8 n -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN 2swap >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN HBT-CACHE-ENV$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/warm-run.f"  >LEN PROC-ARGV+
   s" tools/hb-build-lib.f"  >LEN PROC-ARGV+
   s" tools/hb-build.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: HBT-ARGV-BASE ( -- )
   HBT-TMP HBT-ARGV-BASE-TMP ;

: HBT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: HBT-RUN-HB-BUILD ( -- n n n )
   s" bin/hb" >LEN HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   HBT-CAPTURE>N ;

: HBT-HBB-PREPARE-REPL ( ptr u8 n ptr u8 n -- )
   HBB-RESET-OPTIONS
   HBB-REPL-ON
   HBB-PATHS!
   HBT-TMP BF-TMP! ;

: HBT-HBB-BUILD-OUT ( -- )
   HBB-BUILD
   BF-TMP-RESET ;

: HBT-HBB-RUN-MAKER ( -- n n n )
   HBB-BUILD-MAKER
   HBB-PREPARE-PROGRAM-SOURCE
   HBB-RUN-MAKER-CMD
   BF-TMP-RESET ;

: HBT-ADD-BAD ( -- )
   s" --json-errors"  >LEN PROC-ARGV+
   HBT-BAD-SRC  >LEN PROC-ARGV+
   s" -o"  >LEN PROC-ARGV+
   HBT-BAD-OUT  >LEN PROC-ARGV+ ;

: HBT-REPL-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" 10" SB-APPEND
   HBB-LF SB-APPEND-C
   HBB-LF SB-APPEND-C
   s" 81" SB-APPEND
   HBB-LF SB-APPEND-C
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-BUILD-REPL ( -- )
   HBT-REPL-SRC HBT-REPL-OUT HBT-HBB-PREPARE-REPL
   HBT-HBB-BUILD-OUT
   HBT-REPL-OUT FILE? TTRUE ;

: HBT-RUN-REPL ( -- )
   HBT-REPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn errn rcn :}
   rcn 0 <> if s" repl rc: " type rcn . cr HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn HBT-REPL-EXPECTED$ T$= ;

: HBT-REPL-ARGS-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" 10" SB-APPEND
   HBB-LF SB-APPEND-C
   HBB-LF SB-APPEND-C
   s" 81" SB-APPEND
   HBB-LF SB-APPEND-C
   HBB-LF SB-APPEND-C
   s" 2" SB-APPEND
   HBB-LF SB-APPEND-C
   HBB-LF SB-APPEND-C
   s" alpha" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-RUN-REPL-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   s" alpha"  >LEN PROC-ARGV+
   s" beta"  >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   HBT-REPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if s" repl args rc: " type rcn . cr HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn HBT-REPL-ARGS-EXPECTED$ T-STR= 0= if
      s" repl args stdout: " type HBT-RUN-OUT outn type cr
      s" actual len: " type outn . cr
      s" expect len: " type HBT-REPL-ARGS-EXPECTED$ nip . cr
   then
   HBT-RUN-OUT outn HBT-REPL-ARGS-EXPECTED$ T$= ;

: HBT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: HBT-IMGDUMP-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" HBT-ARG+
   s" tools/imgdump.f" HBT-ARG+
   s" --" HBT-ARG+
   HBT-REPL-OUT HBT-ARG+ ;

: HBT-IMGDUMP-REPL ( -- )
   HBT-IMGDUMP-ARGV
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn s" + " CONTAINS? TTRUE ;

: HBT-BUILD-REPL-BAD ( -- )
   HBT-REPL-BAD-SRC HBT-REPL-BAD-OUT HBT-HBB-PREPARE-REPL
   HBT-REPL-BAD-OUT FILE? if HBT-REPL-BAD-OUT REMOVE-FILE then
   HBT-HBB-RUN-MAKER {: outu:n erru:n rc:n :}
   rc 0 T<>
   HBB-OUT-BUF outu HBT-EMPTY$ T$=
   HBB-ERR-BUF erru s" expected: i64" CONTAINS? TTRUE
   HBB-ERR-BUF erru s" actual: bool" CONTAINS? TTRUE
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

: HBT-BUILD-MISSING-TMP ( -- )
   HBT-NEW-TMP EXISTS? TFALSE
   HBT-NEW-TMP HBT-ARGV-BASE-TMP
   HBT-ADD-BAD
   HBT-RUN-HB-BUILD 0 T<>
   {: outu erru :}
   HBT-OUT outu HBT-EMPTY$ T$=
   HBT-ERR erru s" E-AOT-UNSUPPORTED" CONTAINS? TTRUE
   HBT-NEW-TMP DIR? TTRUE
   HBT-BAD-OUT EXISTS? TFALSE ;

: HBT-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-BUILD-REPL
   HBT-RUN-REPL
   HBT-RUN-REPL-ARGS
   HBT-IMGDUMP-REPL
   HBT-BUILD-REPL-BAD
   HBT-BUILD-MISSING-TMP
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-test: ok" type cr ;

HBT-MAIN
