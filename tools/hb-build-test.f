\ hb-build-test.f - checked fixture for tools/hb-build-lib.f.
\ Run: bin/hb --load tools/hb-build-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/content-key.f
require lib/object.f
require lib/object-cache.f
require lib/object-index.f
require lib/object-resolve.f
require lib/object-link.f
require tools/build-fixpoint.f
require tools/cli-run.f
require tools/object-image.f
require tools/hb-build-lib.f

64 constant HBT-KEY-U
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
variable HBT-AOT-SRC-U
variable HBT-AOT-OUT-U
variable HBT-DEP-SRC-U
variable HBT-ENTRY-SRC-U
variable HBT-ENG-U

create HBT-ROOT-BUF FS-PATH-CAP allot
create HBT-TMP-BUF FS-PATH-CAP allot
create HBT-NEW-TMP-BUF FS-PATH-CAP allot
create HBT-BAD-SRC-BUF FS-PATH-CAP allot
create HBT-BAD-OUT-BUF FS-PATH-CAP allot
create HBT-REPL-SRC-BUF FS-PATH-CAP allot
create HBT-REPL-OUT-BUF FS-PATH-CAP allot
create HBT-REPL-BAD-SRC-BUF FS-PATH-CAP allot
create HBT-REPL-BAD-OUT-BUF FS-PATH-CAP allot
create HBT-AOT-SRC-BUF FS-PATH-CAP allot
create HBT-AOT-OUT-BUF FS-PATH-CAP allot
create HBT-DEP-SRC-BUF FS-PATH-CAP allot
create HBT-ENTRY-SRC-BUF FS-PATH-CAP allot
create HBT-ENG-BUF FS-PATH-CAP allot
create HBT-ABI-ALT 96 allot
create HBT-OUT HBT-CAPTURE-CAP allot
create HBT-ERR HBT-CAPTURE-CAP allot
create HBT-RUN-OUT HBT-CAPTURE-CAP allot
create HBT-RUN-ERR HBT-CAPTURE-CAP allot
create HBT-AOT-HEX 80 allot
create HBT-SRC-KEY 80 allot
create HBT-KEY-A 64 allot
create HBT-KEY-B 64 allot
variable HBT-EXP-SRC-U
variable HBT-EXP-OUT-U
create HBT-EXP-SRC-BUF FS-PATH-CAP allot
create HBT-EXP-OUT-BUF FS-PATH-CAP allot
create HBT-EXP-DG 32 allot
create HBT-EXP-HEX1 64 allot
create HBT-EXP-HEX2 64 allot

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

: HBT-AOT-SRC ( -- ptr u8 n )
   HBT-AOT-SRC-BUF HBT-AOT-SRC-U @ ;

: HBT-AOT-OUT ( -- ptr u8 n )
   HBT-AOT-OUT-BUF HBT-AOT-OUT-U @ ;

: HBT-DEP-SRC ( -- ptr u8 n )
   HBT-DEP-SRC-BUF HBT-DEP-SRC-U @ ;

: HBT-ENTRY-SRC ( -- ptr u8 n )
   HBT-ENTRY-SRC-BUF HBT-ENTRY-SRC-U @ ;

: HBT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: HBT-CACHE? ( -- bool )
   s" HABU_BUILD_CACHE" GETENV dup 0= if
      2drop 0 0= 0= exit
   then
   2drop 0 0= ;

: HBT-BAD-SRC$ ( -- ptr u8 n )
   s" : MAIN ( -- ) 0 0 patch32 ;" ;

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

: HBT-APPEND-CACHE-MUTATION ( -- )
   SB-RESET
   s" \\ cache key mutation" SB-APPEND
   HBB-LF SB-APPEND-C
   HBT-REPL-SRC SB$ APPEND-FILE ;

: HBT-REPL-BAD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : RBAD ( i64 -- i64 ) 0= ;" SB-APPEND
   HBB-LF SB-APPEND-C
   s" EXPORT RBAD" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-AOT-SRC$ ( -- ptr u8 n )
   s" : MAIN ( -- ) ; \ trailing source comment" ;

: HBT-AOT-SRC2$ ( -- ptr u8 n )
   s" : MAIN ( -- ) 0 drop ;" ;

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
   HBT-ROOT s" aot.f" HBT-AOT-SRC-BUF HBT-AOT-SRC-U HBT-PATH!
   HBT-ROOT s" aot" HBT-AOT-OUT-BUF HBT-AOT-OUT-U HBT-PATH!
   HBT-BAD-SRC HBT-BAD-SRC$ WRITE-ALL
   HBT-REPL-SRC HBT-REPL-SRC$ WRITE-ALL
   HBT-REPL-BAD-SRC HBT-REPL-BAD-SRC$ WRITE-ALL
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL ;

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
   s" lib/content-key.f"  >LEN PROC-ARGV+
   s" lib/object.f"  >LEN PROC-ARGV+
   s" lib/object-cache.f"  >LEN PROC-ARGV+
   s" lib/object-index.f"  >LEN PROC-ARGV+
   s" lib/object-resolve.f"  >LEN PROC-ARGV+
   s" lib/object-link.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/cli-run.f"  >LEN PROC-ARGV+
   s" tools/object-image.f"  >LEN PROC-ARGV+
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

: HBT-HBB-PREPARE-AOT ( ptr u8 n ptr u8 n -- )
   HBB-RESET-OPTIONS
   HBB-PATHS!
   HBT-TMP BF-TMP! ;

: HBT-HBB-BUILD-OUT ( -- )
   HBB-BUILD
   BF-TMP-RESET ;

: HBT-REMOVE-FILE? ( ptr u8 n -- )
   2dup FILE? if REMOVE-FILE else 2drop then ;

: HBT-REMOVE-AOT-OUT ( -- )
   HBT-AOT-OUT HBT-REMOVE-FILE? ;

: HBT-REMOVE-ARTIFACT ( -- )
   HBB-ARTIFACT$ HBT-REMOVE-FILE? ;

: HBT-HBB-KEY-REPL ( ptr u8 n ptr u8 -- )
   {: src:ptr srcu:n dst:ptr :}
   src srcu HBT-REPL-OUT HBT-HBB-PREPARE-REPL
   HBB-PREPARE-ARTIFACT-CACHE
   HBB-ARTIFACT-CACHE @ 0= if E-BUILD-SOURCE throw then
   HBB-ARTIFACT-KEY-HEX dst 64 BYTE-COPY
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

: HBT-REBUILD-REPL-CACHE ( -- )
   HBT-CACHE? 0= if exit then
   HBT-REPL-OUT FILE? if HBT-REPL-OUT REMOVE-FILE then
   HBT-REPL-SRC HBT-REPL-OUT HBT-HBB-PREPARE-REPL
   HBT-HBB-BUILD-OUT
   HBB-ARTIFACT-HIT @ 0 <> TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBT-REPL-OUT FILE? TTRUE ;

: HBT-CACHE-KEY-CHANGES ( -- )
   HBT-CACHE? 0= if exit then
   HBT-REPL-SRC HBT-REPL-SRC$ WRITE-ALL
   HBT-REPL-SRC HBT-KEY-A HBT-HBB-KEY-REPL
   HBT-APPEND-CACHE-MUTATION
   HBT-REPL-SRC HBT-KEY-B HBT-HBB-KEY-REPL
   HBT-KEY-A 64 HBT-KEY-B 64 STR= TFALSE
   HBT-REPL-SRC HBT-REPL-SRC$ WRITE-ALL ;

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
   s" lib/errors.f" HBT-ARG+
   s" lib/string.f" HBT-ARG+
   s" lib/memory.f" HBT-ARG+
   s" lib/fs.f" HBT-ARG+
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

\ The object cache key is the ordered-closure hex of the source (a self-contained
\ AOT source closes over only itself), so the fixtures that pre-store objects must
\ key them the same way hb-build now does.
: HBT-AOT-HEX! ( -- )
   HBT-AOT-SRC HBB-SRC!
   HBB-SRC-CLOSURE-HEX!
   HBB-SRC-CLOSURE-HEX HBT-AOT-HEX 64 BYTE-COPY ;

: HBT-AOT-SOURCE-KEY! ( -- )
   HBT-AOT-HEX HBT-KEY-U HBB-TARGET-ABI$ HBB-CHECKER-ABI$ HBB-COMPILER-ABI$
   HBT-SRC-KEY OBJIDX:SOURCE-KEY-HEX ;

: HBT-BUILD-EXIT-OBJ ( ptr u8 n -- ) {: target:ptr targetu:n :}
   HBT-AOT-HEX!
   OBJ:RESET
   HBT-AOT-HEX HBT-KEY-U OBJ:SOURCE!
   target targetu OBJ:TARGET!
   HBB-CHECKER-ABI$ OBJ:CHECKER!
   HBB-COMPILER-ABI$ OBJ:COMPILER!
   ASM-INIT
   0 0 MOVZ,
   NR-EXIT-GROUP SYS,
   CODE ASM-LEN OBJ:TEXT+
   s" MAIN" s" --" OBJ:EXPORT+
   s" MAIN" 0 s" --" OBJ:DEF+ ;

: HBT-STORE-AOT-OBJ ( -- )
   HBB-RESET-OPTIONS
   HBT-TMP OBJRES:ROOT!
   HBB-TARGET-ABI$ HBT-BUILD-EXIT-OBJ
   OBJRES:STORE nip HBT-KEY-U T= ;

: HBT-STORE-WRONG-AOT-OBJ ( -- )
   HBB-RESET-OPTIONS
   HBT-TMP OBJRES:ROOT!
   s" wrong-aarch64" HBT-BUILD-EXIT-OBJ
   OBJSTORE:STORE {: key:ptr keyu:n :}
   HBT-AOT-SOURCE-KEY!
   HBT-SRC-KEY HBT-KEY-U key keyu OBJIDX:STORE ;

: HBT-RUN-AOT ( -- )
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   outn 0 T=
   errn 0 T= ;

: HBT-OBJ-LOAD? ( -- bool )
   HBB-RESET-OPTIONS
   HBT-TMP OBJRES:ROOT!
   HBT-AOT-HEX!
   HBT-AOT-HEX HBT-KEY-U HBB-TARGET-ABI$ HBB-CHECKER-ABI$ HBB-COMPILER-ABI$ OBJRES:LOAD ;

: HBT-BUILD-AOT-OBJECT-PRODUCER ( -- )
   HBT-TMP HBB-CACHE-ROOT!
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBB-MAKER-RUN @ 0 <> TTRUE
   HBB-OBJECT-HIT @ 0= TTRUE
   HBB-OBJECT-STORE @ 0 <> TTRUE
   HBT-OBJ-LOAD? TTRUE
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBB-OBJECT-HIT @ 0 <> TTRUE
   HBB-OBJECT-STORE @ 0= TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBB-MAKER-BUILD @ 0= TTRUE
   HBT-RUN-AOT
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   BF-TMP-RESET ;

: HBT-BUILD-AOT-OBJECT-HIT ( -- )
   HBT-TMP HBB-CACHE-ROOT!
   HBT-STORE-AOT-OBJ
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   HBB-OBJECT-HIT @ 0 <> TTRUE
   HBB-OBJECT-STORE @ 0= TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBB-MAKER-BUILD @ 0= TTRUE
   HBT-AOT-OUT FILE? TTRUE
   HBT-RUN-AOT ;

\ EXPORT keeps one body (dot habu-compiler-pkg-re-688212c1): the stripped AOT
\ binary carries no names, so a program calling a word through its defining
\ package AND a re-exported alias must be byte-identical to the same program
\ calling the defining name twice — a second body or a diverged call target
\ changes the bytes. Both variants build to the SAME output path so the ad-hoc
\ signature identifier cannot differ; the alias variant also runs, proving
\ both names execute the one body.
: HBT-EXP-SRC ( -- ptr u8 n )
   HBT-EXP-SRC-BUF HBT-EXP-SRC-U @ ;

: HBT-EXP-OUT ( -- ptr u8 n )
   HBT-EXP-OUT-BUF HBT-EXP-OUT-U @ ;

: HBT-EXP-COMMON ( -- )
   s" package XA" SB-APPEND HBB-LF SB-APPEND-C
   s" public" SB-APPEND HBB-LF SB-APPEND-C
   s" : W ( i64 -- i64 ) dup * ;" SB-APPEND HBB-LF SB-APPEND-C
   s" ;package" SB-APPEND HBB-LF SB-APPEND-C ;

: HBT-EXP-REF-SRC$ ( -- ptr u8 n )
   SB-RESET
   HBT-EXP-COMMON
   s" : MAIN ( -- ) 5 XA:W . cr 5 XA:W . cr ;" SB-APPEND HBB-LF SB-APPEND-C
   SB$ ;

: HBT-EXP-ALIAS-SRC$ ( -- ptr u8 n )
   SB-RESET
   HBT-EXP-COMMON
   s" package XB" SB-APPEND HBB-LF SB-APPEND-C
   s" public" SB-APPEND HBB-LF SB-APPEND-C
   s" EXPORT XA:W" SB-APPEND HBB-LF SB-APPEND-C
   s" ;package" SB-APPEND HBB-LF SB-APPEND-C
   s" : MAIN ( -- ) 5 XA:W . cr 5 XB:W . cr ;" SB-APPEND HBB-LF SB-APPEND-C
   SB$ ;

: HBT-EXP-BUILD ( ptr u8 n -- ) {: sa:ptr su:n :}
   HBT-EXP-SRC sa su WRITE-ALL
   HBT-EXP-OUT HBT-REMOVE-FILE?
   HBT-EXP-SRC HBT-EXP-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBT-REMOVE-ARTIFACT ;

: HBT-EXP-HASH ( ptr u8 -- ) {: hex:ptr :}
   HBT-EXP-OUT HBT-EXP-DG SHA256-FILE 0 T=
   HBT-EXP-DG hex SHA256>HEX ;

: HBT-EXP-RUN ( -- )
   HBT-EXP-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn s" 25" CONTAINS? TTRUE ;

: HBT-BUILD-AOT-EXPORT-ONE-BODY ( -- )
   HBT-TMP HBB-CACHE-ROOT!
   HBT-ROOT s" exp.f" HBT-EXP-SRC-BUF HBT-EXP-SRC-U HBT-PATH!
   HBT-ROOT s" exp" HBT-EXP-OUT-BUF HBT-EXP-OUT-U HBT-PATH!
   HBT-EXP-REF-SRC$ HBT-EXP-BUILD
   HBT-EXP-HEX1 HBT-EXP-HASH
   HBT-EXP-ALIAS-SRC$ HBT-EXP-BUILD
   HBT-EXP-HEX2 HBT-EXP-HASH
   HBT-EXP-HEX1 64 HBT-EXP-HEX2 64 T$=
   HBT-EXP-RUN
   HBT-EXP-OUT HBT-REMOVE-FILE?
   BF-TMP-RESET ;

: HBT-ENG ( -- ptr u8 n )
   HBT-ENG-BUF HBT-ENG-U @ ;

: HBT-WRITE-ENG ( -- )
   HBT-ROOT s" engine-alt" HBT-ENG-BUF HBT-ENG-U HBT-PATH!
   HBT-ENG s" not-the-real-engine" WRITE-ALL ;

: HBT-ABI-MAKER-SUFFIX ( -- )
   HBB-CHECKER-ABI$ {: a:ptr u:n :}
   a u HBT-KEY-U - BYTE+ HBT-KEY-U HBB-MAKER-KEY-HEX HBT-KEY-U STR= TTRUE ;

: HBT-ABI-MAKER-QUALIFIED ( -- )
   HBB-RESET-OPTIONS
   HBB-CHECKER-ABI$ s" checker-effect-v1+" STARTS-WITH? TTRUE
   HBB-COMPILER-ABI$ s" hb-arm64-v1+" STARTS-WITH? TTRUE
   HBB-CHECKER-ABI$ nip 82 T=
   HBB-COMPILER-ABI$ nip 76 T=
   HBT-ABI-MAKER-SUFFIX ;

: HBT-ENGINE-KEY-FLIP ( -- )
   HBT-ABI-MAKER-QUALIFIED
   HBT-WRITE-ENG
   HBT-OBJ-LOAD? TTRUE
   HBT-ENG BF-ENGINE!
   HBT-OBJ-LOAD? TFALSE
   BF-ENGINE-RESET
   HBT-OBJ-LOAD? TTRUE ;

: HBT-ALT-CHECKER$ ( -- ptr u8 n )
   HBB-CHECKER-ABI$ {: a:ptr u:n :}
   a HBT-ABI-ALT u BYTE-COPY
   HBT-ABI-ALT u 1 - + c@ STR-ZERO = if STR-ZERO 1 + else STR-ZERO then
   HBT-ABI-ALT u 1 - + c!
   HBT-ABI-ALT u ;

: HBT-PRODUCER-KEY-MISS ( -- )
   HBB-RESET-OPTIONS
   HBT-TMP OBJRES:ROOT!
   HBT-AOT-HEX!
   HBT-ALT-CHECKER$ {: alt:ptr altu:n :}
   HBT-AOT-HEX HBT-KEY-U HBB-TARGET-ABI$ alt altu HBB-COMPILER-ABI$ OBJRES:LOAD TFALSE ;

: HBT-BUILD-AOT-WRONG-OBJECT-FAILS ( -- )
   HBT-AOT-SRC HBT-AOT-SRC2$ WRITE-ALL
   HBT-AOT-OUT FILE? if HBT-AOT-OUT REMOVE-FILE then
   HBT-STORE-WRONG-AOT-OBJ
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   [: HBB-BUILD ;] E-OBJ-SCHEMA TTHROWSQ
   HBB-MAKER-RUN @ 0= TTRUE
   HBT-AOT-OUT EXISTS? TFALSE ;

\ The AOT/REPL/object cache keys fold the whole require/include closure, so a
\ content edit to a required file (not just the top-level source) must change the
\ key. This is the property a single-file digest could not provide.
: HBT-ENTRY-CLOSURE$ ( -- ptr u8 n )
   SB-RESET
   s" require " SB-APPEND
   HBT-DEP-SRC SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-ARTIFACT-KEY ( ptr u8 -- ) {: dst:ptr :}
   HBB-ARTIFACT-KEY!
   HBB-ARTIFACT-KEY-HEX dst 64 BYTE-COPY ;

: HBT-CLOSURE-KEY-CHANGES ( -- )
   HBB-RESET-OPTIONS
   HBT-TMP HBB-CACHE-ROOT!
   HBT-ROOT s" dep-closure.f" HBT-DEP-SRC-BUF HBT-DEP-SRC-U HBT-PATH!
   HBT-ROOT s" entry-closure.f" HBT-ENTRY-SRC-BUF HBT-ENTRY-SRC-U HBT-PATH!
   HBT-DEP-SRC s\" \\ dep v1\n" WRITE-ALL
   HBT-ENTRY-SRC HBT-ENTRY-CLOSURE$ WRITE-ALL
   HBT-ENTRY-SRC HBB-SRC!
   HBT-KEY-A HBT-ARTIFACT-KEY
   HBT-DEP-SRC s\" \\ dep v2 changed\n" APPEND-FILE
   HBT-KEY-B HBT-ARTIFACT-KEY
   HBT-KEY-A 64 HBT-KEY-B 64 STR= TFALSE
   HBT-ENTRY-SRC HBB-SRC!
   HBB-SRC-CLOSURE-HEX! HBB-SRC-CLOSURE-HEX HBT-KEY-A 64 BYTE-COPY
   HBT-DEP-SRC s\" \\ dep v3 changed\n" APPEND-FILE
   HBB-SRC-CLOSURE-HEX! HBB-SRC-CLOSURE-HEX HBT-KEY-B 64 BYTE-COPY
   HBT-KEY-A 64 HBT-KEY-B 64 STR= TFALSE ;

\ Maker-build failures must be attributable even when the child is silent:
\ the die message carries the child rc.
: HBT-MAKER-DIE-MSG ( -- )
   7 HBB-MAKER-DIE$ s" hb-build: native maker build failed, rc 7" T$=
   75 HBB-MAKER-DIE$ s" rc 75" CONTAINS? TTRUE ;

\ tools/dynamic-tail-manifest.f is a behaviour-bearing dependency of the
\ discovery producer (tools/source-discovery.f requires it, and its rows steer
\ closure computation), so its content must fold into the maker cache key. The
\ key preimage records each tool source through CK-FILE+, which appends the path
\ fragment and then the file's content digest with no earlier return, so the
\ presence of the manifest path in CK-BUF proves its content participates in the
\ key. If the manifest is missing from HBB-KEY-LOAD-FILES a manifest edit
\ silently reuses a stale hb-build maker artifact.
: HBT-MAKER-KEY-FOLDS-MANIFEST ( -- )
   CK-CACHE-CLEAR!
   CK-RESET
   HBB-KEY-LOAD-FILES
   CK-BUF CK-U @ s" tools/dynamic-tail-manifest.f" CONTAINS? TTRUE ;

: HBT-MAIN ( -- )
   T-RESET
   HBT-MAKER-DIE-MSG
   HBT-MAKER-KEY-FOLDS-MANIFEST
   HBT-PREPARE
   HBT-BUILD-REPL
   HBT-REBUILD-REPL-CACHE
   HBT-CACHE-KEY-CHANGES
   HBT-CLOSURE-KEY-CHANGES
   HBT-RUN-REPL
   HBT-RUN-REPL-ARGS
   HBT-IMGDUMP-REPL
   HBT-BUILD-REPL-BAD
   HBT-BUILD-MISSING-TMP
   HBT-BUILD-AOT-OBJECT-PRODUCER
   HBT-BUILD-AOT-OBJECT-HIT
   HBT-BUILD-AOT-EXPORT-ONE-BODY
   HBT-ENGINE-KEY-FLIP
   HBT-PRODUCER-KEY-MISS
   HBT-BUILD-AOT-WRONG-OBJECT-FAILS
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-test: ok" type cr ;

HBT-MAIN
