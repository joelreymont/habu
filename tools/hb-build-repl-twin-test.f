\ hb-build-repl-twin-test.f - checked fixture for tools/hb-build-lib.f: --repl
\ builds of one program write the same image across the run's warm cache (A, D)
\ and a fresh one (B) and an output of another name. The restored image answers
\ with its own process where the build's would be wrong: a `does>` run before
\ the session's first `create` patches the image's own created word, a restored
\ FILE import keeps its retired context handle stale, the AOT code-span table
\ reads as it does in a fresh engine, and a persist that names no output path
\ is refused.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-repl-twin-test.f
\ Set HABU_RETAIN_TWIN=1 to keep the three executable images and FILE objects
\ under the printed test root for independent inspection.

require tools/hb-build-test-lib.f
require src/lib/string.f
require src/lib/engine-candidate.f
require test/app-builder-engine.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

variable HBT-TWIN-SRC-U
variable HBT-TWIN-CHILD-U
variable HBT-TWIN-CTX-U
variable HBT-TWIN-TMP-A-U
variable HBT-TWIN-TMP-B-U
variable HBT-TWIN-OUT-A-U
variable HBT-TWIN-OUT-B-U
variable HBT-TWIN-OUT-D-U
create HBT-TWIN-SRC-BUF FS-PATH-CAP allot
create HBT-TWIN-CHILD-BUF FS-PATH-CAP allot
create HBT-TWIN-CTX-BUF FS-PATH-CAP allot
create HBT-TWIN-TMP-A-BUF FS-PATH-CAP allot
create HBT-TWIN-TMP-B-BUF FS-PATH-CAP allot
create HBT-TWIN-OUT-A-BUF FS-PATH-CAP allot
create HBT-TWIN-OUT-B-BUF FS-PATH-CAP allot
create HBT-TWIN-OUT-D-BUF FS-PATH-CAP allot

: HBT-TWIN-SRC ( -- ptr u8 n ) HBT-TWIN-SRC-BUF HBT-TWIN-SRC-U @ ;
: HBT-TWIN-CHILD ( -- ptr u8 n ) HBT-TWIN-CHILD-BUF HBT-TWIN-CHILD-U @ ;
: HBT-TWIN-CTX ( -- ptr u8 n ) HBT-TWIN-CTX-BUF HBT-TWIN-CTX-U @ ;
: HBT-TWIN-TMP-A ( -- ptr u8 n ) HBT-TWIN-TMP-A-BUF HBT-TWIN-TMP-A-U @ ;
: HBT-TWIN-TMP-B ( -- ptr u8 n ) HBT-TWIN-TMP-B-BUF HBT-TWIN-TMP-B-U @ ;
: HBT-TWIN-OUT-A ( -- ptr u8 n ) HBT-TWIN-OUT-A-BUF HBT-TWIN-OUT-A-U @ ;
: HBT-TWIN-OUT-B ( -- ptr u8 n ) HBT-TWIN-OUT-B-BUF HBT-TWIN-OUT-B-U @ ;
: HBT-TWIN-OUT-D ( -- ptr u8 n ) HBT-TWIN-OUT-D-BUF HBT-TWIN-OUT-D-U @ ;

\ The build creates X and nothing after it, so X is the record LASTC-CELL names
\ when the image is written. The clause cannot be part of the program: the build
\ compiles at tier 1, which refuses a definition that is only a `does>`.
: HBT-TWIN-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" : LOAD-CHILD ( -- ) s\" " SB-APPEND
   HBT-TWIN-CHILD SB-APPEND
   S\" \" required ;\nLOAD-CHILD\ns\" " SB-APPEND
   HBT-TWIN-CTX SB-APPEND
   S\" \" required\npackage HBT-TWIN\npublic\n: MAKE ( -- ) create 5 , ;\nMAKE X\n: REFUSAL ( -- ) s\" TW-BAD ( -- ptr u8 n ptr u8 n ) 1 2 3 4 5 6\" CHECK-QUIET-CANDIDATE! 0 <> IF 74 throw THEN ;\n;package\n: MAIN ( -- ) HBT-TWIN-CHILD:NUMBER drop ;\nHBT-TWIN:REFUSAL\n" SB-APPEND
   SB$ ;

: HBT-TWIN-CTX$ ( -- ptr u8 n )
   SB-RESET
   S\" package HBT-TWIN-CTX\nprivate\n: BND ( -- CBIND:binding )\n" SB-APPEND
   S\" CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE\n" SB-APPEND
   S\" CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT\n" SB-APPEND
   S\" CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN\n" SB-APPEND
   S\" CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY CBIND:BIND ;\n" SB-APPEND
   S\" public\n1 TYPED-BUFFER SAVED IR-CTX:ctx\n" SB-APPEND
   S\" 1 TYPED-BUFFER SAVED-MODULE IR-ID:ir-module-id\n" SB-APPEND
   S\" : SAVE-CTX ( -- ) BND [: 0 SAVED ! ;] IR-CTX:WITH-CONTEXT ;\n" SB-APPEND
   S\" : SAVE-MODULE ( -- ) IR-ID:NEW-MODULE nip 0 SAVED-MODULE ! ;\n" SB-APPEND
   S\" : CHECK-CTX ( IR-CTX:ctx -- n )\n" SB-APPEND
   S\" dup IR-CTX:SERIAL swap drop 0 SAVED @ IR-CTX:LIVE? if 73 throw then ;\n" SB-APPEND
   S\" : PROBE-CTX ( -- n )\n" SB-APPEND
   S\" BND IR-CTX:SESSION-OPEN drop\n0\n10 0 ?do\n" SB-APPEND
   S\" drop BND [: CHECK-CTX ;] IR-CTX:WITH-CONTEXT\nloop\n" SB-APPEND
   S\" IR-CTX:SESSION-CLOSE ;\n" SB-APPEND
   S\" : PROBE-MODULE ( -- n )\n0\n32 0 ?do\n" SB-APPEND
   S\" drop IR-ID:NEW-MODULE nip 0 SAVED-MODULE @ IR-ID:MODULE-SAME? if 75 throw then 0\nloop ;\n" SB-APPEND
   S\" ;package\nHBT-TWIN-CTX:SAVE-CTX\n" SB-APPEND
   S\" HBT-TWIN-CTX:SAVE-MODULE\n" SB-APPEND
   SB$ ;

: HBT-TWIN-CHILD$ ( -- ptr u8 n )
   S\" package HBT-TWIN-CHILD\npublic\n: NUMBER ( -- n ) 5 ;\n;package\n" ;

\ The session's first `does>` runs before it creates anything, so it patches the
\ record the restored LASTC-CELL names. Naming this process's X, the clause reads
\ the 5 the build stored; the build's address instead lands wherever that
\ address falls in this process.
: HBT-TWIN-INPUT$ ( -- ptr u8 n )
   S\" : BEHAVE ( -- ) does> ( -- n ) @ ;\nBEHAVE\nHBT-TWIN:REFUSAL\nHBT-TWIN:X . cr\n" ;

\ A and D share HB_TMP and the run's cache; B gets its own HB_TMP and cache root.
\ B's output name is longer than A's and D's.
: HBT-TWIN-PREPARE ( -- )
   HBT-ROOT s" twin.f" HBT-TWIN-SRC-BUF HBT-TWIN-SRC-U HBT-PATH!
   HBT-ROOT s" twin-child.f" HBT-TWIN-CHILD-BUF HBT-TWIN-CHILD-U HBT-PATH!
   HBT-ROOT s" twin-ctx.f" HBT-TWIN-CTX-BUF HBT-TWIN-CTX-U HBT-PATH!
   HBT-ROOT s" twin-tmp-a" HBT-TWIN-TMP-A-BUF HBT-TWIN-TMP-A-U HBT-PATH!
   HBT-ROOT s" twin-tmp-b" HBT-TWIN-TMP-B-BUF HBT-TWIN-TMP-B-U HBT-PATH!
   HBT-ROOT s" twin-out-a" HBT-TWIN-OUT-A-BUF HBT-TWIN-OUT-A-U HBT-PATH!
   HBT-ROOT s" twin-direct-image" HBT-TWIN-OUT-B-BUF HBT-TWIN-OUT-B-U HBT-PATH!
   HBT-ROOT s" twin-out-d" HBT-TWIN-OUT-D-BUF HBT-TWIN-OUT-D-U HBT-PATH!
   HBT-TWIN-TMP-A MAKE-DIR
   HBT-TWIN-TMP-B MAKE-DIR
   HBT-TWIN-CHILD HBT-TWIN-CHILD$ WRITE-ALL
   HBT-TWIN-CTX HBT-TWIN-CTX$ WRITE-ALL
   HBT-TWIN-SRC HBT-TWIN-SRC$ WRITE-ALL ;

\ An empty cache leaves HABU_BUILD_CACHE inherited: under the gate, the run's
\ shared root, which holds the app-builder family image.
: HBT-TWIN-BUILD ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: tmp:ptr tmpu:n cache:ptr cacheu:n out:ptr outu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN tmp tmpu >LEN PROC-ENV+
   cacheu 0 > if s" HABU_BUILD_CACHE" >LEN cache cacheu >LEN PROC-ENV+ then
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build-lib.f" >LEN PROC-ARGV+
   s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --repl" >LEN PROC-ARGV+
   HBT-TWIN-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   out outu >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE HBT-CAPTURE>N
      {: outn:n errn:n rc:n :}
   rc 0 <> if HBT-OUT outn type HBT-ERR errn type then
   rc 0 T= ;

: HBT-TWIN-READ ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   buf  path pathu buf cap READ-ALL ;

\ Run one executable with one stdin script, capturing into the given buffers.
: HBT-TWIN-RUN ( ptr u8 n ptr u8 n ptr u8 ptr u8 -- n n n )
   {: exe:ptr exeu:n in:ptr inu:n out:ptr err:ptr :}
   PROC-ARGV-RESET
   exe exeu >LEN in inu >LEN
   out HBT-CAPTURE-CAP >LEN err HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE HBT-CAPTURE>N ;

\ The same program built three times is the same image across the run's warm
\ cache (A, D) and a fresh one (B) and an output of another name: every cell
\ the build process owns is written as a value any process restores, or not
\ written at all, and the signature names no file (a codesign identifier
\ defaults to the name).
: HBT-TWIN-SAME ( -- )
   HBT-TWIN-OUT-A HBT-TWIN-READ {: a:ptr au:n :}
   HBT-TWIN-OUT-B HBT-TWIN-READ {: b:ptr bu:n :}
   HBT-TWIN-OUT-D HBT-TWIN-READ {: d:ptr du:n :}
   a au b bu HBT-DIFF-AT -1 T=
   a au d du HBT-DIFF-AT -1 T= ;

\ A restored FILE import keeps its retired context handle stale when restored
\ compilation reuses the same registry slot. D's bytes equal A's, so A alone runs.
: HBT-TWIN-GENERATIONS ( -- )
   HBT-TWIN-OUT-A
   S\" IR-CTX:EPOCH . HBT-TWIN-CTX:PROBE-CTX . HBT-TWIN-CTX:PROBE-MODULE . cr\n"
   HBT-RUN-OUT HBT-RUN-ERR HBT-TWIN-RUN
   {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$= ;

: HBT-TWIN-DOES ( -- )
   HBT-TWIN-OUT-A HBT-TWIN-INPUT$ HBT-RUN-OUT HBT-RUN-ERR HBT-TWIN-RUN
   {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn S\" 5\n\n" T$= ;

\ The seed publishes the engine's code-span table - its __text address, row count
\ and the blob's landing address - before the snapshot restore copies DATA, so
\ the restore has to keep this boot's three. The image carries the same engine
\ text as the fresh engine, so both must read the same rows at the same distance
\ from their own region base; the build's addresses instead name the build's
\ text and region, which fault or read other bytes here.
: HBT-TWIN-SPAN-INPUT$ ( -- ptr u8 n )
   S\" require src/habu/aot-closure.f\npackage AOT-LINK\n: HBT-SPAN-PROBE ( -- )\nSPAN-N . SPAN-BASE-N dbase@ - .\nSPAN-N 0 > if 0 SPAN-OFF . 0 SPAN-BYTES . then cr ;\nHBT-SPAN-PROBE\n;package\n" ;

: HBT-TWIN-SPAN ( -- )
   ENGINE-CANDIDATE:PATH$ HBT-TWIN-SPAN-INPUT$ HBT-OUT HBT-ERR HBT-TWIN-RUN
   {: wantn:n wanterr:n wantrc:n :}
   wantrc 0 T=
   HBT-TWIN-OUT-A HBT-TWIN-SPAN-INPUT$ HBT-RUN-OUT HBT-RUN-ERR HBT-TWIN-RUN
   {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn HBT-OUT wantn T$= ;

\ The image stores no output path, so a persist in the restored process that does
\ not name its own is refused rather than writing into the build's directory.
: HBT-TWIN-UNNAMED ( -- )
   HBT-TWIN-OUT-A S\" SNAP:PERSIST\n" HBT-RUN-OUT HBT-RUN-ERR HBT-TWIN-RUN
   {: outn:n errn:n rcn:n :}
   rcn 74 T=
   HBT-RUN-ERR errn s" snap: persist has no output path" CONTAINS? TTRUE ;

: HBT-TWIN-FINISH ( -- )
   s" HABU_RETAIN_TWIN" GETENV dup 0 > if
      2drop CLEANUP-RESET
      s" hb-build-repl-twin-test artifacts: " type HBT-ROOT type cr
   else
      2drop CLEANUP-RUN
      HBT-ROOT EXISTS? TFALSE
   then ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-TWIN-MAIN ( -- )
   T-RESET
   APP-BUILDER-ENGINE:ENSURE
   HBT-PREPARE
   HBT-TWIN-PREPARE
   HBT-TWIN-TMP-A NULL$ HBT-TWIN-OUT-A HBT-TWIN-BUILD
   \ B's fresh cache root is the cold boundary: its builder is built there.
   HBT-TWIN-TMP-B HBT-TWIN-TMP-B HBT-TWIN-OUT-B HBT-TWIN-BUILD
   HBT-TWIN-TMP-A NULL$ HBT-TWIN-OUT-D HBT-TWIN-BUILD
   HBT-TWIN-SAME
   HBT-TWIN-GENERATIONS
   HBT-TWIN-DOES
   HBT-TWIN-SPAN
   HBT-TWIN-UNNAMED
   HBT-TWIN-FINISH
   T-REPORT
   s" hb-build-repl-twin-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-TWIN-MAIN
