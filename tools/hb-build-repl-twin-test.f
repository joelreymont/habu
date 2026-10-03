\ hb-build-repl-twin-test.f - checked fixture for tools/hb-build-lib.f: two
\ --repl builds of one program write the same bytes, and so does the maker run
\ directly to an output of another name, and the restored image
\ answers with its own process where the build's would be wrong: a `does>` run
\ before the session's first `create` patches the image's own created word, the
\ AOT code-span table reads as it does in a fresh engine, and a persist that
\ names no output path is refused. tools/hb-build-test-lib.f lists the other
\ hb-build rows.
\ Run: bin/hb --load tools/hb-build-repl-twin-test.f

require tools/hb-build-test-lib.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

variable HBT-TWIN-SRC-U
variable HBT-TWIN-TMP-A-U
variable HBT-TWIN-TMP-B-U
variable HBT-TWIN-OUT-A-U
variable HBT-TWIN-OUT-B-U
variable HBT-TWIN-OUT-C-U
create HBT-TWIN-SRC-BUF FS-PATH-CAP allot
create HBT-TWIN-TMP-A-BUF FS-PATH-CAP allot
create HBT-TWIN-TMP-B-BUF FS-PATH-CAP allot
create HBT-TWIN-OUT-A-BUF FS-PATH-CAP allot
create HBT-TWIN-OUT-B-BUF FS-PATH-CAP allot
create HBT-TWIN-OUT-C-BUF FS-PATH-CAP allot

: HBT-TWIN-SRC ( -- ptr u8 n ) HBT-TWIN-SRC-BUF HBT-TWIN-SRC-U @ ;
: HBT-TWIN-TMP-A ( -- ptr u8 n ) HBT-TWIN-TMP-A-BUF HBT-TWIN-TMP-A-U @ ;
: HBT-TWIN-TMP-B ( -- ptr u8 n ) HBT-TWIN-TMP-B-BUF HBT-TWIN-TMP-B-U @ ;
: HBT-TWIN-OUT-A ( -- ptr u8 n ) HBT-TWIN-OUT-A-BUF HBT-TWIN-OUT-A-U @ ;
: HBT-TWIN-OUT-B ( -- ptr u8 n ) HBT-TWIN-OUT-B-BUF HBT-TWIN-OUT-B-U @ ;
: HBT-TWIN-OUT-C ( -- ptr u8 n ) HBT-TWIN-OUT-C-BUF HBT-TWIN-OUT-C-U @ ;

\ The build creates X and nothing after it, so X is the record LASTC-CELL names
\ when the image is written. The clause cannot be part of the program: the build
\ compiles at tier 1, which refuses a definition that is only a `does>`.
: HBT-TWIN-SRC$ ( -- ptr u8 n )
   S\" package HBT-TWIN\npublic\n: MAKE ( -- ) create 5 , ;\nMAKE X\n;package\n: MAIN ( -- ) ;\n" ;

\ The session's first `does>` runs before it creates anything, so it patches the
\ record the restored LASTC-CELL names. Naming this process's X, the clause reads
\ the 5 the build stored; the build's address instead lands wherever that
\ address falls in this process.
: HBT-TWIN-INPUT$ ( -- ptr u8 n )
   S\" : BEHAVE ( -- ) does> ( -- n ) @ ;\nBEHAVE\nHBT-TWIN:X . cr\n" ;

\ Each build gets its own HB_TMP and cache root, so a path either one wrote into
\ its image shows up as a difference. The two outputs have the same length; the
\ direct maker run writes to a name of another length.
: HBT-TWIN-PREPARE ( -- )
   HBT-ROOT s" twin.f" HBT-TWIN-SRC-BUF HBT-TWIN-SRC-U HBT-PATH!
   HBT-ROOT s" twin-tmp-a" HBT-TWIN-TMP-A-BUF HBT-TWIN-TMP-A-U HBT-PATH!
   HBT-ROOT s" twin-tmp-b" HBT-TWIN-TMP-B-BUF HBT-TWIN-TMP-B-U HBT-PATH!
   HBT-ROOT s" twin-out-a" HBT-TWIN-OUT-A-BUF HBT-TWIN-OUT-A-U HBT-PATH!
   HBT-ROOT s" twin-out-b" HBT-TWIN-OUT-B-BUF HBT-TWIN-OUT-B-U HBT-PATH!
   HBT-ROOT s" twin-direct-image" HBT-TWIN-OUT-C-BUF HBT-TWIN-OUT-C-U HBT-PATH!
   HBT-TWIN-TMP-A MAKE-DIR
   HBT-TWIN-TMP-B MAKE-DIR
   HBT-TWIN-SRC HBT-TWIN-SRC$ WRITE-ALL ;

: HBT-TWIN-BUILD ( ptr u8 n ptr u8 n -- ) {: tmp:ptr tmpu:n out:ptr outu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN tmp tmpu >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN tmp tmpu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build-lib.f" >LEN PROC-ARGV+
   s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --repl" >LEN PROC-ARGV+
   HBT-TWIN-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   out outu >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outn:n errn:n rc:n :}
   rc 0 <> if HBT-OUT outn type HBT-ERR errn type then
   rc 0 T= ;

\ The maker hb-build runs, run directly: its image is written and signed at this
\ name, where hb-build's is written at a private name and renamed.
: HBT-TWIN-MAKE ( ptr u8 n -- ) {: out:ptr outu:n :}
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   HBT-TWIN-SRC >LEN PROC-ARGV+
   out outu >LEN PROC-ARGV+
   s" bin/hb" >LEN S\" require tools/app-build.f\nAPP-BUILD:RUN\n" >LEN
   HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rc:n :}
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

\ The same program built three times is the same image: every cell the build
\ process owns is written as a value any process restores, or not written at all,
\ and the signature names no file (a codesign identifier defaults to the name).
: HBT-TWIN-SAME ( -- )
   HBT-TWIN-OUT-A HBT-TWIN-READ {: a:ptr au:n :}
   HBT-TWIN-OUT-B HBT-TWIN-READ {: b:ptr bu:n :}
   HBT-TWIN-OUT-C HBT-TWIN-READ {: c:ptr cu:n :}
   a au b bu HBT-DIFF-AT -1 T=
   a au c cu HBT-DIFF-AT -1 T= ;

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
   S\" require src/habu/aot-closure.f\npackage AOT-LINK\nSPAN-N . SPAN-BASE-N dbase@ - .\n0 SPAN-OFF . 0 SPAN-BYTES . cr\n;package\n" ;

: HBT-TWIN-SPAN ( -- )
   s" bin/hb" HBT-TWIN-SPAN-INPUT$ HBT-OUT HBT-ERR HBT-TWIN-RUN
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

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-TWIN-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-TWIN-PREPARE
   HBT-TWIN-TMP-A HBT-TWIN-OUT-A HBT-TWIN-BUILD
   HBT-TWIN-TMP-B HBT-TWIN-OUT-B HBT-TWIN-BUILD
   HBT-TWIN-OUT-C HBT-TWIN-MAKE
   HBT-TWIN-SAME
   HBT-TWIN-DOES
   HBT-TWIN-SPAN
   HBT-TWIN-UNNAMED
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-repl-twin-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-TWIN-MAIN
