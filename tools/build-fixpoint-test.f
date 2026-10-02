\ build-fixpoint-test.f - checked fixture for tools/build-fixpoint.f: the
\ refresh build and every case that reads its output (candidate boot, cached
\ skip, the watermark refusals on its capture host, the source-arena boundary
\ on its candidate), the CLI footguns, the stamp key and the boot pins. The
\ stale-seed sandbox cases are tools/build-fixpoint-sandbox-test.f, the emitted
\ and certified sources tools/build-fixpoint-source-test.f and the snapshot
\ trailer tools/build-fixpoint-snapshot-test.f; each is a gate row of its own,
\ because one row running every case took 293-338 s in the gate's pool.
\ Run: bin/hb --load tools/build-fixpoint-test.f

require tools/build-fixpoint-test-lib.f
require lib/string-roles.f               \ package STR: the typed string surface
require lib/adt/option.f                 \ option<NUM:index> STR:FIND-SUB consumer
require lib/test/mapped.f

\ The shared fixture's words are private words of the tool's package, so this
\ row reopens it the way tools/build-fixpoint-test-lib.f does.
package BUILD-FIXPOINT

13 constant BFT-BUILD-ARGV#
create BFT-KEY1 64 allot

: BFT-ENV$ ( -- ptr u8 n )
   BFT-ROOT ;

: BFT-ARGV-ENV+ ( ptr u8 n ptr u8 n -- ) {: tmp:ptr tmpu:n stamp:ptr stampu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN tmp tmpu >LEN PROC-ENV+
   s" HABU_FIXPOINT_STAMP" >LEN stamp stampu >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN BFT-HB >LEN PROC-ENV+ ;

: BFT-ARGV-FIXPOINT ( ptr u8 n ptr u8 n -- n )
   BFT-ARGV-ENV+
   BFT-ARGV-LOAD-FILES
   PROC-ARGV-N @ COUNT>N ;

: BFT-ARGV-FIXPOINT-NO-MAIN ( ptr u8 n ptr u8 n -- n )   \ BFT-ARGV-FIXPOINT minus the -main companion: reproduces the recovery footgun
   BFT-ARGV-ENV+
   BFT-ARGV-LOAD-LIBS
   PROC-ARGV-N @ COUNT>N ;

: BFT-ARGV-NO-PREAMBLE ( ptr u8 n ptr u8 n -- n )   \ tool source WITHOUT the lib preamble: reproduces the missing-preamble footgun (bare `E-UNDEFINED: FS-PATH-CAP` pre-fix)
   BFT-ARGV-ENV+
   s" --load" >LEN PROC-ARGV+
   s" tools/build-fixpoint.f" BFT-ARG+
   PROC-ARGV-N @ COUNT>N ;

: BFT-ARGV-BUILD ( -- n )
   BFT-ENV$ BFT-STAMP BFT-ARGV-FIXPOINT ;

: BFT-ARGV-FAIL ( -- n )
   BFT-NOTDIR BFT-STAMP2 BFT-ARGV-FIXPOINT ;

: BFT-ARGV-ALL-FORCE ( -- )
   s" --" BFT-ARG+
   s" all" BFT-ARG+
   s" --force" BFT-ARG+ ;

: BFT-ARGV-ALL ( -- )
   s" --" BFT-ARG+
   s" all" BFT-ARG+ ;

: BFT-ARGV-INSTALL-FORCE ( -- )
   s" --" BFT-ARG+
   s" install" BFT-ARG+
   s" --force" BFT-ARG+ ;

: BFT-SPAWN-FIXPOINT ( -- n n n )
   s" bin/hb" >LEN PROC-ARGV-CHECK-PATH
   BFT-CAPTURE-CAP >LEN BFT-CAPTURE-CAP >LEN PROC-CAPTURE-CHECK-CAPS
   s" bin/hb" >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   BFT-TIMEOUT-MS >MS PROC-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-CAPTURE
   BFT-OUT BFT-CAPTURE-CAP >LEN BFT-ERR BFT-CAPTURE-CAP >LEN PROC-RUN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC
   BFT-CAPTURE>N ;

: BFT-RUN-BUILD ( -- n n n )
   BFT-ARGV-BUILD BFT-BUILD-ARGV# T=
   BFT-ARGV-ALL-FORCE
   BFT-SPAWN-FIXPOINT ;

: BFT-RUN-CACHED ( -- n n n )
   BFT-ARGV-BUILD BFT-BUILD-ARGV# T=
   BFT-ARGV-ALL
   BFT-SPAWN-FIXPOINT ;

: BFT-STAMP-SCOPE ( -- )
   BFT-ROOT BF-TMP!
   BFT-STAMP BF-STAMP-PATH! ;

: BFT-STAMP-UNSCOPE ( -- )
   BF-STAMP-PATH-RESET
   BF-ENGINE-RESET
   BF-TMP-RESET ;

: BFT-RECORD! ( -- )
   BF-PREFIX-SOURCE
   BF-RECORD-PREFIX
   BF-STAGE2-SOURCE
   BF-RECORD-STAGE
   BF-STDIN-SOURCE
   BF-RECORD-STDIN ;

: BFT-TEST-BUILD ( -- )
   BFT-RUN-BUILD 0 T=
   {: outu erru :}
   BFT-ERR erru BFT-EMPTY$ T$=
   BFT-OUT outu s" bin/hb refresh OK: compiler fixpoint" CONTAINS? TTRUE
   BFT-OUT outu s" boot prefix = " CONTAINS? TTRUE      \ the census reports both phases
   BFT-OUT outu s" assembled = " CONTAINS? TTRUE
   BFT-OUT outu s" snapshot image OK: candidate validated" CONTAINS? TFALSE
   BFT-OUT outu s" fixpoint: cached " CONTAINS? TFALSE
   BFT-HB FILE? TTRUE
   BFT-HB-NEW FILE? TFALSE
   BFT-STAMP FILE? TTRUE
   BFT-STAMP FILE-SIZE BF-STAMP-HEX-U 1 + T=
   BFT-STAMP-SCOPE
   BFT-HB BF-ENGINE!
   BF-STAMP-MATCH? TTRUE
   BFT-STAMP-UNSCOPE ;

\ Reuse the build case's product and host, but promote them from a copied
\ checkout. Its bin directory belongs to the source tree, not the private
\ install target, so cleanup must leave an unrelated file there alone. Repeat
\ with no bin directory: a private install also works from a source-only tree.
: BFT-CLEAN-BIN$ ( -- ptr u8 n )
   BFT-STALE s" bin" BFT-CP-BUF JOIN-PATH BFT-CP-BUF swap ;

: BFT-CLEAN-KEEP$ ( -- ptr u8 n )
   BFT-STALE s" bin/keep" BFT-CP-BUF JOIN-PATH BFT-CP-BUF swap ;

: BFT-CLEAN-DRIVER$ ( -- ptr u8 n )
   BFT-STALE s" cleanup-driver.f" BFT-CP-BUF JOIN-PATH BFT-CP-BUF swap ;

: BFT-CLEAN-ARTIFACTS ( -- )
   BFT-STALE-TMP BF-TMP!
   BFT-HB s" hb-stdin" BF-A$ COPY-FILE-STREAM
   BFT-ROOT s" hb-host" BFT-CP-BUF JOIN-PATH BFT-CP-BUF swap
   s" hb-host" BF-A$ COPY-FILE-STREAM
   BF-TMP-RESET ;

: BFT-CLEAN-ARGV ( -- )
   PROC-ARGV-RESET PROC-ENV-RESET
   s" HB_TMP" >LEN BFT-STALE-TMP >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN BFT-ENG-B >LEN PROC-ENV+
   BFT-ARGV-LOAD-LIBS
   s" cleanup-driver.f" BFT-ARG+ ;

: BFT-CLEAN-SPAWN ( -- n n n )
   BFT-CLEAN-ARGV
   BFT-ENG-A >LEN BFT-STALE >LEN
   BFT-BIG-OUT BFT-BIG-CAP >LEN BFT-BIG-ERR BFT-BIG-CAP >LEN
   BFT-TIMEOUT-MS >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   BFT-CAPTURE>N ;

: BFT-CLEAN-PROMOTED ( -- )
   BFT-CLEAN-ARTIFACTS
   BFT-CLEAN-SPAWN {: outu:n erru:n rc:n :}
   rc 0 T=
   BFT-BIG-ERR erru BFT-EMPTY$ T$=
   BFT-ENG-B BFT-HB BF-FILE= TTRUE
   BFT-ENG-B BF-ENGINE!
   BFT-ROOT s" hb-host" BFT-CP-BUF JOIN-PATH BFT-CP-BUF swap
   BF-HOST-DST$ BF-FILE= TTRUE
   BF-ENGINE-RESET ;

: BFT-TEST-PRIVATE-CLEANUP ( -- )
   BFT-STALE-PREPARE
   BFT-STALE-HB BFT-ENG-A COPY-FILE-STREAM
   BFT-ENG-A CHMOD-X
   BFT-STALE-HB REMOVE-FILE
   BFT-CLEAN-DRIVER$ S\" package BUILD-FIXPOINT\nBF-INSTALL-HB\nBF-INSTALL-HOST\nBF-CLEAN-BIN\n;package\n" WRITE-ALL
   BFT-CLEAN-KEEP$ s" unrelated" WRITE-ALL
   BFT-CLEAN-PROMOTED
   BFT-CLEAN-KEEP$ FILE? TTRUE
   BFT-CLEAN-BIN$ REMOVE-TREE
   BFT-CLEAN-BIN$ DIR? TFALSE
   BFT-CLEAN-PROMOTED
   BFT-CLEAN-BIN$ DIR? TFALSE ;

: BFT-TEST-STAMP-SEED ( -- )
   BFT-STAMP-SCOPE
   BFT-RECORD!
   BF-STAMP-WRITE
   BFT-STAMP FILE? TTRUE
   BFT-STAMP FILE-SIZE BF-STAMP-HEX-U 1 + T=
   BF-STAMP-MATCH? TTRUE
   BFT-STAMP REMOVE-FILE
   BFT-STAMP-UNSCOPE ;

: BFT-BOOT-REFUSED ( ptr u8 n -- )
   s" hb-stdin" BF-A$ COPY-FILE-STREAM
   [: BF-INSTALL-HB ;] E-BUILD-STATUS TTHROWSQ
   BFT-ENG-A s" /usr/bin/true" BF-FILE= TTRUE
   BF-INSTALL-TMP$ EXISTS? TFALSE
   BF-BOOT-ROOT$ EXISTS? TFALSE ;

: BFT-TEST-CANDIDATE-BOOT ( -- )
   BFT-ROOT s" candidate-boot" BFT-CP-BUF JOIN-PATH
   BFT-CP-BUF swap 2dup MAKE-DIRS BF-TMP!
   BFT-ENG-A BF-ENGINE!
   s" /usr/bin/true" BFT-ENG-A COPY-FILE-STREAM
   \ A failed boot and a successful exit without running the probe both refuse.
   s" /usr/bin/false" BFT-BOOT-REFUSED
   s" /usr/bin/true" BFT-BOOT-REFUSED
   BFT-HB s" hb-stdin" BF-A$ COPY-FILE-STREAM
   BF-INSTALL-HB
   BFT-ENG-A BFT-HB BF-FILE= TTRUE
   s" hb-stdin" BF-A$ EXISTS? TFALSE
   BF-INSTALL-TMP$ EXISTS? TFALSE
   BF-BOOT-ROOT$ EXISTS? TFALSE
   BF-ENGINE-RESET BF-TMP-RESET ;

\ The selected engine must be executed, not silently replaced by bin/hb.
: BFT-TEST-ENGINE-SELECTION ( -- )
   BFT-ROOT BF-TMP!
   s" /usr/bin/false" BF-ENGINE!
   [: BF-BOOTSTRAP-STAGE ;] E-BUILD-STATUS TTHROWSQ
   BF-ENGINE-RESET
   BF-TMP-RESET ;

: BFT-TEST-CACHED-SKIP ( -- )
   BFT-ROOT BF-TMP!
   s" hb-stage" BF-REMOVE-TMP
   BF-TMP-RESET
   BFT-RUN-CACHED 0 T=
   {: outu:n erru:n :}
   BFT-ERR erru BFT-EMPTY$ T$=
   BFT-OUT outu s" fixpoint: cached " CONTAINS? TTRUE
   BFT-OUT outu s" bin/hb refresh OK" CONTAINS? TFALSE
   BFT-ROOT BF-TMP!
   s" hb-stage" BF-A$ FILE? TFALSE
   BF-TMP-RESET
   BFT-HB FILE? TTRUE ;

: BFT-TEST-BUILD-FAIL-NO-STAMP ( -- )
   BFT-ARGV-FAIL BFT-BUILD-ARGV# T=
   BFT-ARGV-ALL-FORCE
   BFT-SPAWN-FIXPOINT {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-ERR erru s" build-fixpoint: failed" CONTAINS? TTRUE
   BFT-STAMP2 FILE? TFALSE ;

\ Recovery footgun (dot habu-stale-bin-hb): build-fixpoint.f loaded WITHOUT its
\ tools/build-fixpoint-main.f companion but handed an explicit `install` verb.
\ Pre-fix BF-CLI was never called, so the verb was silently dropped - the loaded
\ stdin engine read its program from the closed stdin, hit EOF, and exited 0
\ having built nothing (rc 0, empty output). The tail self-dispatch must now run
\ the verb; a non-directory HB_TMP makes the install fail fast and die loudly,
\ so a build verb can no longer exit 0 having done nothing. Base (unfixed
\ source): rc 0, empty stderr -> both direction assertions fail.
: BFT-TEST-NO-MAIN-DISPATCHES ( -- )
   BFT-NOTDIR BFT-STAMP2 BFT-ARGV-FIXPOINT-NO-MAIN BFT-BUILD-ARGV# 1 - T=
   BFT-ARGV-INSTALL-FORCE
   BFT-SPAWN-FIXPOINT {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-ERR erru s" build-fixpoint: failed" CONTAINS? TTRUE
   BFT-STAMP2 FILE? TFALSE ;

\ Missing-preamble footgun (dot habu-make-build-fixpoint): build-fixpoint.f loaded
\ WITHOUT its lib preamble handed an explicit `install` verb. Pre-fix the first
\ FS-PATH-CAP buffer create died mid-load with a bare `E-UNDEFINED: FS-PATH-CAP`
\ (rc 70) - no hint the preamble was missing. The load-discipline guard must now
\ fail fast with BF-USAGE-RC and a named diagnostic that lists the required load.
\ Base (unfixed source): rc 70, stderr `E-UNDEFINED: FS-PATH-CAP` -> both direction
\ assertions fail.
: BFT-TEST-MISSING-PREAMBLE ( -- )
   BFT-NOTDIR BFT-STAMP2 BFT-ARGV-NO-PREAMBLE DROP
   BFT-ARGV-INSTALL-FORCE
   BFT-SPAWN-FIXPOINT {: outu:n erru:n rcn:n :}
   rcn BF-USAGE-RC T=
   BFT-ERR erru s" missing required load" CONTAINS? TTRUE ;

\ The stamp key with ONE emitted source mutated between its emission and its
\ digest row. A word per phase would be a copy of the key sequence per phase,
\ and the copy is what goes stale: a phase added to BF-STAMP-KEY! and forgotten
\ here would leave its mutation case silently testing the old key. One
\ sequence, one selector.
0 constant BFT-MUT-NONE
1 constant BFT-MUT-PREFIX
2 constant BFT-MUT-STAGE
3 constant BFT-MUT-STDIN

: BFT-KEY-MUT! ( n -- ) {: mut:n :}
   BF-STAMP-KEY-BEGIN
   BF-PREFIX-SOURCE
   mut BFT-MUT-PREFIX = if s" prefix-src" BF-A$ s" \ mutated boot prefix" APPEND-FILE then
   BF-STAMP-PREFIX-KEY+
   BF-STAGE2-SOURCE
   mut BFT-MUT-STAGE = if s" stage2-src" BF-A$ s" \ mutated stage source" APPEND-FILE then
   BF-STAMP-STAGE-KEY+
   BF-STDIN-SOURCE
   mut BFT-MUT-STDIN = if s" stage2-src" BF-A$ s" \ mutated stdin source" APPEND-FILE then
   BF-STAMP-STDIN-KEY+
   BF-STAMP-KEY-END ;

: BFT-KEY-DIFFERS ( n -- ) {: mut:n :}
   mut BFT-KEY-MUT!
   BFT-KEY1 BF-STAMP-HEX-U BF-STAMP-KEY BF-STAMP-HEX-U STR= TFALSE ;

\ Every emitted source the key covers is load-bearing, and the unmutated run is
\ the control: without it, three keys differing would also be the signature of
\ a key that simply never reproduces.
: BFT-TEST-STAMP-SOURCE-KEY ( -- )
   BFT-STAMP-SCOPE
   BF-STAMP-KEY!
   BF-STAMP-KEY BFT-KEY1 BF-STAMP-HEX-U BYTE-COPY
   BFT-MUT-PREFIX BFT-KEY-DIFFERS
   BFT-MUT-STAGE BFT-KEY-DIFFERS
   BFT-MUT-STDIN BFT-KEY-DIFFERS
   BFT-MUT-NONE BFT-KEY-MUT!
   BFT-KEY1 BF-STAMP-HEX-U BF-STAMP-KEY BF-STAMP-HEX-U STR= TTRUE
   BFT-STAMP-UNSCOPE ;

: BFT-ZERO-KEY$ ( -- ptr u8 n )
   s" 0000000000000000000000000000000000000000000000000000000000000000" ;

: BFT-TEST-STAMP-CORRUPT ( -- )
   BFT-STAMP-SCOPE
   SB-RESET
   BFT-ZERO-KEY$ SB-APPEND
   BF-LF SB-APPEND-C
   BFT-STAMP SB$ WRITE-ALL
   BF-STAMP-MATCH? TFALSE
   BFT-STAMP s" short" WRITE-ALL
   BF-STAMP-MATCH? TFALSE
   BFT-STAMP REMOVE-FILE
   BF-STAMP-MATCH? TFALSE
   BFT-STAMP-UNSCOPE ;

: BFT-WRITE-ENGINES ( -- )
   BFT-ENG-A s" engine-a-bytes" WRITE-ALL
   BFT-ENG-B s" engine-b-bytes" WRITE-ALL ;

: BFT-TEST-STAMP-ENGINE ( -- )
   BFT-STAMP-SCOPE
   BFT-RECORD!
   BFT-WRITE-ENGINES
   BFT-ENG-A BF-ENGINE!
   BF-STAMP-WRITE
   BF-STAMP-MATCH? TTRUE
   BFT-ENG-B BF-ENGINE!
   BF-STAMP-MATCH? TFALSE
   BFT-ENG-A BF-ENGINE!
   -1 BF-FORCE !
   BF-STAMP-MATCH? TFALSE
   0 BF-FORCE !
   BF-STAMP-MATCH? TTRUE
   BFT-STAMP-UNSCOPE ;

: BFT-TEST-ALL-STAMP-GUARD ( -- )
   BFT-STAMP-SCOPE
   BFT-RECORD!
   BFT-WRITE-ENGINES
   BFT-STAMP FILE? if BFT-STAMP REMOVE-FILE then
   BFT-ENG-A BF-ENGINE!
   BF-ALL-STAMP
   BFT-STAMP FILE? TFALSE
   BFT-HB BF-ENGINE!
   BF-ALL-STAMP
   BFT-STAMP FILE? TTRUE
   BFT-STAMP-UNSCOPE ;

: BFT-TEST-STAMP-NESTED ( -- )
   BFT-ROOT BF-TMP!
   BFT-NEST BF-STAMP-PATH!
   BFT-RECORD!
   BF-STAMP-WRITE
   BFT-NEST FILE? TTRUE
   BF-STAMP-MATCH? TTRUE
   BFT-STAMP-UNSCOPE ;

\ typed STR:FIND-SUB boundary: route byte-lengths through the STR: role surface,
\ then read the found option<NUM:index> through NUM's one public
\ projection and mint the switchover option<idx> from it.
: BFT-FIND ( ptr u8 n ptr u8 n -- option<idx> ) {: a:ptr u:n b:ptr v:n :}
   a u STR:LENGTH b v STR:LENGTH STR:FIND-SUB MATCH option
     none OF OPTION:NONE ENDOF
     some OF NUM:ORDINAL >IDX OPTION:SOME ENDOF
   ;MATCH ;

: BFT-FOUND ( option<idx> -- n )                     \ assert found; found index (-1 after a recorded miss)
   MATCH option
     none OF STR-FALSE TTRUE -1 ENDOF
     some OF STR-TRUE TTRUE IDX>N ENDOF
   ;MATCH ;

\ THE HOST CAPABILITY THE BUILD REQUIRES, refused at the door. The payload
\ rewinds to the mark src/core/lower-cert-seal.f records at the core prefix's
\ end and carries no copy of that prefix, so a host engine without the mark
\ cannot build this tree at all - BF-PREFLIGHT has to say so before a byte is
\ emitted, not leave an undefined core word to surface from inside a generated
\ file. The sabotage is that file with its PREFIX-MARK block cut off and nothing
\ else changed: an engine reads its boot prefix from the tree it runs in, so the
\ sandbox's copy IS the child's prefix. The sandbox engine is the build case's
\ capture host (BFT-SOURCE-HOST$, hb-host in the scratch root), so this case
\ and the value case below run after the build, in a sandbox of their own.
: BFT-MARK-SABOTAGE ( -- )
   s" src/core/lower-cert-seal.f" BFT-READ {: u:n :}
   BFT-READ-BUF u s" package PREFIX-MARK" BFT-FIND BFT-FOUND {: cut:n :}
   cut 0 > TTRUE
   BFT-STALE-MARK BFT-READ-BUF cut WRITE-ALL ;

: BFT-SOURCE-HOST$ ( -- ptr u8 n )
   BFT-ROOT s" hb-host" BFT-CP-BUF JOIN-PATH BFT-CP-U !
   BFT-CP-BUF BFT-CP-U @ ;

: BFT-SOURCE-HOST! ( -- )
   BFT-SOURCE-HOST$ BFT-STALE-HB COPY-FILE-STREAM
   BFT-STALE-HB CHMOD-X ;

: BFT-TEST-WATERMARK-REQUIRED ( -- )
   BFT-STALE-PREPARE
   BFT-SOURCE-HOST!
   BFT-MARK-SABOTAGE
   BFT-STALE-ARGV
   BFT-STALE-SPAWN {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-BIG-ERR erru s" no core-prefix watermark" CONTAINS? TTRUE
   BFT-BIG-ERR erru s" src/core/lower-cert-seal.f" CONTAINS? TTRUE
   BFT-BIG-OUT outu s" self-check census" CONTAINS? TFALSE
   BFT-STALE-HB BFT-SOURCE-HOST$ BF-FILE= TTRUE
   BFT-STALE-STAMP FILE? TFALSE ;

\ THE SAME PROBE'S VALUE LAYER, forged twice. Both fixtures leave every name in
\ place, so the resolvability clause above passes and only the value can refuse -
\ which is the whole point of reading one: a name resolves whether or not
\ anything ever wrote through it, and an engine built from a tree that carried
\ the words but never took the boundary is exactly the host that would emit a
\ rewind restoring nothing.
\
\ The fixture stops the file one line short of taking the boundary width, so
\ every PREFIX-MARK name resolves, the mark still records the dictionary end and
\ the include registry, and only CURSORS reads zero. That is the one state the
\ name clause is blind to and the value clause exists for. The rewrite starts
\ from the workspace file, so it cannot inherit an earlier case's damage.
: BFT-MARK-VALUE-FORGE ( ptr u8 n -- ) {: tail:ptr tailu:n :}
   s" src/core/lower-cert-seal.f" BFT-READ {: u:n :}
   BFT-READ-BUF u s" CHECKER-BOUND:CURSORS CU !" BFT-FIND BFT-FOUND {: cut:n :}
   cut 0 > TTRUE
   BFT-STALE-MARK BFT-READ-BUF cut WRITE-ALL
   BFT-STALE-MARK tail tailu APPEND-FILE ;

: BFT-MARK-VALUE-REFUSED ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   BFT-STALE-ARGV
   BFT-STALE-SPAWN {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-BIG-ERR erru msg msgu CONTAINS? TTRUE
   BFT-BIG-OUT outu s" self-check census" CONTAINS? TFALSE
   BFT-STALE-HB BFT-SOURCE-HOST$ BF-FILE= TTRUE
   BFT-STALE-STAMP FILE? TFALSE ;

: BFT-TEST-WATERMARK-VALUE ( -- )
   s\" ;package\n" BFT-MARK-VALUE-FORGE
   s" carries no checker boundary" BFT-MARK-VALUE-REFUSED ;

\ ---- effective source boundary --------------------------------------------
\ The cold prefix already occupies IBUFSZ and the reader performs an EOF probe,
\ so IBUFSZ+1 is not the runtime input boundary. Bracket the boundary with a
\ bounded exponential search, refine it by binary search, then rerun the adjacent
\ successful/failing sizes against the freshly built candidate's --build path.

$10000 constant PROBE-START

variable EXITED
variable EXIT-CODE
variable ERR-U
TYPED-VARIABLE SRC-A ptr u8
variable OK-N
variable BAD-N

: SRC-BUF ( -- ptr u8 )
   SRC-A @ ;

: SRC-ALLOC ( -- )
   SRC-A @ 0= 0= if exit then
   SOURCE-ARENA-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop SRC-A !
   SOURCE-ARENA-CAP 0 ?do 32 SRC-BUF i + c! loop ;

: ERR$ ( -- ptr u8 n )
   BFT-ERR ERR-U @ ;

: WRITE-SRC ( n -- ) {: bytes:n :}
   s" bft-srcfull.f" BF-A$ SRC-BUF bytes WRITE-ALL ;

: RUN-CANDIDATE ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN BFT-ROOT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   BFT-HB >LEN BFT-OUT BFT-CAPTURE-CAP >LEN
   BFT-ERR BFT-CAPTURE-CAP >LEN BFT-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME
   MATCH outcome
     exited OF EXIT-CODE ! 0 0= EXITED ! ENDOF
     signaled OF EXIT-CODE ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 EXIT-CODE ! 0 0= 0= EXITED ! ENDOF
   ;MATCH {: ou:len eu:len :}
   eu LEN>N ERR-U ! ;

: RUN-SOURCE ( -- )
   PROC-ARGV-RESET
   s" --build" >LEN PROC-ARGV+
   s" bft-srcfull.f" BF-A$ >LEN PROC-ARGV+
   RUN-CANDIDATE ;

: PROBE ( n -- bool ) {: bytes:n :}
   bytes WRITE-SRC
   RUN-SOURCE
   EXITED @ 0= if
      EXITED @ TTRUE
      0 0= 0= exit
   then
   EXIT-CODE @ 0= if
      ERR-U @ 0 T=
      0 0= exit
   then
   EXIT-CODE @ 74 T=
   ERR$ s" hb: source prefix buffer full" CONTAINS? TTRUE
   0 0= 0= ;

: EXP-NEXT ( -- n )
   OK-N @ 2 * SOURCE-ARENA-CAP min ;

: EXP-STEP ( -- bool )
   EXP-NEXT {: bytes:n :}
   bytes PROBE if
      bytes SOURCE-ARENA-CAP < {: below:bool :}
      below TTRUE
      below if
         bytes OK-N !
         0 0= 0= exit
      then
      bytes BAD-N !
      0 0= exit
   then
   bytes BAD-N !
   0 0= ;

: EXP ( -- )
   PROBE-START PROBE TTRUE
   PROBE-START OK-N !
   begin EXP-STEP until ;

: MID ( -- n )
   OK-N @ BAD-N @ OK-N @ - 2 / + ;

: BINARY ( -- )
   begin BAD-N @ OK-N @ - 1 > while
      MID {: bytes:n :}
      bytes PROBE if
         bytes OK-N !
      else
         bytes BAD-N !
      then
   repeat ;

: SOURCE-BOUNDARY ( -- )
   BFT-ROOT BF-TMP!
   SRC-ALLOC
   EXP
   BINARY
   BAD-N @ OK-N @ 1 + T=
   OK-N @ PROBE TTRUE
   BAD-N @ PROBE TFALSE
   BF-TMP-RESET ;

: RUN-BUILD ( ptr u8 n -- )
   PROC-ARGV-RESET
   s" --build" >LEN PROC-ARGV+
   >LEN PROC-ARGV+
   RUN-CANDIDATE ;

: EXPECT-OK ( -- )
   EXITED @ TTRUE
   EXIT-CODE @ 0 T=
   ERR-U @ 0 T= ;

: EXPECT-74 ( ptr u8 n -- ) {: diag:ptr diagu:n :}
   EXITED @ TTRUE
   EXIT-CODE @ 74 T=
   ERR$ diag diagu T$= ;

: STAGE2-DRIVER$ ( -- ptr u8 n )
   s" bft-stage2-driver.f" ;

: STAGE2-DRIVER ( -- ptr u8 n )
   STAGE2-DRIVER$ BF-A$ ;

: DRIVER-BASE ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu COMPILER-BUILD:SEAL
   out outu BF-APPEND-DRIVER-IO ;

: WRITE-STAGE2-DRIVER ( -- )
   STAGE2-DRIVER$ {: out:ptr outu:n :}
   out outu DRIVER-BASE
   out outu s" src/habu/stage2.f" s" : RUN ( -- )" BF-APPEND-SOURCE-BEFORE
   out outu s" : BFT-S2-READ-EXIT ( -- ) READ-SRC DRV-EXIT-OK ;" BF-APPEND-LINE
   out outu s" BFT-S2-READ-EXIT" BF-APPEND-LINE ;

: WRITE-SPACES ( ptr u8 n n -- ) {: path:ptr pathu:n u:n :}
   path pathu SRC-BUF u WRITE-ALL ;

: STAGE2 ( -- )
   BFT-ROOT BF-TMP!
   SRC-ALLOC
   WRITE-STAGE2-DRIVER
   BFT-STAGE2 SOURCE-ARENA-CAP 1 - WRITE-SPACES
   STAGE2-DRIVER RUN-BUILD
   EXPECT-OK
   BFT-STAGE2 SRC-BUF 1 APPEND-FILE
   STAGE2-DRIVER RUN-BUILD
   S\" stage2: source exceeds buffer\n" EXPECT-74
   BF-TMP-RESET ;

: MAKER-SOURCE ( -- ptr u8 n )
   s" hb-maker-src" BF-A$ ;

: MAKER-DRIVER$ ( -- ptr u8 n )
   s" bft-maker-driver.f" ;

: MAKER-DRIVER ( -- ptr u8 n )
   MAKER-DRIVER$ BF-A$ ;

: WRITE-MAKER-DRIVER ( -- )
   MAKER-DRIVER$ {: out:ptr outu:n :}
   out outu DRIVER-BASE
   out outu s" src/habu/maker.f" s" : MK-RUN" BF-APPEND-SOURCE-BEFORE
   out outu s" LOWER-CERT-HOOK:INSTALL" BF-APPEND-LINE
   out outu S\" s\" MK-READ-SRC\" s\" --\" TRUST" BF-APPEND-LINE
   out outu s" : BFT-MK-READ-EXIT ( -- ) MK-READ-SRC DRV-EXIT-OK ;" BF-APPEND-LINE
   out outu s" BFT-MK-READ-EXIT" BF-APPEND-LINE ;

: MAKER ( -- )
   BFT-ROOT BF-TMP!
   SRC-ALLOC
   WRITE-MAKER-DRIVER
   MAKER-SOURCE SOURCE-ARENA-CAP 1 - WRITE-SPACES
   MAKER-DRIVER RUN-BUILD
   EXPECT-OK
   MAKER-SOURCE SRC-BUF 1 APPEND-FILE
   MAKER-DRIVER RUN-BUILD
   S\" maker: source exceeds buffer\n" EXPECT-74
   BF-TMP-RESET ;

: BFT-TEST-TMP-OVERRIDE ( -- )
   BFT-ROOT BF-TMP!
   BF-TMP$ BFT-ROOT T$=
   s" stage2-src" BF-A$ BFT-STAGE2 T$=
   BF-STAGE2-SOURCE
   BFT-STAGE2 FILE? TTRUE
   BF-TMP-RESET ;

: BFT-TEST-STAGE-ARGV-RESET ( -- )
   BFT-ROOT BF-TMP!
   PROC-ARGV-RESET
   s" stale" >LEN PROC-ARGV+
   s" bin/hb" BF-PREPARE-STAGE-ARGV 2drop
   PROC-ARGV-N @ COUNT>N 2 T=
   PROC-ARGV-RESET
   s" stale" >LEN PROC-ARGV+
   s" bin/hb" s" stage2-src" BF-A$ BF-PREPARE-LOAD-STAGE-ARGV 2drop
   PROC-ARGV-N @ COUNT>N 4 T=
   BF-TMP-RESET ;

: BFT-CERT-WRITE ( ptr u8 n -- )
   BFT-CERT 2swap WRITE-ALL ;

\ Hash-pin mismatch: pin a sandbox boot-prefix file, reload unchanged (no
\ throw), then mutate it mid-sequence - the reload must fail closed with
\ E-BUILD-BOOT-DRIFT rather than silently entering the image.
: BFT-PIN-RELOAD ( -- )
   BFT-CERT BF-PIN-FILE ;

: BFT-TEST-BOOT-PIN ( -- )
   BF-PIN-RESET
   BF-PIN-ON!
   s" \ boot prefix v1" BFT-CERT-WRITE
   BFT-CERT BF-PIN-FILE
   BFT-CERT BF-PIN-FILE
   BFT-CERT s" \ mid-build edit" APPEND-FILE
   [: BFT-PIN-RELOAD ;] E-BUILD-BOOT-DRIFT TTHROWSQ
   BF-PIN-OFF!
   BF-PIN-RESET ;

\ Split emission pins the whole input before copying either side. Changing the
\ file between its two halves must refuse, and stripped prefixes obey the same
\ pin instead of opening an untracked source-read path.
: BFT-TEST-SPLIT-PIN ( -- )
   BFT-ROOT BF-TMP!
   BF-PIN-RESET
   BF-PIN-ON!
   s" \ before marker after" BFT-CERT-WRITE
   s" pin-split" BF-RESET-OUT
   s" pin-split" BFT-CERT s" marker" BF-APPEND-SOURCE-BEFORE
   BFT-CERT s" \ mid-split edit" APPEND-FILE
   [: s" pin-split" BFT-CERT s" marker" BF-APPEND-SOURCE-FROM ;]
      E-BUILD-BOOT-DRIFT TTHROWSQ
   [: s" pin-split" BFT-CERT s" marker" BF-APPEND-SOURCE-BEFORE-STRIPPED ;]
      E-BUILD-BOOT-DRIFT TTHROWSQ
   BF-PIN-OFF!
   BF-PIN-RESET
   BF-TMP-RESET ;

\ The native chain's contribution to the stamp key (package STAMP-KEY in
\ tools/build-fixpoint.f). Two claims, and the second is what makes the first
\ mean anything for the real refresh.
\
\ CLOSURE-KEY runs the production digest word over a fixture entry, so the
\ closure it walks is built here rather than read from the tree: a content edit
\ to a file the entry loads must change the digest, an edit to a file it does
\ not load must leave the digest alone, and restoring the first file's bytes
\ must restore the original digest exactly. The entry carries two decoys - a
\ `require` of the unrelated file inside a `\` comment, and the same text inside
\ a string literal - so a discovery pass that matched loader text instead of
\ loader FORMS would pull the unrelated file into the closure and fail both the
\ membership assertions and the untouched-digest claim.
\
\ STAMP-FOLD then pins that the refresh's own key preimage carries that digest,
\ for THIS repository's chain entry: it rebuilds the length-framed `chain-src`
\ fragment BF-STAMP-KEY-BEGIN is supposed to have appended and finds those exact
\ bytes in the preimage. The same fragment built from the fixture entry's digest
\ must be absent, so keying the wrong entry file fails here rather than passing
\ on the tag alone.

variable ENTRY-U
variable DEP-U
variable OTHER-U
create ENTRY-BUF FS-PATH-CAP allot
create DEP-BUF FS-PATH-CAP allot
create OTHER-BUF FS-PATH-CAP allot
create DG-A 40 allot
create DG-B 40 allot
create DG-C 40 allot

: FIXTURE-ENTRY$ ( -- ptr u8 n )   ENTRY-BUF ENTRY-U @ ;
: DEP$ ( -- ptr u8 n )     DEP-BUF DEP-U @ ;
: OTHER$ ( -- ptr u8 n )   OTHER-BUF OTHER-U @ ;

\ One entry line per append: SB holds one path and its line, not three.
: ENTRY-LINE+ ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: pre:ptr preu:n a:ptr u:n post:ptr postu:n :}
   SB-RESET
   pre preu SB-APPEND a u SB-APPEND post postu SB-APPEND BFT-NL 1 SB-APPEND
   FIXTURE-ENTRY$ SB$ APPEND-FILE ;

\ Entry source: one real `require`, plus the same loader text hidden in a
\ comment and in a string literal.
: WRITE-ENTRY ( -- )
   FIXTURE-ENTRY$ s" " WRITE-ALL
   s" \ require " OTHER$ s" " ENTRY-LINE+
   s\" s\" require " OTHER$ s\" \"" ENTRY-LINE+
   s" require " DEP$ s" " ENTRY-LINE+ ;

: WRITE-FIXTURES ( -- )
   DEP$ s\" \\ dep v1\n" WRITE-ALL
   OTHER$ s\" \\ other v1\n" WRITE-ALL
   WRITE-ENTRY ;

: MEMBER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup EC:COUNT < while
      dup EC:PATH$ a u STR= if drop BF-TRUE exit then
      1+
   repeat drop BF-FALSE ;

\ One preimage field, as the key writes it: a length byte then that many bytes.
: FIELD+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u SB-APPEND-C
   a u SB-APPEND ;

\ The framed capture-source fragment BF-STAMP-KEY-BEGIN appends for a given
\ digest: the tag as one field, the digest as the next. The tag comes from the
\ tool (BF-STAMP-CAPTURE-TAG$), not from a copy here - a copy is what let this
\ case go on looking for `chain-src` after the key started writing
\ `capture-src`, while the negative case beside it passed for the wrong reason.
: FRAGMENT$ ( ptr u8 -- ptr u8 n ) {: dg:ptr :}
   SB-RESET
   BF-STAMP-CAPTURE-TAG$ FIELD+
   dg BF-STAMP-DG-U FIELD+
   SB$ ;

: PREIMAGE-HAS? ( ptr u8 n -- bool )
   BF-STAMP-BUF BF-STAMP-U @ 2swap CONTAINS? ;

: PREP ( -- )
   BFT-ROOT s" chain-entry.f" ENTRY-BUF ENTRY-U BFT-PATH!
   BFT-ROOT s" chain-dep.f" DEP-BUF DEP-U BFT-PATH!
   BFT-ROOT s" chain-other.f" OTHER-BUF OTHER-U BFT-PATH! ;

: TEST-CLOSURE-KEY ( -- )
   PREP
   WRITE-FIXTURES
   FIXTURE-ENTRY$ EC:BUILD
   EC:COUNT 2 T=
   FIXTURE-ENTRY$ MEMBER? TTRUE
   DEP$ MEMBER? TTRUE
   OTHER$ MEMBER? TFALSE
   FIXTURE-ENTRY$ DG-A CHAIN-DIGEST!
   DEP$ s\" \\ dep v2\n" APPEND-FILE
   FIXTURE-ENTRY$ DG-B CHAIN-DIGEST!
   DG-A BF-STAMP-DG-U DG-B BF-STAMP-DG-U STR= TFALSE
   OTHER$ s\" \\ other v2\n" APPEND-FILE
   FIXTURE-ENTRY$ DG-C CHAIN-DIGEST!
   DG-B BF-STAMP-DG-U DG-C BF-STAMP-DG-U STR= TTRUE
   DEP$ s\" \\ dep v1\n" WRITE-ALL
   FIXTURE-ENTRY$ DG-C CHAIN-DIGEST!
   DG-A BF-STAMP-DG-U DG-C BF-STAMP-DG-U STR= TTRUE ;

: TEST-STAMP-FOLD ( -- )
   PREP
   WRITE-FIXTURES
   ENTRY$ DG-A CHAIN-DIGEST!
   FIXTURE-ENTRY$ DG-B CHAIN-DIGEST!
   DG-A BF-STAMP-DG-U DG-B BF-STAMP-DG-U STR= TFALSE
   BF-STAMP-KEY-BEGIN
   DG-A FRAGMENT$ PREIMAGE-HAS? TTRUE
   DG-B FRAGMENT$ PREIMAGE-HAS? TFALSE ;

\ Public so the driver below runs with the package CLOSED, as a gate row must.
public
: BFT-SOURCE-GROWTH-RELEASES ( -- )
   BF-SOURCE-BUF {: old:ptr :}
   old MAPPED:LIVE? TTRUE
   s" (growth probe)" BF-SOURCE-CAP 1+ BF-SOURCE-ENSURE
   old MAPPED:LIVE? TFALSE
   BF-SOURCE-BUF MAPPED:LIVE? TTRUE ;

: BFT-FIXTURES-RUN ( -- )
   T-RESET
   BFT-PREPARE
   s" tmp override" [: BFT-TEST-TMP-OVERRIDE ;] BFT-STEP
   s" stage argv reset" [: BFT-TEST-STAGE-ARGV-RESET ;] BFT-STEP
   s" stamp seed" [: BFT-TEST-STAMP-SEED ;] BFT-STEP
   s" build" [: BFT-TEST-BUILD ;] BFT-STEP
   s" private install cleanup" [: BFT-TEST-PRIVATE-CLEANUP ;] BFT-STEP
   s" candidate boot" [: BFT-TEST-CANDIDATE-BOOT ;] BFT-STEP
   s" stage engine selection" [: BFT-TEST-ENGINE-SELECTION ;] BFT-STEP
   s" cached skip" [: BFT-TEST-CACHED-SKIP ;] BFT-STEP
   s" build fail no stamp" [: BFT-TEST-BUILD-FAIL-NO-STAMP ;] BFT-STEP
   s" no-main self dispatch" [: BFT-TEST-NO-MAIN-DISPATCHES ;] BFT-STEP
   s" missing preamble diag" [: BFT-TEST-MISSING-PREAMBLE ;] BFT-STEP
   s" watermark required" [: BFT-TEST-WATERMARK-REQUIRED ;] BFT-STEP
   s" watermark value" [: BFT-TEST-WATERMARK-VALUE ;] BFT-STEP
   s" stamp source key" [: BFT-TEST-STAMP-SOURCE-KEY ;] BFT-STEP
   s" chain closure key" [: TEST-CLOSURE-KEY ;] BFT-STEP
   s" chain stamp fold" [: TEST-STAMP-FOLD ;] BFT-STEP
   s" stamp corrupt" [: BFT-TEST-STAMP-CORRUPT ;] BFT-STEP
   s" stamp engine" [: BFT-TEST-STAMP-ENGINE ;] BFT-STEP
   s" all stamp guard" [: BFT-TEST-ALL-STAMP-GUARD ;] BFT-STEP
   s" stamp nested" [: BFT-TEST-STAMP-NESTED ;] BFT-STEP
   s" boot pin mismatch" [: BFT-TEST-BOOT-PIN ;] BFT-STEP
   s" split source pin mismatch" [: BFT-TEST-SPLIT-PIN ;] BFT-STEP
   s" source boundary" [: SOURCE-BOUNDARY ;] BFT-STEP
   s" stage2 source cap" [: STAGE2 ;] BFT-STEP
   s" maker source cap" [: MAKER ;] BFT-STEP
   s" source buffer growth releases" [: BFT-SOURCE-GROWTH-RELEASES ;] BFT-STEP
   s" build-fixpoint-test: ok" BFT-FINISH ;

;package

BUILD-FIXPOINT:BFT-FIXTURES-RUN
