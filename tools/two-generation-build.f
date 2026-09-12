\ two-generation-build.f - build five engine generations from one host and say
\ whether the chain converges or accretes image DATA, then whether it reaches
\ its byte fixpoint.
\
\ Generation 1 is built by the host named on the command line (default: the
\ checkout's own bin/hb), generation 2 by generation 1, and so on to generation
\ 5 - each through the production entry point, tools/native-build.f. Engines land
\ in build/twogen, which is ignored. native-build.f always promotes to bin/hb, so
\ the checkout's engine is moved aside for the run and put back.
\
\ Each generation prints one line: the image size plus the shape
\ tools/two-generation-probe.f reads out of it. The chain fails, naming the
\ generation, when a generation does not build or when generation 3's image
\ size and shape differ from generation 2's - that pair agreeing is what "the
\ chain has stopped growing" means. Two power-of-two-capped checker pools once
\ put 2 MB of zero padding in every image, which the next host booted AND
\ persisted again, and generation 3 died in LOAD-TARGET with
\ "hb: data space out of range".
\
\ THE LAST PAIR IS COMPARED BYTE FOR BYTE, and the pair is (4,5) rather than
\ (2,3) because the product is a function of its host as well as of the source:
\ the capture bakes the window's DATA as its non-zero extents, so build-time
\ residue in that DATA - cells the boot path re-initialises and no restored
\ reader consults - changes the run partitioning and displaces every later
\ section. Measured 2026-09-12 on linux-aarch64 from a seed hb-stdin: 21 such
\ cells (one non-zero byte each) account for the whole of the ~1 MB by which
\ generations 3 and 4 differ, while their restored DATA differs only in live
\ per-process addresses. The residue reaches its own fixpoint at generation 4,
\ where f(B4) = B4 exactly. A byte difference in the last pair is therefore a
\ real defect - a clock, a pid, an unpinned address or an unordered walk in the
\ build - and this is the check that names it. docs/bootstrap.md records the
\ measurement and the counts per pair.
\
\ Invocation and today's lines: docs/bootstrap.md, "Generation Chain Check".

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package TWO-GEN

1 constant TG-FAIL-RC
48 constant TG-ZERO                \ ASCII '0'
10 constant TG-LF                  \ ASCII newline
$8000 constant TG-CAP-OUT
$1000 constant TG-SHAPE-CAP
$8000 constant TG-CMP-CAP          \ one compare chunk per side
1800000 constant TG-BUILD-MS       \ a cold generation is ~30-50 s here
60000 constant TG-PROBE-MS

create TG-OUT TG-CAP-OUT allot
create TG-ERR TG-CAP-OUT allot
create TG-SHAPE TG-SHAPE-CAP allot
create TG-PREV TG-SHAPE-CAP allot
create TG-HOST FS-PATH-CAP allot
create TG-PATH FS-PATH-CAP allot
create TG-PATH-B FS-PATH-CAP allot
create TG-NAME 8 allot
create TG-NAME-B 8 allot
create TG-CMP-A TG-CMP-CAP allot
create TG-CMP-B TG-CMP-CAP allot
variable TG-SHAPE-U
variable TG-PREV-U
variable TG-HOST-U
variable TG-IMG
variable TG-PREV-IMG
variable TG-STASHED
variable TG-FDA
variable TG-FDB
variable TG-RA
variable TG-RB
variable TG-DIFF

: TG-U. ( n -- ) {: v:n :}
   v 0 < if E-STR-BOUNDS throw then
   v 10 >= if v 10 / RECURSE then
   v 10 mod TG-ZERO + emit ;

: TG-DIR$ ( -- ptr u8 n ) s" build/twogen" ;
: TG-BIN$ ( -- ptr u8 n ) s" bin/hb" ;
: TG-STASH$ ( -- ptr u8 n ) s" build/twogen/hb-entry" ;
: TG-TOOL$ ( -- ptr u8 n ) s" tools/native-build.f" ;
: TG-PROBE$ ( -- ptr u8 n ) s" tools/two-generation-probe.f" ;

: TG-ENSURE-DIR ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DIR? if exit then
   a u MAKE-DIR ;

: TG-MKDIRS ( -- )
   s" build" TG-ENSURE-DIR
   TG-DIR$ TG-ENSURE-DIR ;

: TG-HOST! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-STR-BOUNDS throw then
   a TG-HOST u BYTE-COPY
   u TG-HOST-U ! ;

: TG-HOST$ ( -- ptr u8 n ) TG-HOST TG-HOST-U @ ;

\ build/twogen/hb-b<g>; one digit, because the chain is five generations long.
\ The buffers are the caller's so a compare can hold two generation paths at once.
: TG-GEN-PATH ( n ptr u8 ptr u8 -- ptr u8 n ) {: g:n nm:ptr path:ptr :}
   g 1 < g 9 > or if E-STR-BOUNDS throw then
   s" hb-b" drop nm 4 BYTE-COPY
   g TG-ZERO + nm 4 + c!
   TG-DIR$ nm 5 path JOIN-PATH {: u:n :}
   path u ;

: TG-GEN$ ( n -- ptr u8 n ) TG-NAME TG-PATH TG-GEN-PATH ;

: TG-GEN-B$ ( n -- ptr u8 n ) TG-NAME-B TG-PATH-B TG-GEN-PATH ;

\ The first line of a capture, so a stop names one diagnostic, not a trace.
: TG-LINE1 ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ TG-LF = if a swap exit then
      1 +
   repeat
   drop a u ;

: TG-STASH ( -- )
   0 TG-STASHED !
   TG-BIN$ EXISTS? 0= if exit then
   TG-BIN$ TG-STASH$ RENAME-FILE
   -1 TG-STASHED ! ;

: TG-UNSTASH ( -- )
   TG-STASHED @ 0= if exit then
   TG-STASH$ TG-BIN$ RENAME-FILE
   0 TG-STASHED ! ;

\ With no argument the stashed checkout engine is generation 0, which is why
\ the host is resolved after the stash and never through the bin/hb path.
: TG-HOST0 ( -- )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ TG-HOST! exit then
   TG-STASHED @ 0= if
      s" two-gen: no host: pass a seed engine path, or install bin/hb" type cr
      TG-FAIL-RC throw
   then
   TG-STASH$ TG-HOST! ;

\ Only the stderr length and the completion code matter to a caller: a build
\ that works says nothing on stdout.
: TG-BUILD ( -- len n )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   TG-TOOL$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   TG-HOST$ >LEN
   TG-OUT TG-CAP-OUT >LEN  TG-ERR TG-CAP-OUT >LEN
   TG-BUILD-MS >MS RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outl:len errl:len code:n :}
   errl code ;

: TG-PROBE ( ptr u8 n -- ) {: a:ptr u:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   TG-PROBE$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   a u >LEN
   TG-SHAPE TG-SHAPE-CAP >LEN  TG-ERR TG-CAP-OUT >LEN
   TG-PROBE-MS >MS RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outl:len errl:len code:n :}
   code 0 <> if
      s" two-gen: probe failed: " type TG-ERR errl LEN>N TG-LINE1 type cr
      TG-FAIL-RC throw
   then
   outl LEN>N TG-SHAPE-U ! ;

: TG-FAILED ( n -- ) {: g:n :}
   s" two-gen: FAILED at generation " type g TG-U. cr
   TG-FAIL-RC throw ;

: TG-STOPPED ( n len n -- ) {: g:n errl:len code:n :}
   s" two-gen: gen " type g TG-U.
   s"  stopped rc " type code TG-U.
   s"  " type TG-ERR errl LEN>N TG-LINE1 type cr
   g TG-FAILED ;

\ The shape line already ends the line; the driver owns everything before it.
: TG-REPORT ( n -- ) {: g:n :}
   s" two-gen: gen " type g TG-U.
   s"  built img " type TG-IMG @ TG-U.
   s"  " type TG-SHAPE TG-SHAPE-U @ type ;

: TG-GEN ( n -- ) {: g:n :}
   TG-BUILD {: errl:len code:n :}
   code 0 <> if g errl code TG-STOPPED then
   TG-BIN$ EXISTS? 0= if
      s" two-gen: gen " type g TG-U. s"  built no engine" type cr
      g TG-FAILED
   then
   TG-BIN$ g TG-GEN$ RENAME-FILE
   g TG-GEN$ FILE-SIZE TG-IMG !
   g TG-GEN$ TG-PROBE
   g TG-REPORT
   g TG-GEN$ TG-HOST! ;

: TG-KEEP ( -- )
   TG-SHAPE TG-PREV TG-SHAPE-U @ BYTE-COPY
   TG-SHAPE-U @ TG-PREV-U !
   TG-IMG @ TG-PREV-IMG ! ;

: TG-SAME? ( -- bool )
   TG-IMG @ TG-PREV-IMG @ =
   TG-SHAPE TG-SHAPE-U @ TG-PREV TG-PREV-U @ STR= and ;

\ ---- the byte compare of one generation pair --------------------------------
\ Chunked, because an engine image is over five megabytes and the count is all
\ the caller wants: a differing byte is a defect to name, not a diff to print.

: TG-CMP-CLOSE ( -- )
   TG-FDA @ 0 >= if TG-FDA @ close then
   TG-FDB @ 0 >= if TG-FDB @ close then
   -1 TG-FDA !
   -1 TG-FDB ! ;

: TG-CMP-OPEN ( ptr u8 n ptr u8 n -- ) {: a:ptr au:n b:ptr bu:n :}
   -1 TG-FDA !
   -1 TG-FDB !
   a au FS-PATHZ open-rd TG-FDA !
   TG-FDA @ 0 < if E-FS-OPEN throw then
   b bu FS-PATHZ open-rd TG-FDB !
   TG-FDB @ 0 < if TG-CMP-CLOSE E-FS-OPEN throw then ;

: TG-READ ( n ptr u8 -- n ) {: fd:n buf:ptr :}
   fd buf TG-CMP-CAP read {: got:n :}
   got 0 < got TG-CMP-CAP > or if TG-CMP-CLOSE E-FS-IO throw then
   got ;

: TG-CHUNK-DIFF ( n -- ) {: u:n :}
   u 0 ?do
      TG-CMP-A i + c@ TG-CMP-B i + c@ <> if
         TG-DIFF @ 1 + TG-DIFF !
      then
   loop ;

\ Differing byte count, or -1 when the two files are not the same length.
: TG-BYTE-DIFF ( ptr u8 n ptr u8 n -- n )
   TG-CMP-OPEN
   0 TG-DIFF !
   begin
      TG-FDA @ TG-CMP-A TG-READ TG-RA !
      TG-FDB @ TG-CMP-B TG-READ TG-RB !
      TG-RA @ TG-RB @ <> if TG-CMP-CLOSE -1 exit then
      TG-RA @ 0= if TG-CMP-CLOSE TG-DIFF @ exit then
      TG-RA @ TG-CHUNK-DIFF
   again ;

\ One line per pair, and the count the caller decides about.
: TG-PAIR-DIFF ( n n -- n ) {: a:n b:n :}
   a TG-GEN$ b TG-GEN-B$ TG-BYTE-DIFF {: n:n :}
   s" two-gen: bytes gen " type a TG-U. s"  vs " type b TG-U. s"  " type
   n 0 < if s" length mismatch" type cr else n TG-U. cr then
   n ;

\ The whole chain's byte report, and the fixpoint the last pair has to be at.
: TG-FIXPOINT ( -- )
   2 3 TG-PAIR-DIFF drop
   3 4 TG-PAIR-DIFF drop
   4 5 TG-PAIR-DIFF {: n:n :}
   n 0= if
      s" two-gen: ok gen 5 matches gen 4 byte for byte" type cr
      exit
   then
   s" two-gen: gen 5 is not byte-identical to gen 4" type cr
   5 TG-FAILED ;

: TG-SHAPE-CHECK ( -- )
   TG-SAME? if
      s" two-gen: ok gen 3 matches gen 2" type cr
      exit
   then
   s" two-gen: gen 3 image size or shape differs from gen 2" type cr
   3 TG-FAILED ;

: TG-CHAIN ( -- )
   TG-HOST0
   1 TG-GEN
   2 TG-GEN
   TG-KEEP
   3 TG-GEN
   TG-SHAPE-CHECK
   4 TG-GEN
   5 TG-GEN
   TG-FIXPOINT ;

public

\ The stash is taken and returned here so it survives every failure path.
: MAIN ( -- )
   TG-MKDIRS
   TG-STASH
   [: TG-CHAIN ;] catch {: code:n :}
   TG-UNSTASH
   code 0 <> if s" " code die then ;

;package

TWO-GEN:MAIN
