\ aot-chain-capture-suite.f - the compiler-chain capture tool, run for real.
\
\ WHAT THIS LOCKS. tools/aot-chain-capture.f captures the native compiler chain
\ inside a booted engine so the metabuild can bake it. Two things make that
\ capture correct and neither of them is visible in its output:
\
\   THE LOAD ORDER. The chain must load INSIDE the window and the capture's own
\   tooling after it closes. Reversed, the tool's closure pulls
\   src/arch/arm64/asm.f in first, the chain's `require` of asm.f becomes a no-op,
\   and every chain word that calls an asm.f word calls a word the target engine
\   has not got - 98 of 18602 call sites, measured. That capture RUNS; only the
\   band audit says the seed it produced could never boot.
\
\   BEING FIRST. The band audit trusts every record BELOW the tool's mark to be a
\   word the target has, which is true of the engine's own surface and of nothing
\   else. Run the tool behind one other file and the marks swallow that file: the
\   capture succeeds, the window is smaller, and the seed calls names by hand-off
\   that no target defines.
\
\ THE ORDER CASE IS A MUTANT DERIVED FROM THE REAL FILE, not a second copy of it:
\ the suite reads tools/aot-chain-capture.f, splices `require src/arch/arm64/asm.f`
\ in front of the line that opens the window, and runs THAT. So the fixture cannot
\ drift from the tool, and the mutation is exactly the refuted ordering. The
\ splice is fail-closed on its anchor: if the anchor line is not present exactly
\ once the suite stops rather than running an unmutated file and passing.
\
\   THE BOOT-RUN LIST. The window's load runs four installers that write cells
\   below it, and a seeded engine re-runs them only because the tool names them.
\   The tool checks its list against the engine's own declared-address-cell
\   registry, so this suite runs the tool with one row deleted and requires the
\   refusal - the alternative being an engine whose code-reclaim watchers are
\   silently absent.
\
\ AND THE SUITE PROVES ITS OWN DETECTOR. test/aot-chain-decoy.f prints the
\ refusal sentence and exits 0. A checker that read text and not exit codes would
\ call that a refusal; the self-test asserts it does not.
\
\ THE IDENTITY CASE READS ONE FACT TWICE. The capture reports the content key of
\ the engine it is running in (lib/engine-id.f, resolving its own executable) and
\ this suite hashes bin/hb from the outside with SHA256-FILE; the two must agree.
\ That is the mechanism the metabuild uses to refuse an artifact some other engine
\ produced, so it is worth a case of its own rather than trust. The closure the
\ capture names comes from the engine's require registry, so the case asserts it
\ starts at the file the window actually required.
\
\ AND THE PRODUCT OWES NOTHING TO THE PATHS THE BUILD WAS GIVEN. The bake splices
\ the artifact path into the driver it generates; a top-level `s"` allots at HERE,
\ which is the capture window's DATA base, so the length of that path used to
\ reach the engine's bytes - 12081 of them. Two cases hold the repair: the same
\ artifact baked under a longer path must produce the same engine, and a spliced
\ line that moves the cursor must be refused by name instead of quietly baked.
\
\ Cost: four child engine runs (~4s) plus three metabuild bakes (~20s). Registered as
\ `SUITE aot-chain-capture` in test/gate-stdlib-cases.f. Run standalone:
\   bin/hb --load test/aot-chain-capture-suite.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test.f
require lib/test/src-shape.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package AOT-CHAIN-CAPTURE-TEST

$4000 constant CAP
120000 constant CASE-TIMEOUT-MS
74 constant REFUSE-RC        \ what a refused capture exits with
75 constant READ-RC          \ ... and what src/habu/aot-file.f's own refusals exit with
74 constant STOP-RC          \ ... and what this suite exits with when its own fixture has gone vacuous

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
variable RC

create ROOT-BUF FS-PATH-CAP allot    variable ROOT-U
create MUT-BUF FS-PATH-CAP allot     variable MUT-U
create ROWMUT-BUF FS-PATH-CAP allot  variable ROWMUT-U
create ART-BUF FS-PATH-CAP allot     variable ART-U
create CASE-BUF FS-PATH-CAP allot    variable CASE-U
create LONG-BUF FS-PATH-CAP allot    variable LONG-U
create PROD1-BUF FS-PATH-CAP allot   variable PROD1-U
create DPMUT-BUF FS-PATH-CAP allot   variable DPMUT-U
create DIG 32 allot
create DHEX 64 allot
create EHEX 64 allot

\ The artifact under test, held whole so a case can edit bytes and write the
\ result out. 4 MiB covers today's 2.95 MiB with room; READ-ALL refuses a file
\ that outgrows it rather than truncating one.
$400000 constant ART-CAP
create ART ART-CAP allot    variable ART-LEN

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: MUT$ ( -- ptr u8 n )  MUT-BUF MUT-U @ ;
: ROWMUT$ ( -- ptr u8 n ) ROWMUT-BUF ROWMUT-U @ ;
: ART$ ( -- ptr u8 n )  ART-BUF ART-U @ ;
: CASE$ ( -- ptr u8 n ) CASE-BUF CASE-U @ ;
: LONG$ ( -- ptr u8 n ) LONG-BUF LONG-U @ ;
: PROD1$ ( -- ptr u8 n ) PROD1-BUF PROD1-U @ ;
: DPMUT$ ( -- ptr u8 n ) DPMUT-BUF DPMUT-U @ ;
: HB$ ( -- ptr u8 n )   s" bin/hb" ;
: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;
: TOOL$ ( -- ptr u8 n ) s" tools/aot-chain-capture.f" ;

\ The line that opens the window. Splicing in front of it is what puts the
\ capture's own closure ahead of the chain, which is the refuted order.
: ANCHOR$ ( -- ptr u8 n ) s\" \nAOT-CHAIN:OPEN\n" ;
: INJECT$ ( -- ptr u8 n ) s\" require src/arch/arm64/asm.f\n" ;

\ <root><name> into dst, answering the length the caller latches. Every path this
\ suite hands a child hangs off the one temporary root.
: UNDER-ROOT ( ptr u8 n ptr u8 -- n ) {: a:ptr u:n dst:ptr :}
   ROOT-BUF dst ROOT-U @ BYTE-COPY
   a dst ROOT-U @ + u BYTE-COPY
   ROOT-U @ u + ;

\ The two artifact names differ in LENGTH on purpose: the bake splices the path it
\ is given into the driver it generates, and PROBE-REPRO is the assertion that the
\ length of that path cannot reach the engine's bytes.
: SETUP ( -- )
   s" habu-aot-chain" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   s" /hoisted.f"       MUT-BUF    UNDER-ROOT MUT-U !
   s" /chain.aot"       ART-BUF    UNDER-ROOT ART-U !
   s" /case.aot"        CASE-BUF   UNDER-ROOT CASE-U !
   s" /dropped-row.f"   ROWMUT-BUF UNDER-ROOT ROWMUT-U !
   s" /chain-under-a-considerably-longer-name.aot" LONG-BUF UNDER-ROOT LONG-U !
   s" /hb-chain-first"  PROD1-BUF  UNDER-ROOT PROD1-U !
   s" /cursor-moved.f"  DPMUT-BUF  UNDER-ROOT DPMUT-U ! ;

\ ---- the derived mutant ------------------------------------------------------

: ANCHOR-OFF ( -- n )
   SHAPE:TEXT ANCHOR$ FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

\ Fail-closed on the anchor: an anchor that is missing, or that a later comment
\ has spelled a second time, means the splice would land somewhere else or
\ nowhere. Either way the mutant would stop being the refuted order and the order
\ case would pass by doing nothing.
: ?ANCHOR ( -- )
   ANCHOR$ SHAPE:COUNT 1 = if exit then
   s" aot-chain-capture-suite: window-open anchors in " type TOOL$ type s" =" type
   ANCHOR$ SHAPE:COUNT .
   s" aot-chain-capture-suite: the order case cannot be built" STOP-RC die ;

\ prefix (up to and including the newline that ends the previous line), then the
\ injected require, then the rest starting at the anchor's own line.
: WRITE-MUTANT ( -- )
   ANCHOR-OFF 1 + {: cut:n :}
   SHAPE:TEXT drop {: src:ptr :}
   MUT$ src cut WRITE-ALL
   MUT$ INJECT$ APPEND-FILE
   MUT$ src cut +  SHAPE:TEXT nip cut -  APPEND-FILE ;

: MUTANT-BUILD ( -- )
   TOOL$ SHAPE:LOAD
   ?ANCHOR
   WRITE-MUTANT
   s" the mutant is the tool plus exactly the injected require" T-LABEL
   MUT$ FILE-SIZE  SHAPE:TEXT nip INJECT$ nip + T= ;

\ ---- the boot-run list's own mutant ------------------------------------------
\
\ WHAT THIS ONE LOCKS, and why a comment could not. The window's load runs four
\ installers whose writes land in cells BELOW it - three CHECKER-TAPE observer
\ cells and three CODE-RECLAIM watcher slots - and a seeded engine gets those
\ back from the boot-run list or not at all. Only one of the four fails loudly:
\ without the tape observer the first definition dies "checker: no source-tape
\ observer to arm", while a missing watcher leaves inline and publish rows naming
\ code a reclamation already took back, and nothing prints. So the tool counts
\ the cells the window planted, asks the engine's own declared-address-cell
\ registry rather than its own list, and refuses when the two disagree.
\
\ THE MUTANT IS THE REAL FILE WITH ONE DECLARE ROW DELETED - a line, not a
\ substring - so the fixture cannot drift from the list it tests, and it is
\ fail-closed on its anchor: a row spelled twice, or not at all, stops the suite
\ rather than running an unmutated file and passing.
: ROW$ ( -- ptr u8 n ) s" NCLOB:WATCH-INSTALL" ;

: ?ROW ( -- )
   ROW$ SHAPE:COUNT 1 = if exit then
   s" aot-chain-capture-suite: boot-run rows naming " type ROW$ type s" =" type
   ROW$ SHAPE:COUNT .
   s" aot-chain-capture-suite: the dropped-row case cannot be built" STOP-RC die ;

: ROW-OFF ( -- n )
   SHAPE:TEXT ROW$ FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

\ The whole line the row sits on: back to the newline above it, forward past the
\ one that ends it.
: LINE-FROM ( n -- n ) {: at:n :}
   SHAPE:TEXT drop {: src:ptr :}
   at begin dup 0 > while
      dup 1 - src + c@ 10 = if exit then
      1 -
   repeat ;

: LINE-PAST ( n -- n ) {: at:n :}
   SHAPE:TEXT {: src:ptr u:n :}
   at begin dup u < while
      dup src + c@ 10 = if 1 + exit then
      1 +
   repeat ;

: ROW-MUTANT-BUILD ( -- )
   TOOL$ SHAPE:LOAD
   ?ROW
   ROW-OFF LINE-FROM {: a:n :}
   ROW-OFF LINE-PAST {: b:n :}
   SHAPE:TEXT drop {: src:ptr :}
   ROWMUT$ src a WRITE-ALL
   ROWMUT$ src b +  SHAPE:TEXT nip b -  APPEND-FILE
   s" the dropped-row mutant is the tool less exactly one line" T-LABEL
   ROWMUT$ FILE-SIZE  SHAPE:TEXT nip b a - -  T= ;

\ ---- one child engine run ----------------------------------------------------

: EXEC ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN  OUT CAP >LEN  ERR CAP >LEN  CASE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: RUN-CASE ( ptr u8 n -- ) {: f:ptr fu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   f fu >LEN PROC-ARGV+
   EXEC ;

\ The capture tool asked for an artifact: same entry point, one script argument.
: RUN-TOOL-ART ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ART$ >LEN PROC-ARGV+
   EXEC ;

\ The production reader, run on whatever bytes the case put at CASE$.
: RUN-READ ( bool -- ) {: mm:bool :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-file-read.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   CASE$ >LEN PROC-ARGV+
   mm if s" mismatch" >LEN PROC-ARGV+ then
   EXEC ;

: SAID? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   OUT$ a u CONTAINS?  ERR$ a u CONTAINS? or ;

: DIAG. ( -- )
   s" aot-chain-capture: child stdout:" type cr OUT$ type cr
   s" aot-chain-capture: child stderr:" type cr ERR$ type cr ;

\ A refusal is an exit code AND a sentence. Either alone is a text search. The
\ tool and the reader exit with different codes on purpose - a refusal from
\ src/habu/aot-file.f is not a refusal from the capture that called it - so the
\ code is a parameter and every case states which one it expects.
: RC-REFUSED? ( n ptr u8 n -- bool ) {: want:n r:ptr ru:n :}
   RC @ want = r ru SAID? and ;

: RC-REFUSED ( n ptr u8 n -- ) {: want:n r:ptr ru:n :}
   want r ru RC-REFUSED? 0= if DIAG. then
   want r ru RC-REFUSED? TTRUE ;

: REFUSED? ( ptr u8 n -- bool ) {: r:ptr ru:n :} REFUSE-RC r ru RC-REFUSED? ;
: REFUSED ( ptr u8 n -- ) {: r:ptr ru:n :} REFUSE-RC r ru RC-REFUSED ;
: READ-REFUSED ( ptr u8 n -- ) {: r:ptr ru:n :} READ-RC r ru RC-REFUSED ;

\ ---- reading the census the tool prints --------------------------------------

: DIGIT-RUN ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ STR-DIGIT? 0= if exit then
      1+
   repeat ;

\ The value printed after `label` on the child's stdout, or -1 when the label is
\ absent or carries no digits. Reading the number, not the label, is what makes a
\ tool that printed the labels and stopped fail here.
: FIELD ( ptr u8 n -- n ) {: la:ptr lu:n :}
   OUT$ la lu FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N lu + {: off:n :}
             OUT off +  OUT-U @ off -  {: va:ptr vu:n :}
             va vu DIGIT-RUN {: d:n :}
             d 0= if -1 exit then
             va d STR>NUMBER? MATCH option
               none OF -1 ENDOF
               some OF ENDOF
             ;MATCH
     ENDOF
   ;MATCH ;

\ The `want` bytes printed after `label`, or a zero-length string when the label is
\ absent or the line is short. Used for the two digests, which are fixed-width hex.
: TEXT-FIELD ( ptr u8 n n -- ptr u8 n ) {: la:ptr lu:n want:n :}
   OUT$ la lu FIND-SUB MATCH option
     none OF OUT 0 ENDOF
     some OF IDX>N lu + {: off:n :}
             OUT-U @ off - want < if OUT 0 exit then
             OUT off + want
     ENDOF
   ;MATCH ;

: FLOOR ( n n ptr u8 n -- ) {: got:n want:n la:ptr lu:n :}
   la lu T-LABEL
   got want >= 0= if
      s" aot-chain-capture-suite: " type la lu type s"  got " type got .
      s" aot-chain-capture-suite: wanted at least " type want .
   then
   got want >= TTRUE ;

\ ---- the cases ---------------------------------------------------------------

\ The chain capture, run the way the metabuild will run it. The two equalities are
\ the structural half: the capture's own blob and DATA sizes must be the window it
\ measured, so a census printed from stale buffers cannot agree with itself. The
\ floors are the non-vacuity half: this window is the compiler chain (6764 records,
\ 18939 sites, 1.16 MiB of code when this was written), and a capture that fell to
\ a fraction of it captured something else.
: PROBE-REAL ( -- )
   TOOL$ RUN-CASE
   s" the chain capture runs to completion in a booted engine" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   s" the captured blob is the window's own code span" T-LABEL
   s" blob=" FIELD  s" codespan=" FIELD T=
   s" the captured DATA is the window's own DATA span" T-LABEL
   s" datasz=" FIELD  s" dataspan=" FIELD T=
   s" blob=" FIELD    1000000 s" the window holds the chain's code" FLOOR
   s" recs=" FIELD       6000 s" ... its dictionary records" FLOOR
   s" sites=" FIELD     15000 s" ... its call sites" FLOOR
   s" dsites=" FIELD     3000 s" ... its DATA-literal sites" FLOOR
   s" xtoff=" FIELD         1 s" ... and its declared address cell" FLOOR
   s" bandrecs=" FIELD      1 s" the prelude band is not empty" FLOOR
   s" bandbytes=" FIELD     1 s" ... on the DATA axis either" FLOOR

   \ The signature audit's own numbers, and the identity that makes them a
   \ statement rather than two counters: it visits EVERY window record exactly
   \ once and puts each in one of the two buckets, so their sum is the record
   \ count. A walk that skipped a class, or counted one twice, breaks this
   \ without breaking either floor below it.
   s" every window record is either checked or exempt, and none is both" T-LABEL
   s" sigknown=" FIELD  s" sigexempt=" FIELD +  s" recs=" FIELD T=
   s" sigknown=" FIELD    6000 s" the window's checked words" FLOOR
   s" sigexempt=" FIELD     50 s" ... and its package records" FLOOR
   \ The pool carries a row for each checked word and may carry more than one -
   \ a redefinition leaves both and the newest wins - so the floor is the checked
   \ count itself, which is what the audit proved per record.
   s" sigs=" FIELD  s" sigknown=" FIELD  s" the pool covers every checked word" FLOOR ;

\ The capture's identity, read from the same run PROBE-REAL just made. The producer
\ key is the interesting one: the tool reports the content key of the binary it is
\ RUNNING (lib/engine-id.f, which resolves its own executable), and this suite
\ hashes bin/hb from the outside. Two independent readings of one fact, compared -
\ which is the whole mechanism the metabuild will use to refuse an artifact that
\ some other engine produced.
: PROBE-IDENT ( -- )
   s" the capture names the closure it loaded" T-LABEL
   s" closure=" FIELD  40 s" closure files" FLOOR
   s" ... starting at the chain root the window required" T-LABEL
   s" first=src/compiler/native/migrate.f" SAID? TTRUE
   s" ... and carries a full-width digest over their bytes" T-LABEL
   s" chaindigest=" 64 TEXT-FIELD nip 64 T=
   s" the producer key is the engine that ran the capture" T-LABEL
   s" bin/hb" DIG SHA256-FILE 0 T=
   DIG DHEX SHA256>HEX
   s" producer=" 64 TEXT-FIELD  DHEX 64 T$= ;

\ The refuted order, spliced into the real file. The diagnostic must name a window
\ word and a callee; `<no record>` on either side would mean the audit fired
\ without being able to say on what, and a reader could not act on it.
: PROBE-ORDER ( -- )
   MUT$ RUN-CASE
   s" the capture's own closure ahead of the chain is refused" T-LABEL
   s" aot-capture: window call into the prelude band" REFUSED
   s" ... naming the window word that makes the call" T-LABEL
   s" aot-capture: window word " SAID? TTRUE
   s" ... and the callee no target defines" T-LABEL
   s" which the booting engine has and no target does" SAID? TTRUE
   s" ... both by name" T-LABEL
   s" <no record>" SAID? 0= TTRUE ;

\ Anything loaded before the tool is refused before a single record is captured.
: PROBE-NOT-FIRST ( -- )
   s" test/aot-chain-preloaded.f" RUN-CASE
   s" a capture that is not the first file loaded is refused" T-LABEL
   s" aot-chain-capture: the capture must be the first file this process loads"
   REFUSED
   s" ... and says how many files got in first" T-LABEL
   s" aot-chain-capture: files loaded before the capture=2" SAID? TTRUE
   s" ... before any window is captured" T-LABEL
   s" recs=" SAID? 0= TTRUE ;

\ One installer missing from the list. The two counts are read as NUMBERS and
\ their difference asserted, so a refusal that fired for some other reason - or
\ one that printed the labels and no measurement - fails here.
: PROBE-ROW-DROPPED ( -- )
   ROWMUT$ RUN-CASE
   s" a boot-run list one installer short is refused" T-LABEL
   s" aot-chain-capture: a load-time installer this list does not name runs once here and never again"
   REFUSED
   s" ... naming what the window planted and what the list refills" T-LABEL
   s" aot-chain-capture: pre-window declared cells holding a window address=" FIELD
   s" aot-chain-capture: cells the declared installers refill=" FIELD -
   1 T=
   s" ... and no artifact is written" T-LABEL
   s" roundtrip=ok" SAID? 0= TTRUE ;

\ Fail-closed proof of this suite's own refusal detector: a child that prints the
\ refusal sentence and exits 0 is not a refusal.
: SELFTEST ( -- )
   s" test/aot-chain-decoy.f" RUN-CASE
   s" the decoy prints the refusal sentence" T-LABEL
   s" aot-capture: window call into the prelude band" SAID? TTRUE
   s" ... and exits 0" T-LABEL
   RC @ 0 T=
   s" ... so the detector must not call it a refusal" T-LABEL
   s" aot-capture: window call into the prelude band" REFUSED? 0= TTRUE ;

\ ---- the artifact, and the shapes its reader must refuse ---------------------
\
\ THE CORRUPTIONS ARE NOT ALL THE SAME KIND, and the difference is the point. A
\ header field, a truncation, a trailing byte and a flipped payload byte are what
\ a damaged FILE looks like, and the reader catches each with a check of its own.
\ A bad section table is not: the payload digest covers the table, so no amount of
\ damage produces a bad table the digest still accepts. Those cases have to be
\ FORGED - edit the table, then recompute the payload digest and restamp it - and
\ without that the reader's table and capacity checks would be code no test can
\ reach. RESEAL is what forges them, and the case that reseals WITHOUT editing
\ anything is what proves RESEAL stamps the field the reader really reads: if it
\ wrote its digest anywhere else, that case would be refused instead of accepted
\ and every forged case below would be passing on a broken seal.

136 constant HDR-BYTES
8 constant O-VERSION
16 constant O-TARGET
24 constant O-SECTIONS
104 constant O-PAYSHA
14 constant SEC-N
16 constant ROW-BYTES
13 constant S-CLOSURE            \ the last section: the closure list
1 constant TARGET-MACOS
2 constant TARGET-LINUX

variable T-RECS  variable T-SITES  variable T-BLOB  variable T-DATASZ  variable T-CLOSURE
create SW0 512 allot     \ the two closure entries the chain case swaps
create SW1 512 allot

: A64@ ( n -- n ) {: at:n :}
   0 8 0 ?do  8 lshift  ART at 7 i - + + c@ or  loop ;

: A64! ( n n -- ) {: v:n at:n :}
   8 0 ?do  v i 8 * rshift $FF and  ART at i + + c!  loop ;

: LOAD-ART ( -- ) ART$ ART ART-CAP READ-ALL ART-LEN ! ;
: EMIT-N ( n -- ) {: n:n :} CASE$ ART n WRITE-ALL ;
: EMIT ( -- ) ART-LEN @ EMIT-N ;

\ Fail-closed on the header's own anchor. Every offset below is a fixture's idea
\ of the format; if the format moved, the edits would land somewhere else and the
\ negative cases would pass by corrupting nothing.
: ?MAGIC ( -- )
   0 A64@ $00544F4155424148 = if exit then
   s" aot-chain-capture-suite: the artifact header is not the shape the fixtures edit"
   STOP-RC die ;

: ROW-OFF-AT ( n -- n ) ROW-BYTES * HDR-BYTES + ;
: ROW-LEN-AT ( n -- n ) ROW-BYTES * HDR-BYTES + 8 + ;
: SEC-LEN@ ( n -- n ) ROW-LEN-AT A64@ ;
: SEC-LEN! ( n n -- ) {: v:n k:n :} v k ROW-LEN-AT A64! ;
: SEC-AT ( n -- n ) {: k:n :} k ROW-OFF-AT A64@ HDR-BYTES + ;

\ Offsets rewritten from the lengths, contiguous from the end of the table. A case
\ that wants the contiguity refusal must NOT call this.
: RETABLE ( -- )
   SEC-N ROW-BYTES * {: base:n :}
   base
   SEC-N 0 ?do
      dup i ROW-OFF-AT A64!
      i SEC-LEN@ +
   loop drop ;

: RESEAL ( -- )
   ART HDR-BYTES +  ART-LEN @ HDR-BYTES -  ART O-PAYSHA +  SHA256 ;

: FRESH ( -- ) LOAD-ART ?MAGIC ;

\ ---- the cases ---------------------------------------------------------------

\ The tool asked for an artifact. The digest it reports is its own record of the
\ bytes it handed the descriptor, so hashing the file from the outside and getting
\ the same answer is what makes `artifact=` a fact about the file rather than a
\ claim about the writer.
: PROBE-ARTIFACT ( -- )
   RUN-TOOL-ART
   s" the capture writes an artifact and exits clean" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   s" ... and the write survives its own round trip" T-LABEL
   s" roundtrip=ok" SAID? TTRUE
   s" ... and the digest it reports is the file on disk" T-LABEL
   ART$ DIG SHA256-FILE 0 T=
   DIG DHEX SHA256>HEX
   s" artifact=" 64 TEXT-FIELD  DHEX 64 T$=
   s" recs=" FIELD T-RECS !
   s" sites=" FIELD T-SITES !
   s" blob=" FIELD T-BLOB !
   s" datasz=" FIELD T-DATASZ !
   s" closure=" FIELD T-CLOSURE ! ;

\ What the capture measured is what the reader restores. Comparing the two
\ censuses is the round trip's other half: the tool proved the BYTES survive, and
\ this proves the numbers those bytes stand for arrive in the right buffers.
: PROBE-READBACK ( -- )
   FRESH EMIT
   false RUN-READ
   s" the production reader accepts the artifact the capture wrote" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   s" ... restoring the record count the capture measured" T-LABEL
   s" recs=" FIELD T-RECS @ T=
   s" ... its call sites" T-LABEL
   s" sites=" FIELD T-SITES @ T=
   s" ... its blob" T-LABEL
   s" blob=" FIELD T-BLOB @ T=
   s" ... its DATA window" T-LABEL
   s" datasz=" FIELD T-DATASZ @ T=
   s" ... and the closure it names" T-LABEL
   s" closure=" FIELD T-CLOSURE @ T=
   s" ... starting at the chain root" T-LABEL
   s" first=src/compiler/native/migrate.f" SAID? TTRUE ;

: PROBE-DAMAGE ( -- )
   FRESH  0 A64@ 1 xor 0 A64!  EMIT
   false RUN-READ
   s" a wrong magic is refused by name" T-LABEL
   s" aot-file: magic=" READ-REFUSED

   FRESH  O-VERSION A64@ 1+ O-VERSION A64!  EMIT
   false RUN-READ
   s" a wrong version is refused by name" T-LABEL
   s" aot-file: version=" READ-REFUSED

   FRESH
   O-TARGET A64@ TARGET-MACOS = if TARGET-LINUX else TARGET-MACOS then O-TARGET A64!
   EMIT
   false RUN-READ
   s" an artifact for the other target is refused by name" T-LABEL
   s" aot-file: target=" READ-REFUSED

   FRESH  SEC-N 1 - O-SECTIONS A64!  EMIT
   false RUN-READ
   s" a wrong section count is refused before the table is read" T-LABEL
   s" aot-file: section count=" READ-REFUSED

   FRESH EMIT
   true RUN-READ
   s" an artifact from another engine is refused" T-LABEL
   s" aot-file: the artifact was produced by a different engine" READ-REFUSED

   FRESH  ART-LEN @ 1 - EMIT-N
   false RUN-READ
   s" a truncated payload is refused" T-LABEL
   s" aot-file: the artifact ends before its header says it should" READ-REFUSED

   FRESH  $A5 ART ART-LEN @ + c!  ART-LEN @ 1 + EMIT-N
   false RUN-READ
   s" a byte past the payload is refused" T-LABEL
   s" aot-file: the artifact carries bytes past the end of its payload" READ-REFUSED

   FRESH
   ART HDR-BYTES 2000 + + c@ 1 xor  ART HDR-BYTES 2000 + + c!
   EMIT
   false RUN-READ
   s" one flipped payload byte is refused by the payload digest" T-LABEL
   s" aot-file: the artifact's payload does not match its payload digest" READ-REFUSED ;

\ The forged half. Each of these is a file whose payload digest AGREES with its
\ bytes, so nothing but the reader's own structural checks can catch it.
: PROBE-FORGED ( -- )
   FRESH RESEAL EMIT
   false RUN-READ
   s" resealing without an edit leaves the artifact acceptable" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=

   FRESH  1 SEC-LEN@ 8 + 1 SEC-LEN!  RESEAL EMIT
   false RUN-READ
   s" a section table that is not contiguous is refused" T-LABEL
   s" does not start where the table says the last one ended" READ-REFUSED

   FRESH  3 SEC-LEN@ 4 - 3 SEC-LEN!  RETABLE RESEAL EMIT
   false RUN-READ
   s" a section length that is not a whole number of rows is refused" T-LABEL
   s" is not a whole number of rows" READ-REFUSED

   FRESH
   1 SEC-LEN@ 8 SEC-LEN@ + {: both:n :}
   both 1 SEC-LEN!  0 8 SEC-LEN!
   RETABLE RESEAL EMIT
   false RUN-READ
   s" a section larger than the buffer it fills is refused" T-LABEL
   s" is larger than the buffer it fills" READ-REFUSED

   FRESH  1 S-CLOSURE SEC-AT A64!  RESEAL EMIT
   false RUN-READ
   s" a closure list that does not fill its section is refused" T-LABEL
   s" the closure list does not fill its section" READ-REFUSED ;

\ The chain digest is re-derived from the files the artifact names, so the test
\ is a list that names the same files in a different order - which changes the
\ ordered concatenation and nothing else. Swapping two real entries keeps every
\ path readable, so the refusal that fires is the digest comparison and not an
\ open failure, and it needs no edit to a file the rest of the tree is using.
: PROBE-CHAIN ( -- )
   FRESH
   S-CLOSURE SEC-AT {: cl:n :}
   cl 8 + A64@ {: l0:n :}
   cl 8 + 8 + l0 + A64@ {: l1:n :}
   l0 l1 = if
      s" aot-chain-capture-suite: the first two closure paths are the same length"
      STOP-RC die
   then
   ART cl 16 + +          SW0 l0 BYTE-COPY            \ park both entries' bytes
   ART cl 24 + l0 + +     SW1 l1 BYTE-COPY
   l1 cl 8 + A64!                                     \ then lay them down swapped
   SW1  ART cl 16 + +  l1 BYTE-COPY
   l0 cl 16 + l1 + A64!
   SW0  ART cl 24 + l1 + +  l0 BYTE-COPY
   RESEAL EMIT
   false RUN-READ
   s" a closure list the chain digest does not re-derive is refused" T-LABEL
   s" aot-file: the chain sources have changed since this capture" READ-REFUSED ;

\ ---- the merge, against the same artifact -----------------------------------
\ AOT-FILE:MERGE appends this artifact to a capture that already happened, in
\ that capture's coordinates. test/aot-file-merge.f is what runs it - it captures
\ a window of its own so every shift has a nonzero quantity, and it checks each
\ row family's SUM against the artifact's own plus the row count times the shift,
\ which is what pins a shift to its size rather than to a range. What is asserted
\ HERE is the other half: the merged counts are the host's plus THIS capture's,
\ measured in the tool's process and checked in the merge's, so the two censuses
\ have to agree across two processes rather than within one.
: RUN-MERGE ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-file-merge.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ART$ >LEN PROC-ARGV+
   EXEC ;

: MERGED= ( ptr u8 n ptr u8 n n -- ) {: h:ptr hu:n m:ptr mu:n want:n :}
   m mu FIELD  h hu FIELD -  want T= ;

\ The DATA window is the one axis that does not append flush. A captured
\ address has to keep its 8-residue - the atomics fault on a misaligned cell -
\ and the two windows were captured against DATA cursors with residues of their
\ own, so the merge starts the artifact's slice up to seven bytes late (dot
\ habu-merged-data-window-b8fec035). The exact placement is asked of the merge
\ by test/aot-file-merge.f, which reads AOT-FILE:WDATA-BASE and checks the
\ shift is a whole number of cells; what is asked HERE, across the two
\ processes, is that the whole difference is this capture's own window and a
\ pad below one cell - so a merge that dropped the artifact, or padded by a
\ cell too many, is still named.
8 constant PAD-BOUND

: MERGED-PAD= ( ptr u8 n ptr u8 n n -- ) {: h:ptr hu:n m:ptr mu:n want:n :}
   m mu FIELD  h hu FIELD -  want - {: pad:n :}
   pad 0 >= pad PAD-BOUND < and TTRUE ;

: PROBE-MERGE ( -- )
   RUN-MERGE
   s" the production merge accepts the artifact the capture wrote" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   s" ... appending exactly the records this capture measured" T-LABEL
   s" hostrecs=" s" mergedrecs=" T-RECS @ MERGED=
   s" ... exactly its call sites" T-LABEL
   s" hostsites=" s" mergedsites=" T-SITES @ MERGED=
   s" ... exactly its code" T-LABEL
   s" hostblob=" s" mergedblob=" T-BLOB @ MERGED=
   s" ... and its DATA window, placed at most a cell late for alignment" T-LABEL
   s" hostdata=" s" mergeddatasz=" T-DATASZ @ MERGED-PAD= ;

\ ---- the engine the artifact is for -----------------------------------------
\ WHY A BAKE BELONGS IN THIS SUITE. Everything above proves the artifact is
\ well-formed and that the reader and the merge put its pieces where they say
\ they do. None of that can prove the engine those pieces make will RUN the
\ chain: a captured reference no relocation pass can see is well-formed on both
\ sides and wrong only when the code executes. That is exactly what a merged
\ engine did - a table's accessor addressed its storage by a baked offset from
\ `data-base`, the merge placed the window 152 bytes elsewhere, and BKEY's
\ writes landed on BMID's cells (dot habu-bmid-module-id-ec6c709b) - and it
\ passed every structural case here.
\
\ So this case bakes the artifact with the production tool and asks the engine
\ it writes to compile and run a word: the shortest program that goes all the
\ way through the chain's own definer into native code and back with an answer.
\ Cost: one metabuild (~7s).

create ENGINE-BUF FS-PATH-CAP allot   variable ENGINE-U
: ENGINE$ ( -- ptr u8 n ) ENGINE-BUF ENGINE-U @ ;

\ tools/aot-chain-bake.f leaves the engine at <HB_TMP>/hb-chain, and EXEC gives
\ every child HB_TMP=ROOT$, so the suite's own root is where it lands.
: ENGINE-PATH! ( -- )
   s" /hb-chain" ENGINE-BUF UNDER-ROOT ENGINE-U ! ;

: BAKE-TOOL$ ( -- ptr u8 n ) s" tools/aot-chain-bake.f" ;

\ The bake, parameterized by the tool file and the artifact path, because two
\ cases below vary exactly one of those and nothing else.
: BAKE-WITH ( ptr u8 n ptr u8 n -- ) {: tool:ptr toolu:n art:ptr artu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   tool toolu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   art artu >LEN PROC-ARGV+
   HB$ >LEN PROC-ARGV+
   EXEC ;

: RUN-BAKE ( -- ) BAKE-TOOL$ ART$ BAKE-WITH ;

\ Define a word through NMIGRATE:DEFINE - the chain's entry point - then call it.
: PROGRAM$ ( -- ptr u8 n )
   s\" s\" : FOO ( n -- n ) 1 + ;\" 1 1 8 NMIGRATE:DEFINE 7 FOO .\n" ;

: RUN-BAKED-PROGRAM ( ptr u8 n -- ) {: p:ptr pu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE$ >LEN  p pu >LEN  OUT CAP >LEN  ERR CAP >LEN  CASE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: PROBE-BAKED ( -- )
   ENGINE-PATH!
   RUN-BAKE
   s" the production bake accepts this artifact and writes an engine" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   s" ... at the path the tool names" T-LABEL
   ENGINE$ FILE-SIZE 0 > TTRUE
   PROGRAM$ RUN-BAKED-PROGRAM
   s" ... and that engine compiles a word through the chain and runs it" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   s" ... answering what the word computes, and printing nothing else" T-LABEL
   OUT$ s\" 8\n" T$= ;

\ ---- and it says it has the files the seed carries ---------------------------
\
\ WHAT THIS LOCKS. Baking the chain puts its 43 files' definitions in the engine
\ but tells src/core/include.f nothing, so a program's `require
\ src/compiler/native/migrate.f` reads the file a second time and dies on a
\ duplicate definition - the seeded engine would be WORSE than the source one for
\ the chain's own consumers. src/habu/habu2.f marks every merged closure file
\ `provided` in the cold prefix, ahead of the freeze that decides what
\ ENGINE-PROVIDES? answers for.
\
\ THE PROBE IS GENERATED FROM THE ARTIFACT'S OWN CLOSURE LIST, not from a list
\ written here: the suite walks the closure section it already parses and asks the
\ engine about every path in it, so a row the emitter drops is a count that does
\ not match and no fixture edit can hide it. The control is a real file the chain
\ never loads - the capture tool itself - because a detector that answers true for
\ everything would otherwise pass.
\ AND THE LAST LINE IS THE POINT: the same `require` a consumer writes, followed
\ by a definition through the chain. Without the rows that line is the duplicate
\ definition, so the case fails on the behaviour and not only on a count.
$8000 constant PROG-CAP
create PROG PROG-CAP allot   variable PROG-U
variable CUR
: PROG$ ( -- ptr u8 n ) PROG PROG-U @ ;
: NOT-IN-CLOSURE$ ( -- ptr u8 n ) s" tools/aot-chain-capture.f" ;

: PROG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   PROG-U @ u + PROG-CAP > if
      s" aot-chain-capture-suite: the provided-rows probe outgrew its buffer" STOP-RC die
   then
   a PROG PROG-U @ + u BYTE-COPY
   PROG-U @ u + PROG-U ! ;

\ `if` is a compile-time word, so the question is asked by a word the program
\ defines and every row is one call to it.
: ASK+ ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n c:ptr cu:n :}
   s\" s\" " PROG+  a u PROG+
   s\" \" " PROG+  c cu PROG+  s\" \n" PROG+ ;

\ Every path the artifact names, read out of the section table the fixtures above
\ already anchor. A list that runs past its own section is a broken fixture, not a
\ failed assertion.
: ASK-CLOSURE+ ( -- )
   S-CLOSURE SEC-AT {: cl:n :}
   cl S-CLOSURE SEC-LEN@ + {: end:n :}
   cl 8 + CUR !
   T-CLOSURE @ 0 ?do
      CUR @ A64@ {: pu:n :}
      CUR @ 8 + pu + end > if
         s" aot-chain-capture-suite: the closure list runs past its section" STOP-RC die
      then
      ART CUR @ 8 + +  pu  s" PROBE-ASK" ASK+
      CUR @ 8 + pu + CUR !
   loop ;

: BUILD-PROG ( -- )
   FRESH
   0 PROG-U !
   s\" variable PROBE-PROV\nvariable PROBE-EXTRA\n" PROG+
   s\" : PROBE-ASK ( ptr u8 n -- ) ENGINE-PROVIDES? if 1 PROBE-PROV +! then ;\n" PROG+
   s\" : PROBE-ASK-X ( ptr u8 n -- ) ENGINE-PROVIDES? if 1 PROBE-EXTRA +! then ;\n" PROG+
   ASK-CLOSURE+
   NOT-IN-CLOSURE$ s" PROBE-ASK-X" ASK+
   s\" s\" provided=\" type PROBE-PROV @ .\n" PROG+
   s\" s\" extra=\" type PROBE-EXTRA @ .\n" PROG+
   s\" require src/compiler/native/migrate.f\n" PROG+
   PROGRAM$ PROG+ ;

: TAIL= ( ptr u8 n -- bool ) {: a:ptr u:n :}
   OUT-U @ u < if 0 0= 0= exit then
   OUT OUT-U @ u - + u  a u  STR= ;

: PROBE-PROVIDED ( -- )
   BUILD-PROG
   PROG$ RUN-BAKED-PROGRAM
   s" the baked engine takes a chain require as the no-op the seed makes it" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   s" ... marking every file the artifact's closure names, and no more" T-LABEL
   s" provided=" FIELD T-CLOSURE @ T=
   s" ... and nothing the chain never loaded" T-LABEL
   s" extra=" FIELD 0 T=
   s" ... with the chain still compiling a word after the require" T-LABEL
   s\" 8\n" TAIL= TTRUE ;

\ ---- the product's bytes owe nothing to the paths the build was given ---------
\
\ WHY THIS IS A CASE AND NOT AN ASSUMPTION. The bake splices the artifact path
\ into the driver source it generates, and an interpret-mode `s"` ALLOTS its bytes
\ at HERE - which src/habu/stdin.f CAPTURE-REPL latches as the capture window's
\ DATA base. Measured before the fix: the same artifact under two paths of
\ different length baked engines differing in 12081 bytes, so `bin/hb` stopped
\ being reproducible the moment two developers used different temporary roots.
\ The fix puts the two literals inside a colon body, where they compile into code
\ below the window and the seed's canonical CODE-B0 absorbs them; this case is the
\ property that fix exists for, asserted over the real production bake.
\ A file that cannot be hashed is a broken fixture, not a failed assertion: stop
\ rather than compare a digest of nothing against a digest of nothing.
: HEX-OF ( ptr u8 n ptr u8 -- ) {: p:ptr u:n hex:ptr :}
   p u DIG SHA256-FILE 0 <> if
      s" aot-chain-capture-suite: cannot hash " type p u type cr
      s" aot-chain-capture-suite: the reproducibility case cannot be built" STOP-RC die
   then
   DIG hex SHA256>HEX ;

: PROBE-REPRO ( -- )
   ENGINE$ PROD1$ COPY-FILE-STREAM
   ART$ LONG$ COPY-FILE-STREAM
   BAKE-TOOL$ LONG$ BAKE-WITH
   s" the same artifact under a longer path bakes an engine" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   PROD1$ DHEX HEX-OF
   ENGINE$ EHEX HEX-OF
   s" ... byte for byte the engine the shorter path baked" T-LABEL
   DHEX 64 EHEX 64 T$= ;

\ ---- and the driver refuses any parameter that would ---------------------------
\
\ THE MUTANT IS THE REAL BAKE TOOL PLUS ONE LINE, spliced at a fail-closed anchor,
\ and the line makes the generated driver consume DATA at top level - which is
\ what the historical string literal did, stated as the rule rather than as the
\ one spelling of it. src/habu/stdin.f DP-MARK latches the cursor as the last
\ thing the driver's own load does, so RUN sees any such splice and refuses by
\ name. Without the refusal the build writes an engine that is simply not the one
\ the same tree would write elsewhere, and nothing anywhere says so.
: DP-ANCHOR$ ( -- ptr u8 n ) s\" \n   ART$ ?PATH  ENG$ ?PATH\n" ;
: DP-INJECT$ ( -- ptr u8 n ) S\"    S\\\" 8 allot\" DRV-LINE\n" ;

: DP-ANCHOR-OFF ( -- n )
   SHAPE:TEXT DP-ANCHOR$ FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

: ?DP-ANCHOR ( -- )
   DP-ANCHOR$ SHAPE:COUNT 1 = if exit then
   s" aot-chain-capture-suite: parameter anchors in " type BAKE-TOOL$ type s" =" type
   DP-ANCHOR$ SHAPE:COUNT .
   s" aot-chain-capture-suite: the moved-cursor case cannot be built" STOP-RC die ;

: DP-MUTANT-BUILD ( -- )
   BAKE-TOOL$ SHAPE:LOAD
   ?DP-ANCHOR
   DP-ANCHOR-OFF DP-ANCHOR$ nip + {: cut:n :}
   SHAPE:TEXT drop {: src:ptr :}
   DPMUT$ src cut WRITE-ALL
   DPMUT$ DP-INJECT$ APPEND-FILE
   DPMUT$ src cut +  SHAPE:TEXT nip cut -  APPEND-FILE
   s" the moved-cursor mutant is the bake tool plus exactly the injected line" T-LABEL
   DPMUT$ FILE-SIZE  SHAPE:TEXT nip DP-INJECT$ nip + T= ;

: PROBE-DP-MOVED ( -- )
   DP-MUTANT-BUILD
   ENGINE$ 2dup EXISTS? if REMOVE-FILE else 2drop then
   DPMUT$ ART$ BAKE-WITH
   s" a build parameter that moves the DATA cursor is refused" T-LABEL
   RC @ 0 <> TTRUE
   s" ... naming the cursor, so the reason is actionable" T-LABEL
   s" the DATA cursor moved after the driver marked it" SAID? TTRUE
   s" ... and no engine is written" T-LABEL
   ENGINE$ EXISTS? 0= TTRUE ;

: BODY ( -- )
   SETUP
   MUTANT-BUILD
   ROW-MUTANT-BUILD
   SELFTEST
   PROBE-REAL
   PROBE-IDENT
   PROBE-ORDER
   PROBE-NOT-FIRST
   PROBE-ROW-DROPPED
   PROBE-ARTIFACT
   PROBE-READBACK
   PROBE-MERGE
   PROBE-DAMAGE
   PROBE-FORGED
   PROBE-CHAIN
   PROBE-BAKED
   PROBE-PROVIDED
   PROBE-REPRO
   PROBE-DP-MOVED ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-chain-capture: ok" type cr ;

;package

AOT-CHAIN-CAPTURE-TEST:RUN
