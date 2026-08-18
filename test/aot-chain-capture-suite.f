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
70 constant CHECK-RC         \ ... and what a definition the checker will not certify exits with
76 constant REG-RC           \ ... and what a seeded type registry the engine cannot take exits with

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
create BFMUT-BUF FS-PATH-CAP allot   variable BFMUT-U
create AFMUT-BUF FS-PATH-CAP allot   variable AFMUT-U
create RDMUT-BUF FS-PATH-CAP allot   variable RDMUT-U
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
: BFMUT$ ( -- ptr u8 n ) BFMUT-BUF BFMUT-U @ ;
: AFMUT$ ( -- ptr u8 n ) AFMUT-BUF AFMUT-U @ ;
: RDMUT$ ( -- ptr u8 n ) RDMUT-BUF RDMUT-U @ ;
\ SUBJECT: source-loading the chain. The capture and the bake must LOAD the
\ chain's 43 files, so they run on the CAPTURE HOST the install keeps beside
\ the product (bin/hb-host) - the product already provides every closure file,
\ which turns the load this suite exists to measure into a no-op and captures
\ an empty window. The BAKED programs the suite then runs are chain BEHAVIOR
\ and go through ENGINE$, the engine the bake itself emitted - never this one.
: HB$ ( -- ptr u8 n )   s" bin/hb-host" ;
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
   s" /cursor-moved.f"  DPMUT-BUF  UNDER-ROOT DPMUT-U !
   s" /generator-mutant.f" BFMUT-BUF UNDER-ROOT BFMUT-U !
   s" /reader-mutant.f"    AFMUT-BUF UNDER-ROOT AFMUT-U !
   s" /read-entry-mutant.f" RDMUT-BUF UNDER-ROOT RDMUT-U ! ;

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

\ ---- deriving a mutant from a real file --------------------------------------
\ Four case families below are "the real file plus (or minus) exactly one line",
\ so the anchor check and the splice live here once rather than beside whichever
\ family was written first. `?ONE` names the FILE it was looking in, because a
\ single message now serves anchors in four of them.

: FIND-OFF ( ptr u8 n -- n ) {: a:ptr u:n :}
   SHAPE:TEXT a u FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

: ?ONE ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n f:ptr fu:n :}
   a u SHAPE:COUNT 1 = if exit then
   s" aot-chain-capture-suite: anchors in " type f fu type s" =" type
   a u SHAPE:COUNT .
   s" aot-chain-capture-suite: a derived-mutant case cannot be built" STOP-RC die ;

\ text[0,cut) + inject + text[cut,end)
: SPLICE ( ptr u8 n n ptr u8 n -- ) {: dst:ptr dstu:n cut:n inj:ptr inju:n :}
   SHAPE:TEXT drop {: src:ptr :}
   dst dstu src cut WRITE-ALL
   dst dstu inj inju APPEND-FILE
   dst dstu src cut +  SHAPE:TEXT nip cut -  APPEND-FILE
   s" the mutant is its file plus exactly the injected line" T-LABEL
   dst dstu FILE-SIZE  SHAPE:TEXT nip inju + T= ;

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

\ The production reader, run on whatever bytes the case put at CASE$. `entry` is
\ the file that loads AOT-FILE:READ; the one case below that has to reach a
\ refusal only a defective READER can raise passes a derived copy of it instead.
: READ-ENTRY$ ( -- ptr u8 n ) s" test/aot-file-read.f" ;

: RUN-READ-WITH ( ptr u8 n bool -- ) {: e:ptr eu:n mm:bool :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   e eu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   CASE$ >LEN PROC-ARGV+
   mm if s" mismatch" >LEN PROC-ARGV+ then
   EXEC ;

: RUN-READ ( bool -- ) {: mm:bool :} READ-ENTRY$ mm RUN-READ-WITH ;

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
   \ The window's DATA is SPARSE, and these three are what says so without
   \ letting the sparseness hide a capture that stopped working. The span is the
   \ chain's whole window and is now a scalar rather than a section length; the
   \ runs are its INITIALISED extents, measured at four rows of eight bytes, and
   \ a capture that emitted none would have dropped the four cells the seed has
   \ to deliver. The identity above already ties the span to the window's own
   \ measurement, so a shrunken span breaks that before it reaches this.
   s" datasz=" FIELD  1000000 s" the window's DATA span is the chain's" FLOOR
   s" runs=" FIELD          1 s" ... and its initialised extents are carried" FLOOR
   s" runbytes=" FIELD      8 s" ... with their bytes" FLOOR
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
   HB$ DIG SHA256-FILE 0 T=
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
32 constant O-PAYLEN
104 constant O-PAYSHA
18 constant SEC-N
16 constant ROW-BYTES
0 constant S-SCALARS             \ the first section: the five genuine scalars, fixed width
1 constant S-BLOB                \ the code blob: bytes, so any length is a whole number of rows
8 constant S-WDATA               \ the window's non-zero extents: 8-byte rows
9 constant S-WRUNS               \ ... and their bytes, concatenated in row order
12 constant S-PWID               \ the protected-WID bitmap, the other fixed-width section
14 constant S-SIGS               \ the window's checker signature rows
16 constant S-REG                \ ... and the type registry those rows resolve against
17 constant S-CLOSURE            \ the last section: the closure list
32 constant O-SCAL-DSPAN         \ the window DATA span, the fifth scalar's offset in that section
\ src/habu/aot-decl.f AOT-WINDOW:SPAN-CAP. The two cases below pin it from both
\ sides, so a cap that moved without this moving fails here instead of leaving a
\ forged span accepted.
$800000 constant SPAN-CAP
1 constant TARGET-MACOS
2 constant TARGET-LINUX

variable T-RECS  variable T-SITES  variable T-BLOB  variable T-DATASZ  variable T-CLOSURE
create SW0 512 allot     \ the two closure entries the chain case swaps
create SW1 512 allot

: A64@ ( n -- n ) {: at:n :}
   0 8 0 ?do  8 lshift  ART at 7 i - + + c@ or  loop ;

: A64! ( n n -- ) {: v:n at:n :}
   8 0 ?do  v i 8 * rshift $FF and  ART at i + + c!  loop ;

\ The window's run rows are packed u32 pairs, so the run cases need the narrower
\ accessor the rest of this fixture's edits do not.
: A32@ ( n -- n ) {: at:n :}
   0 4 0 ?do  8 lshift  ART at 3 i - + + c@ or  loop ;

: A32! ( n n -- ) {: v:n at:n :}
   4 0 ?do  v i 8 * rshift $FF and  ART at i + + c!  loop ;

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
   s" a section table that is not contiguous is refused, by section" T-LABEL
   s" section records does not start where the table says the last one ended"
   READ-REFUSED

   FRESH  3 SEC-LEN@ 4 - 3 SEC-LEN!  RETABLE RESEAL EMIT
   false RUN-READ
   s" a section length that is not a whole number of rows is refused, by section" T-LABEL
   s" section call sites is not a whole number of rows" READ-REFUSED

   \ THE PAIR IS CHOSEN SO THE OVERFLOW IS REAL, and it moved when the window
   \ went sparse: this used to hand the window's 1.5 MB to the blob, which no
   \ longer exists to hand over - and no redistribution of today's payload can
   \ fill the blob's own 2 MiB buffer. The run-byte section's buffer is the
   \ smallest one a large section can be poured into, so the blob's megabyte
   \ goes there instead. If it ever stops overflowing, the read succeeds and this
   \ case fails rather than passing on a check it never reached.
   FRESH
   1 SEC-LEN@ S-WRUNS SEC-LEN@ + {: both:n :}
   both S-WRUNS SEC-LEN!  0 1 SEC-LEN!
   RETABLE RESEAL EMIT
   false RUN-READ
   s" a section larger than the buffer it fills is refused, by section" T-LABEL
   s" section window DATA run bytes is larger than the buffer it fills" READ-REFUSED

   FRESH  1 S-CLOSURE SEC-AT A64!  RESEAL EMIT
   false RUN-READ
   s" a closure list that does not fill its section is refused" T-LABEL
   s" the closure list does not fill its section" READ-REFUSED ;

\ ---- the rest of the reader's table and closure refusals ---------------------
\
\ WHY THESE ARE A SEPARATE BLOCK FROM PROBE-FORGED ABOVE. Those five are the
\ shapes a corrupt file takes; these are the shapes a WELL-FORMED-LOOKING table
\ takes - each one passes every check before it and is caught by exactly one
\ named refusal, so a case that fired early would be pinning the wrong word. The
\ payload-length case is the only one that needs no reseal, and that is a fact
\ about the format rather than a shortcut: the header is the one part the payload
\ digest does not cover, so an edit there survives on its own.
\ EACH CASE NAMES THE SECTION AS WELL AS THE REFUSAL. `blob has a negative
\ length` and `scalars is not its fixed width` are one string, so a refusal that
\ fired on some other section - which is how a fixture stops testing what it was
\ written for - fails here instead of passing.
: PROBE-SHORT-PAYLOAD ( -- )
   FRESH  SEC-N ROW-BYTES * 1 -  O-PAYLEN A64!  EMIT
   false RUN-READ
   s" a payload too short to hold its own section table is refused" T-LABEL
   s" the payload is shorter than its own section table" READ-REFUSED ;

: PROBE-TABLE-SHAPE ( -- )
   FRESH  -1 S-BLOB SEC-LEN!  RESEAL EMIT
   false RUN-READ
   s" a section with a negative length is refused, by section" T-LABEL
   s" section blob has a negative length" READ-REFUSED

   FRESH  O-PAYLEN A64@ S-BLOB SEC-LEN!  RESEAL EMIT
   false RUN-READ
   s" a section that runs past the payload is refused, by section" T-LABEL
   s" section blob runs past the payload" READ-REFUSED

   FRESH  S-CLOSURE SEC-LEN@ 8 - S-CLOSURE SEC-LEN!  RESEAL EMIT
   false RUN-READ
   s" a set of sections that does not fill the payload is refused" T-LABEL
   s" the sections do not fill the payload" READ-REFUSED ;

\ The two fixed-width sections, each shortened by a whole number of ITS rows and
\ the bytes given to the blob, so the table stays contiguous and still fills the
\ payload: what is left is only the width, which is what ?EXACT is for.
\ THE BITMAP CASE CARRIES A SECOND JOB. It is the standing proof of the pin that
\ makes src/habu/aot-file.f SKIP-SECTION's buffer wide enough by construction -
\ the merge reads that one section past without keeping it, and its length is
\ already fixed before the section loop begins.
: SHRINK-INTO-BLOB ( n n -- ) {: k:n by:n :}
   k SEC-LEN@ by - k SEC-LEN!
   S-BLOB SEC-LEN@ by + S-BLOB SEC-LEN!
   RETABLE RESEAL EMIT ;

: PROBE-FIXED-WIDTH ( -- )
   FRESH  S-SCALARS 8 SHRINK-INTO-BLOB
   false RUN-READ
   s" a scalars section that is not its fixed width is refused" T-LABEL
   s" section scalars is not its fixed width" READ-REFUSED

   FRESH  S-PWID 4 SHRINK-INTO-BLOB
   false RUN-READ
   s" ... and so is a short protected-WID bitmap, before any section is read" T-LABEL
   s" section protected-WID bitmap is not its fixed width" READ-REFUSED ;

\ The closure section is a count followed by (length, bytes) entries, and the two
\ ways it can end early are told apart by which bound the walk crosses first: one
\ more entry than the bytes hold, and one entry claiming more bytes than remain.
: PROBE-CLOSURE-SHAPE ( -- )
   FRESH  S-CLOSURE SEC-AT A64@ 1 +  S-CLOSURE SEC-AT A64!  RESEAL EMIT
   false RUN-READ
   s" a closure count one entry past its bytes is refused" T-LABEL
   s" the closure list ends inside an entry" READ-REFUSED

   FRESH  S-CLOSURE SEC-LEN@  S-CLOSURE SEC-AT 8 + A64!  RESEAL EMIT
   false RUN-READ
   s" a closure entry claiming more bytes than remain is refused" T-LABEL
   s" the closure list ends inside a path" READ-REFUSED ;

\ ---- the window's runs, which are a well-formed table describing no window ----
\
\ WHY THESE ARE THEIR OWN BLOCK. The span used to be a section's LENGTH, so the
\ table's own contiguity arithmetic bounded it and ?TABLE caught every shape a
\ forged window could take. It is a SCALAR now and the content is a table of
\ extents, so a run table can be a whole number of rows, sit inside its buffer
\ and fill the payload while describing a window no engine could reserve. Each
\ case below is such a table, and each one passes every check before the one it
\ is written for.
\
\ THE FIRST ROW IS EIGHT BYTES AT A REAL OFFSET, which is what makes the edits
\ minimal: a length set to zero, an offset pushed past the span, two rows swapped,
\ one row's start moved four bytes into the previous row's extent, and one row's
\ length shortened. Nothing else in the artifact moves, so the refusal that fires
\ is the one the edit describes.
: RUN-OFF-AT ( n -- n ) {: k:n :} S-WDATA SEC-AT k 8 * + ;
: RUN-LEN-AT ( n -- n ) {: k:n :} S-WDATA SEC-AT k 8 * + 4 + ;
: RUN-OFF@ ( n -- n ) RUN-OFF-AT A32@ ;
: RUN-LEN@ ( n -- n ) RUN-LEN-AT A32@ ;
: RUN-OFF! ( n n -- ) {: v:n k:n :} v k RUN-OFF-AT A32! ;
: RUN-LEN! ( n n -- ) {: v:n k:n :} v k RUN-LEN-AT A32! ;
: DSPAN@ ( -- n ) S-SCALARS SEC-AT O-SCAL-DSPAN + A64@ ;
: DSPAN! ( n -- ) {: v:n :} v S-SCALARS SEC-AT O-SCAL-DSPAN + A64! ;

\ The fixture's own anchor: every case below edits row 0 and row 1, so a capture
\ that stopped producing two runs would leave them editing nothing.
: ?RUNS-PRESENT ( -- )
   S-WDATA SEC-LEN@ 8 / 2 >= if exit then
   s" aot-chain-capture-suite: the capture produced fewer than two window runs"
   STOP-RC die ;

: PROBE-RUN-SHAPE ( -- )
   FRESH ?RUNS-PRESENT  0 0 RUN-LEN!  RESEAL EMIT
   false RUN-READ
   s" a window run of no length is refused" T-LABEL
   s" a window DATA run is empty" READ-REFUSED

   FRESH  DSPAN@ 4 -  S-WDATA SEC-LEN@ 8 / 1 - RUN-OFF!  RESEAL EMIT
   false RUN-READ
   s" a window run reaching past the span is refused" T-LABEL
   s" a window DATA run reaches past the window DATA span" READ-REFUSED

   FRESH
   0 RUN-OFF@ {: o0:n :}
   1 RUN-OFF@ {: o1:n :}
   o1 0 RUN-OFF!  o0 1 RUN-OFF!
   RESEAL EMIT
   false RUN-READ
   s" window runs out of ascending order are refused" T-LABEL
   s" the window DATA runs are not in ascending order" READ-REFUSED

   FRESH  0 RUN-OFF@ 0 RUN-LEN@ + 4 -  1 RUN-OFF!  RESEAL EMIT
   false RUN-READ
   s" window runs that overlap are refused" T-LABEL
   s" the window DATA runs overlap" READ-REFUSED

   FRESH  0 RUN-LEN@ 4 -  0 RUN-LEN!  RESEAL EMIT
   false RUN-READ
   s" run lengths that do not add up to the byte section are refused" T-LABEL
   s" the window DATA runs do not fill their own byte section" READ-REFUSED ;

\ The span stopped being derivable when the window went sparse, so it is the one
\ number the artifact ASSERTS. Both sides of its cap are pinned: the cap itself
\ is a span this engine will bake, and one byte more is not.
: PROBE-SPAN-CAP ( -- )
   FRESH  SPAN-CAP DSPAN!  RESEAL EMIT
   false RUN-READ
   s" a window DATA span at the cap is accepted" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=

   FRESH  SPAN-CAP 1+ DSPAN!  RESEAL EMIT
   false RUN-READ
   s" ... and one byte past it is refused" T-LABEL
   s" the window DATA span exceeds what this engine can bake" READ-REFUSED ;

\ ---- the refusal only a defective reader can raise ---------------------------
\
\ WHY THIS ONE NEEDS A MUTANT. ?PAYLOAD-AGAIN asks whether the second pass read
\ the bytes the first pass verified. On a static file it cannot fail: the table
\ ?TABLE accepted is contiguous and sums to the payload, so pass two reads
\ exactly what pass one streamed. Its two real producers are a file replaced
\ between the passes - which no artifact this suite can write reaches - and a
\ reader that does not read what its own table promised. The second is the one
\ the header claimed was "measured", and this turns that measurement into a case.
\
\ THE MUTANT IS THE REAL READER PLUS ONE LINE, the way the order and cursor cases
\ above are the real tool plus one line: src/habu/aot-file.f with a line that
\ makes LOAD-SECTION return early for the closure section, and a copy of the read
\ entry point whose require is repointed at it. Skipping the LAST section is what
\ leaves the file cursor where the reader expects it, so the refusal that fires is
\ the digest comparison and not a short read. Both splices are fail-closed on
\ anchors that must appear exactly once.
: AF-TOOL$ ( -- ptr u8 n ) s" src/habu/aot-file.f" ;
: LS-ANCHOR$ ( -- ptr u8 n ) s\" \n: LOAD-SECTION ( n -- ) {: k:n :}\n" ;
: LS-INJECT$ ( -- ptr u8 n ) s\"    k S-CLOSURE = if exit then\n" ;
: RD-ANCHOR$ ( -- ptr u8 n ) s\" \nrequire src/habu/aot-file.f\n" ;

: AF-MUTANT-BUILD ( -- )
   AF-TOOL$ SHAPE:LOAD
   LS-ANCHOR$ AF-TOOL$ ?ONE
   AFMUT$ LS-ANCHOR$ FIND-OFF LS-ANCHOR$ nip + 1 - LS-INJECT$ SPLICE ;

\ The entry point, with its require of the real reader rewritten to name the
\ mutant. Rewritten and not added, so the assertion is on the bytes either side.
: RD-MUTANT-BUILD ( -- )
   READ-ENTRY$ SHAPE:LOAD
   RD-ANCHOR$ READ-ENTRY$ ?ONE
   RD-ANCHOR$ FIND-OFF 1 + {: cut:n :}
   SHAPE:TEXT drop {: src:ptr :}
   RDMUT$ src cut WRITE-ALL
   RDMUT$ s" require " APPEND-FILE
   RDMUT$ AFMUT$ APPEND-FILE
   RDMUT$ s\" \n" APPEND-FILE
   RDMUT$ src cut RD-ANCHOR$ nip 1 - + +
          SHAPE:TEXT nip cut - RD-ANCHOR$ nip 1 - -  APPEND-FILE
   s" the read mutant is the entry with its reader require repointed" T-LABEL
   RDMUT$ FILE-SIZE
   SHAPE:TEXT nip AF-TOOL$ nip - AFMUT$ nip +  T= ;

: PROBE-SECOND-PASS ( -- )
   AF-MUTANT-BUILD
   RD-MUTANT-BUILD
   FRESH EMIT
   RDMUT$ false RUN-READ-WITH
   s" a reader that skips a section it promised to read is refused" T-LABEL
   s" the second pass did not read the payload the first pass verified" READ-REFUSED
   s" ... and the unmutated reader takes the same bytes" T-LABEL
   false RUN-READ
   RC @ 0 <> if DIAG. then
   RC @ 0 T= ;

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
   s\" s\" : FOO ( n -- n ) 1 + ;\" NMIGRATE:DEFINE 7 FOO .\n" ;

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

\ ---- and its words can be NAMED by checked code ------------------------------
\
\ WHAT THIS LOCKS, and why running a chain word was not enough. PROBE-BAKED
\ proves the seeded code executes. It does not prove the seeded engine can
\ COMPILE against it: a seed puts a word in the runtime dictionary and nothing in
\ the checker's record set, so `: T ( n -- n ) ENC-B ;` died E-UNDEFINED at that
\ token in an engine that could call ENC-B all day. The product is a compiler, so
\ that is the difference between an engine that ships and one that does not -
\ `bin/hb --load src/arch/arm64/icode.f` and fourteen battery phases died on it.
\
\ THE FAMILY-TYPED CASE IS THE ONE THAT CANNOT PASS BY LUCK. A signature is text,
\ and text mentioning `IR-ARENA:view` only parses where that family is
\ registered, so the third program below certifies exactly when the type registry
\ travelled too. Its negative is what proves the family arrived as a FAMILY
\ rather than as an untyped cell: passing an `n` where the view belongs has to be
\ refused with both type names, and an engine that had merely made the name
\ resolvable would accept it.
\
\ AND THE FOURTH IS THE REFUSAL THAT MUST SURVIVE. A lazy intake that answered
\ for names it has no row for would turn every typo into a certified call into
\ nothing, so an undefined word still has to die.
\
\ THE FAMILY CASE IS RUN TWICE, AND THE SECOND RUN IS THE PLACEMENT. Registry
\ ids are absolute, so a program that declares its own type before it names a
\ chain word is the whole difference between installing the registry at the seed
\ point and installing it at the first intake. Measured three ways on the
\ product: at the seed point both runs certify; installed at the intake instead,
\ the first certifies and the second dies `does not start where its capture
\ did`; installed nowhere, both die `a seeded signature does not parse`. Only
\ the second run separates the first world from the second.

: SIG-OK$ ( -- ptr u8 n )
   s\" using A64ASM\n: SIGT ( n -- n ) ENC-B ;\ns\" sig-ok\" type cr\n" ;

: SIG-FAM$ ( -- ptr u8 n )
   s\" : SIGFT ( IR-ARENA:view -- n ) NTAPE:TOKENS ;\ns\" fam-ok\" type cr\n" ;

: SIG-FAM-BAD$ ( -- ptr u8 n )
   s\" : SIGFTBAD ( n -- n ) NTAPE:TOKENS ;\ns\" reached\" type cr\n" ;

\ The same family-typed definition with a type of the PROGRAM'S OWN ahead of it.
\ This is the case that decides WHERE the registry goes in, and the one an
\ engine that installed it at the first signature intake fails: the carried rows
\ name families by absolute id, so they only mean those families while the live
\ registry still ends where the capture's did, and `NEWTYPE zz1 0` has already
\ moved the high-water by the time the intake runs. Installing at the seed point
\ - before this program's first token - is what makes it ordinary again.
: SIG-FAM-AFTER-DECL$ ( -- ptr u8 n )
   s\" NEWTYPE zz1 0\n: SIGFD ( IR-ARENA:view -- n ) NTAPE:TOKENS ;\ns\" decl-fam-ok\" type cr\n" ;

: SIG-UNDEF$ ( -- ptr u8 n )
   s\" : SIGX ( -- ) NO-SUCH-SEEDED-WORD ;\ns\" reached\" type cr\n" ;

: PROBE-SEEDED-SIG ( -- )
   SIG-OK$ RUN-BAKED-PROGRAM
   s" a definition in the seeded engine may name a seeded word" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   s" ... and runs" T-LABEL
   OUT$ s\" sig-ok\n" T$=
   SIG-FAM$ RUN-BAKED-PROGRAM
   s" ... including one whose signature names a family the window declared" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   OUT$ s\" fam-ok\n" T$=
   SIG-FAM-AFTER-DECL$ RUN-BAKED-PROGRAM
   s" ... even when the program declared a type of its own first" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   OUT$ s\" decl-fam-ok\n" T$=
   SIG-FAM-BAD$ RUN-BAKED-PROGRAM
   s" ... and that family is a FAMILY there: the wrong type is refused" T-LABEL
   CHECK-RC s" ir-arena:view" RC-REFUSED
   s" ... and the body that would have run it never does" T-LABEL
   OUT$ s" reached" CONTAINS? 0= TTRUE
   SIG-UNDEF$ RUN-BAKED-PROGRAM
   s" ... while a word no window ever compiled is still undefined, by name" T-LABEL
   CHECK-RC s" NO-SUCH-SEEDED-WORD" RC-REFUSED ;

\ ---- and the registry it resolves against is asserted, not assumed -----------
\
\ WHAT THIS LOCKS. The type registry travels as a DELTA appended at the
\ high-water its capture window opened on, because its records name each other by
\ family id, schema node id and interned string offset and none of those rebase.
\ That is sound exactly while the two engines agree on the base, and they do -
\ the cold prefix is all that fills the registry before a seed runs. There is no
\ remap if they ever disagree, so the engine has to refuse rather than append a
\ delta whose ids mean other families.
\
\ THE FORGE IS THE ONLY WAY TO REACH IT. Both engines derive the base the same
\ way, so no source edit produces a disagreement; the artifact's own recorded
\ base is what a corrupt or stale registry section would carry, and moving it by
\ one is exactly that. The payload digest covers it, so the case reseals - the
\ same machinery every forged-table case above uses, proven to stamp the field
\ the reader reads by the case that reseals without editing.
: REG-BASE-AT ( -- n ) S-REG SEC-AT 8 + ;      \ store 0's base, first row of the table

: PROBE-REG-BASE ( -- )
   FRESH
   s" the artifact carries a type registry to forge" T-LABEL
   S-REG SEC-LEN@ 0 > TTRUE
   REG-BASE-AT A64@ {: was:n :}
   s" ... whose first store opens at a base of its own" T-LABEL
   was 0 > TTRUE
   was 1 + REG-BASE-AT A64!
   RESEAL
   EMIT
   BAKE-TOOL$ CASE$ BAKE-WITH
   s" a forged registry base still bakes: the artifact is well-formed" T-LABEL
   RC @ 0 T=
   SIG-FAM$ RUN-BAKED-PROGRAM
   s" ... and the engine refuses it the moment a signature needs the registry" T-LABEL
   REG-RC s" does not start where its capture did" RC-REFUSED
   s" ... naming the store whose base disagreed" T-LABEL
   s" families" SAID? TTRUE ;

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
\ THE MUTANT IS THE REAL GENERATOR PLUS ONE LINE, and the line makes the generated
\ driver consume DATA at top level - which is what the historical string literal
\ did, stated as the rule rather than as the one spelling of it. src/habu/stdin.f
\ DP-MARK latches the cursor as the last thing the driver's own load does, so RUN
\ sees any such splice and refuses by name. Without the refusal the build writes an
\ engine that is simply not the one the same tree would write elsewhere, and
\ nothing anywhere says so.
\
\ TWO DERIVED FILES, because the generator moved to where the build's other path
\ construction lives: tools/build-fixpoint.f with one more append spliced in ahead
\ of the one that writes the driver's own tail, and
\ tools/aot-chain-bake.f with its require of build-fixpoint repointed at that copy.
\ Both splices are fail-closed on anchors that must appear exactly once, so a
\ generator that moved again stops this suite instead of running an unmutated file.
: BF-TOOL$ ( -- ptr u8 n ) s" tools/build-fixpoint.f" ;
: DP-ANCHOR$ ( -- ptr u8 n ) s\" \n   BF-DRV-TAIL$ BF-DRV+\n" ;
: DP-INJECT$ ( -- ptr u8 n ) S\"    S\\\" 8 allot\\n\" BF-DRV+\n" ;
: REQ-ANCHOR$ ( -- ptr u8 n ) s\" \nrequire tools/build-fixpoint.f\n" ;

: BF-MUTANT-BUILD ( -- )
   BF-TOOL$ SHAPE:LOAD
   DP-ANCHOR$ BF-TOOL$ ?ONE
   BFMUT$ DP-ANCHOR$ FIND-OFF 1 + DP-INJECT$ SPLICE ;

\ The bake tool, requiring the mutant generator instead of the real one. The
\ replacement is the same length question as the splice above: the require line is
\ rewritten, not added, so the assertion is on the bytes either side of it.
: BAKE-MUTANT-BUILD ( -- )
   BAKE-TOOL$ SHAPE:LOAD
   REQ-ANCHOR$ BAKE-TOOL$ ?ONE
   REQ-ANCHOR$ FIND-OFF 1 + {: cut:n :}
   SHAPE:TEXT drop {: src:ptr :}
   DPMUT$ src cut WRITE-ALL
   DPMUT$ s" require " APPEND-FILE
   DPMUT$ BFMUT$ APPEND-FILE
   DPMUT$ s\" \n" APPEND-FILE
   DPMUT$ src cut REQ-ANCHOR$ nip 1 - + +
          SHAPE:TEXT nip cut - REQ-ANCHOR$ nip 1 - -  APPEND-FILE
   s" the bake mutant is the tool with its generator require repointed" T-LABEL
   DPMUT$ FILE-SIZE
   SHAPE:TEXT nip BF-TOOL$ nip - BFMUT$ nip +  T= ;

: PROBE-DP-MOVED ( -- )
   BF-MUTANT-BUILD
   BAKE-MUTANT-BUILD
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
   PROBE-SHORT-PAYLOAD
   PROBE-TABLE-SHAPE
   PROBE-FIXED-WIDTH
   PROBE-RUN-SHAPE
   PROBE-SPAN-CAP
   PROBE-CLOSURE-SHAPE
   PROBE-SECOND-PASS
   PROBE-CHAIN
   PROBE-BAKED
   PROBE-SEEDED-SIG
   PROBE-PROVIDED
   PROBE-REPRO
   PROBE-DP-MOVED
   PROBE-REG-BASE ;                  \ last: it leaves a deliberately unusable engine

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
