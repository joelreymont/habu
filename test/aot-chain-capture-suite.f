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
\ Cost: three child engine runs (~3s), no metabuild. Registered as
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
create ART-BUF FS-PATH-CAP allot     variable ART-U
create CASE-BUF FS-PATH-CAP allot    variable CASE-U
create DIG 32 allot
create DHEX 64 allot

\ The artifact under test, held whole so a case can edit bytes and write the
\ result out. 4 MiB covers today's 2.95 MiB with room; READ-ALL refuses a file
\ that outgrows it rather than truncating one.
$400000 constant ART-CAP
create ART ART-CAP allot    variable ART-LEN

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: MUT$ ( -- ptr u8 n )  MUT-BUF MUT-U @ ;
: ART$ ( -- ptr u8 n )  ART-BUF ART-U @ ;
: CASE$ ( -- ptr u8 n ) CASE-BUF CASE-U @ ;
: HB$ ( -- ptr u8 n )   s" bin/hb" ;
: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;
: TOOL$ ( -- ptr u8 n ) s" tools/aot-chain-capture.f" ;

\ The line that opens the window. Splicing in front of it is what puts the
\ capture's own closure ahead of the chain, which is the refuted order.
: ANCHOR$ ( -- ptr u8 n ) s\" \nAOT-CHAIN:OPEN\n" ;
: INJECT$ ( -- ptr u8 n ) s\" require src/arch/arm64/asm.f\n" ;

: SETUP ( -- )
   s" habu-aot-chain" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT-BUF MUT-BUF ROOT-U @ BYTE-COPY
   s" /hoisted.f" {: sa:ptr su:n :}
   sa MUT-BUF ROOT-U @ + su BYTE-COPY
   ROOT-U @ su + MUT-U !
   ROOT-BUF ART-BUF ROOT-U @ BYTE-COPY
   s" /chain.aot" {: aa:ptr au:n :}
   aa ART-BUF ROOT-U @ + au BYTE-COPY
   ROOT-U @ au + ART-U !
   ROOT-BUF CASE-BUF ROOT-U @ BYTE-COPY
   s" /case.aot" {: ca:ptr cu:n :}
   ca CASE-BUF ROOT-U @ + cu BYTE-COPY
   ROOT-U @ cu + CASE-U ! ;

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
   s" bandbytes=" FIELD     1 s" ... on the DATA axis either" FLOOR ;

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
   s" ... and exactly its DATA window" T-LABEL
   s" hostdata=" s" mergeddatasz=" T-DATASZ @ MERGED= ;

: BODY ( -- )
   SETUP
   MUTANT-BUILD
   SELFTEST
   PROBE-REAL
   PROBE-IDENT
   PROBE-ORDER
   PROBE-NOT-FIRST
   PROBE-ARTIFACT
   PROBE-READBACK
   PROBE-MERGE
   PROBE-DAMAGE
   PROBE-FORGED
   PROBE-CHAIN ;

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
