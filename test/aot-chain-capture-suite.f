\ aot-chain-capture-suite.f - the AOT artifact format and the chain capture tool,
\ both through the real load path (dot habu-retire-the-s-4fbc244f).
\
\ The small capture runs WRITE/READ over real records, DATA runs and declared
\ address cells, clearing the row buffer before restoration. The parent checks
\ the header's independent magic/version/count and the producer engine digest.
\ A separate reader process checks chosen rows; MERGE checks location/target
\ rebasing and raw-cell/instruction-chain DATA relocation sites. Old versions,
\ partial rows and invalid row coordinates must fail through the file reader.
\
\ The production capture tool is also loaded and must name its empty-window
\ refusal: the booted engine already provides its chain. A private source-built
\ host then captures the real compiler and exercises the producer's live-row
\ checks, including count-preserving corruptions and an order-independent control.
\
\ Registered as `TEST:SUITE aot-chain-capture`. Run standalone:
\   bin/hb --load test/aot-chain-capture-suite.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/codesign.f
require tools/build-fixpoint.f

package AOT-CHAIN-SUITE

$8000 constant CAP
60000 constant CHILD-TIMEOUT-MS

\ The header this reads back is src/habu/aot-file.f's, and these are its offsets
\ and its identity. They are written out again rather than imported because that
\ file cannot be loaded here - its closure is a capture's, not a test's - and the
\ two spellings agreeing is the thing being checked.
136 constant HDR-BYTES
0 constant O-MAGIC
8 constant O-VERSION
24 constant O-SECTIONS
32 constant O-PAYLEN
40 constant O-PRODUCER
$00544F4155424148 constant MAGIC     \ "HABUAOT\0" in LE byte order
12 constant VERSION
18 constant SECTIONS
64 constant HEX-LEN

\ tools/aot-chain-capture.f's refusal code and the sentence the product must die
\ with, which is a different exit from every undefined-word death.
$4A constant REFUSE-RC

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
create EMPTY 1 allot                          \ zero-length stdin
variable RC

create ROOT-BUF FS-PATH-CAP allot   variable ROOT-U
create ART-BUF FS-PATH-CAP allot    variable ART-U
create HDR HDR-BYTES allot
create PROD-HEX HEX-LEN allot        \ sha256(bin/hb), taken here
create ART-HEX HEX-LEN allot         \ the producer key the artifact carries

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: ART$ ( -- ptr u8 n ) ART-BUF ART-U @ ;
: HB$ ( -- ptr u8 n ) s" bin/hb" ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

\ One tree per run, registered for cleanup, so "the artifact exists" is a statement
\ about the capture that just ran and never about a leftover.
: SETUP ( -- )
   s" habu-aot-chain" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" small.aot" ART-BUF JOIN-PATH ART-U ! ;

: RUN-ENGINE ( ptr u8 n -- )
   >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  CHILD-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: RUN-CHILD ( -- ) HB$ RUN-ENGINE ;

: RUN-ROUNDTRIP ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-artifact-roundtrip.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ART$ >LEN PROC-ARGV+
   RUN-CHILD ;

: RUN-OWNED-CAPTURE ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-owned-capture.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ART$ >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   RUN-CHILD ;

: RUN-DATA-SITES ( ptr u8 n ptr u8 n -- )
   {: mode:ptr modeu:n transport:ptr transportu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-data-sites.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ART$ >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   transport transportu >LEN PROC-ARGV+
   RUN-CHILD ;


: RUN-ADDRESS-CELLS ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-address-cells.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ART$ >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   RUN-CHILD ;

: RUN-ROW-CASE ( ptr u8 n -- )
   {: name u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-artifact-rows.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ART$ >LEN PROC-ARGV+
   name u >LEN PROC-ARGV+
   RUN-CHILD ;

\ The argv the build uses (tools/build-fixpoint.f BF-PREPARE-CAPTURE-ARGV), with
\ the artifact path this suite would have taken if the product could capture.
: RUN-CAPTURE-TOOL ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/aot-chain-capture.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ROOT$ >LEN PROC-ARGV+
   RUN-CHILD ;

: SAID? ( ptr u8 n -- ) {: m:ptr mu:n :}
   m mu T-LABEL
   OUT$ m mu CONTAINS? TTRUE ;

: ERR-SAID? ( ptr u8 n -- ) {: m:ptr mu:n :}
   m mu T-LABEL
   ERR$ m mu CONTAINS? TTRUE ;

: CHILD-FAILED. ( -- )
   RC @ 0 = if exit then
   s" aot-chain-capture-suite: child stdout:" type cr OUT$ type cr
   s" aot-chain-capture-suite: child stderr:" type cr ERR$ type cr ;

\ Exactly the header, read with the engine's own descriptor words: the artifact is
\ larger than a header and READ-ALL refuses a cap smaller than the whole file.
: HEADER@ ( -- )
   ART$ FS-PATHZ open-rd {: fd:n :}
   fd 0 < if
      s" aot-chain-capture-suite: cannot open the artifact" type cr
      E-FS-OPEN throw
   then
   fd HDR HDR-BYTES read {: got:n :}
   fd close
   s" the artifact is at least a header long" T-LABEL
   got HDR-BYTES T= ;

: HDR-U64@ ( n -- n ) {: at:n :} HDR at + FS-U64@ ;

: PROBE-ROUNDTRIP ( -- )
   SETUP
   RUN-ROUNDTRIP
   s" a capture writes, reads and rewrites its artifact byte for byte" T-LABEL
   CHILD-FAILED.
   RC @ 0 T=
   s" roundtrip: recs=" SAID?
   s" pwin=1" SAID?
   s" xtcells=declared+targeted" SAID?
   s" xtcells-restored=exact" SAID?
   s" roundtrip=ok" SAID? ;

: PROBE-ARTIFACT ( -- )
   s" the artifact is at the path the capture was given" T-LABEL
   ART$ FILE? TTRUE
   HEADER@
   s" its magic is this format's" T-LABEL
   O-MAGIC HDR-U64@ MAGIC T=
   s" its version is the one the reader accepts" T-LABEL
   O-VERSION HDR-U64@ VERSION T=
   s" its section count is the one the reader accepts" T-LABEL
   O-SECTIONS HDR-U64@ SECTIONS T=
   s" the header's payload length accounts for the whole file" T-LABEL
   ART$ FILE-SIZE  HDR-BYTES O-PAYLEN HDR-U64@ +  T= ;

\ The producer key, hashed here rather than believed.
: PROBE-PRODUCER ( -- )
   s" bin/hb hashes at the path this suite reads it from" T-LABEL
   HB$ PROD-HEX SHA256-FILE-HEX 0 T=
   s" the artifact names that hash as its producer" T-LABEL
   HDR O-PRODUCER + ART-HEX SHA256>HEX
   ART-HEX HEX-LEN  PROD-HEX HEX-LEN  T$= ;

\ The tool loads in a booted engine and refuses the one way the product must.
: PROBE-CAPTURE-TOOL ( -- )
   RUN-CAPTURE-TOOL
   s" the capture tool refuses the product by its own code" T-LABEL
   RC @ REFUSE-RC T=
   s" and names the empty window rather than a missing word" T-LABEL
   s" the window is empty - the chain did not load" ERR-SAID? ;


: ROW-RC ( n -- )
   {: want:n :}
   RC @ want <> if
      s" artifact-row child stdout:" type cr OUT$ type cr
      s" artifact-row child stderr:" type cr ERR$ type cr
   then
   RC @ want T= ;


: ROW-REFUSED ( ptr u8 n ptr u8 n -- )
   {: name u:n message mu:n :}
   name u T-LABEL
   name u RUN-ROW-CASE
   $4B ROW-RC
   message mu ERR-SAID? ;


: PROBE-ADDRESS-ROWS ( -- )
   s" matrix" RUN-ROW-CASE
   s" eight address rows merge with separate location and target coordinates" T-LABEL
   0 ROW-RC
   s" address-rows: merge=ok" SAID?
   s" fresh" RUN-ROW-CASE
   s" all address rows survive a fresh reader process" T-LABEL
   0 ROW-RC
   s" address-rows: fresh=ok" SAID?
   s" old-version" s" the artifact is not one this engine can read" ROW-REFUSED
   s" short-row" s" address cells is not a whole number of rows" ROW-REFUSED
   s" bad-window" s" address cell reaches past its window DATA span" ROW-REFUSED
   s" bad-fixed" s" fixed address cell is outside DATA" ROW-REFUSED
   s" bad-data" s" address cell DATA target is outside its window" ROW-REFUSED
   s" bad-code" s" address cell CODE target is outside its blob" ROW-REFUSED
   s" bad-data-site" s" DATA relocation site reaches past its blob" ROW-REFUSED
   s" bad-chain-site" s" DATA relocation site reaches past its blob" ROW-REFUSED ;

\ Run the actual producer in a source-only host. Its private copy adds two
\ declarations around the window and replaces only the final MAIN invocation
\ with row checks; no production test switch or alternate row writer is used.
$10000 constant TOOL-CAP
create TOOL-SOURCE TOOL-CAP allot variable TOOL-U
create TOOL-PATH FS-PATH-CAP allot variable TOOL-PATH-U
create HOST-PATH FS-PATH-CAP allot variable HOST-PATH-U

: TOOL$ ( -- ptr u8 n ) TOOL-PATH TOOL-PATH-U @ ;
: HOST$ ( -- ptr u8 n ) HOST-PATH HOST-PATH-U @ ;
: TOOL+ ( ptr u8 n -- ) {: a:ptr u:n :} TOOL$ a u APPEND-FILE ;
: TOOL-PART ( n n -- ) {: start:n finish:n :}
   TOOL-SOURCE start + finish start - TOOL+ ;

: TOOL-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   TOOL-SOURCE TOOL-U @ a u FIND-SUB MATCH option
      some OF IDX>N ENDOF
      none OF s" chain-address-rows: producer source boundary missing" 75 die ENDOF
   ;MATCH ;

: PREPARE-PRODUCER ( -- )
   ROOT$ s" row-producer.f" TOOL-PATH JOIN-PATH TOOL-PATH-U !
   ROOT$ s" hb-stdin" HOST-PATH JOIN-PATH HOST-PATH-U !
   s" tools/aot-chain-capture.f" TOOL-SOURCE TOOL-CAP READ-ALL TOOL-U !
   S\" AOT-CHAIN:OPEN\n" TOOL-AT {: opened:n :}
   S\" AOT-CHAIN:CLOSE\n" TOOL-AT {: closed:n :}
   S\" AOT-CHAIN:MAIN\n" TOOL-AT {: called:n :}
   called S\" AOT-CHAIN:MAIN\n" nip + TOOL-U @ T=
   TOOL$ TOOL-SOURCE opened WRITE-ALL
   S\" package CHAIN-ROW-OUTSIDE\npublic\nPERSISTED-PTR-VARIABLE SLOT\n;package\n" TOOL+
   opened closed TOOL-PART
   S\" package CHAIN-ROW-INSIDE\npublic\nPERSISTED-PTR-VARIABLE NIL\nvariable TARGET\n;package\nCHAIN-ROW-INSIDE:TARGET CHAIN-ROW-OUTSIDE:SLOT !\n" TOOL+
   closed called TOOL-PART
   S\" require test/aot-chain-row-checks.f\n" TOOL+
   ROOT$ BUILD-FIXPOINT:BF-TMP!
   BUILD-FIXPOINT:BF-PREFLIGHT
   BUILD-FIXPOINT:BF-STAGE-FIXPOINT
   s" src/habu/stdin.f" BUILD-FIXPOINT:BF-EMIT-ENGINE
   BUILD-FIXPOINT:BF-TMP-RESET ;

: PRODUCER-CASE ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u T-LABEL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   a u >LEN PROC-ARGV+
   HOST$ RUN-ENGINE
   want ROW-RC
   want 0= if s" chain-address-rows: ok" SAID? else
      s" declared address rows do not match the live window" ERR-SAID?
   then ;

: PROBE-PRODUCER-ROWS ( -- )
   PREPARE-PRODUCER
   s" valid" 0 PRODUCER-CASE
   s" chain-closure: portable" SAID?
   s" reorder" 0 PRODUCER-CASE
   s" index-scale" 0 PRODUCER-CASE
   s" missing" REFUSE-RC PRODUCER-CASE
   s" duplicate" REFUSE-RC PRODUCER-CASE
   s" location" REFUSE-RC PRODUCER-CASE
   s" kind" REFUSE-RC PRODUCER-CASE
   s" target" REFUSE-RC PRODUCER-CASE
   s" null-target" REFUSE-RC PRODUCER-CASE ;

: SPAN-CASE ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u s" file" RUN-DATA-SITES want ROW-RC
   a u s" owned" RUN-DATA-SITES want ROW-RC ;

: ADDRESS-BUDGET-CASE ( ptr u8 n -- )
   RUN-ADDRESS-CELLS $4B ROW-RC
   s" encoded sections exceed their byte budget" ERR-SAID? ;
: PROBE-ADDRESS-STORAGE ( -- )
   s" rows" RUN-ADDRESS-CELLS 0 ROW-RC s" aot-address-cells: ok" SAID?
   s" reserve-negative" RUN-ADDRESS-CELLS REFUSE-RC ROW-RC
   s" reserve-overflow" RUN-ADDRESS-CELLS REFUSE-RC ROW-RC
   s" reserve-limit" RUN-ADDRESS-CELLS REFUSE-RC ROW-RC
   s" budget-write" ADDRESS-BUDGET-CASE
   s" budget-owned" ADDRESS-BUDGET-CASE
   s" budget-read" ADDRESS-BUDGET-CASE
   s" budget-import" ADDRESS-BUDGET-CASE
   s" budget-merge" ADDRESS-BUDGET-CASE ;

: PROBE-DATA-SITES ( -- )
   s" sites" s" file" RUN-DATA-SITES 0 ROW-RC
   s" aot-data-sites: ok" SAID?
   s" reserve-overflow" s" file" RUN-DATA-SITES REFUSE-RC ROW-RC
   s" relocation site count exceeds the code blob bound" ERR-SAID?
   s" reserve-limit" s" file" RUN-DATA-SITES REFUSE-RC ROW-RC
   s" reserve-negative" s" file" RUN-DATA-SITES REFUSE-RC ROW-RC
   s" bad-order" s" file" RUN-DATA-SITES REFUSE-RC ROW-RC
   s" DATA sites follow CODE sites" ERR-SAID?
   s" shared-overflow" s" owned" RUN-DATA-SITES $4B ROW-RC
   s" CODE sites is larger than the buffer it fills" ERR-SAID?
   s" span-negative" $4B SPAN-CASE
   s" span-min" $4B SPAN-CASE
   s" span-zero" 0 SPAN-CASE
   s" span-cap" 0 SPAN-CASE
   s" span-large" $4B SPAN-CASE
   s" span-negative" s" merge" RUN-DATA-SITES $4B ROW-RC
   s" window DATA span exceeds what this engine can bake" ERR-SAID?
   s" span-zero" s" merge" RUN-DATA-SITES 0 ROW-RC ;

: BODY ( -- )
   PROBE-ROUNDTRIP
   PROBE-ARTIFACT
   PROBE-PRODUCER
   PROBE-CAPTURE-TOOL
   PROBE-ADDRESS-ROWS
   s" owned" RUN-OWNED-CAPTURE
   0 ROW-RC
   s" owned-capture: restored after source release" SAID?
   s" overflow" RUN-OWNED-CAPTURE
   $4B ROW-RC
   s" scalars runs past the payload" ERR-SAID?
   PROBE-DATA-SITES
   PROBE-ADDRESS-STORAGE
   PROBE-PRODUCER-ROWS ;

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

AOT-CHAIN-SUITE:RUN
