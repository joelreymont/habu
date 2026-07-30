\ ir-pass.f - checked pass-result and witness-header tests.
\
\ Proves the two halves of design section 6.7 that src/compiler/ir/pass.f owns:
\ a witness is refused before its pass-specific payload is read if any binding
\ differs, and a validated pass result is consumed exactly once.
\
\ HOW THE FIXTURE WORKS. One context holds two frozen modules built through the
\ real IR-BUILD API by test/compiler/ir-module-fixture.f - the module the
\ canonicalizer, renderer and diff tests all use - and their canonical tables.
\ The "input" module is the fixture as it stands; the "output" module is the same
\ fixture with its tagged operation carrying the other integer, which is the
\ smallest semantic change that module can carry, so the two mean different
\ things and have different canonical frames. A payload and a metrics record are
\ byte spans of known content, and one more span is the scratch the validator
\ re-derives module frames in. PREPARE takes all of them once per context and
\ STAGE presents them, so a case can stage the same truth as often as it likes
\ without building or allocating again.
\
\ WHY EVERY REFUSAL IS CAUGHT INSIDE ITS CONTEXT. A throw that escapes
\ IR-CTX:WITH-CONTEXT never reaches the lines of IR-CTX:CTX-ENTER that retire the
\ context's registry slot, so the abandoned context still answers SERIAL-LIVE?
\ and every arena it owned stays unswept for the rest of the process. The module
\ fixture costs seventeen arenas, so two escaping refusals would exhaust the
\ sixty-four-slot arena registry and later cases would fail with
\ E-IR-ARENA-SLOTS instead of the refusal they came to measure. Each group below
\ therefore opens one context, prepares once, and asserts its refusals with
\ TTHROWSQ from inside, so no throw ever crosses a context boundary. The leak is
\ real and belongs to IR-CTX rather than to this stage; it is recorded as its own
\ dot.
\
\ WHY THE SLOT NUMBERS ARE RESTATED HERE. The witness layout below mirrors the
\ private layout of src/compiler/ir/pass.f rather than asking the package for its
\ own offsets. A corrupt-binding case has to flip the byte position a field
\ actually occupies; if it asked the package where that field lives, a package
\ that wrote the field in the wrong place would still pass, because the case
\ would corrupt whatever the package pointed at. Restating the map means a drift
\ between the two turns cases red.
\
\ WHAT THE CORRUPT-BINDING CASES PROVE. There is one case per bound header field,
\ fifteen in all: magic, the two format-version slots, the pass identity and its
\ two version slots, the input and output module digests, the target contract and
\ numeric policy digests, the schema digest, and the payload and metrics length
\ and digest. Each writes a truthful witness, flips exactly one field against an
\ unchanged payload, and requires that field's own named refusal. Two more cases
\ leave the header alone and change the payload bytes and the metrics bytes, so
\ the binding is shown to cover content and not only the length slot, and one
\ swaps the two module digests, which no single-field flip would catch.
\
\ THE ORDER CLAIM IS BY CONSTRUCTION, NOT BY PROBE. Section 6.7 requires the
\ refusal to happen before the pass-specific payload is read, and IR-PASS never
\ interprets a payload byte at all: it measures a length and a SHA-256 and
\ nothing else. Interpreting the payload needs PAYLOAD-CK, PAYLOAD-CK needs a
\ live result handle, and VALIDATE mints that handle after the last comparison
\ agreed. So the cases below prove the order by proving the capability: a refused
\ validation hands back no handle, and the handles a caller can still present are
\ refused by the name of the consumption that already happened.

require lib/test.f
require src/compiler/digest.f
require src/compiler/ir/build.f
require src/compiler/ir/canon.f
require src/compiler/ir/encode.f
require src/compiler/ir/pass.f
require test/compiler/ir-module-fixture.f

package IR-PASS-TEST
private

\ ---- the witness layout this file corrupts -----------------------------------
0 constant WS-MAGIC
1 constant WS-MAJOR
2 constant WS-MINOR
3 constant WS-IDENT                  \ the pass name's digest
7 constant WS-PMAJOR
8 constant WS-PMINOR
9 constant WS-INPUT
13 constant WS-OUTPUT
17 constant WS-TARGET
21 constant WS-POLICY
25 constant WS-SCHEMA
29 constant WS-PAYLEN
30 constant WS-PAYDIG
34 constant WS-METLEN
35 constant WS-METDIG
39 constant WIT-SLOTS

WIT-SLOTS CDIGEST:SLOT-BYTES * constant WIT-BYTES
WIT-BYTES CDIGEST:SLOT-BYTES + constant WIT-ROOM

\ ---- what the fixture presents -----------------------------------------------
64 constant PAY-BYTES
32 constant MET-BYTES
7 constant PAY-SEED
19 constant MET-SEED
131 constant OTHER-SEED              \ bytes the witness did not bind
3 constant PVER-MAJOR
1 constant PVER-MINOR
8 constant RESULT-CEIL               \ live and consumed results one context holds

\ ---- the artifacts one context holds -----------------------------------------
1 TYPED-BUFFER IN-M IR-BUILD:module
1 TYPED-BUFFER IN-T IR-CANON:table
1 TYPED-BUFFER OUT-M IR-BUILD:module
1 TYPED-BUFFER OUT-T IR-CANON:table
1 TYPED-BUFFER CUR-CTX IR-CTX:ctx
1 TYPED-BUFFER HELD IR-PASS:result

0 constant SP-SCR
1 constant SP-PAY
2 constant SP-MET
3 constant SP-WIT
4 constant SPAN#

here CELL 1- and CELL swap - CELL 1- and allot
create SPANS SPAN# cells allot
create SPAN-LENS SPAN# cells allot
variable AT-CELL                     \ which header slot the running case flips

: SPAN-FIELD ( n -- ptr ptr u8 )
   cells SPANS + 0 ptr-field ;

: SPAN@ ( n -- ptr u8 )
   SPAN-FIELD @ ;

: SPAN! ( ptr u8 n -- )
   SPAN-FIELD ! ;

: SPAN-LEN@ ( n -- n )
   cells SPAN-LENS + @ ;

: SPAN-LEN! ( n n -- )
   cells SPAN-LENS + ! ;

: TAKE-SPAN ( IR-CTX:ctx n n -- )
   {: c:IR-CTX:ctx len:n k:n :}
   c len IR-CTX:SCRATCH-TAKE {: p room:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p k SPAN!
   room k SPAN-LEN! ;

: FILL-SPAN ( n n -- )
   {: k:n seed:n :}
   k SPAN@ {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   k SPAN-LEN@ 0 ?do
      seed i + $FF and  p i + c!
   loop ;

: CUR ( -- IR-CTX:ctx )
   0 CUR-CTX @ ;

\ ---- building the two modules ------------------------------------------------
: IN-BUILD ( IR-CTX:ctx -- IR-BUILD:module )
   IR-FIXTURE:RESET
   0 0 IR-FIXTURE:MODULE-OF ;

: OUT-BUILD ( IR-CTX:ctx -- IR-BUILD:module )
   IR-FIXTURE:RESET
   IR-FIXTURE:CHANGED-ATTR!
   0 0 IR-FIXTURE:MODULE-OF ;

: PREPARE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 CUR-CTX !
   c IN-BUILD {: mi:IR-BUILD:module :}
   c OUT-BUILD {: mo:IR-BUILD:module :}
   IR-FIXTURE:RESET
   c mi IR-CANON:CANON {: ti:IR-CANON:table :}
   c mo IR-CANON:CANON {: to:IR-CANON:table :}
   mi 0 IN-M !
   ti 0 IN-T !
   mo 0 OUT-M !
   to 0 OUT-T !
   c  ti IR-ENCODE:SIZE  to IR-ENCODE:SIZE max  SP-SCR TAKE-SPAN
   c PAY-BYTES SP-PAY TAKE-SPAN
   c MET-BYTES SP-MET TAKE-SPAN
   c WIT-ROOM SP-WIT TAKE-SPAN
   SP-PAY PAY-SEED FILL-SPAN
   SP-MET MET-SEED FILL-SPAN ;

\ ---- staging the truth -------------------------------------------------------
\ Everything the consumer knows without believing anything a producer wrote.
: STAGE ( -- )
   IR-PASS:CHECK-BEGIN
   CUR IR-PASS:CHECK-CTX
   s" const-fold" IR-PASS:CHECK-PASS
   PVER-MAJOR PVER-MINOR IR-PASS:CHECK-VERSION
   0 IN-M @  0 IN-T @  IR-PASS:CHECK-INPUT
   0 OUT-M @ 0 OUT-T @ IR-PASS:CHECK-OUTPUT
   SP-PAY SPAN@ SP-PAY SPAN-LEN@ IR-PASS:CHECK-PAYLOAD
   SP-MET SPAN@ SP-MET SPAN-LEN@ IR-PASS:CHECK-METRICS
   SP-SCR SPAN@ SP-SCR SPAN-LEN@ IR-PASS:CHECK-SCRATCH ;

: WIT ( -- ptr u8 n )
   STAGE
   SP-WIT SPAN@ SP-WIT SPAN-LEN@ IR-PASS:WITNESS {: wrote:n :}
   SP-WIT SPAN@ wrote ;

: RESULT-OF ( -- IR-PASS:result )
   WIT {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   STAGE
   p n IR-PASS:VALIDATE ;

: FRAME-DIG ( IR-BUILD:module IR-CANON:table -- CDIGEST:digest )
   {: m:IR-BUILD:module t:IR-CANON:table :}
   CUR m t  SP-SCR SPAN@ SP-SCR SPAN-LEN@  IR-ENCODE:ENCODE {: wrote:n :}
   SP-SCR SPAN@ wrote IR-ENCODE:DIGEST ;

\ ---- corrupting one field ----------------------------------------------------
: FLIP ( ptr u8 n -- )
   {: p at:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p at CDIGEST:SLOT@ 1 xor  p at CDIGEST:SLOT! ;

: SLOT-COPY ( ptr u8 n n -- )
   {: p from:n to:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   4 0 ?do
      p from i + CDIGEST:SLOT@  p to i + CDIGEST:SLOT!
   loop ;

\ One truthful witness with exactly one header field flipped, validated against
\ the unchanged artifacts.
: CORRUPT-RUN ( -- )
   WIT {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p AT-CELL @ FLIP
   STAGE
   p n IR-PASS:VALIDATE drop ;

\ Label, the header slot to flip, and the refusal that slot must produce.
: CORRUPT-CASE ( ptr u8 n n n -- )
   {: at:n code:n :}
   T-LABEL
   at AT-CELL !
   [: CORRUPT-RUN ;] code TTHROWSQ ;

\ ---- a witness that agrees with the artifacts --------------------------------
\ The published module is the output module: its canonical frame digest is the
\ output module's and not the input module's.
: ACCEPT-BODY ( -- bool bool bool )
   RESULT-OF {: r:IR-PASS:result :}
   r IR-PASS:LIVE?
   r IR-PASS:ACCEPT {: m:IR-BUILD:module :}
   m 0 OUT-T @ FRAME-DIG  0 OUT-M @ 0 OUT-T @ FRAME-DIG  CDIGEST-DIGEST:EQ
   m 0 OUT-T @ FRAME-DIG  0 IN-M @  0 IN-T @  FRAME-DIG  CDIGEST-DIGEST:EQ ;

: ACCEPT-CASE ( -- )
   s" a witness that agrees with the artifacts publishes the output module" T-LABEL
   ACCEPT-BODY
   TFALSE TTRUE TTRUE ;

\ The witness is a fixed number of slots and the writer says how many it wrote.
: SIZE-CASE ( -- )
   s" a witness is exactly the slots the layout states" T-LABEL
   WIT nip {: n:n :}
   n WIT-BYTES T=
   IR-PASS:WITNESS-BYTES WIT-BYTES T= ;

\ A live result reads back the payload and the metrics it bound.
: BOUND-CASE ( -- )
   s" a validated result reads back the payload and metrics it bound" T-LABEL
   RESULT-OF {: r:IR-PASS:result :}
   r  SP-PAY SPAN@ SP-PAY SPAN-LEN@  IR-PASS:PAYLOAD-CK
   r  SP-MET SPAN@ SP-MET SPAN-LEN@  IR-PASS:METRICS-CK
   r IR-PASS:LIVE? TTRUE ;

\ The same pass over two independently built module pairs states one witness. A
\ module serial, a context serial or an allocation address that leaked into a
\ witness would make the two differ, which is the property the canonical frame's
\ own cross-context case pins for a module's bytes.
: WIT-DIG-BODY ( IR-CTX:ctx -- CDIGEST:digest )
   PREPARE
   WIT CDIGEST:COMPUTE ;

: CROSS-CASE ( -- )
   s" one pass over two independently built module pairs states one witness" T-LABEL
   IR-FIXTURE:BND [: WIT-DIG-BODY ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: WIT-DIG-BODY ;] IR-CTX:WITH-CONTEXT
   CDIGEST-DIGEST:EQ TTRUE ;

\ ---- the payload and metrics bindings cover content ---------------------------
: PAY-BAD-RUN ( -- )
   WIT {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   SP-PAY OTHER-SEED FILL-SPAN
   STAGE
   p n IR-PASS:VALIDATE drop ;

: MET-BAD-RUN ( -- )
   WIT {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   SP-MET OTHER-SEED FILL-SPAN
   STAGE
   p n IR-PASS:VALIDATE drop ;

\ Neither module digest is wrong on its own; they are each other's.
: SWAP-DIG-RUN ( -- )
   WIT {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-OUTPUT WS-INPUT SLOT-COPY
   STAGE
   p n IR-PASS:VALIDATE drop ;

\ ---- framing -----------------------------------------------------------------
: SHORT-RUN ( -- )
   WIT {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   STAGE
   p n CDIGEST:SLOT-BYTES - IR-PASS:VALIDATE drop ;

: TRAIL-RUN ( -- )
   WIT {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   STAGE
   p n CDIGEST:SLOT-BYTES + IR-PASS:VALIDATE drop ;

: ROOM-RUN ( -- )
   STAGE
   SP-WIT SPAN@ WIT-BYTES CDIGEST:SLOT-BYTES - IR-PASS:WITNESS drop ;

\ ---- the stage ---------------------------------------------------------------
\ A check run before every field arrived. The check closes the stage before it
\ refuses, so the next case begins clean.
: FIELD-RUN ( -- )
   IR-PASS:CHECK-BEGIN
   CUR IR-PASS:CHECK-CTX
   SP-WIT SPAN@ SP-WIT SPAN-LEN@ IR-PASS:WITNESS drop ;

\ The stage a refused begin or a repeated field leaves open is consumed by the
\ next check, which closes it and answers E-IR-PASS-FIELD, so a misuse cannot
\ poison the checks that follow it.
: DRAIN-RUN ( -- )
   SP-WIT SPAN@ SP-WIT SPAN-LEN@ IR-PASS:WITNESS drop ;

: TWICE-BEGIN-RUN ( -- )
   IR-PASS:CHECK-BEGIN
   IR-PASS:CHECK-BEGIN ;

: TWICE-FIELD-RUN ( -- )
   IR-PASS:CHECK-BEGIN
   CUR IR-PASS:CHECK-CTX
   CUR IR-PASS:CHECK-CTX ;

\ ---- consuming a result exactly once -----------------------------------------
: TWICE-ACCEPT-RUN ( -- )
   RESULT-OF {: r:IR-PASS:result :}
   r IR-PASS:ACCEPT drop
   r IR-PASS:ACCEPT drop ;

: ACCEPT-RELEASE-RUN ( -- )
   RESULT-OF {: r:IR-PASS:result :}
   r IR-PASS:ACCEPT drop
   r IR-PASS:RELEASE ;

: RELEASE-ACCEPT-RUN ( -- )
   RESULT-OF {: r:IR-PASS:result :}
   r IR-PASS:RELEASE
   r IR-PASS:ACCEPT drop ;

: TWICE-RELEASE-RUN ( -- )
   RESULT-OF {: r:IR-PASS:result :}
   r IR-PASS:RELEASE
   r IR-PASS:RELEASE ;

\ Reading the payload needs a live handle, so a consumed result cannot reach it.
: RELEASE-PAYLOAD-RUN ( -- )
   RESULT-OF {: r:IR-PASS:result :}
   r IR-PASS:RELEASE
   r  SP-PAY SPAN@ SP-PAY SPAN-LEN@  IR-PASS:PAYLOAD-CK ;

: ACCEPT-PAYLOAD-RUN ( -- )
   RESULT-OF {: r:IR-PASS:result :}
   r IR-PASS:ACCEPT drop
   r  SP-PAY SPAN@ SP-PAY SPAN-LEN@  IR-PASS:PAYLOAD-CK ;

: PAYLOAD-OTHER-RUN ( -- )
   RESULT-OF {: r:IR-PASS:result :}
   r  SP-MET SPAN@ SP-MET SPAN-LEN@  IR-PASS:PAYLOAD-CK ;

: METRICS-OTHER-RUN ( -- )
   RESULT-OF {: r:IR-PASS:result :}
   r  SP-PAY SPAN@ SP-PAY SPAN-LEN@  IR-PASS:METRICS-CK ;

\ ---- the registry ------------------------------------------------------------
: FILL-SLOTS ( n -- )
   {: k:n :}
   k 0 ?do
      RESULT-OF drop
   loop ;

\ One validation that must be refused, run against artifacts already prepared,
\ so it costs nothing but the refusal.
: BAD-VALIDATE ( -- )
   WIT {: p n:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p WS-MAGIC FLIP
   STAGE
   p n IR-PASS:VALIDATE drop ;

\ A result does not outlive the context that owns it.
: STALE-MAKE ( IR-CTX:ctx -- )
   PREPARE
   RESULT-OF 0 HELD ! ;

: STALE-RUN ( -- )
   0 HELD @ IR-PASS:ACCEPT drop ;

\ ---- the refusals ------------------------------------------------------------
: FORMAT-CASES ( -- )
   s" a witness whose leading slot is not the magic rejects"
   WS-MAGIC E-IR-PASS-STATE CORRUPT-CASE
   s" a witness of another format major version rejects"
   WS-MAJOR E-IR-PASS-VERSION CORRUPT-CASE
   s" a witness of a later format minor version rejects"
   WS-MINOR E-IR-PASS-VERSION CORRUPT-CASE ;

: IDENT-CASES ( -- )
   s" a witness naming another pass rejects"
   WS-IDENT E-IR-PASS-PASS CORRUPT-CASE
   s" a witness naming another pass major version rejects"
   WS-PMAJOR E-IR-PASS-PASS CORRUPT-CASE
   s" a witness naming another pass minor version rejects"
   WS-PMINOR E-IR-PASS-PASS CORRUPT-CASE ;

: MODULE-CASES ( -- )
   s" a witness whose input-module digest is not the input module's rejects"
   WS-INPUT E-IR-PASS-INPUT CORRUPT-CASE
   s" a witness whose output-module digest is not the output module's rejects"
   WS-OUTPUT E-IR-PASS-OUTPUT CORRUPT-CASE
   s" a witness whose schema digest is not the output module's rejects"
   WS-SCHEMA E-IR-PASS-SCHEMA CORRUPT-CASE ;

: BINDING-CASES ( -- )
   s" a witness of another target contract rejects"
   WS-TARGET E-IR-PASS-TARGET CORRUPT-CASE
   s" a witness of another numeric policy rejects"
   WS-POLICY E-IR-PASS-POLICY CORRUPT-CASE ;

: SPAN-CASES ( -- )
   s" a witness stating another payload length rejects"
   WS-PAYLEN E-IR-PASS-PAYLOAD CORRUPT-CASE
   s" a witness stating another payload digest rejects"
   WS-PAYDIG E-IR-PASS-PAYLOAD CORRUPT-CASE
   s" a witness stating another metrics length rejects"
   WS-METLEN E-IR-PASS-METRICS CORRUPT-CASE
   s" a witness stating another metrics digest rejects"
   WS-METDIG E-IR-PASS-METRICS CORRUPT-CASE ;

: CONTENT-CASES ( -- )
   s" a payload whose bytes the witness did not bind rejects" T-LABEL
   [: PAY-BAD-RUN ;] E-IR-PASS-PAYLOAD TTHROWSQ
   s" a metrics record whose bytes the witness did not bind rejects" T-LABEL
   [: MET-BAD-RUN ;] E-IR-PASS-METRICS TTHROWSQ
   s" a witness whose two module digests are each other's rejects" T-LABEL
   [: SWAP-DIG-RUN ;] E-IR-PASS-INPUT TTHROWSQ ;

: FRAME-CASES ( -- )
   s" bytes too short to hold a witness reject" T-LABEL
   [: SHORT-RUN ;] E-IR-PASS-STATE TTHROWSQ
   s" bytes trailing the witness reject" T-LABEL
   [: TRAIL-RUN ;] E-IR-PASS-FRAME TTHROWSQ
   s" a destination shorter than one witness rejects" T-LABEL
   [: ROOM-RUN ;] E-IR-PASS-ROOM TTHROWSQ ;

: STAGE-CASES ( -- )
   s" a check run before every field arrived rejects" T-LABEL
   [: FIELD-RUN ;] E-IR-PASS-FIELD TTHROWSQ
   s" a check begun while one is open rejects" T-LABEL
   [: TWICE-BEGIN-RUN ;] E-IR-PASS-STAGE TTHROWSQ
   s" the stage a refused begin left open is consumed by the next check" T-LABEL
   [: DRAIN-RUN ;] E-IR-PASS-FIELD TTHROWSQ
   s" a staged field declared twice rejects" T-LABEL
   [: TWICE-FIELD-RUN ;] E-IR-PASS-STAGE TTHROWSQ
   s" the stage a repeated field left open is consumed by the next check" T-LABEL
   [: DRAIN-RUN ;] E-IR-PASS-FIELD TTHROWSQ ;

\ Four results, which is inside the ceiling one context holds.
: CONSUME-CASES ( -- )
   s" a second accept of one result rejects" T-LABEL
   [: TWICE-ACCEPT-RUN ;] E-IR-PASS-CONSUMED TTHROWSQ
   s" releasing a result already accepted rejects" T-LABEL
   [: ACCEPT-RELEASE-RUN ;] E-IR-PASS-CONSUMED TTHROWSQ
   s" accepting a result already released rejects" T-LABEL
   [: RELEASE-ACCEPT-RUN ;] E-IR-PASS-RELEASED TTHROWSQ
   s" a second release of one result rejects" T-LABEL
   [: TWICE-RELEASE-RUN ;] E-IR-PASS-RELEASED TTHROWSQ ;

: READBACK-CASES ( -- )
   s" reading the payload of a released result rejects" T-LABEL
   [: RELEASE-PAYLOAD-RUN ;] E-IR-PASS-RELEASED TTHROWSQ
   s" reading the payload of an accepted result rejects" T-LABEL
   [: ACCEPT-PAYLOAD-RUN ;] E-IR-PASS-CONSUMED TTHROWSQ
   s" a span that is not the bound payload rejects" T-LABEL
   [: PAYLOAD-OTHER-RUN ;] E-IR-PASS-PAYLOAD TTHROWSQ
   s" a span that is not the bound metrics record rejects" T-LABEL
   [: METRICS-OTHER-RUN ;] E-IR-PASS-METRICS TTHROWSQ ;

\ Fill the registry one short, refuse one validation, and require the last slot
\ to still be there: a refused validation consumes no ownership. Then the next
\ mint has nowhere to go, which is the committed ceiling.
: REGISTRY-CASES ( -- )
   RESULT-CEIL 1- FILL-SLOTS
   s" a witness refused on its magic throws by name" T-LABEL
   [: BAD-VALIDATE ;] E-IR-PASS-STATE TTHROWSQ
   s" the slot a refused validation did not take is still free" T-LABEL
   RESULT-OF IR-PASS:LIVE? TTRUE
   s" a result past the committed ceiling has no registry slot" T-LABEL
   [: RESULT-OF drop ;] E-IR-PASS-SLOTS TTHROWSQ ;

public

\ Each group opens one context and prepares once, so no throw crosses a context
\ boundary and no group mints more results than one context holds.
: AGREEMENT-GROUP ( IR-CTX:ctx -- )
   PREPARE
   ACCEPT-CASE
   SIZE-CASE
   BOUND-CASE ;

: HEADER-GROUP ( IR-CTX:ctx -- )
   PREPARE
   FORMAT-CASES
   IDENT-CASES
   MODULE-CASES
   BINDING-CASES
   SPAN-CASES
   CONTENT-CASES ;

: FRAMING-GROUP ( IR-CTX:ctx -- )
   PREPARE
   FRAME-CASES
   STAGE-CASES ;

: CONSUME-GROUP ( IR-CTX:ctx -- )
   PREPARE
   CONSUME-CASES ;

: READBACK-GROUP ( IR-CTX:ctx -- )
   PREPARE
   READBACK-CASES ;

: REGISTRY-GROUP ( IR-CTX:ctx -- )
   PREPARE
   REGISTRY-CASES ;

: STALE-CASE ( -- )
   s" a result whose context has torn down rejects" T-LABEL
   IR-FIXTURE:BND [: STALE-MAKE ;] IR-CTX:WITH-CONTEXT
   [: STALE-RUN ;] E-IR-PASS-STALE TTHROWSQ ;

: RUN ( -- )
   T-RESET
   IR-FIXTURE:BND [: AGREEMENT-GROUP ;] IR-CTX:WITH-CONTEXT
   CROSS-CASE
   IR-FIXTURE:BND [: HEADER-GROUP ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: FRAMING-GROUP ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: CONSUME-GROUP ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: READBACK-GROUP ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: REGISTRY-GROUP ;] IR-CTX:WITH-CONTEXT
   STALE-CASE
   T-REPORT ;

;package

IR-PASS-TEST:RUN
