\ p2-map-rewind.f - the width-aware recompile puts both relocation maps back.
\
\ WHAT IS BEING TESTED. A definition whose signature carries a multi-cell layout
\ value is compiled TWICE: pass 1 lowers every value as one cell, the checker
\ then certifies the body and hands back the widths it proved, and pass 2
\ (src/habu/habu2.f EM-P2-START) rewinds the code pointer to the colon entry and
\ lowers the same body again knowing them. Both relocation maps are indexed by
\ REGION OFFSET and both are written AT the code pointer as a pass emits - the
\ address-literal map by SNAP-RELOC:MARK-SITE (a `[']`, a `[: ;]`, a data-word
\ address) and by SNAP-RELOC:CARRY-SITE (a chain the inliner copied), the
\ region-to-text call map by EMIT-CEMITBL (a call whose callee is in the engine's
\ loaded __text). Pass 2's stream has different lengths, so pass 1's records
\ describe words pass 2 fills with something else. The rewind therefore owns
\ them: it clears both maps over [entry, pass-1 CP) in the same breath as it puts
\ the two cursors back.
\
\ WHY A STALE BIT IS A DEFECT AND NOT A WASTED BIT. Both maps' readers treat a
\ recorded word as authoritative and refuse the image outright when the bytes
\ underneath are not the recorded shape: SNAP-RELOC:EMIT-ADDRS requires the four
\ move-wide words and exits ADDRMAP-RC otherwise, SNAP-RELOC:EMIT-CALLS requires
\ a BL and exits CALLMAP-RC. The AOT capture refuses the same site one build step
\ earlier (src/habu/aot-capture.f ACAP-UNCLASSIFIED, exit 74) because the address
\ a word that is not a chain decodes to lands in neither of the window's spans.
\ And a stale call bit that happens to land on a word that IS a call is worse
\ than a refusal: the relocation pass would shift the displacement of a call that
\ never needed shifting. Measured on the whole native compiler chain compiled in
\ one window before this landed: 26 stale address records and 17 stale call
\ records, 15 of the latter on words that are not calls.
\
\ HOW THE SUBJECTS ARE BUILT. Each pass-2 subject has a NARROW TWIN: the same
\ body text, the same name length, and a signature whose values are all one cell,
\ so the twin is compiled once and its lowering is exactly what pass 1 emits for
\ the subject. The twin therefore tells the suite two things it would otherwise
\ have to hard-code - where pass 1 put its record, and what a correct record at
\ that site looks like - and every assertion below is stated against it rather
\ than against a written-down offset or an instruction encoding.
\
\ WHAT EACH CASE PROVES.
\
\   1. The twins carry exactly one record each. Without this the rest could pass
\      against a body that compiles no chain and no outside call at all - which
\      is what a future inline limit that swallowed `emit`, or a change to how
\      `[']` is compiled, would silently produce.
\   2. Pass 2 MOVED the record. If the two passes happened to put their records
\      at the same offset, pass 1's stale bit would land on pass 2's live chain
\      and every later case would pass on the broken engine. This is the case
\      that keeps the suite honest, and it is asserted for both maps.
\   3. The subject carries exactly one record, at pass 2's offset.
\   4. The word at PASS 1's offset is not recorded. This is the bug itself,
\      addressed at the exact word.
\   5. The record names a real site: the four words at the subject's recorded
\      address are byte-identical to the four words at its twin's recorded
\      address (the same chain, to the same target, in the same register), and
\      the subject's recorded call word has the twin's opcode and resolves to the
\      same callee. Nothing here recognises a site by searching for a shape - it
\      holds the subject against a site the engine created with no rewind in it.
\   6. A record made BEFORE the pass-2 definition survives it. Clearing the whole
\      window, or clearing from the region base, would discard live sites; the
\      span cleared is the rewound one and nothing below it.

require lib/errors.f
require lib/test.f

package P2-MAP-REWIND-TEST

private

\ ---- the boundaries ----------------------------------------------------------
\ Reading the engine's own relocation bands and its own compiled code needs the
\ same raw casts src/habu/aot-capture.f, test/addrmap-set.f and
\ test/addrmap-inline.f declare. They choose nothing: every address handed to
\ them is computed by the checked words below from `cp@`.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: DATA-A ( -- ptr u8 )
   data-base ;

TRUSTED: REGION-BASE ( -- n )
   dbase@ ;

TRUSTED: CODE-A ( n -- ptr u8 ) ;

\ ---- reading a band ----------------------------------------------------------
\ Read exactly the way habu2.f EMIT-ADDR-SITE and EMIT-CEMITBL write: the region
\ byte offset of the word, its map byte at offset >> 5, and its bit at
\ (offset >> 2) & 7. The two maps differ only in the base the byte index is added
\ to, which is the parameter.
: MAP-BIT@ ( n n -- n ) {: base:n at:n :}
   at REGION-BASE - {: off:n :}
   DATA-A base + off 5 rshift + c@
   off 2 rshift 7 and rshift 1 and ;

: ADDR-MAP ( -- n ) SNAP-RELOC:ADDRMAP-OFF ;
: CALL-MAP ( -- n ) SNAP-RELOC:CALLMAP-OFF ;

variable MARK-N

: MARKS ( n n n -- n ) {: base:n from:n to:n :}     \ recorded words in [from,to)
   0 MARK-N !
   to from ?do
      base i MAP-BIT@ MARK-N @ + MARK-N !
   4 +loop
   MARK-N @ ;

variable FIRST-A

: FIRST-MARK ( n n n -- n ) {: base:n from:n to:n :}  \ first recorded address, or 0
   0 FIRST-A !
   to from ?do
      base i MAP-BIT@ 1 = FIRST-A @ 0= and if i FIRST-A ! then
   4 +loop
   FIRST-A @ ;

\ ---- reading the code --------------------------------------------------------
: W32@ ( n -- n ) {: at:n :}
   at CODE-A c@
   at 1 + CODE-A c@ 8 lshift or
   at 2 + CODE-A c@ 16 lshift or
   at 3 + CODE-A c@ 24 lshift or ;

variable BYTES-NE

: BYTES= ( n n n -- bool ) {: a:n b:n u:n :}
   0 BYTES-NE !
   u 0 ?do
      a i + CODE-A c@  b i + CODE-A c@  <> if 1 BYTES-NE ! then
   loop
   BYTES-NE @ 0= ;

\ The AArch64 branch-with-link encoding, which is what makes a recorded call word
\ answerable: the top six bits name the instruction and the low 26 are a signed
\ instruction count from the word itself. Both are read off the twin as well as
\ off the subject, so the assertions compare two sites and never a written-down
\ opcode.
$3FFFFFF constant IMM26
$2000000 constant IMM26-SGN
16 constant CHAIN-BYTES              \ the four move-wide words of one address chain

: BL-OP ( n -- n ) W32@ 26 rshift ;

: BL-TARGET ( n -- n ) {: at:n :}
   at W32@ IMM26 and {: d:n :}
   d IMM26-SGN >= if d IMM26-SGN 2 * - else d then
   2 lshift at + ;

\ ---- the subjects ------------------------------------------------------------
\ Compiled here, by the engine under test, through the ordinary interpreter.
\ P2M-A/P2M-B and P2M-C/P2M-D are twin pairs: identical body text, identical name
\ length, and signatures that differ only in whether the transported value is one
\ cell or a two-cell layout bundle. The bundle is what makes the second of each
\ pair run pass 2.
SUMTYPE p2m-res 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE

: P2M-SINK ( n -- ) drop ;

variable Q0  variable Q1  variable Q2  variable Q3  variable Q4  variable Q5

cp@ Q0 !
: P2M-Z ( -- ) ['] P2M-SINK P2M-SINK ;                  \ the neighbour below
cp@ Q1 !
: P2M-A ( n -- n n ) dup ['] P2M-SINK P2M-SINK ;        \ narrow twin
cp@ Q2 !
: P2M-B ( p2m-res<n,n> -- p2m-res<n,n> p2m-res<n,n> ) dup ['] P2M-SINK P2M-SINK ;
cp@ Q3 !
: P2M-C ( n -- n n ) dup 65 emit ;                      \ narrow twin
cp@ Q4 !
: P2M-D ( p2m-res<n,n> -- p2m-res<n,n> p2m-res<n,n> ) dup 65 emit ;
cp@ Q5 !

\ ---- 1. the twins are live ---------------------------------------------------
: TEST-TWINS ( -- )
   s" the narrow twin compiles one recorded address chain" T-LABEL
   ADDR-MAP Q1 @ Q2 @ MARKS 1 T=
   s" the narrow twin compiles one recorded call into engine text" T-LABEL
   CALL-MAP Q3 @ Q4 @ MARKS 1 T= ;

\ ---- 2. pass 2 moved both records --------------------------------------------
\ Stated as offsets from each body's own start. Equal offsets would put pass 1's
\ stale bit on pass 2's live site, and every case below would pass either way.
: A-OFF ( -- n ) ADDR-MAP Q1 @ Q2 @ FIRST-MARK Q1 @ - ;
: B-OFF ( -- n ) ADDR-MAP Q2 @ Q3 @ FIRST-MARK Q2 @ - ;
: C-OFF ( -- n ) CALL-MAP Q3 @ Q4 @ FIRST-MARK Q3 @ - ;
: D-OFF ( -- n ) CALL-MAP Q4 @ Q5 @ FIRST-MARK Q4 @ - ;

: TEST-MOVED ( -- )
   s" the width-aware pass puts the chain at a different offset" T-LABEL
   B-OFF A-OFF <> TTRUE
   s" and the call site at a different offset" T-LABEL
   D-OFF C-OFF <> TTRUE ;

\ ---- 3. one record each, after pass 2 ----------------------------------------
: TEST-ONE-EACH ( -- )
   s" the pass-2 body carries exactly one recorded chain" T-LABEL
   ADDR-MAP Q2 @ Q3 @ MARKS 1 T=
   s" the pass-2 body carries exactly one recorded call" T-LABEL
   CALL-MAP Q4 @ Q5 @ MARKS 1 T= ;

\ ---- 4. pass 1's word is not recorded ----------------------------------------
\ The twin's offset is where pass 1 put its record, because the twin IS pass 1's
\ lowering of the same body. That word now holds part of the width-aware stream.
: TEST-NO-STALE ( -- )
   s" the word pass 1 recorded its chain in is not recorded" T-LABEL
   ADDR-MAP Q2 @ A-OFF + MAP-BIT@ 0 T=
   s" the word pass 1 recorded its call in is not recorded" T-LABEL
   CALL-MAP Q4 @ C-OFF + MAP-BIT@ 0 T= ;

\ ---- 5. the surviving record names a real site -------------------------------
\ Both bodies build the same chain to the same target in the same register, so
\ the four words are byte-identical; both bodies call the same callee, so the two
\ recorded call words share an opcode and resolve to one address.
: TEST-REAL-SITE ( -- )
   s" the recorded chain is byte-identical to the twin's chain" T-LABEL
   ADDR-MAP Q2 @ Q3 @ FIRST-MARK
   ADDR-MAP Q1 @ Q2 @ FIRST-MARK
   CHAIN-BYTES BYTES= TTRUE
   s" the recorded call word has the twin's instruction" T-LABEL
   CALL-MAP Q4 @ Q5 @ FIRST-MARK BL-OP
   CALL-MAP Q3 @ Q4 @ FIRST-MARK BL-OP T=
   s" and reaches the same callee" T-LABEL
   CALL-MAP Q4 @ Q5 @ FIRST-MARK BL-TARGET
   CALL-MAP Q3 @ Q4 @ FIRST-MARK BL-TARGET T= ;

\ ---- 6. the clear stops at the rewound span ----------------------------------
\ P2M-Z was compiled before either pass-2 definition and its chain is still
\ recorded, at the same word. A clear that took the whole map, or started at the
\ region base, would have taken this one with it.
: TEST-NEIGHBOUR ( -- )
   s" the record made before the pass-2 definitions is still there" T-LABEL
   ADDR-MAP Q0 @ Q1 @ MARKS 1 T=
   s" and it still names the chain it was made for" T-LABEL
   ADDR-MAP Q0 @ Q1 @ FIRST-MARK
   ADDR-MAP Q1 @ Q2 @ FIRST-MARK
   CHAIN-BYTES BYTES= TTRUE ;

public

: RUN ( -- )
   T-RESET
   TEST-TWINS
   TEST-MOVED
   TEST-ONE-EACH
   TEST-NO-STALE
   TEST-REAL-SITE
   TEST-NEIGHBOUR
   T-REPORT
   s" p2-map-rewind: ok" type cr ;

;package

P2-MAP-REWIND-TEST:RUN
