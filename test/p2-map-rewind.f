\ Width-aware JIT recompilation rewinds both relocation maps with the code.
\ A narrow twin locates each pass-1 site without hardcoded instruction offsets.
\ The wide subject must move the site, clear the old mark, preserve its target
\ and leave its earlier neighbour unchanged. Calls are identified by the actual
\ callee: the narrow `dup` is a call, while the wide `dup` expands at its site.

require lib/errors.f
require lib/test.f
require src/compiler/native/codewalk.f

package P2-MAP-REWIND-TEST

private

\ ---- the boundaries ----------------------------------------------------------
\ Reading the engine's own relocation bands and its own compiled code needs the
\ same raw casts src/habu/aot-capture.f, test/addrmap-set.f and
\ test/addrmap-call.f declare. They choose nothing: every address handed to
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

\ The engine's stack guards (src/compiler/native/codewalk.f) each call the
\ engine, and the call map records those BLs too, because relocation has to
\ move them. A body's own recorded calls are the marks outside its guards.
: OWN-MARKS ( n n n -- n ) {: base:n from:n to:n :}
   0 MARK-N !
   to from ?do
      from to from - i from - 4 / NWALK:SPAN-GUARDED? 0= if
         base i MAP-BIT@ MARK-N @ + MARK-N !
      then
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

\ Find the marked call to a specific word, not whichever call happens first.
: MARKED-CALL ( n n n -- n ) {: from:n to:n target:n :}
   0 FIRST-A !
   to from ?do
      CALL-MAP i MAP-BIT@ 1 = if
         i BL-TARGET target = if i FIRST-A ! then
      then
   4 +loop
   FIRST-A @ 0= if s" missing marked call target" 76 die then
   FIRST-A @ ;

: EMIT-CALL ( n n -- n ) s" emit" 0 search-wl MARKED-CALL ;

\ How many marked calls in [from,to) reach one target.
: MARKED-CALLS ( n n n -- n ) {: from:n to:n target:n :}
   0 MARK-N !
   to from ?do
      CALL-MAP i MAP-BIT@ 1 = if
         i BL-TARGET target = if MARK-N @ 1 + MARK-N ! then
      then
   4 +loop
   MARK-N @ ;

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

\ The sink takes the target's execution token as a typed quotation; the
\ chains the passes record all name P2M-TARGET, the same code address.
: P2M-TARGET ( n -- ) drop ;
: P2M-SINK ( [ n -- ] -- ) drop ;

variable Q0  variable Q1  variable Q2  variable Q3  variable Q4  variable Q5

cp@ Q0 !
: P2M-Z ( -- ) ['] P2M-TARGET P2M-SINK ;                  \ the neighbour below
cp@ Q1 !
: P2M-A ( n -- n n ) dup ['] P2M-TARGET P2M-SINK ;        \ narrow twin
cp@ Q2 !
: P2M-B ( p2m-res<n,n> -- p2m-res<n,n> p2m-res<n,n> ) dup ['] P2M-TARGET P2M-SINK ;
cp@ Q3 !
: P2M-C ( n -- n n ) dup 65 emit ;                        \ narrow twin
cp@ Q4 !
: P2M-D ( p2m-res<n,n> -- p2m-res<n,n> p2m-res<n,n> ) dup 65 emit ;
cp@ Q5 !

\ ---- 1. the twins are live ---------------------------------------------------
: TEST-TWINS ( -- )
   s" the narrow twin compiles one recorded address chain" T-LABEL
   ADDR-MAP Q1 @ Q2 @ MARKS 1 T=
   s" the narrow twin records both dup and emit calls" T-LABEL
   CALL-MAP Q3 @ Q4 @ OWN-MARKS 2 T=
   Q3 @ Q4 @ s" dup" 0 search-wl MARKED-CALL drop
   Q3 @ Q4 @ EMIT-CALL drop ;

\ ---- 2. pass 2 moved both records --------------------------------------------
\ Stated as offsets from each body's own start. Equal offsets would put pass 1's
\ stale bit on pass 2's live site, and every case below would pass either way.
: A-OFF ( -- n ) ADDR-MAP Q1 @ Q2 @ FIRST-MARK Q1 @ - ;
: B-OFF ( -- n ) ADDR-MAP Q2 @ Q3 @ FIRST-MARK Q2 @ - ;
: DUP-OFF ( -- n ) Q3 @ Q4 @ s" dup" 0 search-wl MARKED-CALL Q3 @ - ;
: C-OFF ( -- n ) Q3 @ Q4 @ EMIT-CALL Q3 @ - ;
: D-OFF ( -- n ) Q4 @ Q5 @ EMIT-CALL Q4 @ - ;

: TEST-MOVED ( -- )
   s" the width-aware pass puts the chain at a different offset" T-LABEL
   B-OFF A-OFF <> TTRUE
   s" and the call site at a different offset" T-LABEL
   D-OFF C-OFF <> TTRUE ;

\ ---- 3. one record each, after pass 2 ----------------------------------------
: TEST-ONE-EACH ( -- )
   s" the pass-2 body carries exactly one recorded chain" T-LABEL
   ADDR-MAP Q2 @ Q3 @ MARKS 1 T=
   s" the pass-2 body carries exactly one recorded call of its own" T-LABEL
   CALL-MAP Q4 @ Q5 @ OWN-MARKS 1 T=
   s" and every other recorded call in it is a guard's" T-LABEL
   CALL-MAP Q4 @ Q5 @ MARKS  Q4 @ Q5 @ Q4 @ - NWALK:SPAN-GUARDS 1 +  T= ;

\ ---- 4. pass 1's word is not recorded ----------------------------------------
\ The twin's offset is where pass 1 put its record, because the twin IS pass 1's
\ lowering of the same body. That word now holds part of the width-aware stream.
: TEST-NO-STALE ( -- )
   s" the word pass 1 recorded its chain in is not recorded" T-LABEL
   ADDR-MAP Q2 @ A-OFF + MAP-BIT@ 0 T=
   \ The twin's offset may now hold one of the pass-2 body's guard calls, so
   \ a stale record is told apart by its target: pass 1's emit call is the one
   \ site pass 2 moved, and its dup call has no site at all after widening.
   s" the word pass 1 recorded its call in is not recorded" T-LABEL
   Q4 @ Q5 @ s" emit" 0 search-wl MARKED-CALLS 1 T=
   s" the narrow dup call leaves no stale mark after widening" T-LABEL
   Q4 @ Q5 @ s" dup" 0 search-wl MARKED-CALLS 0 T= ;

\ ---- 5. the surviving record names a real site -------------------------------
\ Both bodies build the same chain to the same target in the same register, so
\ the four words are byte-identical; both bodies call the same callee, so the two
\ recorded call words share an opcode and resolve to one address.
: TEST-REAL-SITE ( -- )
   s" the recorded chain is byte-identical to the twin's chain" T-LABEL
   ADDR-MAP Q2 @ Q3 @ FIRST-MARK
   ADDR-MAP Q1 @ Q2 @ FIRST-MARK
   CHAIN-BYTES BYTES= TTRUE
   s" each recorded call has the AArch64 BL opcode" T-LABEL
   Q4 @ Q5 @ EMIT-CALL BL-OP $25 T=
   Q3 @ Q4 @ EMIT-CALL BL-OP $25 T=
   s" and reaches the same callee" T-LABEL
   Q4 @ Q5 @ EMIT-CALL BL-TARGET
   Q3 @ Q4 @ EMIT-CALL BL-TARGET T= ;

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
