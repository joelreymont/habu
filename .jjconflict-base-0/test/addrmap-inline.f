\ addrmap-inline.f - the inliner carries an address chain's record with the chain.
\
\ WHAT IS BEING TESTED, AND WHY IT NEEDS ITS OWN SUITE. The address-literal map
\ (src/habu/layout.f SNAP-RELOC:ADDRMAP-OFF) records the region word each
\ four-instruction MOVZ/MOVK address chain starts in, and it is written where the
\ chain is CREATED: src/habu/habu2.f C-DATA-ADDR, C-DATA-ADDR-RAW and C-CODE-ADDR
\ each call SNAP-RELOC:MARK-SITE before they emit. test/addrmap-set.f owns the
\ primitive that writes one bit; this suite owns a different question, which is
\ what happens to a chain that is REPRODUCED rather than created.
\
\ The engine's compile-mode call emitter (habu2.f C-CALL) does reproduce one. A
\ callee short enough to inline is copied word for word into the caller instead of
\ being called, and the copy is a second chain at a second region offset that
\ nothing had recorded. The map is keyed by offset, so the callee's own record says
\ nothing about the copy. A `create`d data word is exactly this case and it is not
\ a corner: its whole body is one chain plus the two-word push stencil, well under
\ the inline limit, so every compiled reference to a data word copies a chain.
\
\ WHY IT MATTERS. The AOT capture reads this map to find the DATA address chains it
\ has to rebase for the seeded engine (src/habu/aot-capture.f). A chain whose copy
\ is unrecorded is invisible there and keeps the building host's address. Measured
\ on the metabuild capture window before the inliner carried the record: 21 chains
\ recorded, 142 present, so 121 copies were unrecorded.
\
\ WHAT EACH CASE PROVES.
\
\   1. The callee is a chain and it is recorded. Without this the rest could pass
\      against a data word whose body changed shape.
\   2. The call really was inlined. The caller's body must have grown by the
\      callee's whole copied span; a BL would have added four bytes. This is the
\      precondition every later case rests on, so it is asserted rather than
\      assumed - if a future inline limit stops copying data words, this case says
\      so instead of the suite quietly testing nothing.
\   3. The copy is recorded, exactly once.
\   4. The record names the copy's FIRST word. The recorded address must begin a
\      byte-for-byte duplicate of the callee's body. That is not a decode of the
\      instructions - nothing here recognises a chain by its shape, which is the
\      guess the map exists to replace - it is the definition of where a copy
\      landed, asked of the copy and its source.
\   5. Two copies in one caller are two records. One record for a body that
\      carries two chains would pass cases 3 and 4 read loosely.
\   6. Inlining a callee with no chain records nothing. Without this the suite
\      would also pass for an inliner that marked every copied word, or every
\      inlined call.
\   7. A callee too long to inline records nothing in its caller. The record must
\      follow the COPY, not the call: this caller emits one BL and copies no bytes.
\
\ Cases 6 and 7 are the ones that fail an over-eager mark, and case 4 is the one
\ that fails a mark that is set one word early or one word late.

require lib/errors.f
require lib/test.f

package ADDRMAP-INLINE-TEST

private

\ ---- the boundaries ----------------------------------------------------------
\ Reading the engine's own relocation band and its own compiled code needs the
\ same raw casts src/habu/aot-capture.f and test/addrmap-set.f declare. They
\ choose nothing; every address handed to them is computed by the checked words
\ below from `cp@` and from the dictionary.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: DATA-A ( -- ptr u8 )
   data-base ;

TRUSTED: REGION-BASE ( -- n )
   dbase@ ;

TRUSTED: CODE-A ( n -- ptr u8 ) ;

\ ---- reading the band --------------------------------------------------------
\ Read exactly the way habu2.f EMIT-ADDR-SITE writes: the region byte offset of
\ the address, its map byte at offset >> 5, and its bit at (offset >> 2) & 7.
: ADDR-BIT@ ( n -- n ) {: at:n :}
   at REGION-BASE - {: off:n :}
   DATA-A SNAP-RELOC:ADDRMAP-OFF + off 5 rshift + c@
   off 2 rshift 7 and rshift 1 and ;

variable MARK-N

: MARKS ( n n -- n ) {: from:n to:n :}       \ recorded words in [from,to)
   0 MARK-N !
   to from ?do
      i ADDR-BIT@ MARK-N @ + MARK-N !
   4 +loop
   MARK-N @ ;

variable FIRST-A

: FIRST-MARK ( n n -- n ) {: from:n to:n :}  \ address of the first recorded word, or 0
   0 FIRST-A !
   to from ?do
      i ADDR-BIT@ 1 = FIRST-A @ 0= and if i FIRST-A ! then
   4 +loop
   FIRST-A @ ;

variable BYTES-NE

: BYTES= ( n n n -- bool ) {: a:n b:n u:n :}
   0 BYTES-NE !
   u 0 ?do
      a i + CODE-A c@  b i + CODE-A c@  <> if 1 BYTES-NE ! then
   loop
   BYTES-NE @ 0= ;

\ ---- the subjects ------------------------------------------------------------
\ Compiled here, by the engine under test, through the ordinary interpreter. The
\ code pointer either side of each definition is its body's span, which is all the
\ addresses this suite needs; nothing is hard-coded about the frame the compiler
\ builds around a body.
4 constant RET-BYTES                         \ the one-instruction tail `create` emits after the body

variable P0  variable P1  variable P2  variable P3  variable P4  variable P5
variable P6  variable P7  variable P8  variable P9  variable P10

cp@ P0 !
create AMI-DATA 8 allot                      \ body: one address chain + the push stencil + RET
cp@ P1 !

: AMI-REF ( -- ) ;                           \ an empty body: the frame, and nothing else
cp@ P2 !

: AMI-ONE ( -- ptr a ) AMI-DATA ;            \ short enough to inline: the chain is copied
cp@ P3 !

: AMI-TWO ( -- ptr a ptr a ) AMI-DATA AMI-DATA ;
cp@ P4 !

: AMI-NOCHAIN ( n -- n ) 1 + ;               \ inlinable, and carries no address
cp@ P5 !

: AMI-PLAIN ( n -- n ) AMI-NOCHAIN ;
cp@ P6 !

: AMI-BIGCALL ( -- ptr a ptr a ) AMI-TWO ;   \ AMI-TWO is past the inline limit: one BL
cp@ P7 !

\ ---- the copied span ---------------------------------------------------------
\ `create` emits the body and then one RET, and C-CALL copies the body without it,
\ so the span the inliner duplicates is the created word's whole code minus that
\ one instruction. Derived rather than written down, so the suite does not have to
\ be edited when the push stencil or the chain width changes.
: COPIED-BYTES ( -- n )
   P1 @ P0 @ - RET-BYTES - ;

: FRAME-BYTES ( -- n )
   P2 @ P1 @ - ;

\ ---- 1. the callee is one recorded chain -------------------------------------
: TEST-CALLEE ( -- )
   s" the created data word's first code word is recorded" T-LABEL
   P0 @ ADDR-BIT@ 1 T=
   s" and it is the only record in that word's body" T-LABEL
   P0 @ P1 @ MARKS 1 T= ;

\ ---- 2. the call was inlined, not called --------------------------------------
: TEST-INLINED ( -- )
   s" calling the data word copies its body into the caller" T-LABEL
   P3 @ P2 @ -  FRAME-BYTES COPIED-BYTES + T= ;

\ ---- 3. the copy is recorded, once -------------------------------------------
: TEST-COPY-RECORDED ( -- )
   s" the copied chain is recorded in the caller's body" T-LABEL
   P2 @ P3 @ MARKS 1 T= ;

\ ---- 4. the record names the copy's first word -------------------------------
\ FIRST-MARK answers 0 for a body with no record, and 0 is not an address to read
\ bytes from, so the presence of a record is asserted and the comparison is skipped
\ when there is none. Without that guard this case turns a plain assertion failure
\ into a fault, and a fault reports nothing.
: TEST-COPY-POSITION ( -- )
   P2 @ P3 @ FIRST-MARK {: at:n :}
   s" the caller's body has a recorded word to name" T-LABEL
   at 0 <> TTRUE
   at 0= if exit then
   s" the recorded word begins a verbatim copy of the callee's body" T-LABEL
   at  P0 @  COPIED-BYTES BYTES= TTRUE ;

\ ---- 5. two copies are two records -------------------------------------------
: TEST-TWO-COPIES ( -- )
   s" a caller that inlines two chains records both" T-LABEL
   P3 @ P4 @ MARKS 2 T= ;

\ ---- 6. inlining without a chain records nothing ------------------------------
: TEST-NO-CHAIN ( -- )
   s" inlining a body that carries no address records nothing" T-LABEL
   P5 @ P6 @ MARKS 0 T=
   s" and that caller really did inline its callee" T-LABEL
   P6 @ P5 @ -  P5 @ P4 @ - FRAME-BYTES - FRAME-BYTES + T= ;

\ ---- 7. a call that copies nothing records nothing ---------------------------
\ AMI-TWO's body is past the span C-CALL will copy, so this caller emits one
\ direct BL. Its size says so - one instruction on top of the empty frame - and
\ its body carries no record even though the callee it names is full of them.
: TEST-CALL-ONLY ( -- )
   s" a callee too long to inline is called, not copied" T-LABEL
   P7 @ P6 @ -  FRAME-BYTES RET-BYTES + T=
   s" and calling one records nothing in the caller" T-LABEL
   P6 @ P7 @ MARKS 0 T= ;

public

: RUN ( -- )
   T-RESET
   TEST-CALLEE
   TEST-INLINED
   TEST-COPY-RECORDED
   TEST-COPY-POSITION
   TEST-TWO-COPIES
   TEST-NO-CHAIN
   TEST-CALL-ONLY
   T-REPORT
   s" addrmap-inline: ok" type cr ;

;package

ADDRMAP-INLINE-TEST:RUN
