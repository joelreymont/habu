\ The window DATA run row's varint codec, at the widths and the malformations the
\ engine's own content never reaches. src/habu/aot-decl.f package AOT-WINDOW is
\ the authority for the grammar; the two emitted decoders (habu2.f APPLY-RUNS for
\ the baked window, aot-lib.f EMIT-VGET for a stripped image's own DATA) are the
\ same grammar in ARM64 and are pinned where they are emitted
\ (test/gate-aot-image.f CHECK-VGET) and exercised by every build.
require lib/test.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f

package AOT-RUN-ROW-TEST
using AOT-WINDOW

create VBUF 16 allot

: B! ( n n -- ) {: v:n at:n :} v VBUF at + c! ;

\ Write, then read back: the width the encoder charges, the width it wrote, the
\ width the reader spent and the value it answers are one number and one value.
: ROUND ( n n -- ) {: v:n want:n :}
   v RUN-VLEN want T=
   v VBUF RUN-V! want T=
   VBUF 16 RUN-V@ {: got:n w:n :}
   w want T=
   got v T= ;

\ Every malformation answers width 0 and value 0, so a caller that checks the
\ width cannot be handed a plausible value from a row this format never wrote.
: REFUSED ( n -- ) {: avail:n :}
   VBUF avail RUN-V@ {: got:n w:n :}
   w 0 T=
   got 0 T= ;

: WIDTHS ( -- )
   s" seven bits travel in one byte" T-LABEL
   0 1 ROUND
   s" seven bits travel in one byte" T-LABEL
   1 1 ROUND
   s" seven bits travel in one byte" T-LABEL
   $7F 1 ROUND
   s" the eighth bit opens a second group" T-LABEL
   $80 2 ROUND
   s" fourteen bits still fit two groups" T-LABEL
   $3FFF 2 ROUND
   s" the fifteenth bit opens a third group" T-LABEL
   $4000 3 ROUND
   s" twenty-one bits still fit three groups" T-LABEL
   $1FFFFF 3 ROUND
   s" the twenty-second bit opens a fourth group" T-LABEL
   $200000 4 ROUND
   s" twenty-eight bits still fit four groups" T-LABEL
   $FFFFFFF 4 ROUND
   s" the twenty-ninth bit opens the fifth group" T-LABEL
   $10000000 5 ROUND
   s" a full u32 is the widest row field" T-LABEL
   $FFFFFFFF 5 ROUND ;

: TRUNCATED ( -- )
   $80 VBUF RUN-V! 2 T=
   s" a varint that does not end inside the section is refused" T-LABEL
   1 REFUSED ;

: EMPTY ( -- )
   s" no bytes left is not a field" T-LABEL
   0 REFUSED ;

: PADDED ( -- )
   $81 0 B!  $00 1 B!
   s" a value padded with a zero high group is refused" T-LABEL
   2 REFUSED ;

: OVERLONG ( -- )
   $80 0 B!  $80 1 B!  $80 2 B!  $80 3 B!  $80 4 B!  $00 5 B!
   s" a varint that runs past five bytes is refused" T-LABEL
   6 REFUSED ;

: TOO-WIDE ( -- )
   $80 0 B!  $80 1 B!  $80 2 B!  $80 3 B!  $7F 4 B!
   s" a value no u32 row field could have held is refused" T-LABEL
   5 REFUSED ;

\ The writer refuses what the reader refuses: a value outside u32 has no width
\ and RUN-V! leaves the buffer alone. A negative value is the case that matters,
\ because a gap is `start - last end` and a backwards row is otherwise silent.
: UNWRITABLE ( -- )
   $55 0 B!
   s" a negative value has no width" T-LABEL
   -1 RUN-VLEN 0 T=
   s" ... and is not written" T-LABEL
   -1 VBUF RUN-V! 0 T=
   VBUF c@ $55 T=
   s" a value past u32 has no width" T-LABEL
   $100000000 RUN-VLEN 0 T=
   s" ... and is not written" T-LABEL
   $100000000 VBUF RUN-V! 0 T=
   VBUF c@ $55 T= ;

\ A gap is an unsigned distance from the last run's END, so the merged threshold
\ and the row's own arithmetic have to agree: a row is never narrower than two
\ one-byte fields, and that width is what decides whether a zero gap travels.
: THRESHOLD ( -- )
   s" the narrowest row is two one-byte fields" T-LABEL
   0 RUN-VLEN 0 RUN-VLEN + RUN-ROW-MIN T=
   s" the gap threshold is the narrowest row" T-LABEL
   RUN-GAP-MIN RUN-ROW-MIN T= ;

: RUN ( -- )
   T-RESET
   WIDTHS  TRUNCATED  EMPTY  PADDED  OVERLONG  TOO-WIDE  UNWRITABLE  THRESHOLD
   T-REPORT ;

RUN
;using
;package
