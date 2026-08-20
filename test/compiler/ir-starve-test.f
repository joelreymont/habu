\ ir-starve-test.f - the shared starve fixture has to actually starve.
\
\ Every table's torn-row case rests on IR-STARVE:EDGE leaving a context with
\ less scratch than one arena doubling needs. A fixture that quietly stopped
\ starving - because the mapping grew again, or the header changed width -
\ would let all of those cases pass while proving nothing, so the two facts
\ they rest on are pinned here: EDGE leaves exactly MARGIN bytes, and after it
\ the first arena growth refuses with E-IR-CTX-SCRATCH while every append
\ below the growth still lands.

require lib/test.f
require test/compiler/ir-starve.f
require src/compiler/ir/arena.f

package IR-STARVE-TEST
private

: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- EDGE leaves exactly MARGIN ----------------------------------------------
\ Taking the whole margin must succeed and one cell more must not. That is what
\ pins IR-STARVE's copies of the mapping size and header width to the real ones:
\ if either pin drifted, EDGE would leave a different amount and one of these
\ two halves would fail.
: EXACT-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c IR-STARVE:EDGE
   c IR-STARVE:MARGIN IR-CTX:SCRATCH-TAKE {: got:ptr n:n :} \ typed-local-lint: allow-bare-local - got keeps the ptr u8 span role
   n ;

: EXACT-OVER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-STARVE:EDGE
   c IR-STARVE:MARGIN IR-CTX:SCRATCH-TAKE 2drop
   c CDIGEST:SLOT-BYTES IR-CTX:SCRATCH-TAKE 2drop ;

: EXACT-OVER ( -- )
   BND [: EXACT-OVER-BODY ;] IR-CTX:WITH-CONTEXT ;

: EXACT-CASES ( -- )
   s" the starve leaves exactly one margin, spendable to the byte" T-LABEL
   BND [: EXACT-BODY ;] IR-CTX:WITH-CONTEXT
   IR-STARVE:MARGIN T=
   s" one cell past the margin refuses" T-LABEL
   [: EXACT-OVER ;] E-IR-CTX-SCRATCH TTHROWSQ ;

\ ---- EDGE blocks growth and only growth ---------------------------------------
\ An arena seeded at eight cells takes eight appends without allocating and
\ needs a doubled span for the ninth. Before the starve all nine land; after it
\ the first eight still land - the starve is not a blanket refusal - and the
\ ninth refuses.
: FILL8 ( IR-CTX:ctx IR-ARENA:arena -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena :}
   8 0 ?do
      c a i IR-ARENA:PUSH drop
   loop ;

: GROWS-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c 64 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a FILL8
   c a 8 IR-ARENA:PUSH drop
   a IR-ARENA:USED ;

: STARVED-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c 64 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c IR-STARVE:EDGE
   c a FILL8
   a IR-ARENA:USED ;

: STARVED-NINTH ( IR-CTX:ctx IR-ARENA:arena -- IR-CTX:ctx IR-ARENA:arena )
   {: c:IR-CTX:ctx a:IR-ARENA:arena :}
   c a
   c a 8 IR-ARENA:PUSH drop ;

: NINTH-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 64 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c IR-STARVE:EDGE
   c a FILL8
   c a [: STARVED-NINTH ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena rc:n :}
   rc
   a2 IR-ARENA:USED ;

: GROWTH-CASES ( -- )
   s" a fed context grows the arena past its seed" T-LABEL
   BND [: GROWS-BODY ;] IR-CTX:WITH-CONTEXT 9 T=
   s" a starved context still takes every append below the growth" T-LABEL
   BND [: STARVED-BODY ;] IR-CTX:WITH-CONTEXT 8 T=
   s" the append that needs a doubled span refuses, and writes nothing" T-LABEL
   BND [: NINTH-BODY ;] IR-CTX:WITH-CONTEXT
   8 T= E-IR-CTX-SCRATCH T= ;

public

: RUN ( -- )
   T-RESET
   EXACT-CASES
   GROWTH-CASES
   T-REPORT ;

;package

IR-STARVE-TEST:RUN
