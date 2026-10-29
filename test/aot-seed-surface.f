\ aot-seed-surface.f - print one order-sensitive number folding the dictionary
\ surface of the engine that loads this file; every boot mode must print the
\ same one. It prints at load and requires nothing (ndict@, the XREF readers
\ and STR= are engine-prefix words in every mode), so batch and interactive
\ loads add the same definitions.

package AOT-SEED-SURFACE

private

variable H

\ An odd prime multiplier makes a byte moved between two names change the
\ answer (an xor fold misses a swap); the 64-bit wrap is fine for an identity.
1000003 constant MIX-ODD

: MIX ( n -- ) {: b:n :}
   H @ MIX-ODD * b xor  H ! ;

: NAME-MIX ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ MIX
      1+
   repeat drop
   u MIX ;                                  \ the length too: "AB" + "C" must not fold like "A" + "BC"

public

: SUM ( -- n )
   0 H !
   0 begin dup ndict@ < while
      dup XREF-REC XREF-NAME$ NAME-MIX
      1+
   repeat drop
   H @ ;

\ How many dictionary records carry one exact spelling. One is the answer for a
\ baked name on a healthy boot; two means the AOT seed ran twice.
: COUNT-NAMED ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   0 begin dup ndict@ < while
      dup XREF-REC XREF-NAME$ a u STR= if swap 1+ swap then
      1+
   repeat drop ;

;package

AOT-SEED-SURFACE:SUM . cr
