\ aot-seed-surface.f - print ONE number that identifies the whole dictionary
\ surface of the engine that loads this file.
\
\ WHY A NUMBER AND NOT A NAME LIST. The claim under test is that a boot's
\ dictionary does not depend on how the boot was entered (dot
\ habu-decide-arm-the-5234727b, USER RULING 2026-08-11: one dictionary surface for
\ every boot mode). Comparing surfaces means comparing ~7000 names between
\ processes; folding them into one order-sensitive number lets each mode print its
\ answer and lets the caller compare two integers. A fold is not a name list: it
\ cannot say WHICH name moved. That is deliberate - when the numbers disagree the
\ suite's own message says how to get the list (the two commands are below), and
\ the fold is what makes the comparison cheap enough to run on every boot mode.
\
\ WHY IT PRINTS AT LOAD. Both callers reach this file the same way: the batch
\ modes hand the path to the engine and the interactive mode types
\ `s" test/aot-seed-surface.f" required` at the prompt, so the file's own
\ definitions land in the dictionary identically in both and are folded in too.
\ Anything else here (a require of its own, an argument, a second entry point)
\ would make the two loads differ and the comparison would measure THIS FILE.
\
\ It therefore requires nothing: `ndict@`, the XREF record readers and `STR=` are
\ all engine-prefix words that exist before any user token in every mode.
\
\ The name list, when a mismatch needs one:
\   bin/hb --load test/aot-seed-names.f > /tmp/batch.txt
\   (interactive) s" test/aot-seed-names.f" required   -> compare the two

package AOT-SEED-SURFACE

private

variable H

\ One byte into the running fold. The multiplier is an odd 32-bit prime so a byte
\ moved between two names changes the answer (a pure xor fold would not see a
\ swap), and the wrap is 64-bit two's complement, which is fine: the answer is an
\ identity, not an arithmetic result.
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
