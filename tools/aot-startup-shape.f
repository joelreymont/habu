\ aot-startup-shape.f - the instruction shapes a stripped image's startup is
\ made of, named once for both readers of a built image: test/gate-aot-image.f
\ validates the startup and tools/image-size-lib.f attributes its bytes, so a
\ startup change is made in one place. PURE on instruction words: each reader
\ fetches with its own bounds check and passes the words in.

require src/arch/arm64/asm.f

package AOT-STARTUP-SHAPE
using A64ASM

4 constant INSN-BYTES               \ one A64 instruction
$10 constant MOVW-LANE-BITS         \ one movz/movk immediate lane (icode.f LOFF,)

\ The bits ENC-ADR packs its displacement into, taken from the encoder itself:
\ the word with every displacement bit set differs from the word with none in
\ exactly those two fields (immlo at bit 29, immhi at bit 5).
$1FFFFF constant ADR-DISP-ONES      \ a 21-bit displacement of all ones
0 ADR-DISP-ONES ENC-ADR  0 0 ENC-ADR xor constant ADR-DISP-BITS

: MOVW-LANE ( n -- n ) 5 rshift $FFFF and ;      \ the 16-bit immediate it carries
: MOVW-HW ( n -- n ) 21 rshift 3 and ;           \ which lane that immediate is shifted into

\ The signed byte displacement of an ADR, the inverse of ENC-ADRD. No division,
\ so it is exact for a target this startup never has: one off the word grid.
: ADR-DISP ( n -- n ) {: w:n :}
   w 5 rshift $7FFFF and 2 lshift  w 29 rshift 3 and or {: imm:n :}
   imm $100000 >= if imm $200000 - exit then
   imm ;

public

\ TEXT-ADR, borrows one scratch register for the code base: every emitter site
\ in src/habu/aot-link-arm.f passes x12, and every reader compares against this.
12 constant TEXT-ADR-SCRATCH

\ Written with the emitter's own encoders, not hex masks: a move-wide word is its
\ encoder's output for the rd, immediate and lane it carries, and no other word
\ is, so one equality decides it and no field can drift. The lane is free
\ because a LIT64, chain writes up to four of them.
: MOVZ-RD? ( n n -- bool ) {: w:n rd:n :}
   rd w MOVW-LANE w MOVW-HW MOVZHW  w = ;

: MOVN-RD? ( n n -- bool ) {: w:n rd:n :}
   rd w MOVW-LANE w MOVW-HW MOVNHW  w = ;

: MOVK-RD? ( n n -- bool ) {: w:n rd:n :}
   rd w MOVW-LANE w MOVW-HW MOVKHW  w = ;

\ One move-wide lane placed at its own shift: the value a pair or a chain spells
\ is the `or` of its words' chunks.
: MOVW-CHUNK ( n -- n ) {: w:n :}
   w MOVW-LANE  w MOVW-HW MOVW-LANE-BITS * lshift ;

\ `adr rd, .+d` for this rd, whatever the displacement. Not re-encoded: ENC-ADRD
\ divides, so a target off the word grid would not round-trip; every bit outside
\ the displacement field the encoder names is compared instead.
: ADR-RD? ( n n -- bool ) {: w:n rd:n :}
   w ADR-DISP-BITS invert and  rd 0 ENC-ADR = ;

: ADR-TARGET ( n n -- n ) {: w:n at:n :}
   at w ADR-DISP + ;

\ THE FOUR WORDS src/habu/aot-link-arm.f TEXT-ADR, EMITS, at `at` and into `rd`:
\ the label's byte offset from text offset zero in an LOFF, movz/movk pair (lane
\ 0 then lane 1), `adr scratch` to the code base itself (LTEXT, bound at text
\ offset zero, which `base` names), and the add that joins them. All four are
\ pinned because the first word alone does not identify the sequence:
\ EMIT-OWNED-CELLS opens a LIT64, of a DATA offset with the same `movz x9`, and
\ it is the `adr` and the add that never follow it.
: TEXT-ADR-SEQ? ( n n n n n n n -- bool )
   {: w0:n w1:n w2:n w3:n at:n base:n rd:n :}
   w0  rd w0 MOVW-LANE 0 MOVZHW <> if false exit then
   w1  rd w1 MOVW-LANE 1 MOVKHW <> if false exit then
   w2 TEXT-ADR-SCRATCH ADR-RD? 0= if false exit then
   w2 at 2 INSN-BYTES * + ADR-TARGET base <> if false exit then
   w3  rd TEXT-ADR-SCRATCH rd ENC-ADD = ;

\ The pair's two lanes spell the label's own byte offset from the code base; the
\ caller adds the base it reads the image at.
: TEXT-ADR-OFFSET ( n n -- n ) {: w0:n w1:n :}
   w0 MOVW-CHUNK  w1 MOVW-CHUNK or ;

\ THE HEAD OF THE BLOB'S COPY LOOP: the three words src/habu/aot-link-arm.f
\ EMIT-DATA-COPY puts at the top of its presence-map walk - the map cursor
\ against its end, the branch out of the whole loop, and the load of the map byte
\ this turn reads. Nothing here is a free field: the registers are the emitter's
\ and the displacement is the loop's own length, so a match is that loop.
\ IT IS WHAT TELLS A CODE-ONLY IMAGE FROM ONE THAT COPIES A DATA BLOB. The
\ empty-window arm of EMIT-DATA-COPY (BLOB-LEN 0) emits no loop at all, and the
\ two words it does emit - the span-end literal into x7 and the DP store - are
\ the same two that end the copying arm, so only the loop separates them.
\ tools/image-size-lib.f FIND-COPY-LOOP looks for them in an image whose blob
\ address it could not read.
41 constant COPY-LOOP-WORDS          \ instructions from the loop's test to past its end

: COPY-LOOP-HEAD? ( n n n -- bool ) {: w0:n w1:n w2:n :}
   w0  9 11 ENC-CMP <> if false exit then
   w1  COPY-LOOP-WORDS C-CS ENC-BCOND <> if false exit then
   w2  23 9 0 ENC-LDRB = ;

;using
;package
