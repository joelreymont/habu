\ aot-startup-shape.f - the instruction shapes a stripped image's startup is
\ made of, named once for every reader of a built image.
\
\ Two readers walk that startup: test/gate-aot-image.f validates it and
\ tools/image-size-lib.f attributes its bytes. Each carried its own copy of
\ these shapes, so converting the blob's address from one `adr x9` to the four
\ words src/habu/aot-lib.f TEXT-ADR, emits had to be done twice, and the second
\ copy failed silently - it counted zero sequences, took the code-only path and
\ reported `data 0 written` for every stripped image. The shapes live here so a
\ startup change is made in one place and both readers follow it.
\
\ They are written with the EMITTER'S OWN ENCODERS rather than hex masks: a word
\ is a move-wide into rd exactly when it equals that encoder's output for the rd,
\ immediate and lane the word itself carries, so the description reads as what
\ the emitter writes and a field this file never mentions cannot drift out of it.
\ The only bit patterns spelled out below are the field extractions a decoder
\ needs. The move-wide ones are checked by the predicates over them, which
\ re-encode what they read and compare whole words; the ADR displacement is not
\ re-encoded, because ENC-ADRD divides and a target off the word grid would not
\ round-trip, so ADR-RD? compares every bit outside the displacement field that
\ the encoder itself names.
\
\ PURE on instruction words. A reader fetches with its own bounds check - the
\ two have different ones - and passes the words in; nothing here holds an image.

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

\ TEXT-ADR, borrows one scratch register to hold the code base while it adds the
\ offset to it. All four emitter sites in src/habu/aot-lib.f pass x12: the entry
\ word (into x11), the sparse blob header in EMIT-DATA-COPY (x9, the only x9
\ site), the signal stub (x11) and the crash handler (x11). Every reader compares
\ against this, so a change of scratch is one edit here.
12 constant TEXT-ADR-SCRATCH

\ A move-wide word is its own encoder's output for the rd, immediate and lane it
\ carries, and no other word is, so one equality decides the whole word. The lane
\ is free here because a LIT64, chain writes up to four of them.
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

\ `adr rd, .+d` for this rd, whatever the displacement.
: ADR-RD? ( n n -- bool ) {: w:n rd:n :}
   w ADR-DISP-BITS invert and  rd 0 ENC-ADR = ;

\ Where such a word points, given the offset it sits at.
: ADR-TARGET ( n n -- n ) {: w:n at:n :}
   at w ADR-DISP + ;

\ THE FOUR WORDS src/habu/aot-lib.f TEXT-ADR, EMITS, at `at` and into `rd`: the
\ label's byte offset from text offset zero in an LOFF, movz/movk pair (lane 0
\ then lane 1), `adr scratch` to the code base itself (LTEXT, bound at text
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

\ THE HEAD OF THE BLOB'S COPY LOOP: the three words src/habu/aot-lib.f
\ EMIT-DATA-COPY puts at the top of its presence-map walk - the map cursor
\ against its end, the branch out of the whole loop, and the load of the map byte
\ this turn reads. Nothing here is a free field: the registers are the emitter's
\ and the displacement is the loop's own length, so a match is that loop.
\ IT IS WHAT TELLS A CODE-ONLY IMAGE FROM ONE THAT COPIES A DATA BLOB. The
\ empty-window arm of EMIT-DATA-COPY (BLOB-LEN 0) emits no loop at all, and the
\ two words it does emit - the span-end literal into x7 and the DP store - are
\ the same two that end the copying arm, so only the loop separates them.
\ Two readers: test/gate-aot-image.f CHECK-COPY pins these words in the startup
\ it validates, and tools/image-size-lib.f FIND-COPY-LOOP looks for them in an
\ image whose blob address it could not read.
41 constant COPY-LOOP-WORDS          \ instructions from the loop's test to past its end

: COPY-LOOP-HEAD? ( n n n -- bool ) {: w0:n w1:n w2:n :}
   w0  9 11 ENC-CMP <> if false exit then
   w1  COPY-LOOP-WORDS C-CS ENC-BCOND <> if false exit then
   w2  23 9 0 ENC-LDRB = ;

;using
;package
