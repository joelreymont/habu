\ reloc-schema.f - the shared frozen description of the snapshot relocation
\ contract.
\
\ The module lives in `package RELOC-PROOF`. Its subject is the three emitted
\ passes that make a snapshot image portable - `SNAP-RELOC:EMIT-CALLS`,
\ `SNAP-RELOC:EMIT-XT` and `SNAP-RELOC:EMIT-ADDRS` in src/habu/habu2.f, with the
\ writer's address-cell half `SND-CANON-XT-CELL` in src/habu/snap-lib.f, the
\ chain's scaffold words in src/habu/habu1.f and the band constants in
\ src/habu/layout.f - and the machine-checked model of that arithmetic in
\ `formal/Common/Reloc.v`.
\
\ It holds data and nothing else. Five tables:
\
\   1. The pinned band constants. The model states REGION-OFF, RBASE-VA,
\      BL-REACH, REGION, CALLMAP-RC and BL-OP-HI as numbers. Each is frozen here
\      beside the shipped constant it mirrors, so `test/compiler/reloc-cases.f`
\      can read the literal out of the shipped source and
\      `test/compiler/reloc-obligations.f` can ask Rocq whether the model still
\      holds the same number.
\
\   2. The frozen writer bodies. The loader's two passes are emitted assembly
\      and the cases file RUNS them, decoded out of habu2.f. The writer's
\      address-cell half is ordinary checked Habu inside the builder-only
\      src/habu/snap-lib.f, which no test can load, so its three words are
\      frozen here as exact token runs instead. A changed body fails the run
\      comparison rather than drifting away from the model.
\
\   3. The call vector rows. A row names the region offset the writing run got,
\      the region offset the restoring run got, and every word of a small
\      region: whether the call map records it, what it holds, what it must hold
\      after the writer's pass, and what it must hold after the loader's. These
\      rows are the one copy. The cases file builds a real region and a real map
\      band and drives the shipped instruction sequence over them; the
\      obligations file turns the very same row into a Rocq obligation about
\      `Habu.Common.Reloc`. Neither side carries a copy.
\
\   4. The address-cell vector rows. The same idea for a persisted DATA cell
\      that was declared to hold a region address: the writing run's region
\      base, the restoring run's, and each cell's three values.
\
\   5. The address-literal chain vector rows. The same idea again for the fixed
\      four-instruction MOVZ/MOVK chain the compiler bakes into region code for
\      a quotation's entry address and for the target of a `[']` or a
\      `postpone`. A row names the band the pass is moving - a base, a length
\      and the base those addresses are moving to - and every slot of a small
\      region: whether the address map records it, whether one of its four words
\      is damaged, and the address it carries before the writer's pass, after
\      it, and after the loader's.
\
\ Where the two sides are not literally the same shape, and why that is sound:
\
\   - A row writes a call site as a DISPLACEMENT in instruction units, and both
\     sides build the same word from it the same way: opcode BL-OP-HI in the top
\     six bits, the displacement masked into the low twenty-six. Neither side
\     computes a displacement; every one of them is a frozen literal here.
\   - The shipped pass reports a refusal by writing a diagnostic and exiting
\     with CALLMAP-RC, and the model reports it as a status beside the partly
\     rewritten image. A row records the exit status, so the two sides stay
\     bound at the refusal rather than only agreeing that something failed.
\   - The shipped pass finds recorded sites through a bitmap indexed by region
\     word offset. A row carries the recorded flag directly, and the cases file
\     turns the row's flags into a real map band, so the bitmap indexing is
\     exercised on the Habu side and left out of the model. That is MODEL GAP 3
\     in `formal/Common/Reloc.v`.
\   - A chain row writes an address, not four instruction words, and both sides
\     build the same four words from it the same way: scaffold word j from
\     src/habu/habu1.f with bits 16j..16j+15 of the address at bit five. A
\     damaged slot replaces one of those words with CHAIN-BAD-WORD. Neither side
\     computes an address; every one of them is a frozen literal here.
\
\ Consumers: `test/compiler/reloc-cases.f`, `test/compiler/reloc-obligations.f`.

require lib/prelude.f
require lib/errors.f
require lib/string.f

package RELOC-PROOF
public

: MODEL-FILE$ ( -- ptr u8 n )
   s" formal/Common/Reloc.v" ;

: EMIT-FILE$ ( -- ptr u8 n )
   s" src/habu/habu2.f" ;

: LAYOUT-FILE$ ( -- ptr u8 n )
   s" src/habu/layout.f" ;

: WRITER-FILE$ ( -- ptr u8 n )
   s" src/habu/snap-lib.f" ;

\ The condition-code names live beside the four-bit field `?COND` bounds, in the
\ encoder itself. src/arch/arm64/mnem.f carried a second copy until the compare
\ and branch forms landed; it reads the encoder's now, and so does this gate.
: COND-FILE$ ( -- ptr u8 n )
   s" src/arch/arm64/asm.f" ;

\ The chain's four scaffold words are declared in the first emitter file, not
\ the second, so the pinned-constant rows below have to read both.
: SCAFFOLD-FILE$ ( -- ptr u8 n )
   s" src/habu/habu1.f" ;

\ ---- 1. the pinned band constants --------------------------------------------
\ Each row is a shipped constant and the model definition that mirrors it. The
\ cases file reads the shipped literal structurally; the obligations file asks
\ Rocq whether the model still carries the same number. Renumber one side and
\ the two sides disagree.

14 constant PIN-COUNT

: PIN-FILE$ ( n -- ptr u8 n )
   case
      0 of LAYOUT-FILE$ endof
      1 of LAYOUT-FILE$ endof
      2 of LAYOUT-FILE$ endof
      3 of LAYOUT-FILE$ endof
      4 of LAYOUT-FILE$ endof
      5 of EMIT-FILE$ endof
      6 of LAYOUT-FILE$ endof
      7 of EMIT-FILE$ endof
      8 of EMIT-FILE$ endof
      9 of EMIT-FILE$ endof
      10 of SCAFFOLD-FILE$ endof
      11 of SCAFFOLD-FILE$ endof
      12 of SCAFFOLD-FILE$ endof
      13 of SCAFFOLD-FILE$ endof
      E-CRL-ROW throw
   endcase ;

: PIN-NAME$ ( n -- ptr u8 n )
   case
      0 of s" REGION-OFF" endof
      1 of s" RBASE-VA" endof
      2 of s" BL-REACH" endof
      3 of s" REGION" endof
      4 of s" CALLMAP-RC" endof
      5 of s" BL-OP-HI" endof
      6 of s" ADDRMAP-RC" endof
      7 of s" ADDR-OPC-MASK" endof
      8 of s" ADDR-IMM-MASK" endof
      9 of s" ADDR-CHAIN-BYTES" endof
      10 of s" W-MOVZ0" endof
      11 of s" W-MOVK1" endof
      12 of s" W-MOVK2" endof
      13 of s" W-MOVK3" endof
      E-CRL-ROW throw
   endcase ;

: PIN-MODEL$ ( n -- ptr u8 n )
   case
      0 of s" region_off" endof
      1 of s" rbase_va" endof
      2 of s" bl_reach" endof
      3 of s" region_bytes" endof
      4 of s" callmap_rc" endof
      5 of s" bl_op_hi" endof
      6 of s" addrmap_rc" endof
      7 of s" addr_opc_mask" endof
      8 of s" addr_imm_mask" endof
      9 of s" addr_chain_bytes" endof
      10 of s" w_movz0" endof
      11 of s" w_movk1" endof
      12 of s" w_movk2" endof
      13 of s" w_movk3" endof
      E-CRL-ROW throw
   endcase ;

: PIN-VALUE ( n -- n )
   case
      0 of $1000000 endof
      1 of $300000000 endof
      2 of $8000000 endof
      3 of $800000 endof
      4 of 95 endof
      5 of $25 endof
      6 of 97 endof
      7 of $FFE0001F endof
      8 of $FFFF endof
      9 of 16 endof
      10 of $D2800009 endof
      11 of $F2A00009 endof
      12 of $F2C00009 endof
      13 of $F2E00009 endof
      E-CRL-ROW throw
   endcase ;

\ ---- 2. the frozen writer bodies ---------------------------------------------
\ src/habu/snap-lib.f is builder-only: SNAP-RETIRE-GO forgets the whole writer
\ before an image is written, and nothing in the test tree can load it. Its
\ three address-cell words are therefore frozen as exact token runs, read back
\ through the shared source lexer, rather than executed. Every other half of
\ this contract is run for real.

3 constant WBODY-COUNT

: WBODY-NAME$ ( n -- ptr u8 n )
   case
      0 of s" SND-XT-ROW" endof
      1 of s" SND-CANON-XT-CELL" endof
      2 of s" SND-CANON-XT-CELLS" endof
      E-CRL-ROW throw
   endcase ;

: WBODY-RUN$ ( n -- ptr u8 n )
   case
      0 of s" {: row:n :} SNAP-RELOC:XTCELL-ROWS-OFF row cells + SND-XT-CELL@" endof
      1 of s" {: cell:n :} cell SND-XT-CELL@ {: xt:n :} xt 0= if exit then xt dbase@ - RBASE-VA + cell SND-XT-CELL!" endof
      2 of s" SNAP-RELOC:XTCELL-N-CELL SND-XT-CELL@ 0 ?do i SND-XT-ROW SND-CANON-XT-CELL loop" endof
      E-CRL-ROW throw
   endcase ;

\ ---- 3. the emit vocabulary and its classification ---------------------------
\ The round-trip rows below say that every RECORDED site survives. That is worth
\ nothing on its own, because it is vacuously true of an address class nobody
\ ever recorded - which is how a literal address baked into region code as a
\ MOVZ/MOVK chain came to crash a restored image.
\
\ So this table enumerates every word in src/habu/habu2.f that can put an
\ address-bearing value into region bytes, or declare a persisted cell that
\ holds one, and names the class the model gives it. The model's `classify` is a
\ match over an inductive type, so a producer added there without a class is a
\ Rocq error rather than an omission; this table is what holds that vocabulary
\ to the one the shipped emitter actually has.

13 constant PROD-COUNT

: PROD-NAME$ ( n -- ptr u8 n )
   case
      0 of s" C-LIT" endof
      1 of s" C-RAW-LIT" endof
      2 of s" C-ADDR-RAW" endof
      3 of s" C-ADDR-PUSH" endof
      4 of s" C-DATA-ADDR" endof
      5 of s" C-DATA-ADDR-RAW" endof
      6 of s" C-CODE-ADDR" endof
      7 of s" C-ADR" endof
      8 of s" EMIT-CEMITBL" endof
      9 of s" EM-AOT-PATCH-SITES" endof
      10 of s" EM-AOT-RELOC-CODE" endof
      11 of s" EM-AOT-RELOC-DATA" endof
      12 of s" EMIT-MARK" endof
      E-CRL-ROW throw
   endcase ;

: PROD-MODEL$ ( n -- ptr u8 n )
   case
      0 of s" P_scalar_lit" endof
      1 of s" P_scalar_raw_lit" endof
      2 of s" P_addr_carrier" endof
      3 of s" P_addr_carrier_push" endof
      4 of s" P_data_addr" endof
      5 of s" P_data_addr_raw" endof
      6 of s" P_code_addr" endof
      7 of s" P_pc_relative_adr" endof
      8 of s" P_direct_call" endof
      9 of s" P_aot_call_patch" endof
      10 of s" P_aot_code_reloc" endof
      11 of s" P_aot_data_reloc" endof
      12 of s" P_defer_cell" endof
      E-CRL-ROW throw
   endcase ;

: PROD-CLASS$ ( n -- ptr u8 n )
   case
      0 of s" Not_an_address" endof
      1 of s" Not_an_address" endof
      2 of s" Named_at_site" endof
      3 of s" Named_at_site" endof
      4 of s" Fixed_mapping" endof
      5 of s" Fixed_mapping" endof
      6 of s" Recorded R_addrmap" endof
      7 of s" Position_independent" endof
      8 of s" Recorded R_callmap" endof
      9 of s" Recorded R_callmap" endof
      10 of s" Recorded R_addrmap" endof
      11 of s" Fixed_mapping" endof
      12 of s" Recorded R_xtcell" endof
      E-CRL-ROW throw
   endcase ;

\ ---- 4. the closure rows that make the vocabulary complete -------------------
\ A table of names is a comment unless something holds it to the source. Each
\ row below names a token and the exact, ordered set of definitions in
\ src/habu/habu2.f whose body carries it. The cases file walks every definition
\ in the file and rebuilds each set, so:
\
\   - a new word that calls the shared MOVZ/MOVK carrier appears in row 0 or 1
\     and fails until it is added to the vocabulary above and classified in the
\     model;
\   - a second, hand-built copy of that chain appears in row 2, because the
\     chain's scaffold constant may occur in exactly one definition;
\   - a new place that bakes a DATA or a CODE address, or emits a direct call,
\     or declares a persisted address cell, appears in rows 3 to 8 and fails
\     until someone has looked at it;
\   - rows 12 and 13 are the two names the address-cell DECLARER itself goes by
\     in this file -- bare inside package SNAP-RELOC, qualified outside it -- so
\     a new caller of that routine shows up whichever side of the package it is
\     written on. They were added when `xt!` (dot
\     habu-declare-persisted-cb-b150b5d5) made the declarer reachable at RUN
\     time as well as from a compile handler; before that, row 8 saw every
\     declaring site because MARK-CELL was the only way in.
\
\ The sets are written in the order the file declares them, so a reordering is a
\ change too. `CORE` and `JIT` appear in several of them because those two
\ emission phases are where the label variables are declared.

14 constant CLOSURE-COUNT

: CLOSURE-TOKEN$ ( n -- ptr u8 n )
   case
      0 of s" C-ADDR-RAW" endof
      1 of s" C-ADDR-PUSH" endof
      2 of s" W-MOVZ0" endof
      3 of s" C-DATA-ADDR" endof
      4 of s" C-DATA-ADDR-RAW" endof
      5 of s" C-CODE-ADDR" endof
      6 of s" C-ADR" endof
      7 of s" LCEMITBL" endof
      8 of s" SNAP-RELOC:MARK-CELL" endof
      9 of s" SNAP-RELOC:MARK-SITE" endof
      10 of s" SNAP-RELOC:ADDRMAP-OFF" endof
      11 of s" SNAP-RELOC:LADDRS" endof
      12 of s" LMARK" endof
      13 of s" SNAP-RELOC:LMARK" endof
      E-CRL-ROW throw
   endcase ;

: CLOSURE-SET$ ( n -- ptr u8 n )
   case
      0 of s" C-ADDR-PUSH C-DATA-ADDR-RAW" endof
      1 of s" C-DATA-ADDR C-CODE-ADDR" endof
      2 of s" C-ADDR-RAW EMIT-ADDRS" endof
      3 of s" EMIT-CREATE" endof
      4 of s" C-DEFER-EMIT-CODE J-IS" endof
      5 of s" J-SEMIQUOT C-POSTPONE C-BTICK" endof
      6 of s" C-SDQ C-CQ C-ESDQ C-ECQ" endof
      7 of s" C-CALL EMIT-CEMITBL EMIT-P2-VALID-EMIT EMIT-P2-STORE CORE" endof
      8 of s" EM-STARTUP-RUNTIME-STATE" endof
      9 of s" C-CODE-ADDR" endof
      10 of s" EM-AOT-RELOC-CODE" endof
      11 of s" BSNAPREBASE EM-SNAPSHOT-RESTORE CORE" endof
      12 of s" MARK-CELL EMIT-MARK BXTSTORE" endof
      13 of s" C-DEFER-CELL J-IS CORE" endof
      E-CRL-ROW throw
   endcase ;

\ ---- what each vector row is there to show -----------------------------------
\ Every role must be covered by at least one row. The tables carry no digest, so
\ the roles are the freeze: a row that stops being covered fails the coverage
\ check in the cases file rather than quietly shrinking what the gate asks.

0 constant ROLE-IDENTITY     \ same base both runs: the image comes back byte for byte
1 constant ROLE-REBASE-UP    \ the restoring run's region sits above the writing run's
2 constant ROLE-REBASE-DOWN  \ and below it, so the delta changes sign
3 constant ROLE-BASE-FREE    \ a third writing base, and the identical canonical image
4 constant ROLE-REFUSE       \ a recorded site that does not hold a call
5 constant ROLE-XT-IDENTITY  \ a declared address cell, restored at the writing run's base
6 constant ROLE-XT-REBASE    \ and at a different one
7 constant ROLE-CH-IDENTITY  \ an address chain restored at the writing run's band base
8 constant ROLE-CH-REBASE    \ and at a different one
9 constant ROLE-CH-OTHER     \ a chain naming the OTHER band: this call must not touch it
10 constant ROLE-CH-REFUSE   \ a recorded chain site whose fourth word is not a MOVK3
11 constant ROLE-CH-WIDE     \ a move that changes every one of the four immediates
12 constant ROLE-COUNT

: ROLE-NAME$ ( n -- ptr u8 n )
   case
      0 of s" identity" endof
      1 of s" rebase_up" endof
      2 of s" rebase_down" endof
      3 of s" base_free" endof
      4 of s" refuse" endof
      5 of s" xt_identity" endof
      6 of s" xt_rebase" endof
      7 of s" chain_identity" endof
      8 of s" chain_rebase" endof
      9 of s" chain_other_band" endof
      10 of s" chain_refuse" endof
      11 of s" chain_wide" endof
      E-CRL-ROW throw
   endcase ;

\ A site holds either a call, written as a displacement in instruction units, or
\ a raw word written whole. Both sides build a call word the same way from the
\ pinned opcode, and neither side ever computes a displacement.
0 constant KIND-BL
1 constant KIND-RAW

\ How a chain slot is spelled out from the address it carries. Both sides build
\ the four words the same way: scaffold word j from src/habu/habu1.f, with bits
\ 16j..16j+15 of the address sitting at bit five. A damaged slot holds
\ CHAIN-BAD-WORD in one of its four words instead, which is a small integer and
\ therefore not any of the four scaffolds.
4 constant CHAIN-WORDS
32 constant IMM-SCALE        \ the shipped `LSLI, 5` that puts an immediate at bit five
$FFFF constant IMM16-MASK
5 constant CHAIN-BAD-WORD
-1 constant CHAIN-WHOLE      \ no word of this slot is damaged

private

\ ---- storage -----------------------------------------------------------------

64 constant SITE-CAP
16 constant ROW-CAP
32 constant CELL-CAP

create SITE-IDX SITE-CAP cells allot
create SITE-REC SITE-CAP cells allot
create SITE-KIND SITE-CAP cells allot
create SITE-V0 SITE-CAP cells allot
create SITE-V1 SITE-CAP cells allot
create SITE-V2 SITE-CAP cells allot

create ROW-ROLE ROW-CAP cells allot
create ROW-WOFF ROW-CAP cells allot
create ROW-LOFF ROW-CAP cells allot
create ROW-WORDS ROW-CAP cells allot
create ROW-RC ROW-CAP cells allot
create ROW-BASE ROW-CAP cells allot
create ROW-LEN ROW-CAP cells allot

variable SITE-N
variable ROW-N
variable OPEN-BASE

create ASITE-IDX SITE-CAP cells allot
create ASITE-REC SITE-CAP cells allot
create ASITE-BAD SITE-CAP cells allot
create ASITE-V0 SITE-CAP cells allot
create ASITE-V1 SITE-CAP cells allot
create ASITE-V2 SITE-CAP cells allot

create AROW-ROLE ROW-CAP cells allot
create AROW-WB ROW-CAP cells allot
create AROW-CB ROW-CAP cells allot
create AROW-LB ROW-CAP cells allot
create AROW-BLEN ROW-CAP cells allot
create AROW-WORDS ROW-CAP cells allot
create AROW-RC ROW-CAP cells allot
create AROW-BASE ROW-CAP cells allot
create AROW-LEN ROW-CAP cells allot

variable ASITE-N
variable AROW-N
variable AOPEN-BASE

create CELL-V0 CELL-CAP cells allot
create CELL-V1 CELL-CAP cells allot
create CELL-V2 CELL-CAP cells allot

create XROW-ROLE ROW-CAP cells allot
create XROW-DBW ROW-CAP cells allot
create XROW-DBL ROW-CAP cells allot
create XROW-BASE ROW-CAP cells allot
create XROW-LEN ROW-CAP cells allot

variable CELL-N
variable XROW-N
variable XOPEN-BASE

: SITE-RANGE ( n -- ) {: i:n :}
   i 0 < i SITE-N @ >= or if E-CRL-ROW throw then ;

: ROW-RANGE ( n -- ) {: i:n :}
   i 0 < i ROW-N @ >= or if E-CRL-ROW throw then ;

: CELL-RANGE ( n -- ) {: i:n :}
   i 0 < i CELL-N @ >= or if E-CRL-ROW throw then ;

: XROW-RANGE ( n -- ) {: i:n :}
   i 0 < i XROW-N @ >= or if E-CRL-ROW throw then ;

: ASITE-RANGE ( n -- ) {: i:n :}
   i 0 < i ASITE-N @ >= or if E-CRL-ROW throw then ;

: AROW-RANGE ( n -- ) {: i:n :}
   i 0 < i AROW-N @ >= or if E-CRL-ROW throw then ;

\ ---- call table builders -----------------------------------------------------
\ One site: its word index inside the region, whether the call map records it,
\ what it holds, what the writer's pass must leave, and what the loader's pass
\ must leave after that. A row lists EVERY word of its region, so the image
\ comparison is exhaustive rather than a spot check.

: SITE+ ( n n n n n n -- ) {: idx:n rec:n kind:n v0:n v1:n v2:n :}
   SITE-N @ SITE-CAP >= if E-CRL-ROW throw then
   idx SITE-IDX SITE-N @ cells + !
   rec SITE-REC SITE-N @ cells + !
   kind SITE-KIND SITE-N @ cells + !
   v0 SITE-V0 SITE-N @ cells + !
   v1 SITE-V1 SITE-N @ cells + !
   v2 SITE-V2 SITE-N @ cells + !
   SITE-N @ 1+ SITE-N ! ;

\ A recorded call: the map bit is set, and the two passes move it.
: BL-SITE ( n n n n -- ) {: idx:n d0:n d1:n d2:n :}
   idx 1 KIND-BL d0 d1 d2 SITE+ ;

\ A call the map does not record: one that stays inside the region, so it keeps
\ its distance wherever the region lands and must come through untouched.
: BL-KEEP ( n n -- ) {: idx:n d0:n :}
   idx 0 KIND-BL d0 d0 d0 SITE+ ;

\ Any other word the map does not record, including one whose bits spell a call.
: RAW-KEEP ( n n -- ) {: idx:n w:n :}
   idx 0 KIND-RAW w w w SITE+ ;

\ A word the map DOES record that is not a call: the corrupt-image case.
: RAW-SITE ( n n -- ) {: idx:n w:n :}
   idx 1 KIND-RAW w w w SITE+ ;

: ROW ( -- )
   SITE-N @ OPEN-BASE ! ;

: ;ROW ( n n n n n -- ) {: role:n woff:n loff:n words:n rc:n :}
   ROW-N @ ROW-CAP >= if E-CRL-ROW throw then
   role ROW-ROLE ROW-N @ cells + !
   woff ROW-WOFF ROW-N @ cells + !
   loff ROW-LOFF ROW-N @ cells + !
   words ROW-WORDS ROW-N @ cells + !
   rc ROW-RC ROW-N @ cells + !
   OPEN-BASE @ ROW-BASE ROW-N @ cells + !
   SITE-N @ OPEN-BASE @ - ROW-LEN ROW-N @ cells + !
   ROW-N @ 1+ ROW-N ! ;

\ ---- the call vector rows ----------------------------------------------------
\ Read a block as: open, list every word of the region, close with the role, the
\ region offset above __text the writing run got, the one the restoring run got,
\ the region's length in words, and the exit status the writer's pass must
\ report.
\
\ Every displacement below is a frozen literal, worked out once from the
\ geometry the row names and never recomputed by either side. For a site at word
\ index i, a run whose region sits `off` bytes above __text, and a callee `c`
\ bytes above __text, the displacement in instruction units is (c - off - 4i)/4;
\ the canonical one puts REGION-OFF ($1000000) where `off` was.

\ Same base both runs, which is the ordinary case: a machine restoring an image
\ it wrote itself. The region comes back byte for byte, including the two words
\ the map does not record - an inline word whose bits spell a call, and a small
\ integer - and the region-internal call at word five.
\ Callees: word 0 calls __text+$2000, word 3 calls __text+$100.
: IDENTITY-ROW ( -- )
   ROW
      0 -5240832 -4192256 -5240832 BL-SITE
      1 0 RAW-KEEP
      2 0 RAW-KEEP
      3 -5242819 -4194243 -5242819 BL-SITE
      4 0 RAW-KEEP
      5 7 BL-KEEP
      6 2550136831 RAW-KEEP
      7 5 RAW-KEEP
   ROLE-IDENTITY $1400000 $1400000 8 0 ;ROW ;

\ The writing run got the canonical offset itself, so its pass moves nothing;
\ the restoring run's region sits four mebibytes higher, so the loader's pass
\ does all the work. Callees: word 0 calls __text+$2000, word 1 calls __text+0.
: REBASE-UP-ROW ( -- )
   ROW
      0 -4192256 -4192256 -5240832 BL-SITE
      1 -4194305 -4194305 -5242881 BL-SITE
      2 0 RAW-KEEP
      3 0 RAW-KEEP
   ROLE-REBASE-UP $1000000 $1400000 4 0 ;ROW ;

\ The restoring run's region sits four mebibytes LOWER than the writing run's,
\ so both deltas are positive where the row above had one of each sign.
\ Callees: word 0 calls __text+$2000, word 2 calls __text+$3FFC.
: REBASE-DOWN-ROW ( -- )
   ROW
      0 -5240832 -4192256 -3143680 BL-SITE
      1 0 RAW-KEEP
      2 -5238787 -4190211 -3141635 BL-SITE
      3 0 RAW-KEEP
   ROLE-REBASE-DOWN $1400000 $C00000 4 0 ;ROW ;

\ A third writing base, the same two callees as REBASE-UP-ROW, and therefore the
\ same canonical words: -4192256 and -4194305 again. That is the whole
\ portability claim - an image is byte-identical whatever base its writing run
\ happened to get - and the cases file compares the two canonical images
\ directly rather than leaving the coincidence to a reader.
: BASE-FREE-ROW ( -- )
   ROW
      0 -3143680 -4192256 -4192256 BL-SITE
      1 -3145729 -4194305 -4194305 BL-SITE
      2 0 RAW-KEEP
      3 0 RAW-KEEP
   ROLE-BASE-FREE $C00000 $1000000 4 0 ;ROW ;

\ A recorded site holding a small integer instead of a call: the region bytes
\ and the call map came from different builds, or one of them is damaged.
\ Relocating it would write a wild branch into live code, so the pass refuses
\ with CALLMAP-RC. Word zero was already rewritten when the refusal happened and
\ stays rewritten; word one keeps its bytes, and word two is never reached.
: REFUSE-ROW ( -- )
   ROW
      0 -5240832 -4192256 -4192256 BL-SITE
      1 5 RAW-SITE
      2 -5240832 -5240832 -5240832 BL-SITE
   ROLE-REFUSE $1400000 $1400000 3 95 ;ROW ;

: BUILD-CALL-ROWS ( -- )
   0 SITE-N !
   0 ROW-N !
   IDENTITY-ROW
   REBASE-UP-ROW
   REBASE-DOWN-ROW
   BASE-FREE-ROW
   REFUSE-ROW ;

\ ---- address-cell table builders ---------------------------------------------

: CELL+ ( n n n -- ) {: v0:n v1:n v2:n :}
   CELL-N @ CELL-CAP >= if E-CRL-ROW throw then
   v0 CELL-V0 CELL-N @ cells + !
   v1 CELL-V1 CELL-N @ cells + !
   v2 CELL-V2 CELL-N @ cells + !
   CELL-N @ 1+ CELL-N ! ;

: XROW ( -- )
   CELL-N @ XOPEN-BASE ! ;

: ;XROW ( n n n -- ) {: role:n dbw:n dbl:n :}
   XROW-N @ ROW-CAP >= if E-CRL-ROW throw then
   role XROW-ROLE XROW-N @ cells + !
   dbw XROW-DBW XROW-N @ cells + !
   dbl XROW-DBL XROW-N @ cells + !
   XOPEN-BASE @ XROW-BASE XROW-N @ cells + !
   CELL-N @ XOPEN-BASE @ - XROW-LEN XROW-N @ cells + !
   XROW-N @ 1+ XROW-N ! ;

\ ---- the address-cell vector rows --------------------------------------------
\ A declared cell holds an execution token inside the JIT region, or zero when
\ nothing is installed there yet. The writer folds it onto the RBASE-VA sentinel
\ ($300000000) and the loader folds it back onto the region this run got. Zero
\ is skipped by both halves, which is why a cleared hook survives a snapshot as
\ a cleared hook.
\
\ The two region bases below are the ones measured under lldb when the stale
\ cell was found: the writing run had its region at $103550000 and the restoring
\ run at $105A1DD30's region base $105A1D000.

: XT-IDENTITY-ROW ( -- )
   XROW
      0 0 0 CELL+
      $103550100 $300000100 $103550100 CELL+
      $103550000 $300000000 $103550000 CELL+
   ROLE-XT-IDENTITY $103550000 $103550000 ;XROW ;

: XT-REBASE-ROW ( -- )
   XROW
      0 0 0 CELL+
      $103550100 $300000100 $105A1D100 CELL+
      $103D50000 $300800000 $10621D000 CELL+
   ROLE-XT-REBASE $103550000 $105A1D000 ;XROW ;

: BUILD-XT-ROWS ( -- )
   0 CELL-N !
   0 XROW-N !
   XT-IDENTITY-ROW
   XT-REBASE-ROW ;

\ ---- address-literal chain table builders ------------------------------------
\ One slot: the word index its chain starts at, whether the address-literal map
\ records it, which of its four words is damaged (CHAIN-WHOLE when none), and
\ the address the chain carries before the writer's pass, after it, and after
\ the loader's. A row lists EVERY slot of its region, in order.

: ASITE+ ( n n n n n n -- ) {: idx:n rec:n bad:n v0:n v1:n v2:n :}
   ASITE-N @ SITE-CAP >= if E-CRL-ROW throw then
   idx ASITE-IDX ASITE-N @ cells + !
   rec ASITE-REC ASITE-N @ cells + !
   bad ASITE-BAD ASITE-N @ cells + !
   v0 ASITE-V0 ASITE-N @ cells + !
   v1 ASITE-V1 ASITE-N @ cells + !
   v2 ASITE-V2 ASITE-N @ cells + !
   ASITE-N @ 1+ ASITE-N ! ;

\ A recorded chain: the map bit is set, and the two passes move its address.
: CHAIN-SITE ( n n n n -- ) {: idx:n v0:n v1:n v2:n :}
   idx 1 CHAIN-WHOLE v0 v1 v2 ASITE+ ;

\ A chain the map does not record: one of the sibling DATA literals, which have
\ this exact shape and must come through untouched.
: CHAIN-KEEP ( n n -- ) {: idx:n v:n :}
   idx 0 CHAIN-WHOLE v v v ASITE+ ;

\ Four words the map does not record whose first word is a small integer: the
\ inline data a compiled word carries.
: CHAIN-DATA-KEEP ( n n -- ) {: idx:n v:n :}
   idx 0 0 v v v ASITE+ ;

\ A slot the map DOES record with one word damaged: the corrupt-image case.
: CHAIN-BROKEN ( n n n -- ) {: idx:n bad:n v:n :}
   idx 1 bad v v v ASITE+ ;

: AROW ( -- )
   ASITE-N @ AOPEN-BASE ! ;

: ;AROW ( n n n n n n n -- )
   {: role:n wb:n cb:n lb:n blen:n words:n rc:n :}
   AROW-N @ ROW-CAP >= if E-CRL-ROW throw then
   role AROW-ROLE AROW-N @ cells + !
   wb AROW-WB AROW-N @ cells + !
   cb AROW-CB AROW-N @ cells + !
   lb AROW-LB AROW-N @ cells + !
   blen AROW-BLEN AROW-N @ cells + !
   words AROW-WORDS AROW-N @ cells + !
   rc AROW-RC AROW-N @ cells + !
   AOPEN-BASE @ AROW-BASE AROW-N @ cells + !
   ASITE-N @ AOPEN-BASE @ - AROW-LEN AROW-N @ cells + !
   AROW-N @ 1+ AROW-N ! ;

\ ---- the address-literal chain vector rows -----------------------------------
\ Read a block as: open, list every slot of the region, close with the role, the
\ band base the writing run's region got, the canonical band base the image
\ carries, the band base the restoring run got, the band's length, the region's
\ length in words, and the exit status the writer's pass must report.
\
\ The pass is called once per band with (base, length, target). The writer's leg
\ is (WB, LEN, CB) and the loader's is (CB, LEN, LB), which is how the engine
\ calls it: once for the JIT region and once for the engine's loaded __text.
\ Every address below is a frozen literal. The band is REGION ($800000) long,
\ the canonical base is the RBASE-VA sentinel ($300000000), and the two live
\ bases are ordinary region bases.

\ Same band base both runs: the region comes back word for word, including the
\ unrecorded chain at slot one - a sibling DATA literal of the identical shape -
\ and the unrecorded inline data at slot three.
: CHAIN-IDENTITY-ROW ( -- )
   AROW
      0  $101000100 $300000100 $101000100 CHAIN-SITE
      4  $101000200 CHAIN-KEEP
      8  $1017FFFF0 $3007FFFF0 $1017FFFF0 CHAIN-SITE
      12 $101000300 CHAIN-DATA-KEEP
   ROLE-CH-IDENTITY $101000000 $300000000 $101000000 $800000 16 0 ;AROW ;

\ The restoring run's region landed four and a bit mebibytes higher, so every
\ recorded chain comes back naming the same word at the new base. Slot one sits
\ at the last address the band contains, which is where an off-by-one in the
\ band test would show.
: CHAIN-REBASE-ROW ( -- )
   AROW
      0 $101000100 $300000100 $105000100 CHAIN-SITE
      4 $1017FFFF0 $3007FFFF0 $1057FFFF0 CHAIN-SITE
   ROLE-CH-REBASE $101000000 $300000000 $105000000 $800000 8 0 ;AROW ;

\ A recorded chain naming an address OUTSIDE the band this call is moving: a
\ `[']` or a `postpone` that names a PRIMITIVE, whose code is in the engine's
\ loaded __text. The region band's call must leave it exactly as it is, for the
\ text band's call to move; slot one is in the band and moves, so the row cannot
\ pass by the pass doing nothing at all.
: CHAIN-OTHER-BAND-ROW ( -- )
   AROW
      0 $1000A0 $1000A0 $1000A0 CHAIN-SITE
      4 $101000100 $300000100 $105000100 CHAIN-SITE
   ROLE-CH-OTHER $101000000 $300000000 $105000000 $800000 8 0 ;AROW ;

\ A recorded slot whose FOURTH word is not a MOVK3: the region bytes and the
\ address map came from different builds, or one of them is damaged. Rewriting
\ it would put four wild immediates into live code, so the pass refuses with
\ ADDRMAP-RC. Slot zero was already rewritten when the refusal happened and
\ stays rewritten; slot one keeps its words, and slot two is never reached.
: CHAIN-REFUSE-ROW ( -- )
   AROW
      0 $101000100 $300000100 $300000100 CHAIN-SITE
      4 3 $101000200 CHAIN-BROKEN
      8 $101000300 $101000300 $101000300 CHAIN-SITE
   ROLE-CH-REFUSE $101000000 $300000000 $101000000 $800000 12 97 ;AROW ;

\ Every band base the engine actually hands the pass is 64 KiB aligned, and the
\ canonical sentinel is too, so in an ordinary image the lowest immediate never
\ changes and the highest is zero on both sides. That is a fact about the
\ CALLER, not about this arithmetic: the chain is four instructions long
\ because an address can use all sixty-four bits, and the pass rewrites all four
\ unconditionally. This row moves a band whose bases differ in every one of the
\ four sixteen-bit fields, so a pass that skipped or skewed any single one of
\ them is caught. Measured: without this row, deleting the fourth immediate's
\ write from the shipped pass left the gate green.
: CHAIN-WIDE-ROW ( -- )
   AROW
      0 $23456789ACE34 $59BDEF0124634 $7111122224534 CHAIN-SITE
   ROLE-CH-WIDE $23456789ABC00 $59BDEF0123400 $7111122223300 $10000 4 0 ;AROW ;

: BUILD-CHAIN-ROWS ( -- )
   0 ASITE-N !
   0 AROW-N !
   CHAIN-IDENTITY-ROW
   CHAIN-REBASE-ROW
   CHAIN-OTHER-BAND-ROW
   CHAIN-REFUSE-ROW
   CHAIN-WIDE-ROW ;

BUILD-CALL-ROWS
BUILD-XT-ROWS
BUILD-CHAIN-ROWS

public

: ROWS ( -- n )             ROW-N @ ;
: SITES ( -- n )            SITE-N @ ;

: ROW-ROLE@ ( n -- n )      dup ROW-RANGE cells ROW-ROLE + @ ;
: ROW-WOFF@ ( n -- n )      dup ROW-RANGE cells ROW-WOFF + @ ;
: ROW-LOFF@ ( n -- n )      dup ROW-RANGE cells ROW-LOFF + @ ;
: ROW-WORDS@ ( n -- n )     dup ROW-RANGE cells ROW-WORDS + @ ;
: ROW-RC@ ( n -- n )        dup ROW-RANGE cells ROW-RC + @ ;
: ROW-BASE@ ( n -- n )      dup ROW-RANGE cells ROW-BASE + @ ;
: ROW-LEN@ ( n -- n )       dup ROW-RANGE cells ROW-LEN + @ ;

: SITE-IDX@ ( n -- n )      dup SITE-RANGE cells SITE-IDX + @ ;
: SITE-REC@ ( n -- n )      dup SITE-RANGE cells SITE-REC + @ ;
: SITE-KIND@ ( n -- n )     dup SITE-RANGE cells SITE-KIND + @ ;
: SITE-V0@ ( n -- n )       dup SITE-RANGE cells SITE-V0 + @ ;
: SITE-V1@ ( n -- n )       dup SITE-RANGE cells SITE-V1 + @ ;
: SITE-V2@ ( n -- n )       dup SITE-RANGE cells SITE-V2 + @ ;

: XROWS ( -- n )            XROW-N @ ;
: CELLS# ( -- n )           CELL-N @ ;

: XROW-ROLE@ ( n -- n )     dup XROW-RANGE cells XROW-ROLE + @ ;
: XROW-DBW@ ( n -- n )      dup XROW-RANGE cells XROW-DBW + @ ;
: XROW-DBL@ ( n -- n )      dup XROW-RANGE cells XROW-DBL + @ ;
: XROW-BASE@ ( n -- n )     dup XROW-RANGE cells XROW-BASE + @ ;
: XROW-LEN@ ( n -- n )      dup XROW-RANGE cells XROW-LEN + @ ;

: CELL-V0@ ( n -- n )       dup CELL-RANGE cells CELL-V0 + @ ;
: CELL-V1@ ( n -- n )       dup CELL-RANGE cells CELL-V1 + @ ;
: CELL-V2@ ( n -- n )       dup CELL-RANGE cells CELL-V2 + @ ;

: AROWS ( -- n )            AROW-N @ ;
: ASITES ( -- n )           ASITE-N @ ;

: AROW-ROLE@ ( n -- n )     dup AROW-RANGE cells AROW-ROLE + @ ;
: AROW-WB@ ( n -- n )       dup AROW-RANGE cells AROW-WB + @ ;
: AROW-CB@ ( n -- n )       dup AROW-RANGE cells AROW-CB + @ ;
: AROW-LB@ ( n -- n )       dup AROW-RANGE cells AROW-LB + @ ;
: AROW-BLEN@ ( n -- n )     dup AROW-RANGE cells AROW-BLEN + @ ;
: AROW-WORDS@ ( n -- n )    dup AROW-RANGE cells AROW-WORDS + @ ;
: AROW-RC@ ( n -- n )       dup AROW-RANGE cells AROW-RC + @ ;
: AROW-BASE@ ( n -- n )     dup AROW-RANGE cells AROW-BASE + @ ;
: AROW-LEN@ ( n -- n )      dup AROW-RANGE cells AROW-LEN + @ ;

: ASITE-IDX@ ( n -- n )     dup ASITE-RANGE cells ASITE-IDX + @ ;
: ASITE-REC@ ( n -- n )     dup ASITE-RANGE cells ASITE-REC + @ ;
: ASITE-BAD@ ( n -- n )     dup ASITE-RANGE cells ASITE-BAD + @ ;
: ASITE-V0@ ( n -- n )      dup ASITE-RANGE cells ASITE-V0 + @ ;
: ASITE-V1@ ( n -- n )      dup ASITE-RANGE cells ASITE-V1 + @ ;
: ASITE-V2@ ( n -- n )      dup ASITE-RANGE cells ASITE-V2 + @ ;

;package
