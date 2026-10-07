\ native-defer.f - production compilation of `[: … ;] is FOO`.
require src/habu/address-cells.f
require lib/test.f

\ Tier 1 below: the deferred-target refusals asserted below are the optimizing
\ compiler's (2 rows fail at the default tier).
1 set-tier

\ A baked compiler must publish its source closure before a caller requires it.
T-RESET
s" src/compiler/native/compiler.f" ENGINE-PROVIDES? TTRUE
s" src/compiler/ir/id.f" ENGINE-PROVIDES? TTRUE
ndict@
require src/compiler/native/compiler.f
require src/compiler/ir/id.f
require src/compiler/native/compiler.f
ndict@ T=

package NDEFER-TEST

private

\ The dispatch cell's contents, and the engine's relocation table. Both are
\ reads of memory the dictionary named; the deciding above them is ordinary
\ checked Habu.
\ A dictionary-named DATA address is an integer; reading its cell needs a pointer.
CAST: N>CELL ( n -- ptr n )
: CELL@ ( n -- n ) N>CELL @ ;
: PCELL@ ( ptr n -- n ) @ ;
\ The DATA base as an integer: its distance from the null address.
: DBASE-N ( -- n ) data-base NULL-PTR BYTE-VIEW - ;

0 constant GLOBAL-WID

: REC ( ptr u8 n -- ptr n )
   GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if s" native-defer: record not found" 76 die then ;

: REC-START ( ptr u8 n -- n )   REC XREF-START ;
: REC-LEN ( ptr u8 n -- n )     REC XREF-LEN ;

\ ---- the engine's own table of declared address cells ------------------------
\ Read through src/habu/layout.f's published offsets, which is where the engine
\ writes them. test/snapshot-xt-cell-decl.f reads the same two numbers the same
\ way; both are readers of one authority rather than two copies of a rule.
: XT-COUNT ( -- n )
   data-base SNAP-RELOC:XTCELL-N-CELL + PCELL@ ;

: XT-ROW ( n -- n ) {: row:n :}
   row ADDRESS-CELLS:ROW@ ;

variable HIT

: XT-LISTED? ( n -- bool ) {: off:n :}
   0 HIT !
   XT-COUNT 0 ?do
      i XT-ROW off = if 1 HIT ! then
   loop
   HIT @ 0<> ;

\ ---- the words the fixtures bind ---------------------------------------------
\ Every execution below goes through `evaluate-closed`, or `TEST-EVAL:N` for a
\ value, so a caller of a word that did not exist when the suite was compiled is
\ compiled only after that new dictionary record exists.
\ The deferred word, the body it is bound to, and a compiled caller of it. The
\ caller exists because a defer is an engine trampoline: it proves a compiled
\ call reads the cell at run time rather than merely proving direct evaluation.
: SETUP ( -- )
   s" defer ND-HOOK ( n -- n )" evaluate-closed
   s" : ND-IMPL ( n -- n ) 1 + ;" evaluate-closed
   s" : ND-ACTION ( n -- n ) ND-HOOK ;" evaluate-closed ;

: DEF-INSTALL ( -- )
   s" : ND-INSTALL ( -- ) [: ND-IMPL ;] is ND-HOOK ;" evaluate-closed ;

\ The cell ND-HOOK dispatches through, asked of the same resolver the chain asks.
: HOOK-CELL ( -- n )
   s" ND-HOOK" NDICT:SPELL-DEFER-CELL ;

: BIND-CASE ( -- )
   SETUP
   DEF-INSTALL
   s" the installer is the chain's code" T-LABEL
   s" ND-INSTALL" REC-START 0 T<>
   s" and running it binds the deferred word to the body" T-LABEL
   s" ND-INSTALL" evaluate-closed
   s" 41 ND-HOOK" TEST-EVAL:N 42 T=
   s" which a compiled caller of the defer then reaches" T-LABEL
   s" 41 ND-ACTION" TEST-EVAL:N 42 T= ;

\ BIND-CASE proves the live behavior. These assertions prove the stored target
\ is the quotation emitted with the installer and that the cell is declared by
\ DATA offset, which is how the snapshot writer and loader find it. Neither fact
\ depends on whether lowering copies, calls, or tail-calls either source word.
: DECLARED-CASE ( -- )
   s" the cell now holds an address inside the installer's own emission" T-LABEL
   HOOK-CELL 0<> TTRUE
   HOOK-CELL CELL@ {: xt:n :}
   xt  s" ND-INSTALL" REC-START  >= TTRUE
   xt  s" ND-INSTALL" REC-START  s" ND-INSTALL" REC-LEN +  < TTRUE
   s" and the cell is in the engine's relocation table" T-LABEL
   HOOK-CELL DBASE-N - XT-LISTED? TTRUE ;

\ ---- the targets that are not deferred words ---------------------------------
\ THE HOSTILE ONE IS THE `create`d WORD. Its first data cell holds the defer
\ magic exactly, which is the value the trailer of a real defer starts with - so
\ a reader that searched for the magic near the record, or that simply took the
\ cell after a record's code as a dispatch cell, would bind a quotation into an
\ ordinary data word and the program would branch into whatever the next cell
\ held. What refuses it is that the trailer is read at the record's own
\ START+LEN, which for a created word is code space and not the data the word
\ names: measured, the two are in different regions entirely.
\
\ THE PLAIN COLON WORD IS THE SECOND HALF OF THE SAME CLAIM. Its trailer read
\ lands on the next definition's instructions - a perfectly ordinary integer -
\ and it answers absent for the same reason.
\
\ The native checker refuses these source programs before elaboration. The
\ resolver assertions separately prove why forged data and ordinary code are
\ not defer targets; the elaborator's own backstop is exercised on a hand-built
\ tape in test/compiler/native-elaborate.f.
: SETUP-BAD ( -- )
   s" create ND-DATA  $4842444546455201 ,  0 ," evaluate-closed
   s" : ND-PLAIN ( n -- n ) 2 * ;" evaluate-closed ;

: BAD-DATA ( -- )
   s" : ND-BAD1 ( -- ) [: ND-IMPL ;] is ND-DATA ;" evaluate-closed ;

: BAD-COLON ( -- )
   s" : ND-BAD2 ( -- ) [: ND-IMPL ;] is ND-PLAIN ;" evaluate-closed ;

: BAD-ABSENT ( -- )
   s" : ND-BAD3 ( -- ) [: ND-IMPL ;] is ND-NOWHERE ;" evaluate-closed ;

70 constant CHECK-RC                 \ the native checker refuses every invalid target

: REFUSE-CASE ( -- )
   SETUP-BAD
   s" the created word's first data cell really does hold the defer magic" T-LABEL
   s" ND-DATA" NDICT:FIXED-VALUE CELL@  DEFER-MAGIC  T=
   s" and the resolver still answers it no dispatch cell" T-LABEL
   s" ND-DATA" NDICT:SPELL-DEFER-CELL 0 T=
   s" an ordinary colon word answers none either" T-LABEL
   s" ND-PLAIN" NDICT:SPELL-DEFER-CELL 0 T=
   s" a name that denotes nothing answers none" T-LABEL
   s" ND-NOWHERE" NDICT:SPELL-DEFER-CELL 0 T=
   s" while the real deferred word answers one" T-LABEL
   s" ND-HOOK" NDICT:SPELL-DEFER-CELL 0<> TTRUE
   s" the native checker refuses non-deferred or absent targets before emission" T-LABEL
   [: BAD-DATA ;] CHECK-RC TTHROWSQ
   [: BAD-COLON ;] CHECK-RC TTHROWSQ
   [: BAD-ABSENT ;] CHECK-RC TTHROWSQ ;

public

: RUN ( -- )
   BIND-CASE
   DECLARED-CASE
   REFUSE-CASE
   T-REPORT ;

;package

NDEFER-TEST:RUN
