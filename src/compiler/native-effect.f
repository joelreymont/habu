\ native-effect.f - the typed machine-state contract of one emitted routine:
\ per register file what it reads, returns in and destroys, its condition flags,
\ link register, frame and net stack-pointer change, calls and syscalls, and how
\ control leaves. The A64IR dialect composes it with its operand records;
\ tools/lint/clobber-lint.f still answers from hand tables.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/native/machine.f

package NEFF
public

\ ---- register sets -----------------------------------------------------------
\ Bit i names register i; the nominal type keeps the files apart. The destroyed
\ set is total and the preserved set derived, so the two cannot disagree; the
\ link register and register 31 are not members of the general sets.

STRUCTURE gprs DERIVE eq
   FIELD bits n
;STRUCTURE

STRUCTURE fprs DERIVE eq
   FIELD bits n
;STRUCTURE

\ ---- an ordered place list ---------------------------------------------------
\ Arguments and results are ordered place lists (a register or a data-stack
\ slot), because a convention is positional; the register sets derive from them
\ (SEQ-SET). Floating values travel in data-stack slots, so the floating file
\ keeps sets only.
\ Element i is the place at position i and the length rides above them. One cell,
\ because a contract field cannot be multi-cell. A place is a kind bit over a
\ five-bit payload (the register operand field), so a cell holds ten positions.
\ The kind rides in the element so one list cannot disagree with a second, and
\ every bit past the last position is zero so a list has exactly one cell.
STRUCTURE placeseq DERIVE eq
   FIELD bits n
;STRUCTURE

\ ---- which convention the two place lists are stated in -----------------------
\ Declared, never inferred: an empty list means either "passes nothing" or "no
\ opinion", and a ( -- ) routine has an empty list under both conventions.
\   dstack   - arguments and results in caller data-stack slots; a Habu word's
\              convention (src/compiler/native/abi.f).
\   register - arguments and results in registers.

ENUM conv DERIVE eq
   dstack
   register
;ENUM

\ ---- the condition flags -----------------------------------------------------
\ NZCV roles are named combinations so no impossible state is spellable.
\   untouched      - neither read nor written
\   clobbered      - written, meaningless to the caller
\   delivered      - written, the caller may branch on it
\   read-preserved - read at entry and left as found
\   read-clobbered - read at entry and then destroyed
\ Not `result`: a variant name becomes a constructor word, and `result` collides
\ with src/lib/adt/result.f's family (E-TDECL-NAME, 7110).
ENUM nzcv DERIVE eq
   untouched
   clobbered
   delivered
   read-preserved
   read-clobbered
;ENUM

\ ---- the link register -------------------------------------------------------
\ Whether the link register holds the caller's return address where control
\ leaves.
\   absent - the machine has no link register; such a contract may declare
\            nothing else, and one over a machine with one may not declare it.
ENUM link DERIVE eq
   preserved
   clobbered
   absent
;ENUM

\ ---- how control leaves ------------------------------------------------------
\   returns   - back to the caller, through the Ret form
\   tail-call - by branching to another routine, which returns to our caller
\   no-return - control never comes back: a trap, an exit, or a handler return
ENUM control DERIVE eq
   returns
   tail-call
   no-return
;ENUM

\ ---- what the routine does besides compute -----------------------------------
\ Each bit is named for the instruction form that creates it.
STRUCTURE traits DERIVE eq
   FIELD bits n
;STRUCTURE

\ ---- the routine contract ----------------------------------------------------
\ Fields in constructor order; VALIDATE is UNMAKE into ROUTINE, so the order is
\ fixed. No DERIVE eq: a structure field is not a derivable role. `conv` says how
\ to read the two lists after it. `frame` is how far below entry SP the frame
\ reaches, in bytes; `sp-delta` is the net SP change at exit, zero or negative.
\ Machine facts (file size, link register, SP operand, alignment, frame reach)
\ come from the `mach` field (src/compiler/native/machine.f). `routine` and
\ `placeseq` are public, so their generated MAKE can forge values: VALIDATE, the
\ writable sets, CHECK-SLOT and the list readers revalidate; plain field readers
\ do not. The record is flat because a multi-cell value cannot be bound to a
\ typed local. Not modelled: the syscall register convention (target contract),
\ signal entry state, and what lives in a frame slot (A64IR).
STRUCTURE routine
   FIELD conv conv
   FIELD gpr-arg placeseq
   FIELD gpr-out placeseq
   FIELD gpr-clobber gprs
   FIELD fpr-live-in fprs
   FIELD fpr-result fprs
   FIELD fpr-clobber fprs
   FIELD nzcv nzcv
   FIELD link link
   FIELD control control
   FIELD traits traits
   FIELD frame n
   FIELD sp-delta n
   FIELD mach NMACH:mach
;STRUCTURE

private

\ ---- what this schema can represent, whatever the machine ---------------------
\ The one representation bound: a place list position carries a five-bit
\ payload, so registers above 31 cannot appear in a convention.

5 constant REG-BITS       \ the payload of one position of a place list

\ ---- how an ordered place list is packed --------------------------------------
\ Positions from the bottom of the cell, six bits each; the length in the bits
\ left at the top.
CELL 8 * constant SEQ-BITS               \ bits in the one cell a list occupies
4 constant SEQ-LEN-BITS                  \ the length, above the last position
REG-BITS 1+ constant PLACE-BITS          \ a payload and the bit that says what it is
SEQ-BITS SEQ-LEN-BITS - constant SEQ-LEN-SHIFT
SEQ-LEN-SHIFT PLACE-BITS / constant SEQ-MAX-N   \ positions one cell holds
1 REG-BITS lshift 1 - constant PAY-MASK  \ the payload: a register number, or a data-stack slot index
1 REG-BITS lshift constant KIND-BIT      \ set on a data-stack slot, clear on a register
1 PLACE-BITS lshift 1 - constant PLACE-MASK
1 SEQ-LEN-BITS lshift 1 - constant SEQ-LEN-MASK
\ A contiguous data-stack interface stores only its length, under a reserved
\ length tag.
SEQ-LEN-MASK SEQ-LEN-SHIFT lshift constant SEQ-RANGE-TAG
$FFFFFFFF constant SEQ-RANGE-MASK       \ u32 count; the remaining payload bits are reserved

\ ---- trait bits --------------------------------------------------------------
$1 constant BIT-CALL       \ contains a direct call: the Bl form
$2 constant BIT-INDIRECT   \ contains an indirect call: the Blr form
$4 constant BIT-SYSCALL    \ enters the kernel: the Svc form

BIT-CALL BIT-INDIRECT or BIT-SYSCALL or constant BIT-ALL

: MK-G ( n -- NEFF:gprs )       NEFF-GPRS:MAKE ;
: G-BITS ( NEFF:gprs -- n )     NEFF-GPRS:UNMAKE ;
: MK-F ( n -- NEFF:fprs )       NEFF-FPRS:MAKE ;
: F-BITS ( NEFF:fprs -- n )     NEFF-FPRS:UNMAKE ;
: MK-T ( n -- NEFF:traits )     NEFF-TRAITS:MAKE ;
: T-BITS ( NEFF:traits -- n )   NEFF-TRAITS:UNMAKE ;
: MK-S ( n -- NEFF:placeseq )     NEFF-PLACESEQ:MAKE ;
: S-BITS ( NEFF:placeseq -- n )   NEFF-PLACESEQ:UNMAKE ;

\ ---- per-field rules ---------------------------------------------------------

\ The sign bit names no register. Which registers a routine may name is the
\ machine's answer, asked in ROUTINE.
: GPR-CK ( n -- n )
   dup 0 < if E-NEFF-GPR throw then ;

: FPR-CK ( n -- n )
   dup 0 < if E-NEFF-FPR throw then ;

\ Refuses any register the VM, the link register or the stack pointer took.
: GPR-FIT ( n n -- )
   {: bits:n alloc:n :}
   bits alloc invert and 0<> if E-NEFF-GPR throw then ;

: FPR-FIT ( n n -- )
   {: bits:n alloc:n :}
   bits alloc invert and 0<> if E-NEFF-FPR throw then ;

: TRAIT-CK ( n -- n )
   dup BIT-ALL invert and 0<> if E-NEFF-TRAIT throw then ;

: REG-CK ( n -- n )
   dup 0 < over PAY-MASK > or if E-NEFF-GPR throw then ;

\ ---- the rules of an ordered place list --------------------------------------
\ Nothing below is public: the public readers validate the whole list first.
: SEQ-RANGE? ( n -- bool )
   SEQ-RANGE-TAG and SEQ-RANGE-TAG = ;

: SEQ-LEN-OF ( n -- n )
   dup SEQ-RANGE? if SEQ-RANGE-MASK and exit then
   SEQ-LEN-SHIFT rshift SEQ-LEN-MASK and ;

\ Kind and payload together, so a register and a slot with one number differ.
: SEQ-AT ( n n -- n )
   {: w:n p:n :}
   w p PLACE-BITS * rshift PLACE-MASK and ;

: PLACE-SLOT? ( n -- bool )
   KIND-BIT and 0<> ;

: PLACE-PAY ( n -- n )
   PAY-MASK and ;

: SEQ-SLOT-CK ( n -- )
   dup 0 < over PAY-MASK > or if E-NEFF-SEQ throw then drop ;

: SEQ-PLACE-CK ( n -- )
   dup PLACE-SLOT? if PLACE-PAY SEQ-SLOT-CK exit then
   PLACE-PAY REG-CK drop ;

: SEQ-REPEATS? ( n n -- bool )
   {: w:n p:n :}
   false
   p 0 ?do
      w i SEQ-AT  w p SEQ-AT  = if drop true leave then
   loop ;

\ No place twice, and nothing past the last position, so the packing is
\ canonical: two lists are equal exactly when their cells are.
: SEQ-CK ( n -- n )
   dup {: w:n :}
   w SEQ-LEN-OF {: len:n :}
   w SEQ-RANGE? if
      w SEQ-RANGE-TAG invert and SEQ-RANGE-MASK > if E-NEFF-SEQ throw then
      len SEQ-MAX-N <= if E-NEFF-SEQ throw then
      exit
   then
   len SEQ-MAX-N > if E-NEFF-SEQ throw then
   len 0 ?do
      w i SEQ-AT SEQ-PLACE-CK
      w i SEQ-REPEATS? if E-NEFF-SEQ throw then
   loop
   SEQ-MAX-N len ?do
      w i SEQ-AT 0<> if E-NEFF-SEQ throw then
   loop ;

\ Data-stack slots contribute nothing.
: SEQ-MASK ( n -- n )
   dup SEQ-RANGE? if drop 0 exit then
   dup SEQ-LEN-OF {: w:n len:n :}
   0
   len 0 ?do
      w i SEQ-AT dup PLACE-SLOT? if drop else PLACE-PAY 1 swap lshift or then
   loop ;

\ Strictly between zero and the length means mixed kinds, which no pass lowers.
: SEQ-SLOTS-OF ( n -- n )
   dup SEQ-RANGE? if SEQ-LEN-OF exit then
   dup SEQ-LEN-OF {: w:n len:n :}
   0
   len 0 ?do
      w i SEQ-AT PLACE-SLOT? if 1+ then
   loop ;

: ROLE-CK ( n n -- )
   {: res:n clob:n :}
   res clob and 0<> if E-NEFF-ROLE throw then ;

: FRAME-CK ( n NMACH:mach -- )
   {: size:n m:NMACH:mach :}
   size 0 < size m NMACH:FRAME-MAX > or if E-NEFF-FRAME throw then
   size m NMACH:ALIGNED? 0= if E-NEFF-FRAME throw then ;

: DELTA-CK ( n n NMACH:mach -- )
   {: size:n delta:n m:NMACH:mach :}
   delta 0 > if E-NEFF-SP throw then
   delta m NMACH:ALIGNED? 0= if E-NEFF-SP throw then
   delta size negate < if E-NEFF-SP throw then ;

\ ---- whole-contract rules ----------------------------------------------------

: RETURNING? ( NEFF:control -- bool )
   MATCH control
      returns   OF true ENDOF
      tail-call OF true ENDOF
      no-return OF false ENDOF
   ;MATCH ;

: BALANCE-CK ( NEFF:control n -- )
   {: c:control delta:n :}
   c RETURNING? 0= if exit then
   delta 0<> if E-NEFF-SP throw then ;

\ A list's place kinds must match the declared convention: the kinds are reached
\ by different instructions and no pass pairs them. An empty list satisfies
\ either.
: SIDE-CK ( NEFF:conv NEFF:placeseq -- )
   {: cv:conv s:placeseq :}
   s S-BITS SEQ-CK {: w:n :}
   w SEQ-SLOTS-OF {: sl:n :}
   cv NEFF-CONV:DSTACK NEFF-CONV:EQ if
      sl w SEQ-LEN-OF <> if E-NEFF-CONV throw then exit
   then
   sl 0<> if E-NEFF-CONV throw then ;

: CONV-CK ( NEFF:conv NEFF:placeseq NEFF:placeseq -- )
   {: cv:conv gi:placeseq gr:placeseq :}
   cv gi SIDE-CK
   cv gr SIDE-CK ;

\ A return and a tail call both end through the link register. Without one,
\ `absent` is the only answer: the return address is in the frame and BALANCE-CK
\ keeps it reachable.
: LINK-CK ( NEFF:control NEFF:link NMACH:mach -- )
   {: c:control l:link m:NMACH:mach :}
   l NEFF-LINK:ABSENT NEFF-LINK:EQ {: none:bool :}
   m NMACH:LINK? 0= if
      none 0= if E-NEFF-LINK throw then exit
   then
   none if E-NEFF-LINK throw then
   c RETURNING? 0= if exit then
   l NEFF-LINK:CLOBBERED NEFF-LINK:EQ if E-NEFF-LINK throw then ;

: RESULT-CK ( n n NEFF:nzcv NEFF:control -- )
   {: gres:n fres:n z:nzcv c:control :}
   c RETURNING? if exit then
   gres 0<> fres 0<> or
   z NEFF-NZCV:DELIVERED NEFF-NZCV:EQ or
   if E-NEFF-CONTROL throw then ;

public

\ ---- the machine a consumer has to agree with ---------------------------------
\ Machine facts are read from NMACH or src/arch/<arch>/machine.f; a contract
\ only answers which machine it is over.

\ ---- register sets -----------------------------------------------------------
: GPR-SET ( n -- NEFF:gprs )    GPR-CK MK-G ;
: GPRS-N ( NEFF:gprs -- n )     G-BITS ;
: FPR-SET ( n -- NEFF:fprs )    FPR-CK MK-F ;
: FPRS-N ( NEFF:fprs -- n )     F-BITS ;

: GPR-NONE ( -- NEFF:gprs )     0 MK-G ;
: FPR-NONE ( -- NEFF:fprs )     0 MK-F ;

\ The only set whose members depend on the machine: the file less what the VM,
\ the link register and the stack pointer took.
: GPR-ALL ( NMACH:mach -- NEFF:gprs )
   NMACH:GPR-ALLOCATABLE MK-G ;

: FPR-ALL ( NMACH:mach -- NEFF:fprs )
   NMACH:FPR-ALLOCATABLE MK-F ;

: GPR-REG ( n -- NEFF:gprs )    REG-CK 1 swap lshift GPR-CK MK-G ;

: GPR-WITH ( NEFF:gprs NEFF:gprs -- NEFF:gprs )
   G-BITS swap G-BITS or GPR-CK MK-G ;

: GPR-WITHOUT ( NEFF:gprs NEFF:gprs -- NEFF:gprs )
   {: set:gprs less:gprs :}
   set G-BITS GPR-CK less G-BITS GPR-CK invert and MK-G ;

\ ---- ordered place lists ------------------------------------------------------
\ "Takes or returns nothing" and "no opinion" are the same statement here.
: SEQ-NONE ( -- NEFF:placeseq )   0 MK-S ;

: SEQ-LIMIT ( -- n )              SEQ-MAX-N ;

private

\ A repeated place is refused: no caller can put two values in one place.
: SEQ-PUT ( NEFF:placeseq n -- NEFF:placeseq )
   {: s:placeseq e:n :}
   s S-BITS SEQ-CK {: w:n :}
   w SEQ-LEN-OF {: len:n :}
   len SEQ-MAX-N >= if E-NEFF-SEQ throw then
   e SEQ-PLACE-CK
   w  e len PLACE-BITS * lshift or  1 SEQ-LEN-SHIFT lshift +
   SEQ-CK MK-S ;

public

: SEQ-WITH ( NEFF:placeseq n -- NEFF:placeseq )
   REG-CK SEQ-PUT ;

\ The Habu convention: argument i arrives in slot i, result j leaves in slot j.
: SEQ-WITH-SLOT ( NEFF:placeseq n -- NEFF:placeseq )
   dup 0 < over PAY-MASK > or if E-NEFF-SEQ throw then
   KIND-BIT or SEQ-PUT ;

\ Short ranges keep the packed list; longer ones use SEQ-RANGE-TAG.
: SEQ-DSTACK ( n -- NEFF:placeseq ) {: n:n :}
   n 0 < n SEQ-RANGE-MASK > or if E-NEFF-SEQ throw then
   n SEQ-MAX-N > if n SEQ-RANGE-TAG or MK-S exit then
   SEQ-NONE
   n 0 ?do i SEQ-WITH-SLOT loop ;

: SEQ-LEN ( NEFF:placeseq -- n )
   S-BITS SEQ-CK SEQ-LEN-OF ;

private

: SEQ-POSITION ( NEFF:placeseq n -- n n )
   {: s:placeseq p:n :}
   s S-BITS SEQ-CK {: w:n :}
   p 0 < p w SEQ-LEN-OF >= or if E-NEFF-SEQ throw then
   w p ;

public

\ A slot position is refused rather than read as a register number.
: SEQ-REG@ ( NEFF:placeseq n -- n )
   SEQ-POSITION {: w:n p:n :}
   w SEQ-RANGE? if E-NEFF-KIND throw then
   w p SEQ-AT dup PLACE-SLOT? if E-NEFF-KIND throw then PLACE-PAY ;

: SEQ-SLOT@ ( NEFF:placeseq n -- n )
   SEQ-POSITION {: w:n p:n :}
   w SEQ-RANGE? if p exit then
   w p SEQ-AT dup PLACE-SLOT? 0= if E-NEFF-KIND throw then PLACE-PAY ;

: SEQ-SLOTS ( NEFF:placeseq -- n )
   S-BITS SEQ-CK SEQ-SLOTS-OF ;

: SEQ-SET ( NEFF:placeseq -- NEFF:gprs )
   S-BITS SEQ-CK SEQ-MASK MK-G ;

\ ---- traits ------------------------------------------------------------------
: TRAITS-N ( NEFF:traits -- n )    T-BITS ;
: TRAITS-NONE ( -- NEFF:traits )   0 MK-T ;
: T-CALL ( -- NEFF:traits )        BIT-CALL MK-T ;

: TRAITS-HAS? ( NEFF:traits NEFF:traits -- bool )
   {: set:traits probe:traits :}
   probe T-BITS TRAIT-CK {: want:n :}
   set T-BITS TRAIT-CK want and want = ;

\ ---- construction and validation ---------------------------------------------
\ An impossible combination throws a named error, and ROUTINE refuses any
\ register the machine reserves. The validator checks only that the fields can
\ hold together; whether the emitted code obeys the contract is the emitted
\ control-flow verifier's.
: ROUTINE ( NEFF:conv NEFF:placeseq NEFF:placeseq NEFF:gprs NEFF:fprs NEFF:fprs NEFF:fprs NEFF:nzcv NEFF:link NEFF:control NEFF:traits n n NMACH:mach -- NEFF:routine )
   {: cv:conv gi:placeseq gr:placeseq gc:gprs fi:fprs fr:fprs fc:fprs z:nzcv
      l:link c:control t:traits size:n delta:n m:NMACH:mach :}
   m NMACH:GPR-ALLOCATABLE {: ga:n :}
   m NMACH:FPR-ALLOCATABLE {: fa:n :}
   gi S-BITS SEQ-CK SEQ-MASK ga GPR-FIT
   gr S-BITS SEQ-CK SEQ-MASK {: res:n :}
   res ga GPR-FIT
   gc G-BITS GPR-CK {: clob:n :}
   clob ga GPR-FIT
   res clob ROLE-CK
   fi F-BITS FPR-CK fa FPR-FIT
   fr F-BITS FPR-CK {: fres:n :}
   fres fa FPR-FIT
   fc F-BITS FPR-CK {: fclob:n :}
   fclob fa FPR-FIT
   fres fclob ROLE-CK
   t T-BITS TRAIT-CK drop
   size m FRAME-CK
   size delta m DELTA-CK
   c delta BALANCE-CK
   c l m LINK-CK
   cv gi gr CONV-CK
   res fres z c RESULT-CK
   cv gi gr gc fi fr fc z l c t size delta m NEFF-ROUTINE:MAKE ;

: VALIDATE ( NEFF:routine -- NEFF:routine )
   NEFF-ROUTINE:UNMAKE ROUTINE ;

\ ---- field readers -----------------------------------------------------------
\ Projections; nothing here revalidates.
: CONV@ ( NEFF:routine -- NEFF:conv )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop drop drop ;

: ARGS@ ( NEFF:routine -- NEFF:placeseq )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop drop nip ;

: RESULTS@ ( NEFF:routine -- NEFF:placeseq )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop nip nip ;

: GPR-CLOBBER@ ( NEFF:routine -- NEFF:gprs )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop nip nip nip ;

: FPR-RESULT@ ( NEFF:routine -- NEFF:fprs )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop nip nip nip nip nip ;

: FPR-CLOBBER@ ( NEFF:routine -- NEFF:fprs )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop nip nip nip nip nip nip ;

: LINK@ ( NEFF:routine -- NEFF:link )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop nip nip nip nip nip nip nip nip ;

: CONTROL@ ( NEFF:routine -- NEFF:control )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop nip nip nip nip nip nip nip nip nip ;

: TRAITS@ ( NEFF:routine -- NEFF:traits )
   NEFF-ROUTINE:UNMAKE
   drop drop drop nip nip nip nip nip nip nip nip nip nip ;

: FRAME@ ( NEFF:routine -- n )
   NEFF-ROUTINE:UNMAKE
   drop drop nip nip nip nip nip nip nip nip nip nip nip ;

: DELTA@ ( NEFF:routine -- n )
   NEFF-ROUTINE:UNMAKE
   drop nip nip nip nip nip nip nip nip nip nip nip nip ;

: MACH@ ( NEFF:routine -- NMACH:mach )
   NEFF-ROUTINE:UNMAKE
   nip nip nip nip nip nip nip nip nip nip nip nip nip ;

\ ---- derived facts -----------------------------------------------------------
\ Destroyed plus returned: the set an allocator may hand out. Derived so it
\ cannot disagree with the two fields.
: GPR-WRITABLE ( NEFF:routine -- NEFF:gprs )
   VALIDATE {: r:routine :}
   r RESULTS@ S-BITS SEQ-MASK
   r GPR-CLOBBER@ G-BITS or MK-G ;

: FPR-WRITABLE ( NEFF:routine -- NEFF:fprs )
   VALIDATE {: r:routine :}
   r FPR-RESULT@ F-BITS
   r FPR-CLOBBER@ F-BITS or MK-F ;

\ ---- frame slots -------------------------------------------------------------
\ A byte offset from the frame's deepest point and an access width. Accepted only
\ if a load or store form reaches it: a carried width, an offset the scale
\ divides, inside the declared frame and inside the width's twelve-bit field.
: CHECK-SLOT ( n n NEFF:routine -- )
   VALIDATE {: off:n width:n r:routine :}
   r MACH@ {: m:NMACH:mach :}
   r FRAME@ {: size:n :}              \ the frame this slot is in
   width m NMACH:WIDTH-OK? 0= if E-NEFF-SLOT throw then
   off 0 < if E-NEFF-SLOT throw then
   off width mod 0<> if E-NEFF-SLOT throw then
   off width + size > if E-NEFF-SLOT throw then
   off  width m NMACH:SLOT-REACH  > if E-NEFF-SLOT throw then ;

;package
