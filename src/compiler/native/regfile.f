\ regfile.f - one machine's register files as the register allocator asks of
\ them: each file's size, the registers the virtual machine reserved, those a
\ call destroys, and the spill-slot width. Reserved and destroyed are stored;
\ allocatable and callee-saved are derived, so no register is declared both.
\ What one routine may write is its contract's pool, which must be a subset of
\ the allocatable set. src/compiler/native/machine.f holds this in a machine.

require src/lib/prelude.f
require src/lib/errors.f

package NREGFILE
public

\ ---- a set of registers of ONE file ------------------------------------------
\ A nominal one-field record over a bit mask, so a set cannot be confused with a
\ register number, a count, or a bare integer. It is not per-file-typed the way
\ NEFF's two sets are: a description holds both files and names which is which
\ in the FIELD it stores the set in, so there is no position a general set could
\ arrive in where a floating one was wanted.

STRUCTURE regs DERIVE eq
   FIELD bits n
;STRUCTURE

\ ---- one machine's register files --------------------------------------------
\ Flat, and for the reason src/compiler/native-effect.f's contract is flat: a
\ multi-cell value cannot be bound to a typed local, so a nested per-file record
\ would leave every reader holding a value it cannot name. The field NAMES carry
\ the grouping. `reserved` is the virtual machine's claim and `clobbered` what a
\ call destroys; the allocatable and callee-saved sets are derived below.

STRUCTURE file
   FIELD gpr-size n
   FIELD gpr-reserved regs
   FIELD gpr-clobbered regs
   FIELD fpr-size n
   FIELD fpr-reserved regs
   FIELD fpr-clobbered regs
   FIELD slot-width n
;STRUCTURE

private

: MK-R ( n -- NREGFILE:regs )       NREGFILE-REGS:MAKE ;
: R-BITS ( NREGFILE:regs -- n )     NREGFILE-REGS:UNMAKE ;

CELL 8 * 1- constant REG-MAX-N       \ one cell's bits, less the sign bit

\ Every register of a file that holds `size` of them. The size is already held
\ to REG-MAX-N, so the shift never reaches the sign bit and the mask is positive.
: FILE-MASK ( n -- n )
   1 swap lshift 1- ;

\ A mask with the sign bit set is not a set of registers, whatever it is.
: MASK-CK ( n -- n )
   dup 0 < if E-NREGFILE throw then ;

: REG-CK ( n -- n )
   dup 0 < over REG-MAX-N >= or if E-NREGFILE throw then ;

\ One file's three declared facts, judged together because each is only
\ meaningful against the others: a reserved register has to be a register of
\ this file, a file every one of whose registers the VM took can hold nothing,
\ and a call cannot destroy a register no routine was allowed to use.
: CLASS-CK ( n NREGFILE:regs NREGFILE:regs -- )
   {: size:n res:NREGFILE:regs clob:NREGFILE:regs :}
   size 1 < size REG-MAX-N > or if E-NREGFILE throw then
   res R-BITS MASK-CK {: rb:n :}
   clob R-BITS MASK-CK {: cb:n :}
   size FILE-MASK {: all:n :}
   rb all invert and 0<> if E-NREGFILE throw then
   all rb invert and {: alloc:n :}
   alloc 0= if E-NREGFILE throw then
   cb alloc invert and 0<> if E-NREGFILE throw then ;

\ A slot is reached by a scaled offset wherever the frame is addressed at all, so
\ a width that is not a positive power of two describes no machine's access.
: WIDTH-CK ( n -- )
   {: w:n :}
   w 1 < if E-NREGFILE throw then
   w w 1- and 0<> if E-NREGFILE throw then ;

public

\ ---- the bound a description is held to --------------------------------------
\ A set is a bit mask in one cell and the sign bit names no register, so a file
\ holds at most REG-MAX-N registers; the machines the chain targets number 16
\ and 32.
: REG-MAX ( -- n )                  REG-MAX-N ;

\ ---- building a set ----------------------------------------------------------
: REGS-NONE ( -- NREGFILE:regs )    0 MK-R ;
: REGS-BITS ( NREGFILE:regs -- n )  R-BITS ;
: REGS-SET ( n -- NREGFILE:regs )   MASK-CK MK-R ;

\ The set holding exactly one register. A number no file this schema can describe
\ holds is refused here rather than silently shifted out of the cell.
: REGS-REG ( n -- NREGFILE:regs )   REG-CK 1 swap lshift MK-R ;

: REGS-WITH ( NREGFILE:regs NREGFILE:regs -- NREGFILE:regs )
   {: set:NREGFILE:regs more:NREGFILE:regs :}
   set R-BITS more R-BITS or MK-R ;

\ ---- the derivation, for a reader holding the numbers and not the record -----
\ A consumer holding the fields and not the record (a seven-cell value cannot be
\ bound to a local) derives with this; GPR-ALLOCATABLE and FPR-ALLOCATABLE apply
\ the same rule.
: ALLOCATABLE-MASK ( n NREGFILE:regs -- n )
   {: size:n res:NREGFILE:regs :}
   size 1 < size REG-MAX-N > or if E-NREGFILE throw then
   size FILE-MASK  res R-BITS invert and ;

\ ---- the description ---------------------------------------------------------
\ The only constructor; every reader takes a value that passed through it. There
\ is no scratch-register field: the allocator spills through the register the
\ value holds and refuses a frame it cannot address (E-A64RA-PRESSURE).
\ NABI:SCRATCH in abi.f is the convention's allocatable set, not a machine fact.
: FILE ( n NREGFILE:regs NREGFILE:regs n NREGFILE:regs NREGFILE:regs n -- NREGFILE:file )
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   gn gr gc CLASS-CK
   fn fr fc CLASS-CK
   w WIDTH-CK
   gn gr gc fn fr fc w NREGFILE-FILE:MAKE ;

\ ---- the general file --------------------------------------------------------
: GPR-SIZE ( NREGFILE:file -- n )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   gn ;

: GPR-RESERVED ( NREGFILE:file -- NREGFILE:regs )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   gr ;

\ Every general register a routine of this machine MAY be given: the file less
\ what the virtual machine took. Derived, so it cannot disagree with `reserved`.
: GPR-ALLOCATABLE ( NREGFILE:file -- NREGFILE:regs )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   gn gr ALLOCATABLE-MASK MK-R ;

: GPR-CLOBBERED ( NREGFILE:file -- NREGFILE:regs )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   gc ;

\ And the allocatable registers a call leaves alone, which is the same fact read
\ from the other side. A value live across a call may stay in one of these.
: GPR-CALLEE-SAVED ( NREGFILE:file -- NREGFILE:regs )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   gn gr ALLOCATABLE-MASK  gc R-BITS invert and  MK-R ;

\ ---- the floating file, asked in the same words ------------------------------
: FPR-SIZE ( NREGFILE:file -- n )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   fn ;

: FPR-RESERVED ( NREGFILE:file -- NREGFILE:regs )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   fr ;

: FPR-ALLOCATABLE ( NREGFILE:file -- NREGFILE:regs )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   fn fr ALLOCATABLE-MASK MK-R ;

: FPR-CLOBBERED ( NREGFILE:file -- NREGFILE:regs )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   fc ;

: FPR-CALLEE-SAVED ( NREGFILE:file -- NREGFILE:regs )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   fn fr ALLOCATABLE-MASK  fc R-BITS invert and  MK-R ;

\ ---- the frame ---------------------------------------------------------------
\ The bytes one spilled value occupies. It is here and not in a dialect because
\ the allocator places slots before any dialect has lowered one.
: SLOT-WIDTH ( NREGFILE:file -- n )
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   w ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
