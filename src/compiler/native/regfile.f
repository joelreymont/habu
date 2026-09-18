\ regfile.f - what one machine's register files ARE, as a single checked
\ declaration the backend supplies and the register allocator reads.
\
\ WHY IT EXISTS. src/compiler/native/regalloc.f used to read the ARM64 numbers
\ directly: how many registers a file holds came from the five-bit register
\ operand of src/compiler/native-effect.f, and a spill slot's width from the A64IR
\ dialect. Neither is a fact about linear scan. A machine with sixteen registers
\ could not be allocated for at all - not badly, but not at all, because the
\ file size was a load-time constant and there was no seam to say otherwise.
\ This file is that seam: the allocator asks a description the backend built,
\ so a second architecture supplies numbers rather than forking the pass.
\
\ WHAT A DESCRIPTION SAYS, AND WHAT IT DELIBERATELY DOES NOT. It answers the
\ questions the allocator asks OF THE MACHINE: how many registers each of the
\ two files numbers, which of them belong to the virtual machine and can never
\ be handed out, which of the remainder a call destroys, and how wide the frame
\ slot a spilled value goes into is. It says nothing about which registers one
\ ROUTINE may write - that is the routine's own contract, it differs per routine,
\ and it arrives at the allocator as a pool alongside the module. The two meet at
\ one rule the allocator checks: a routine's pool is a subset of what this
\ description says is allocatable at all.
\
\ WHY RESERVED IS STORED AND ALLOCATABLE IS DERIVED. A description holding both
\ could say a register is reserved and allocatable at once, and two readers of
\ one description would then disagree about the same register. So the VM's claim
\ is the declaration - a backend author knows which registers the engine took -
\ and what is left over is computed. src/compiler/native-effect.f derives its
\ GPR-MASK from its RESERVED-MASK for exactly this reason, and this file states
\ the same rule for any machine. The caller/callee pair is split the same way:
\ what a call DESTROYS is stored, and what therefore survives one is derived, so
\ a register cannot be declared both saved and destroyed.
\
\ HOW BIG A FILE CAN BE. A register set is a bit mask in one cell - bit i names
\ register i - and the sign bit names no register, because a set is a count of
\ members and never a negative number. So a file holds at most one cell's bits
\ less that one, which is what REG-MAX reports and what the constructor holds
\ every declared size to. This is a fact about the set's representation here and
\ not about any machine: the machines the chain targets number 16 and 32.
\
\ THERE IS NO SCRATCH-REGISTER FIELD, deliberately. A register allocator that
\ reserved one would be a different allocator: this one spills through the
\ register the value already holds, and refuses a frame it cannot address
\ (E-A64RA-PRESSURE) rather than materialising an offset that would need a
\ register to build. src/compiler/native/abi.f's NABI:SCRATCH is a different
\ question with the same word in it - every register a routine of the Habu
\ calling convention may hold state in, which is this description's ALLOCATABLE
\ set - and it belongs to the convention, not to the machine.

require lib/prelude.f
require lib/errors.f

package NREGFILE
public

\ ---- a set of registers of ONE file ------------------------------------------
\ A nominal one-field record over a bit mask, so a set cannot be confused with a
\ register number, a count, or a bare integer. It is not per-file-typed the way
\ NEFF's two sets are: a description holds both files and names which is which
\ in the FIELD it stores the set in, so there is no position a general set could
\ arrive in where a floating one was wanted.

STRUCTURE regs 0 DERIVE eq
   FIELD bits n
;STRUCTURE

\ ---- one machine's register files --------------------------------------------
\ Flat, and for the reason src/compiler/native-effect.f's contract is flat: a
\ multi-cell value cannot be bound to a typed local, so a nested per-file record
\ would leave every reader holding a value it cannot name. The field NAMES carry
\ the grouping. `reserved` is the virtual machine's claim and `clobbered` what a
\ call destroys; the allocatable and callee-saved sets are derived below.

STRUCTURE file 0
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
\ A description is seven cells and a value of more than one cell cannot be bound
\ to a local, so a consumer that wants several of its fields at once takes it
\ apart first and then has no record left to ask. This is the same derivation
\ GPR-ALLOCATABLE and FPR-ALLOCATABLE below are, in the form that reader can
\ use, so there is one rule for what a file leaves over and not two.
: ALLOCATABLE-MASK ( n NREGFILE:regs -- n )
   {: size:n res:NREGFILE:regs :}
   size 1 < size REG-MAX-N > or if E-NREGFILE throw then
   size FILE-MASK  res R-BITS invert and ;

\ ---- the description ---------------------------------------------------------
\ The only constructor: every reader below takes a value that passed through it,
\ so no reader revalidates and none can be handed a description that is not one.
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
