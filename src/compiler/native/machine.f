\ machine.f - what one target machine IS, as far as a routine contract and the
\ passes over it have to know, in a single checked declaration the backend
\ supplies.
\
\ WHY IT EXISTS. src/compiler/native-effect.f is the typed machine-state contract
\ every native pass reads, and its vocabulary - register sets, the ordered place
\ list, the convention, the flags, the link field, the frame and the exits - is
\ target-neutral. What was NOT neutral was a handful of ARM64 facts it held as
\ load-time constants: a file of 32 registers, the link register is x30, operand
\ 31 is the stack pointer, the stack is 16-byte aligned, a frame slot is reached
\ by a twelve-bit offset scaled by the access width. A machine with sixteen
\ registers, no link register and a byte displacement could not declare a
\ contract at all - not badly, but not at all, because those facts were constants
\ and there was no seam to say otherwise. This file is that seam. A contract
\ names the machine it is a contract over, so a second architecture supplies
\ numbers rather than forking 2,300 lines of register allocator.
\
\ WHAT IT HOLDS, AND WHY THE REGISTER FILES ARE IN IT RATHER THAN BESIDE IT.
\ src/compiler/native/regfile.f already says what the two register files are: how
\ many registers each numbers, which the virtual machine took, which a call
\ destroys, how wide a spill slot is. That is exactly the part of a machine the
\ register allocator asks about, and it is a component OF this description rather
\ than a second declaration next to it: a backend declares its machine once, and
\ the allocator's view of it is NMACH:REGFILE, a projection. One authority per
\ fact, and no way for a pass allocating registers and a pass validating a
\ contract to be answering about two different machines.
\
\ THE REST IS WHAT A CONTRACT NEEDS AND A REGISTER FILE DOES NOT SAY:
\   link      - the link register AS A SET, empty on a machine that has none.
\               One field and not a number beside a flag, because a number
\               beside a flag is two statements that can disagree, and "which
\               register, if any" is one question. ARM64 answers x30; x86-64
\               answers the empty set and its routines' contracts declare the
\               link effect `absent`.
\   sp        - the operand number a frame access is taken from. It is a
\               register of the file and reserved, on both machines the chain
\               targets, but WHICH one differs: ARM64 spells the stack pointer
\               as operand 31 of a five-bit field, x86-64 as rsp.
\   sp-align  - the alignment the stack pointer is kept at, which is the
\               alignment every declared frame and every stack-pointer delta is
\               held to.
\   frame-max - the largest frame this machine can describe, already rounded to
\               that alignment by the backend that knows its own offset field.
\   off-max   - the largest offset the instruction that reaches a frame slot can
\               name, in units of the scale below.
\   off-scale - what one unit of that field is: the width of the access (ARM64's
\               unsigned-offset load and store, whose twelve-bit field is scaled
\               by what the access moves) or one byte whatever the access moves
\               (x86-64's signed byte displacement). Two machines answering
\               "how deep can a slot sit" in the same words needs this, because
\               scaling by the width makes the answer depend on the access and
\               not scaling makes it the same for every access.
\   widths    - the bytes the machine's modelled memory forms move, as a set:
\               bit w is set when some load and store form moves w bytes. ARM64
\               has no halfword form, so it has no halfword slot; a machine that
\               has one says so here instead of the schema deciding for it.
\   slot-back - the deepest byte UNDER the base that an access can name, as a
\               positive magnitude, or zero on a machine whose frame-reaching
\               form has no signed field at all.
\
\ WHY A DESCRIPTION IS ONE CELL. Every one of these facts is read by words that
\ already bind a dozen typed locals - a routine contract's constructor binds
\ thirteen - and a multi-cell value cannot be bound to a typed local at all. A
\ description the passes can carry therefore has to be one cell, so the facts
\ live in a table private to this module and the value handed around is a
\ nominal record over the row's ORDINAL. Nothing outside can read a fact except
\ through a reader below and the ordinal is bounds-checked at every one of them,
\ so a forged record refuses rather than reading whatever the number points at.
\ Two descriptions of one machine are also the same value: MACHINE returns the
\ row a machine with these facts already has rather than a second row, so a
\ contract's identity does not depend on how many times a backend was asked to
\ describe itself.
\
\ WHAT IT DELIBERATELY DOES NOT HOLD. Which places an ABI puts arguments in:
\ that is a convention, it is declared per routine, and src/compiler/native/abi.f
\ owns it for the Habu convention. The instruction vocabulary: a dialect owns
\ that. Which registers ONE routine may write: that is the routine's contract.
\ Which register the running engine keeps its data stack in: src/habu/layout.f
\ declares that and the backend reports it. A machine description answers
\ questions whose answer is the same for every routine of the machine.

require lib/prelude.f
require lib/errors.f
require src/compiler/native/regfile.f

package NMACH
public

\ ---- how the frame-access offset field is counted ----------------------------
\ Two machines, two readings of one field, and a reader that has to ask rather
\ than assume: an offset field scaled by the access width reaches a different
\ byte for a byte store than for an eight-byte store, and one counted in bytes
\ reaches the same byte for both.

ENUM offscale DERIVE eq
   by-width
   by-byte
;ENUM

\ ---- one machine -------------------------------------------------------------
\ A nominal one-field record over the ordinal of the row holding its facts, so a
\ description cannot be confused with a count, a register number or a bare
\ integer, and so a pass can bind one to a typed local.

STRUCTURE mach 0 DERIVE eq
   FIELD id n
;STRUCTURE

private

\ ---- the table the ordinals index --------------------------------------------
\ One row per DISTINCT machine. The capacity is not a bound on architectures: a
\ backend declares its machine once and a test declaring variants of one declares
\ few. Reaching it means something is describing a machine per call, which is a
\ defect in that caller, so it is refused rather than grown around.

15 constant ROW-N
0 constant F-GPR-SIZE
1 constant F-GPR-RES
2 constant F-GPR-CLOB
3 constant F-FPR-SIZE
4 constant F-FPR-RES
5 constant F-FPR-CLOB
6 constant F-SLOT-WIDTH
7 constant F-LINK
8 constant F-SP
9 constant F-SP-ALIGN
10 constant F-FRAME-MAX
11 constant F-OFF-MAX
12 constant F-SCALE
13 constant F-WIDTHS
14 constant F-SLOT-BACK

16 constant MACH-MAX

create MACH-T MACH-MAX ROW-N * cells allot
variable MACH-N
0 MACH-N !

\ ---- the scale, as it is stored ----------------------------------------------
\ A cell holds a number, so the family rides in one as a stable code. It never
\ leaves this module: the only reader of it is SLOT-REACH below.
0 constant SCALE-WIDTH
1 constant SCALE-BYTE

: SCALE-CODE ( NMACH:offscale -- n )
   MATCH offscale
      by-width OF SCALE-WIDTH ENDOF
      by-byte  OF SCALE-BYTE ENDOF
   ;MATCH ;

\ ---- reading a row -----------------------------------------------------------
\ One cell of one row, by ordinal. Every public reader arrives here, so an
\ ordinal no machine was declared under is refused once and in one place rather
\ than indexing past the table.
: ROW@ ( n n -- n )
   {: row:n fld:n :}
   row ROW-N * fld + cells MACH-T + @ ;

: ROW! ( n n n -- )
   {: row:n fld:n v:n :}
   v  row ROW-N * fld + cells MACH-T + ! ;

: ROW-CK ( n -- n )
   dup 0 < over MACH-N @ >= or if E-NMACH throw then ;

: F@ ( NMACH:mach n -- n )
   {: fld:n :}
   NMACH-MACH:UNMAKE ROW-CK fld ROW@ ;

\ ---- per-field rules ---------------------------------------------------------

\ Every register of a file that numbers `size` of them. A rule holding the
\ numbers and not the record derives the same mask NREGFILE's own derivation is
\ written over.
: FILE-BITS ( n -- n )
   {: size:n :}
   size 1 < size NREGFILE:REG-MAX > or if E-NMACH throw then
   1 size lshift 1- ;

\ A set holding at most one member, which is what "the link register, if there
\ is one" is. More than one bit is not a register.
: ONE-AT-MOST? ( n -- bool )
   dup 1- and 0= ;

\ A register the machine gave another owner: it is a register of the general
\ file and the file's reserved set names it. Both the link register and the
\ stack pointer are such registers - a routine that held state in either would
\ lose its own return address or its caller's frame - so declaring one that the
\ register files say is allocatable is two statements that disagree. The empty
\ set passes, which is how a machine with no link register declares one.
: RESERVED-CK ( n n n -- )
   {: size:n res:n bits:n :}
   bits 0 < if E-NMACH throw then
   size FILE-BITS {: all:n :}
   bits all invert and 0<> if E-NMACH throw then
   bits res invert and 0<> if E-NMACH throw then ;

: ALIGN-CK ( n -- )
   {: a:n :}
   a 1 < if E-NMACH throw then
   a a 1- and 0<> if E-NMACH throw then ;

\ The access widths, and the one fact that ties them to the register files: a
\ spilled register goes into a slot of the declared width, so a machine whose
\ loads and stores cannot move that many bytes has nothing to spill through.
: WIDTHS-CK ( n n -- )
   {: wd:n w:n :}
   wd 1 < if E-NMACH throw then
   wd  1 w lshift  and 0= if E-NMACH throw then ;

public

\ ---- the description ---------------------------------------------------------
\ The only constructor: every reader below takes a value that passed through it,
\ so no reader revalidates and none can be handed a description that is not one.
: MACHINE ( NREGFILE:file NREGFILE:regs n n n n NMACH:offscale n n -- NMACH:mach )
   {: lk:NREGFILE:regs sp:n al:n fm:n om:n os:NMACH:offscale wd:n bk:n :}
   NREGFILE-FILE:UNMAKE
   {: gn:n gr:NREGFILE:regs gc:NREGFILE:regs
      fn:n fr:NREGFILE:regs fc:NREGFILE:regs w:n :}
   lk NREGFILE:REGS-BITS {: lb:n :}
   lb ONE-AT-MOST? 0= if E-NMACH throw then
   gn gr NREGFILE:REGS-BITS lb RESERVED-CK
   sp 0 < sp gn >= or if E-NMACH throw then
   gn gr NREGFILE:REGS-BITS  1 sp lshift  RESERVED-CK
   al ALIGN-CK
   fm 0 < if E-NMACH throw then
   fm al mod 0<> if E-NMACH throw then
   om 0 < if E-NMACH throw then
   bk 0 < if E-NMACH throw then
   wd w WIDTHS-CK
   \ Compare every fact before reserving storage: an existing description needs
   \ no free row, even when the table is full.
   MACH-N @ 0 ?do
      i F-GPR-SIZE ROW@ gn =
      i F-GPR-RES ROW@ gr NREGFILE:REGS-BITS = and
      i F-GPR-CLOB ROW@ gc NREGFILE:REGS-BITS = and
      i F-FPR-SIZE ROW@ fn = and
      i F-FPR-RES ROW@ fr NREGFILE:REGS-BITS = and
      i F-FPR-CLOB ROW@ fc NREGFILE:REGS-BITS = and
      i F-SLOT-WIDTH ROW@ w = and
      i F-LINK ROW@ lb = and
      i F-SP ROW@ sp = and
      i F-SP-ALIGN ROW@ al = and
      i F-FRAME-MAX ROW@ fm = and
      i F-OFF-MAX ROW@ om = and
      i F-SCALE ROW@ os SCALE-CODE = and
      i F-WIDTHS ROW@ wd = and
      i F-SLOT-BACK ROW@ bk = and
      if i NMACH-MACH:MAKE unloop exit then
   loop
   MACH-N @ {: row:n :}
   row MACH-MAX >= if E-NMACH throw then
   row F-GPR-SIZE gn ROW!
   row F-GPR-RES gr NREGFILE:REGS-BITS ROW!
   row F-GPR-CLOB gc NREGFILE:REGS-BITS ROW!
   row F-FPR-SIZE fn ROW!
   row F-FPR-RES fr NREGFILE:REGS-BITS ROW!
   row F-FPR-CLOB fc NREGFILE:REGS-BITS ROW!
   row F-SLOT-WIDTH w ROW!
   row F-LINK lb ROW!
   row F-SP sp ROW!
   row F-SP-ALIGN al ROW!
   row F-FRAME-MAX fm ROW!
   row F-OFF-MAX om ROW!
   row F-SCALE os SCALE-CODE ROW!
   row F-WIDTHS wd ROW!
   row F-SLOT-BACK bk ROW!
   row 1+ MACH-N !
   row NMACH-MACH:MAKE ;

\ ---- the ordinal, for a caller that has to keep one in a cell -----------------
\ A backend describes its machine once at load and reports it from a constant,
\ because a word that described one per call would pay for it on every register
\ a pass names. A constant holds a number, so the number is here - and BY-ID
\ refuses one no machine was declared under, so what comes back is a description
\ or nothing.
: ID ( NMACH:mach -- n )              NMACH-MACH:UNMAKE ROW-CK ;
: BY-ID ( n -- NMACH:mach )           ROW-CK NMACH-MACH:MAKE ;

\ ---- the register files, for the allocator that asks only about them ----------
: REGFILE ( NMACH:mach -- NREGFILE:file )
   {: m:NMACH:mach :}
   m F-GPR-SIZE F@
   m F-GPR-RES F@ NREGFILE:REGS-SET
   m F-GPR-CLOB F@ NREGFILE:REGS-SET
   m F-FPR-SIZE F@
   m F-FPR-RES F@ NREGFILE:REGS-SET
   m F-FPR-CLOB F@ NREGFILE:REGS-SET
   m F-SLOT-WIDTH F@
   NREGFILE:FILE ;

\ ---- the register files, asked through the machine ----------------------------
\ The questions the contract schema asks of a file. They are NREGFILE's answers,
\ reported here so a caller holding a machine does not have to project the file
\ first and so there is one spelling of each in the passes.
: GPR-SIZE ( NMACH:mach -- n )        F-GPR-SIZE F@ ;
: FPR-SIZE ( NMACH:mach -- n )        F-FPR-SIZE F@ ;
: SLOT-WIDTH ( NMACH:mach -- n )      F-SLOT-WIDTH F@ ;

\ Every register no routine may hold state in, from every owner at once.
: GPR-RESERVED ( NMACH:mach -- n )    F-GPR-RES F@ ;
: FPR-RESERVED ( NMACH:mach -- n )    F-FPR-RES F@ ;

\ Every general register a routine of this machine may hold state in: the file
\ less what the virtual machine, the link register and the stack pointer took.
\ Derived, so it cannot disagree with what is reserved.
: GPR-ALLOCATABLE ( NMACH:mach -- n )
   {: m:NMACH:mach :}
   m F-GPR-SIZE F@ FILE-BITS  m F-GPR-RES F@ invert and ;

: FPR-ALLOCATABLE ( NMACH:mach -- n )
   {: m:NMACH:mach :}
   m F-FPR-SIZE F@ FILE-BITS  m F-FPR-RES F@ invert and ;

\ ---- the link register -------------------------------------------------------
\ Does this machine keep a return address in a register at all? A machine that
\ does not is not a machine whose routines preserve or destroy one: their
\ contracts declare the link effect `absent`, and NEFF refuses any other answer.
: LINK? ( NMACH:mach -- bool )        F-LINK F@ 0<> ;

\ Which register that is. A machine without one is refused rather than answered
\ with a number that would read as register zero.
: LINK-GPR ( NMACH:mach -- n )
   F-LINK F@ {: lb:n :}
   lb 0= if E-NMACH throw then
   0
   NREGFILE:REG-MAX 0 ?do
      lb 1 i lshift and 0<> if drop i leave then
   loop ;

\ ---- the stack ---------------------------------------------------------------
: SP-GPR ( NMACH:mach -- n )          F-SP F@ ;
: SP-ALIGN ( NMACH:mach -- n )        F-SP-ALIGN F@ ;
: FRAME-MAX ( NMACH:mach -- n )       F-FRAME-MAX F@ ;

\ The frame a routine needing `want` bytes below its entry declares: the request
\ rounded up to this machine's stack alignment, and no frame at all for a routine
\ that needs nothing. It is here rather than in either caller because two of them
\ need the same number - src/compiler/native/abi.f turns a spill count into a
\ declaration, and src/compiler/native/regalloc.f measures the frame a walk
\ proved its routine needs - and the two have to agree exactly or the contract
\ validator refuses the difference.
: FRAME-ROUND ( n NMACH:mach -- n )
   SP-ALIGN {: want:n a:n :}
   want 0= if 0 exit then
   want a 1- +  a /  a * ;

: ALIGNED? ( n NMACH:mach -- bool )
   SP-ALIGN mod 0= ;

\ ---- reaching a frame slot ---------------------------------------------------
\ Does a load or store of this machine move this many bytes at all? A width no
\ form moves has no reach and no slot.
: WIDTH-OK? ( n NMACH:mach -- bool )
   F-WIDTHS F@ {: width:n wd:n :}
   width 1 < width NREGFILE:REG-MAX > or if false exit then
   wd 1 width lshift and 0<> ;

\ The deepest byte an access of this width can name through the machine's own
\ offset field. A consumer placing slots asks rather than repeating arithmetic
\ that is not the same on two machines.
: SLOT-REACH ( n NMACH:mach -- n )
   {: width:n m:NMACH:mach :}
   width m WIDTH-OK? 0= if E-NMACH throw then
   m F-OFF-MAX F@ {: om:n :}
   m F-SCALE F@ SCALE-WIDTH = if om width * exit then
   om ;

\ And the deepest byte UNDER the base that an access can name. It takes no width
\ because the field it comes from is not scaled on either machine the chain
\ targets. It is answered as a positive magnitude, so a consumer writes `negate`
\ where it means an offset and reads it as a depth where it means a bound.
: SLOT-BACK ( NMACH:mach -- n )       F-SLOT-BACK F@ ;

\ ---- identity ----------------------------------------------------------------
\ A number that differs when any fact of the machine differs, for a consumer
\ that has to record WHICH machine a value is about - a routine contract's
\ digest preimage is the one that does. It folds the stored row and not the
\ row's ordinal, because an ordinal is a fact about the order backends loaded in
\ and this has to mean the same in every build of the compiler. It is a fold and
\ not a hash: what it separates is the handful of machines a build declares, and
\ the contract digest it feeds is what carries cryptographic identity.
: MARK ( NMACH:mach -- n )
   {: m:NMACH:mach :}
   $811C9DC5
   ROW-N 0 ?do
      m i F@ xor  $1000193 *
   loop ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
