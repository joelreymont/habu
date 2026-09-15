\ codewalk.f - the machine code this process is running, read as evidence. One
\ concern: the walk over a record's instructions, and over every record that
\ holds any.

require lib/prelude.f
require lib/errors.f
require src/arch/arm64/asm.f
require src/compiler/a64-effect.f
require src/compiler/native/branch.f

package NWALK

using A64ASM

private

variable AT

: AT-PTR ( -- ptr u8 )
   AT 0 ptr-field @ ;

-1 constant NAMESPACE-WL              \ a package name, not a word
0 constant LOWEST-WL

public

: INSN@ ( n -- n ) {: a:n :}
   a AT !
   AT-PTR c@
   AT-PTR 1 + c@ 8 lshift or
   AT-PTR 2 + c@ 16 lshift or
   AT-PTR 3 + c@ 24 lshift or ;

\ ---- the engine's stack guard, read as one unit -------------------------------
\ Before a complete transfer and after a returning call the backend writes the
\ engine ABI's guard request (src/compiler/native/emit.f PUT-STACK-GUARD): a
\ 32-byte caller frame that saves x16, x17 and the link, the two bounds in x16
\ and x17, one BL to stack-data-entry, and the frame's release. Eleven
\ instructions, none of them the routine's own work, so a reader that counts
\ what a routine does - its frame moves, the calls it makes, the instructions
\ of its body - steps over each one whole. The fixed words are spelled through
\ the encoders the emitter uses; a guard that changed shape stops matching
\ here and in test/compiler/native-emit.f, which pins every word.
11 constant GUARD-INSNS

private

16 constant GUARD-LOW-GPR                \ the lower bound travels in x16
17 constant GUARD-HIGH-GPR               \ and the upper bound in x17
32 constant GUARD-FRAME-BYTES
$FFE0001F constant MOVZ-FORM-MASK        \ a MOVZ less its sixteen immediate bits

\ The word a guard holds at slot k, or 0 for the three slots that carry an
\ operand (the two bounds and the branch).
: GUARD-FIXED ( n -- n ) {: k:n :}
   k 0 = if A64EFF:SP-GPR A64EFF:SP-GPR GUARD-FRAME-BYTES ENC-SUBI exit then
   k 1 = if GUARD-LOW-GPR A64EFF:SP-GPR 0 ENC-STR exit then
   k 2 = if GUARD-HIGH-GPR A64EFF:SP-GPR 8 ENC-STR exit then
   k 3 = if A64EFF:LINK-GPR A64EFF:SP-GPR 16 ENC-STR exit then
   k 7 = if GUARD-LOW-GPR A64EFF:SP-GPR 0 ENC-LDR exit then
   k 8 = if GUARD-HIGH-GPR A64EFF:SP-GPR 8 ENC-LDR exit then
   k 9 = if A64EFF:LINK-GPR A64EFF:SP-GPR 16 ENC-LDR exit then
   k 10 = if A64EFF:SP-GPR A64EFF:SP-GPR GUARD-FRAME-BYTES ENC-ADDI exit then
   0 ;

: GUARD-SLOT? ( n n -- bool ) {: k:n w:n :}
   k 4 = if w MOVZ-FORM-MASK and GUARD-LOW-GPR 0 0 MOVZHW = exit then
   k 5 = if w MOVZ-FORM-MASK and GUARD-HIGH-GPR 0 0 MOVZHW = exit then
   k 6 = if w NBR:BL? exit then
   w k GUARD-FIXED = ;

public

\ Instruction indices, read through a quotation from index to word, so the same
\ reading serves published code and an emission still in its buffer. A whole
\ guard begins at k when all eleven slots fit and match.
: GUARD-AT? ( n n [ n -- n ] -- bool ) {: insns:n k:n q :}
   k GUARD-INSNS + insns > if false exit then
   GUARD-INSNS 0 ?do
      i  k i + q execute  GUARD-SLOT? 0= if false unloop exit then
   loop
   true ;

\ Whether instruction k is one of a guard's eleven.
: GUARDED? ( n n [ n -- n ] -- bool ) {: insns:n k:n q :}
   0 begin dup k <= while
      dup insns swap q GUARD-AT? if
         dup GUARD-INSNS + k > if drop true exit then
         GUARD-INSNS +
      else
         1+
      then
   repeat drop
   false ;

\ How many guards the instructions hold.
: GUARDS ( n [ n -- n ] -- n ) {: insns:n q :}
   0 0 begin dup insns < while
      dup insns swap q GUARD-AT? if
         GUARD-INSNS + swap 1+ swap
      else
         1+
      then
   repeat drop ;

private

\ The same questions over a span of live code, by address. Live code also has
\ the one fact the buffer form cannot check: the guard's BL reaches the engine's
\ stack-data-entry and nothing else, so a lookalike wrapper stays in the count.
variable SPAN-BASE

: SPAN-WORD ( n -- n )
   NBR:INSN-BYTES * SPAN-BASE @ + INSN@ ;

: SPAN-GUARD-AT? ( n n -- bool ) {: insns:n k:n :}
   insns k [: SPAN-WORD ;] GUARD-AT? 0= if false exit then
   k 6 + NBR:INSN-BYTES * SPAN-BASE @ + {: at:n :}
   at at INSN@ NBR:BL-TARGET stack-data-entry = ;

public

: SPAN-GUARDED? ( n n n -- bool ) {: s:n len:n k:n :}
   s SPAN-BASE !
   len NBR:INSN-BYTES / {: insns:n :}
   0 begin dup k <= while
      insns over SPAN-GUARD-AT? if
         dup GUARD-INSNS + k > if drop true exit then
         GUARD-INSNS +
      else
         1+
      then
   repeat drop
   false ;

: SPAN-GUARDS ( n n -- n ) {: s:n len:n :}
   s SPAN-BASE !
   len NBR:INSN-BYTES / {: insns:n :}
   0 0 begin dup insns < while
      insns over SPAN-GUARD-AT? if
         GUARD-INSNS + swap 1+ swap
      else
         1+
      then
   repeat drop ;

\ the instruction at it, and a local annotation cannot carry a quotation effect.
: SPAN-EACH ( n n [ n n -- ] -- ) {: s:n len:n q :}
   len NBR:INSN-BYTES / 0 ?do
      s i NBR:INSN-BYTES * +  dup INSN@  q execute
   loop ;

: RECS ( -- n )
   ndict@ ;

: REC-WL ( n -- n ) {: k:n :}
   k XREF-REC XREF-WORDLIST ;

: REC-START ( n -- n ) {: k:n :}
   k XREF-REC XREF-START ;

: REC-LEN ( n -- n ) {: k:n :}
   k XREF-REC XREF-CODE-BYTES ;

\ A record holds code when its wordlist is real, including an empty RET body:
\ wordlist -1 is a package name and -2 is retired, and neither start is code.
: CODED? ( n -- bool ) {: k:n :}
   k REC-WL LOWEST-WL < if false exit then
   k REC-LEN 0 > ;

: REC-EACH ( n [ n n -- ] -- ) {: k:n q :}
   k CODED? 0= if exit then
   k REC-START k REC-LEN q SPAN-EACH ;

\ Written out rather than delegating to REC-EACH: a quotation cannot read the
\ enclosing word's locals, so the callback has to stay in this body's scope.
: LIVE-EACH ( [ n n -- ] -- ) {: q :}
   RECS 0 ?do
      i CODED? if i REC-START i REC-LEN q SPAN-EACH then
   loop ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
