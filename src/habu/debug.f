\ debug.f - breakpoints on compiled words: a trap at a word's entry (BRK #0 on
\ arm64, int3 on x86-64), served by the engine's SIGTRAP handler, which prints
\ habu-bp:, pc and the stack top, then removes it (one-shot) or resumes past it
\ (persistent) at its slot's trampoline, which does what the entry instruction
\ does and goes on where it does.
\ With debug-watch.f and stepper.f it makes package DEBUG; the rest is private,
\ so a program keeps its own FIND, FREE or STEP.
\ Every engine carries its target's full disassembler.

require src/core/exit-status.f
require src/lib/le.f
HB-TARGET-LINUX-X86-64? 0= [if] require src/arch/arm64/asm.f [then]
HB-TARGET-LINUX-X86-64? 0= [if] require src/arch/arm64/disasm.f [then]
HB-TARGET-LINUX-X86-64? [if] require src/arch/x86-64/disasm.f [then]
require src/habu/code-bytes.f
require src/compiler/native/dict.f
require src/habu/debug-watch.f
require src/habu/stepper.f

package DEBUG

public

\ Debugger-owned rejection range, outside the library and build-tool ranges.
-9300 constant E-BP-FIRST
-9309 constant E-BP-LAST
-9300 s" a breakpoint address is not an aligned instruction of the live compiled code, or a persistent breakpoint has no trampoline"
   ERROR:CODE E-BP-TARGET

private

: SLOT-OFF ( n -- n )
   32 * BPTAB-OFF + ;

\ The four fixed DATA fields of a breakpoint slot. The address field holds a
\ code pointer, so it is reached through `ptr-field`, the declared pointer-cell
\ door; the other three hold numbers.
: BP-SLOT-ADDR ( n -- ^^u8 )
   SLOT-OFF data-base + 0 ptr-field ;

: BP-SLOT-INSTR ( n -- ^n )
   SLOT-OFF 8 + data-base + ;

: BP-SLOT-HITS ( n -- ^n )
   SLOT-OFF 16 + data-base + ;

: BP-SLOT-CTRL ( n -- ^n )
   SLOT-OFF 24 + data-base + ;

: BP-NULL ( -- ^u8 )
   NULL-PTR ;

: BP-PATCH32 ( n ^u8 -- ) patch32 ;

\ A code address reaches checked code as a number; CODE-BYTES:AT is the one
\ bounded view of the bytes there.
CAST: XT>N ( [ R -- S ] -- n )

: CODE! ( n n -- ) {: w at :}
   w  at 4 CODE-BYTES:AT STR:DATA  BP-PATCH32 ;

\ The block is reserved at boot: a failed REPL line's CP rollback would free one
\ reserved here. A branch from it reaches the whole region.
: TRAMP@ ( -- n ) data-base BPTRAMP-CELL + @ ;

\ One word of trap instructions: brk #0, or four int3.
: TRAPS ( -- n ) HB-TARGET-LINUX-X86-64? if $CCCCCCCC exit then $D4200000 ;

\ The trampoline being written: its address, its bytes and how many are written.
variable TRAMP-AT
create TRAMP-BUF BREAKPOINT:TRAMP allot
variable TRAMP-U

: TRAMP-OPEN ( n -- )
   TRAMP-AT !  0 TRAMP-U !
   BREAKPOINT:TRAMP 0 ?do TRAPS TRAMP-BUF i + LE:U32! 4 +loop ;

: DWORD+ ( n -- ) TRAMP-BUF TRAMP-U @ + LE:U32!  4 TRAMP-U +! ;

: TRAMP-STORE ( -- )
   BREAKPOINT:TRAMP 0 ?do
      TRAMP-BUF i + LE:U32@ TRAMP-AT @ i + CODE!
   4 +loop ;

HB-TARGET-LINUX-X86-64? 0= [if]

\ The word that plants the trap at an entry, and the one that restores the
\ entry from the word a slot saved.
: TRAP-WORD ( ^u8 -- n ) drop TRAPS ;
: RESTORED ( n ^u8 -- n ) drop ;
: ALIGNED? ( n -- bool ) 3 and 0= ;

: B+ ( n -- ) TRAMP-AT @ TRAMP-U @ + - A64ASM:ENC-B DWORD+ ;

: OP? ( n n n -- bool ) {: w:n mask:n op:n :}
   w mask and op = ;

\ The byte distance w's signed field of so many bits from bit lo holds in words.
: DELTA ( n n n -- n ) {: w:n lo:n bits:n :}
   1 bits 1 - lshift {: sign:n :}
   w lo rshift  sign 2 * 1 - and  sign xor sign - 4 * ;

\ adr's distance in bytes, adrp's in pages.
: ADR-DELTA ( n -- n ) {: w:n :}
   w 5 19 DELTA  w 29 rshift 3 and + ;

\ w's register becomes v: `ldr xd,[pc,#8]`, the branch, then v's quad.
: VALUE+ ( n n n -- ) {: next:n w:n v:n :}
   w 31 and $58000040 or DWORD+  next B+
   v DWORD+  v 32 rshift DWORD+ ;

\ Whether w does the same anywhere: a br or ret, an exception or system word,
\ or no branch at all.
: COPIES? ( n -- bool ) {: w:n :}
   w $FFBFFC1F $D61F0000 OP?
   w $FE000000 $D4000000 OP? or
   w $1C000000 $14000000 OP? 0= or ;

\ Append the words that do at the trampoline what w does at `at`; false: w has
\ none. A literal load's are longer than a slot.
: RELOCATED? ( n n -- bool ) {: at:n w:n :}
   at 4 + {: next:n :}
   w $3B000000 $18000000 OP? if false exit then           \ a literal load
   w $FC000000 $14000000 OP? if at w 0 26 DELTA + B+ true exit then
   w $9F000000 $10000000 OP? if                           \ adr
      next w  at w ADR-DELTA +  VALUE+ true exit
   then
   w $9F000000 $90000000 OP? if                           \ adrp
      next w  at $FFF invert and w ADR-DELTA 12 lshift +  VALUE+ true exit
   then
   w COPIES? dup if w DWORD+ next B+ then ;

: ENTRY+ ( n -- bool ) {: at:n :}
   at  at 4 CODE-BYTES:AT STR:DATA LE:U32@  RELOCATED? ;

[then]

HB-TARGET-LINUX-X86-64? [if]

$CC constant INT3
$FFFFFF00 constant ABOVE-BYTE0

\ int3 takes the entry's first byte and leaves the three after it, which may
\ begin another breakpoint's word.
: TRAP-WORD ( ^u8 -- n ) LE:U32@ ABOVE-BYTE0 and INT3 or ;
: RESTORED ( n ^u8 -- n ) LE:U32@ ABOVE-BYTE0 and swap $FF and or ;
: ALIGNED? ( n -- bool ) drop true ;

\ Whether X64DIS decodes the instruction at code; a throw from outside the
\ decoder's block throws on.
: DECODES? ( str -- bool ) {: code:str :}
   [: code X64DIS:SPAN drop drop ;] catch {: rc:n :}
   rc 0= if true exit then
   rc X64DIS:E-LAST < rc X64DIS:E-FIRST > or if rc throw then
   false ;

: BYTE+ ( n -- ) TRAMP-BUF TRAMP-U @ + c!  1 TRAMP-U +! ;

: BYTES+ ( str -- ) {: s:str :}
   s STR:LEN 0 ?do s STR:DATA i + c@ BYTE+ loop ;

\ A rel32 counts from the byte after it.
: REL32+ ( n -- )
   TRAMP-AT @ TRAMP-U @ + 4 + - DWORD+ ;

: JMP+ ( n -- ) $E9 BYTE+ REL32+ ;

\ The code region lies below 2 GB, so `push imm32` holds a return address.
: PUSH+ ( n -- ) $68 BYTE+ DWORD+ ;

\ The instruction with the disp32 at offset disp, which counts from the
\ instruction's end, moved from `at` to the trampoline.
: MOVED+ ( n str n -- ) {: at:n code:str disp:n :}
   code STR:DATA {: a:^ :}
   a disp >str BYTES+
   a disp + LE:U32@ at TRAMP-AT @ - + DWORD+
   disp 4 + {: done:n :}
   a done +  code STR:LEN done -  >str BYTES+ ;

\ Append the bytes that do at the trampoline what the instruction code
\ does at `at`. Tier 0 begins no unit with the jcc or call r/m of `other`.
: RELOCATED? ( n str X64DIS:pc-use -- bool )
   {: at:n code:str use:X64DIS:pc-use :}
   at code STR:LEN + {: next:n :}
   use MATCH X64DIS:pc-use
      none OF code BYTES+ next JMP+ true ENDOF
      operand OF {: disp:n :}
         at code disp MOVED+ next JMP+ true ENDOF
      call OF {: delta:n :}
         next PUSH+ next delta + JMP+ true ENDOF
      jump OF next + JMP+ true ENDOF
      other OF false ENDOF
   ;MATCH ;

: ENTRY+ ( n -- bool ) {: at:n :}
   at  cp@ at - 15 min  CODE-BYTES:AT {: code:str :}
   code DECODES? 0= if false exit then
   code X64DIS:SPAN {: len:n use:X64DIS:pc-use :}
   at  code STR:DATA len >str  use RELOCATED? ;

[then]

\ Slot idx's trampoline for the instruction at `at`; false when it has no
\ out-of-line form.
: TRAMP! ( n n -- bool ) {: at:n idx:n :}
   idx BREAKPOINT:TRAMP * TRAMP@ + TRAMP-OPEN
   at ENTRY+ dup if TRAMP-STORE then ;

\ A slot is filled while its address field holds a code pointer; an empty slot
\ holds BP-NULL, whose address reads 0.
: FILLED? ( n -- bool )  BP-SLOT-ADDR @ BP-NULL = 0= ;

\ The filled slot holding code address at, else -1. An empty slot never
\ matches, so an address of 0 finds nothing.
: FIND ( n -- n )
   {: at :}
   0 BEGIN dup BREAKPOINT:SLOTS < WHILE
      dup FILLED? IF dup BP-SLOT-ADDR @ NULL-PTR - at = IF exit THEN THEN
      1 + REPEAT  drop -1 ;

\ An empty slot, else -1.
: FREE ( -- n )
   0 BEGIN dup BREAKPOINT:SLOTS < WHILE  dup FILLED? 0= IF exit THEN  1 + REPEAT  drop -1 ;

: BP-SET-SLOT ( ^u8 n n -- ) {: xt:^ ctrl idx :}
   xt idx BP-SLOT-ADDR !
   xt LE:U32@ idx BP-SLOT-INSTR !
   0 idx BP-SLOT-HITS !
   ctrl idx BP-SLOT-CTRL ! ;

\ ctrl = (skip << 1) | persistent; an entry with no trampoline gets 0, one-shot
\ with no skip.
: BPADD ( n n -- ) {: at ctrl :}
   \ patch32 stores through the region's alias, so a target lies in the live
   \ compiled region, which CODE-BYTES:AT never rejects.
   at dbase@ DICT-SIZE + <  at cp@ 4 - > or  at ALIGNED? 0= or if
      E-BP-TARGET throw
   then
   at FIND 0 < 0= IF exit THEN                \ already set
   ctrl 1 and 0<>  TRAMP@ 0=  and if E-BP-TARGET throw then
   FREE dup 0 < IF
      drop S\" bp: table full\n" ERROR-TEXT:TYPE EXIT-STATUS:FULL throw
   THEN   \ recoverable in the REPL
   {: idx :}
   at 4 CODE-BYTES:AT STR:DATA {: xt:^ :}
   ctrl  ctrl 1 and 0<> if  at idx TRAMP! 0= if drop 0 then  then {: kept :}
   xt kept idx BP-SET-SLOT
   xt TRAP-WORD xt BP-PATCH32 ;

\ A copy word's mention compiles its steps into the caller, so a trap at its
\ entry fires only for a call through its xt; says so after a breakpoint at
\ code address at is set.
: COPY-NOTE ( n -- ) {: at:n :}
   ndict@ 0 ?do
      i XREF-REC {: rec:^ :}
      rec NDICT:COPY?  rec XREF-START at =  and if
         s" bp: a mention of " ERROR-TEXT:TYPE  rec XREF-NAME$ ERROR-TEXT:TYPE
         S\"  compiles its steps into the caller; this fires only for a call through its xt\n" ERROR-TEXT:TYPE
         unloop exit
      then
   loop ;

public

: BP+ ( [ R -- S ] -- )    XT>N dup 0 BPADD COPY-NOTE ;     \ one-shot
: BP* ( [ R -- S ] -- )    XT>N dup 1 BPADD COPY-NOTE ;     \ persistent (re-fires every call)
: BPN ( n [ R -- S ] -- )                     \ persistent, silent for the first n hits
   XT>N dup rot 1 lshift 1 or BPADD COPY-NOTE ;

\ Only a slot BPADD filled is patched back, through the pointer it stored, so
\ an xt with no breakpoint returns before any byte is touched.
: BP- ( [ R -- S ] -- )                       \ remove a breakpoint
   XT>N FIND dup 0 < IF drop exit THEN
   {: idx :}  idx BP-SLOT-ADDR @ {: at:^ :}
   idx BP-SLOT-INSTR @ at RESTORED at BP-PATCH32       \ restore orig instr, clear slot
   BP-NULL idx BP-SLOT-ADDR ! ;

: BP. ( -- )                                  \ list active breakpoints (addrs)
   0 BEGIN dup BREAKPOINT:SLOTS < WHILE
      dup BP-SLOT-ADDR @ dup BP-NULL = 0= IF NULL-PTR BYTE-VIEW - . cr ELSE drop THEN
      1 + REPEAT  drop ;

\ The last of package DEBUG's three files: no later source reopens it.
seal-package
;package
