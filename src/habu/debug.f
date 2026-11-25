split-str
\ debug.f - breakpoints on compiled words: a trap at a word's entry (BRK #0 on
\ arm64, int3 on x86-64), served by the engine's SIGTRAP handler, which prints
\ habu-bp:, pc and the stack top, then removes (one-shot) or re-arms
\ (persistent) it. Up to 8.
\ With debug-watch.f and stepper.f it makes package DEBUG; the rest is private,
\ so a program keeps its own FIND, FREE or STEP.

require src/core/exit-status.f
HB-TARGET-LINUX-X86-64? 0= [if] require src/arch/arm64/asm.f [then]
HB-TARGET-LINUX-X86-64? [if] require src/arch/x86-64/disasm.f [then]
require src/habu/code-bytes.f
require src/habu/debug-watch.f
require src/habu/stepper.f

package DEBUG

\ A trampoline slot's bytes: BPTRAMP-BYTES holds MAXBP of them.
: TRAMP-BYTES ( -- n ) HB-TARGET-LINUX-X86-64? if 20 exit then 8 ;

public

BPTRAMP-BYTES TRAMP-BYTES / constant MAXBP

\ Debugger-owned refusal range, outside the library and build-tool ranges.
-9300 constant E-BP-FIRST
-9309 constant E-BP-LAST
-9300 s" a breakpoint address is not an aligned instruction of the live compiled code, or a persistent breakpoint has no trampoline"
   ERROR:CODE E-BP-TARGET

private

: W32@ ( ^u8 -- n ) {: a :}
   a c@  a 1 + c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

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
   w  at 4 CODE-BYTES:AT drop  BP-PATCH32 ;

\ A persistent breakpoint runs its entry instruction out of line: slot idx owns
\ the TRAMP-BYTES at [BPTRAMP-CELL] + idx * TRAMP-BYTES, which do what the
\ instruction does at the entry and go on where it does. The block is reserved
\ in the code band once, before any breakpoint, so a branch there reaches
\ anywhere in the region (REGION is far inside b's 128 MB and jmp rel32's 2 GB)
\ and nothing is ever freed while a slot points at it: reserving at CP during
\ BPADD would hand the block to a failed REPL line's CP rollback. It starts as
\ traps, so a stray jump traps and a capture's walk over the gap finds no
\ fall-through into the next definition.
: TRAMP@ ( -- n ) data-base BPTRAMP-CELL + @ ;

HB-TARGET-LINUX-X86-64? 0= [if]

$D4200000 constant BRK0

\ The word that plants the trap at an entry, and the one that restores the
\ entry from the word a slot saved.
: TRAP-WORD ( ^u8 -- n ) drop BRK0 ;
: RESTORED ( n ^u8 -- n ) drop ;
: ALIGNED? ( n -- bool ) 3 and 0= ;

\ Slot idx's trampoline for the instruction at `at`: that instruction, then
\ `b at+4`, whose displacement from the trampoline's second word is at - t.
\ The handler degrades an instruction that reads or sets PC to one-shot when it
\ hits, so every instruction gets one.
: TRAMP! ( n n -- bool )
   {: at idx :}
   idx TRAMP-BYTES * TRAMP@ + {: t :}
   at 4 CODE-BYTES:AT drop W32@ t CODE!
   at t - A64ASM:ENC-B  t 4 + CODE!
   true ;

[then]

HB-TARGET-LINUX-X86-64? [if]

$CC constant INT3
$FFFFFF00 constant ABOVE-BYTE0

\ int3 takes the entry's first byte and leaves the three after it, which may
\ begin another breakpoint's word.
: TRAP-WORD ( ^u8 -- n ) W32@ ABOVE-BYTE0 and INT3 or ;
: RESTORED ( n ^u8 -- n ) W32@ ABOVE-BYTE0 and swap $FF and or ;
: ALIGNED? ( n -- bool ) drop true ;

\ Whether X64DIS decodes the instruction at code; a throw from outside the
\ decoder's block throws on.
: DECODES? ( str -- bool ) {: code:^ u:n :}
   [: code u X64DIS:SPAN drop drop ;] catch {: rc:n :}
   rc 0= if true exit then
   rc X64DIS:E-LAST < rc X64DIS:E-FIRST > or if rc throw then
   false ;

\ The trampoline being written: its address, its bytes and how many there are.
variable TRAMP-AT
create TRAMP-BUF TRAMP-BYTES allot
variable TRAMP-U

: TRAMP-OPEN ( n -- )
   TRAMP-AT !  0 TRAMP-U !
   TRAMP-BYTES 0 ?do INT3 TRAMP-BUF i + c! loop ;

: BYTE+ ( n -- )
   $FF and TRAMP-BUF TRAMP-U @ + c!  1 TRAMP-U +! ;

: DWORD+ ( n -- ) {: w:n :}
   4 0 ?do w i 8 * rshift BYTE+ loop ;

: BYTES+ ( str -- ) {: a:^ u:n :}
   u 0 ?do a i + c@ BYTE+ loop ;

\ A rel32 counts from the byte after it.
: REL32+ ( n -- )
   TRAMP-AT @ TRAMP-U @ + 4 + - DWORD+ ;

: JMP+ ( n -- ) $E9 BYTE+ REL32+ ;

: PUSH+ ( n -- ) $68 BYTE+ DWORD+ ;

: JCC+ ( n n -- ) {: cond:n to:n :}
   $0F BYTE+ $80 cond + BYTE+ to REL32+ ;

\ The instruction with the disp32 at offset disp, which counts from the
\ instruction's end, moved from `at` to the trampoline.
: MOVED+ ( n str n -- ) {: at:n code:^ len:n disp:n :}
   code disp BYTES+
   code disp + W32@ at TRAMP-AT @ - + DWORD+
   disp 4 + {: done:n :}
   code done + len done - BYTES+ ;

\ The bytes that do at the trampoline what the len-byte instruction at code
\ does at `at`, or false: no trampoline. A callee returns to the byte after
\ the call itself; `push imm32` holds that address, the code region lying
\ below 2 GB. `call r/m` gets none, as loop and jrcxz, which X64DIS rejects:
\ tier 0 begins no unit with them.
: RELOCATED? ( n str X64DIS:pc-use -- bool )
   {: at:n code:^ len:n use:X64DIS:pc-use :}
   at len + {: next:n :}
   use MATCH X64DIS:pc-use
      none OF code len BYTES+ next JMP+ true ENDOF
      operand OF {: disp:n :}
         at code len disp MOVED+ next JMP+ true ENDOF
      call OF {: delta:n :}
         next PUSH+ next delta + JMP+ true ENDOF
      jump OF next + JMP+ true ENDOF
      branch OF {: cond:n delta:n :}
         cond next delta + JCC+ next JMP+ true ENDOF
      pushed OF false ENDOF
   ;MATCH ;

: TRAMP-STORE ( -- )
   TRAMP-BYTES 0 ?do
      TRAMP-BUF i + W32@ TRAMP-AT @ i + CODE!
   4 +loop ;

\ Slot idx's trampoline for the instruction at `at`; false when X64DIS rejects
\ the instruction or it has no out-of-line form. The longest is 20 bytes, a
\ 15-byte instruction and `jmp rel32`.
: TRAMP! ( n n -- bool )
   {: at:n idx:n :}
   at  cp@ at - 15 min  CODE-BYTES:AT {: code:^ u:n :}
   code u DECODES? 0= if false exit then
   idx TRAMP-BYTES * TRAMP@ + TRAMP-OPEN
   code u X64DIS:SPAN {: len:n use:X64DIS:pc-use :}
   at code len use RELOCATED? dup if TRAMP-STORE then ;

[then]

\ A slot is filled while its address field holds a code pointer; an empty slot
\ holds BP-NULL, whose address reads 0.
: FILLED? ( n -- bool )  BP-SLOT-ADDR @ BP-NULL = 0= ;

\ The filled slot holding code address at, else -1. An empty slot never
\ matches, so an address of 0 finds nothing.
: FIND ( n -- n )
   {: at :}
   0 BEGIN dup MAXBP < WHILE
      dup FILLED? IF dup BP-SLOT-ADDR @ NULL-PTR - at = IF exit THEN THEN
      1 + REPEAT  drop -1 ;

\ An empty slot, else -1.
: FREE ( -- n )
   0 BEGIN dup MAXBP < WHILE  dup FILLED? 0= IF exit THEN  1 + REPEAT  drop -1 ;

\ BPADD ( xt ctrl -- ) : record + plant. ctrl = (skip << 1) | persistent, and
\ persistent is cleared when TRAMP! answers false.
: BP-SET-SLOT ( str n -- ) {: xt:^ ctrl idx :}
   xt idx BP-SLOT-ADDR !
   xt W32@ idx BP-SLOT-INSTR !
   0 idx BP-SLOT-HITS !
   ctrl idx BP-SLOT-CTRL ! ;

: BPADD ( n n -- ) {: at ctrl :}
   \ patch32 runs in engine text while opening the target's pages RW. An
   \ engine-text target can remove X from the patcher itself. Only the live
   \ compiled region is supported; refuse before publishing a slot or patching.
   \ That region lies inside CODE-BYTES:AT's code band, so AT never rejects it.
   at dbase@ DICT-SIZE + <  at cp@ 4 - > or  at ALIGNED? 0= or if
      E-BP-TARGET throw
   then
   at FIND 0 < 0= IF exit THEN                \ already set
   ctrl 1 and 0<>  TRAMP@ 0=  and if E-BP-TARGET throw then
   FREE dup 0 < IF
      drop s" bp: table full (8)" type EXIT-STATUS:FULL throw
   THEN   \ recoverable in the REPL
   {: idx :}
   at 4 CODE-BYTES:AT drop {: xt:^ :}
   ctrl  ctrl 1 and 0<> if  at idx TRAMP! 0= if 1 invert and then  then {: kept :}
   xt kept idx BP-SET-SLOT
   xt TRAP-WORD xt BP-PATCH32 ;

public

: BP+ ( [ R -- S ] -- )    XT>N 0 BPADD ;     \ one-shot
: BP* ( [ R -- S ] -- )    XT>N 1 BPADD ;     \ persistent (re-fires every call)
: BPN ( n [ R -- S ] -- )                     \ persistent, silent for the first n hits
   XT>N swap 1 lshift 1 or BPADD ;

\ Only a slot BPADD filled is patched back, through the pointer it stored, so
\ an xt with no breakpoint returns before any byte is touched.
: BP- ( [ R -- S ] -- )                       \ remove a breakpoint
   XT>N FIND dup 0 < IF drop exit THEN
   {: idx :}  idx BP-SLOT-ADDR @ {: at:^ :}
   idx BP-SLOT-INSTR @ at RESTORED at BP-PATCH32       \ restore orig instr, clear slot
   BP-NULL idx BP-SLOT-ADDR ! ;

: BP. ( -- )                                  \ list active breakpoints (addrs)
   0 BEGIN dup MAXBP < WHILE
      dup BP-SLOT-ADDR @ dup BP-NULL = 0= IF NULL-PTR BYTE-VIEW - . cr ELSE drop THEN
      1 + REPEAT  drop ;

\ The last of package DEBUG's three files: no later source reopens it.
seal-package
;package
