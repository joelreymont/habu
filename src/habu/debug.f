\ debug.f — breakpoints on compiled words. Plant a BRK #0 at a word's entry;
\ hitting it prints habu-bp: + pc + the stack top, then either resumes (one-shot,
\ removed) or re-arms (persistent) — the engine's SIGTRAP handler does the work.
\   ' WORD BP+      one-shot   (fires once, then gone)
\   ' WORD BP*      persistent (fires every call; emulates the entry prologue or
\                   runs the first instruction out of line, else degrades to
\                   one-shot when that instruction reads or sets PC)
\   N ' WORD BPN    persistent, but silent for the first N hits (skip-count)
\   ' WORD BP-      remove      BP. = list active breakpoints (addrs)
\ Up to 8 at once. The ARM64 product bakes it, the stepper and the shared watch
\ cells after the repl (src/habu/native-runtime.f), and the x86-64 product the
\ two without it, having no breakpoint handler; the hb-stdin recovery engine still
\ loads them with `require src/habu/debug.f`.
\
\ The three files make one package, DEBUG. Its publics are these commands, the
\ watch commands (BPW+ BPW- BPW. BPW-CLEAR), STEP, the two installers, MAXBP and
\ the E-BP- codes; the rest is private, so a program keeps its own FIND, FREE or
\ STEP. A session opens `using DEBUG` once and types the commands bare, or
\ qualifies one: `' WORD DEBUG:BP+`.

require src/habu/code-bytes.f
require src/habu/debug-watch.f
require src/habu/stepper.f

package DEBUG

$D4200000 constant BRK0

public

8 constant MAXBP

\ Debugger-owned refusal range, outside the library and build-tool ranges.
-9300 constant E-BP-FIRST
-9309 constant E-BP-LAST
-9300 constant E-BP-TARGET

private

: W32@ ( ptr u8 -- n ) {: a :}
   a c@  a 1 + c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

: SLOT-OFF ( n -- n )
   32 * BPTAB-OFF + ;

\ The four fixed DATA fields of a breakpoint slot. The address field holds a
\ code pointer, so it is reached through `ptr-field`, the declared pointer-cell
\ door; the other three hold numbers.
: BP-SLOT-ADDR ( n -- ptr ptr u8 )
   SLOT-OFF data-base + 0 ptr-field ;

: BP-SLOT-INSTR ( n -- ptr n )
   SLOT-OFF 8 + data-base + ;

: BP-SLOT-HITS ( n -- ptr n )
   SLOT-OFF 16 + data-base + ;

: BP-SLOT-CTRL ( n -- ptr n )
   SLOT-OFF 24 + data-base + ;

: BP-NULL ( -- ptr u8 )
   NULL-PTR ;

: BP-PATCH32 ( n ptr u8 -- ) patch32 ;

\ A code address reaches checked code as a number; CODE-BYTES:AT is the one
\ bounded view of the bytes there.
CAST: XT>N ( [ R -- S ] -- n )
CAST: N>XT ( n -- [ -- ] )

: CODE! ( n n -- ) {: w at :}
   w  at 4 CODE-BYTES:AT drop  BP-PATCH32 ;

\ ---- trampolines --------------------------------------------------------------
\ A persistent breakpoint whose first instruction the handler does not emulate
\ runs that instruction out of line (src/habu/habu2.f C-BP-EMULATE): slot idx
\ owns the two words at [BPTRAMP-CELL] + idx * 8, the instruction and a `b` back
\ to the one after it. The block is reserved in the code band once, here at
\ load, so the branch reaches anywhere in the region (REGION is far inside b's
\ 128 MB) and nothing is ever freed while a slot points at it: reserving at CP
\ during BPADD would hand the block to a failed REPL line's CP rollback. Each
\ word starts as a BRK, so a stray jump traps and a capture's walk over the gap
\ finds no fall-through into the next definition. xt! declares the cell, so a
\ snapshot and the product capture move it with the code.
8 constant TRAMP-BYTES
$14000000 constant B-OP

: TRAMP@ ( -- n ) data-base BPTRAMP-CELL + @ ;

: TRAMP-INSTALL ( -- )
   cp@ {: base :}
   base MAXBP TRAMP-BYTES * + cp!
   MAXBP TRAMP-BYTES * 0 ?do BRK0 base i + CODE! 4 +loop
   base N>XT data-base BPTRAMP-CELL + xt! ;

TRAMP-INSTALL

\ Slot idx's trampoline for the instruction at `at`: that instruction, then
\ `b at+4`, whose displacement from the trampoline's second word is at - t.
: TRAMP! ( n n n -- ) {: instr at idx :}
   idx TRAMP-BYTES * TRAMP@ + {: t :}
   instr t CODE!
   at t - 2 rshift $3FFFFFF and B-OP or  t 4 + CODE! ;

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

\ BPADD ( xt ctrl -- ) : record + plant. ctrl = (skip << 1) | persistent.
: BP-SET-SLOT ( ptr u8 n n -- ) {: xt:ptr ctrl idx :}
   xt idx BP-SLOT-ADDR !
   xt W32@ idx BP-SLOT-INSTR !
   0 idx BP-SLOT-HITS !
   ctrl idx BP-SLOT-CTRL ! ;

: BPADD ( n n -- ) {: at ctrl :}
   \ patch32 runs in engine text while opening the target's pages RW. An
   \ engine-text target can remove X from the patcher itself. Only the live
   \ compiled region is supported; refuse before publishing a slot or patching.
   \ That region lies inside CODE-BYTES:AT's code band, so AT cannot die.
   at dbase@ DICT-SIZE + <  at cp@ 4 - > or  at 3 and 0<> or if
      E-BP-TARGET throw
   then
   at FIND 0 < 0= IF exit THEN                \ already set
   FREE dup 0 < IF drop s" bp: table full (8)" type 76 throw THEN   \ recoverable in the REPL
   {: idx :}
   at 4 CODE-BYTES:AT drop {: xt:ptr :}
   xt ctrl idx BP-SET-SLOT
   ctrl 1 and 0<> if  idx BP-SLOT-INSTR @ at idx TRAMP!  then
   BRK0 xt BP-PATCH32 ;

public

: BP+ ( [ R -- S ] -- )    XT>N 0 BPADD ;     \ one-shot
: BP* ( [ R -- S ] -- )    XT>N 1 BPADD ;     \ persistent (re-fires every call)
: BPN ( n [ R -- S ] -- )                     \ persistent, silent for the first n hits
   XT>N swap 1 lshift 1 or BPADD ;

\ Only a slot BPADD filled is patched back, through the pointer it stored, so
\ an xt with no breakpoint returns before any byte is touched.
: BP- ( [ R -- S ] -- )                       \ remove a breakpoint
   XT>N FIND dup 0 < IF drop exit THEN
   dup BP-SLOT-INSTR @ over BP-SLOT-ADDR @ BP-PATCH32   \ restore orig instr, clear slot
   BP-NULL swap BP-SLOT-ADDR ! ;

: BP. ( -- )                                  \ list active breakpoints (addrs)
   0 BEGIN dup MAXBP < WHILE
      dup BP-SLOT-ADDR @ dup BP-NULL = 0= IF NULL-PTR BYTE-VIEW - . cr ELSE drop THEN
      1 + REPEAT  drop ;

;package
