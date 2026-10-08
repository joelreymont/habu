\ debug.f — breakpoints on compiled words. Plant a BRK #0 at a word's entry;
\ hitting it prints habu-bp: + pc + the stack top, then either resumes (one-shot,
\ removed) or re-arms (persistent) — the engine's SIGTRAP handler does the work.
\   ' WORD BP+      one-shot   (fires once, then gone)
\   ' WORD BP*      persistent (fires every call; emulates the entry prologue)
\   N ' WORD BPN    persistent, but silent for the first N hits (skip-count)
\   ' WORD BP-      remove      BP. = list active breakpoints (addrs)
\ Up to 8 at once. The product bakes it, the stepper and the shared watch cells
\ after the repl (src/habu/native-runtime.f); the hb-stdin recovery engine still
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

\ The instruction word at an xt: CODE-BYTES:AT is the one bounded view of a code
\ address, and it dies on one outside the code region.
: XT>CODE ( n -- ptr u8 )
   4 CODE-BYTES:AT drop ;

: FIND ( ptr u8 -- n ) {: addr:ptr :}   \ slot holding addr, else -1
   0 BEGIN dup MAXBP < WHILE
      dup BP-SLOT-ADDR @ addr = IF exit THEN  1 + REPEAT  drop -1 ;

: FREE ( -- n )  BP-NULL FIND ;                \ a free slot (addr 0)

\ BPADD ( xt ctrl -- ) : record + plant. ctrl = (skip << 1) | persistent.
: BP-SET-SLOT ( ptr u8 n n -- ) {: xt:ptr ctrl idx :}
   xt idx BP-SLOT-ADDR !
   xt W32@ idx BP-SLOT-INSTR !
   0 idx BP-SLOT-HITS !
   ctrl idx BP-SLOT-CTRL ! ;

: BPADD-PTR ( ptr u8 n -- ) {: xt:ptr ctrl :}
   xt FIND 0 < 0= IF exit THEN                \ already set
   FREE dup 0 < IF drop s" bp: table full (8)" type 76 throw THEN   \ recoverable in the REPL
   xt ctrl rot BP-SET-SLOT
   BRK0 xt BP-PATCH32 ;

: BPADD ( n n -- ) {: xt ctrl :}
   \ patch32 runs in engine text while opening the target's pages RW. An
   \ engine-text target can remove X from the patcher itself. Only the live
   \ compiled region is supported; refuse before publishing a slot or patching.
   \ That region lies inside CODE-BYTES:AT's code band, so XT>CODE cannot die.
   xt dbase@ DICT-SIZE + <  xt cp@ 4 - > or  xt 3 and 0<> or if
      E-BP-TARGET throw
   then
   xt XT>CODE ctrl BPADD-PTR ;

public

: BP+ ( n -- )    0 BPADD ;                  \ one-shot
: BP* ( n -- )    1 BPADD ;                  \ persistent (re-fires every call)
: BPN ( n n -- )  swap 1 lshift 1 or BPADD ; \ persistent, silent for the first n hits

\ An xt outside the code region has no breakpoint to remove; returning before
\ XT>CODE keeps a mistyped number from killing the REPL session.
: BP- ( n -- ) {: xt :}                      \ remove a breakpoint
   xt 4 CODE-BYTES:IN-CODE? 0= IF exit THEN
   xt XT>CODE {: xp:ptr :}
   xp FIND dup 0 < IF drop exit THEN
   dup BP-SLOT-INSTR @ xp BP-PATCH32          \ restore orig instr, clear slot
   BP-NULL swap BP-SLOT-ADDR ! ;

: BP. ( -- )                                  \ list active breakpoints (addrs)
   0 BEGIN dup MAXBP < WHILE
      dup BP-SLOT-ADDR @ dup BP-NULL = 0= IF NULL-PTR BYTE-VIEW - . cr ELSE drop THEN
      1 + REPEAT  drop ;

;package
