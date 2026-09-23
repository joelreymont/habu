\ debug.f — breakpoints on compiled words. Plant a BRK #0 at a word's entry;
\ hitting it prints habu-bp: + pc + the stack top, then either resumes (one-shot,
\ removed) or re-arms (persistent) — the engine's SIGTRAP handler does the work.
\   ' WORD BP+      one-shot   (fires once, then gone)
\   ' WORD BP*      persistent (fires every call; emulates the entry prologue)
\   N ' WORD BPN    persistent, but silent for the first N hits (skip-count)
\   ' WORD BP-      remove      BP. = list active breakpoints (addrs)
\ Up to 8 at once. Not in the engine: `require src/habu/debug.f` loads it, the
\ stepper and the shared watch cells over the baked repl.

require src/habu/debug-watch.f
require src/habu/stepper.f

8 constant MAXBP
$D4200000 constant BRK0

\ Debugger-owned refusal range, outside the library and build-tool ranges.
-9300 constant E-BP-FIRST
-9309 constant E-BP-LAST
-9300 constant E-BP-TARGET

: W32@ ( ptr u8 -- n ) {: a :}
   a c@  a 1 + c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

: W32! ( n ptr u8 -- ) {: w a :}
   w a c!  w 8 rshift a 1 + c!  w 16 rshift a 2 + c!  w 24 rshift a 3 + c! ;

: SLOT-OFF ( n -- n )
   32 * BPTAB-OFF + ;

\ These helpers type the four fixed DATA slot fields, a null/code-pointer view,
\ raw address display, and executable patching.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: BP-SLOT-ADDR ( n -- ptr ptr u8 )
   SLOT-OFF data-base + ;

TRUSTED: BP-SLOT-INSTR ( n -- ptr n )
   SLOT-OFF 8 + data-base + ;

TRUSTED: BP-SLOT-HITS ( n -- ptr n )
   SLOT-OFF 16 + data-base + ;

TRUSTED: BP-SLOT-CTRL ( n -- ptr n )
   SLOT-OFF 24 + data-base + ;

: BP-NULL ( -- ptr u8 )
   NULL-PTR ;

TRUSTED: BP-PRINT-ADDR ( ptr u8 -- )
   . ;

TRUSTED: BP-PATCH32 ( n ptr u8 -- )
   patch32 ;

TRUSTED: BP-XT>PTR ( n -- ptr u8 )
   ;

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
   xt dbase@ DICT-SIZE + <  xt cp@ 4 - > or  xt 3 and 0<> or if
      E-BP-TARGET throw
   then
   xt BP-XT>PTR ctrl BPADD-PTR ;

: BP+ ( n -- )    0 BPADD ;                  \ one-shot
: BP* ( n -- )    1 BPADD ;                  \ persistent (re-fires every call)
: BPN ( n n -- )  swap 1 lshift 1 or BPADD ; \ persistent, silent for the first n hits

: BP- ( n -- ) {: xt :}                      \ remove a breakpoint
   xt BP-XT>PTR {: xp:ptr :}
   xp FIND dup 0 < IF drop exit THEN
   dup BP-SLOT-INSTR @ xp BP-PATCH32          \ restore orig instr, clear slot
   BP-NULL swap BP-SLOT-ADDR ! ;

: BP. ( -- )                                  \ list active breakpoints (addrs)
   0 BEGIN dup MAXBP < WHILE
      dup BP-SLOT-ADDR @ dup BP-NULL = 0= IF BP-PRINT-ADDR cr ELSE drop THEN
      1 + REPEAT  drop ;
