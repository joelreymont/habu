\ debug.f — breakpoints on compiled words. Plant a BRK #0 at a word's entry;
\ hitting it prints habu-bp: + pc + the stack top, then either resumes (one-shot,
\ removed) or re-arms (persistent) — the engine's SIGTRAP handler does the work.
\   ' WORD BP+      one-shot   (fires once, then gone)
\   ' WORD BP*      persistent (fires every call; emulates the entry prologue)
\   N ' WORD BPN    persistent, but silent for the first N hits (skip-count)
\   ' WORD BP-      remove      BP. = list active breakpoints (addrs)
\ Up to 8 at once. Baked into bin/hb after repl/stepper.

8 constant MAXBP
$D4200000 constant BRK0

: W32@ ( ptr u8 -- n ) {: a :}
   a c@  a 1 + c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

: W32! ( n ptr u8 -- ) {: w a :}
   w a c!  w 8 rshift a 1 + c!  w 16 rshift a 2 + c!  w 24 rshift a 3 + c! ;

: SLOT-OFF ( n -- n )
   32 * BPTAB-OFF + ;

TRUSTED: BP-SLOT-ADDR ( n -- ptr ptr u8 )
   SLOT-OFF DATAB + ;

TRUSTED: BP-SLOT-INSTR ( n -- ptr n )
   SLOT-OFF 8 + DATAB + ;

TRUSTED: BP-SLOT-HITS ( n -- ptr n )
   SLOT-OFF 16 + DATAB + ;

TRUSTED: BP-SLOT-CTRL ( n -- ptr n )
   SLOT-OFF 24 + DATAB + ;

TRUSTED: BP-NULL ( -- ptr u8 )
   0 ;

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
   FREE dup 0 < IF drop s" bp: table full (8)" EMITS 76 throw THEN   \ recoverable in the REPL
   xt ctrl rot BP-SET-SLOT
   BRK0 xt BP-PATCH32 ;

: BPADD ( n n -- ) {: xt ctrl :}
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
