\ debug.f — breakpoints on compiled words. Plant a BRK #0 at a word's entry;
\ hitting it prints habu-bp: + pc + the stack top, then either resumes (one-shot,
\ removed) or re-arms (persistent) — the engine's SIGTRAP handler does the work.
\   ' WORD BP+      one-shot   (fires once, then gone)
\   ' WORD BP*      persistent (fires every call; emulates the entry prologue)
\   N ' WORD BPN    persistent, but silent for the first N hits (skip-count)
\   ' WORD BP-      remove      BP. = list active breakpoints (addrs)
\ Up to 8 at once. Baked into bin/hbi after repl/stepper.

$36D0 constant BPTAB             \ 8 x (addr, saved-instr, hits, ctrl) 32 B each
8 constant MAXBP
$D4200000 constant BRK0

: W32@ {: a :}
   a c@  a 1 + c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

: W32! {: w a :}
   w a c!  w 8 rshift a 1 + c!  w 16 rshift a 2 + c!  w 24 rshift a 3 + c! ;

: SLOT ( i -- a )  32 * BPTAB + DATAB + ;     \ &BPTAB[i]

: FIND {: addr :}   \ ( -- i | -1 ) slot holding addr, else -1
   0 BEGIN dup MAXBP < WHILE
      dup SLOT @ addr = IF exit THEN  1 + REPEAT  drop -1 ;

: FREE ( -- i | -1 )  0 FIND ;                \ a free slot (addr 0)

\ BPADD ( xt ctrl -- ) : record + plant. ctrl = (skip << 1) | persistent.
: BPADD {: xt ctrl :}
   xt FIND 0 < 0= IF exit THEN                \ already set
   FREE dup 0 < IF drop s" bp: table full (8)" 76 die THEN
   SLOT                                       \ ( s )  one locals group per word
   xt over !  xt W32@ over 8 + !  0 over 16 + !  ctrl over 24 + !  drop
   BRK0 xt patch32 ;

: BP+ ( xt -- )    0 BPADD ;                  \ one-shot
: BP* ( xt -- )    1 BPADD ;                  \ persistent (re-fires every call)
: BPN ( n xt -- )  swap 1 lshift 1 or BPADD ; \ persistent, silent for the first n hits

: BP- {: xt :}                                \ remove a breakpoint
   xt FIND dup 0 < IF drop exit THEN
   SLOT  dup 8 + @ xt patch32  0 swap ! ;     \ restore orig instr, clear slot

: BP. ( -- )                                  \ list active breakpoints (addrs)
   0 BEGIN dup MAXBP < WHILE
      dup SLOT @ dup IF . cr ELSE drop THEN  1 + REPEAT  drop ;
