\ debug.f — breakpoints on compiled words. `' WORD BP+` plants a BRK #0 at the
\ word's entry; hitting it prints habu-bp: + pc + the stack top, restores the
\ original instruction and RESUMES (the engine's SIGTRAP handler does the work).
\ Up to 16 breakpoints at once, each one-shot (re-arm with BP+). `BP-` removes
\ an unhit one; `BP.` lists active ones. Baked into bin/hbi after repl/stepper.

$36D0 constant BPTAB             \ 16 x (addr, saved-instr), 16 B each; addr 0 = free
16 constant MAXBP
$D4200000 constant BRK0

: W32@ {: a :}
   a c@  a 1 + c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

: W32! {: w a :}
   w a c!  w 8 rshift a 1 + c!  w 16 rshift a 2 + c!  w 24 rshift a 3 + c! ;

: SLOT ( i -- a )  16 * BPTAB + DATAB + ;     \ &BPTAB[i]

: FIND {: addr :}   \ ( -- i | -1 ) slot holding addr, else -1
   0 BEGIN dup MAXBP < WHILE
      dup SLOT @ addr = IF exit THEN  1 + REPEAT  drop -1 ;

: FREE ( -- i | -1 )  0 FIND ;                \ a free slot (addr 0)

: BP+ {: xt :}                                \ ' WORD BP+ — break at the word's entry
   xt FIND 0 < 0= IF exit THEN                \ already set
   FREE dup 0 < IF drop s" bp: table full (16)" 76 die THEN
   SLOT                                       \ ( s )  — one locals group per word
   xt over !  xt W32@ over 8 + !  drop        \ record addr + original instr
   BRK0 xt patch32 ;

: BP- {: xt :}                                \ remove an unhit breakpoint
   xt FIND dup 0 < IF drop exit THEN
   SLOT                                       \ ( s )
   dup 8 + @ xt patch32                       \ restore original instr
   0 swap ! ;                                 \ clear the slot addr

: BP. ( -- )                                  \ list active breakpoints (addrs)
   0 BEGIN dup MAXBP < WHILE
      dup SLOT @ dup IF . cr ELSE drop THEN  1 + REPEAT  drop ;
