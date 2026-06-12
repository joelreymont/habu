\ debug.f — one-shot breakpoints on compiled words. ' WORD BP+ plants a BRK #0
\ at the word's entry; hitting it prints habu-bp: + pc + the stack top,
\ restores the word, and RESUMES (the engine's SIGTRAP handler does the work —
\ this file is only the setter). BP- removes an unhit breakpoint. Baked into
\ bin/hbi after repl.f/stepper.f (uses their DATAB); hb-build programs never
\ see it.

$36C0 constant BPA-CELL
$36C8 constant BPI-CELL
$D4200000 constant BRK0

: W32@ {: a :}
   a c@  a 1 + c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

: W32! {: w a :}
   w a c!  w 8 rshift a 1 + c!  w 16 rshift a 2 + c!  w 24 rshift a 3 + c! ;

: BP+ {: xt :}                  \ ' WORD BP+ — one-shot break at the word's entry
   DATAB BPA-CELL + @ 0 = 0= IF 70 throw THEN
   xt W32@ DATAB BPI-CELL + !
   xt DATAB BPA-CELL + !
   BRK0 xt patch32 ;

: BP- ( -- )                    \ remove an unhit breakpoint
   DATAB BPA-CELL + @ {: a :}
   a 0 = IF exit THEN
   DATAB BPI-CELL + @ a patch32
   0 DATAB BPA-CELL + ! ;
