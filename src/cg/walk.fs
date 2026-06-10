\ walk.fs — tokenize a Forth word BODY and drive the ICode generators in
\ templ.fs, then wrap it as a runnable Mach-O. Numbers emit a literal push; every
\ other token is looked up in CG-PRIMS. The bridge: checked-Forth source ->
\ ARM64 machine code -> native Mac executable.

require templ.fs
require regstack.fs                      \ abstract value stack (register allocation)
require opt.fs
require exec.fs
require cglocals.fs                      \ compile-time locals ({: a b :})

\ Non-primitive token hook: link.fs sets this to emit a BL to another caf word
\ (or RECURSE). Default: not a call.
defer EMIT-CALL   ( a u -- handled? )
:noname ( a u -- f ) 2drop false ;  is EMIT-CALL

: EMIT-PRIM ( a u -- )                  \ throws E-NO-ENC on an unmodeled word
   2dup CG-PRIMS search-wordlist ?dup if  drop nip nip execute  exit  then
   2dup EMIT-CALL if  2drop exit  then
   2drop E-NO-ENC throw ;

\ A numeric literal pushes a VS constant; a VS primitive runs on the register
\ stack; anything else (control flow, calls, return-stack ops, unsupported words)
\ first SPILLS the VS to memory, then takes the proven memory path. The VS folds
\ constants and selects immediate shifts itself, so no separate folding here.
: EMIT-TOKEN ( a u -- )
   2dup CHECK-LOCAL-CG if  2drop exit then                    \ {: a b :} / local-name ref
   2dup s>number? if  2>r 2drop 2r> d>s v-pushc  exit then    \ literal -> VS constant
   2drop
   2dup CG-VS search-wordlist ?dup if  drop nip nip execute  exit  then   \ VS primitive
   v-spill  EMIT-PRIM ;                                       \ spill, then memory-path op

: WALK-BODY {: a u | end cur ts :}
   v-reset
   a u + to end   a to cur
   begin
      begin cur end < cur c@ bl = and while cur 1+ to cur repeat
      cur end <
   while
      cur to ts
      begin cur end < cur c@ bl <> and while cur 1+ to cur repeat
      ts  cur ts -  EMIT-TOKEN
   repeat
   v-spill ;                             \ materialise the register stack to memory

\ Compile a body with one i64 input pushed first; the body's TOS becomes exit().
: COMPILE-WORD {: ba bu input -- :}
   ICODE-RESET  cf-reset  cgl-reset
   512 g-prologue
   input g-lit
   NEWLBL EPILOG !        \ EXIT branches here
   ba bu WALK-BODY
   EPILOG @ LBL,
   g-exit-tos
   OPTIMIZE ;             \ peephole the complete IR

: NATIVE-EVAL ( ba bu input -- exit-code )
   COMPILE-WORD  s" /tmp/caf-word" RUN-EXE ;
