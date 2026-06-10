\ walk.fs — tokenize a Forth word BODY and drive the ICode generators in
\ templ.fs, then wrap it as a runnable Mach-O. Numbers emit a literal push; every
\ other token is looked up in CG-PRIMS. The bridge: checked-Forth source ->
\ ARM64 machine code -> native Mac executable.

require templ.fs
require opt.fs
require exec.fs

\ Non-primitive token hook: link.fs sets this to emit a BL to another caf word
\ (or RECURSE). Default: not a call.
defer EMIT-CALL   ( a u -- handled? )
:noname ( a u -- f ) 2drop false ;  is EMIT-CALL

: EMIT-PRIM ( a u -- )                  \ throws E-NO-ENC on an unmodeled word
   2dup CG-PRIMS search-wordlist ?dup if  drop nip nip execute  exit  then
   2dup EMIT-CALL if  2drop exit  then
   2drop E-NO-ENC throw ;

: EMIT-TOKEN ( a u -- )
   2dup s>number? if  2>r 2drop 2r> d>s g-lit
   else  2drop EMIT-PRIM  then ;

: WALK-BODY {: a u | end cur ts :}
   a u + to end   a to cur
   begin
      begin cur end < cur c@ bl = and while cur 1+ to cur repeat
      cur end <
   while
      cur to ts
      begin cur end < cur c@ bl <> and while cur 1+ to cur repeat
      ts  cur ts -  EMIT-TOKEN
   repeat ;

\ Compile a body with one i64 input pushed first; the body's TOS becomes exit().
: COMPILE-WORD {: ba bu input -- :}
   ICODE-RESET  cf-reset
   512 g-prologue
   input g-lit
   NEWLBL EPILOG !        \ EXIT branches here
   ba bu WALK-BODY
   EPILOG @ LBL,
   g-exit-tos
   OPTIMIZE ;             \ peephole the complete IR

: NATIVE-EVAL ( ba bu input -- exit-code )
   COMPILE-WORD  s" /tmp/caf-word" RUN-EXE ;
