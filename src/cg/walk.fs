\ walk.fs — tokenize a Forth word BODY and drive the ICode generators in
\ templ.fs, then wrap it as a runnable Mach-O. Numbers emit a literal push; every
\ other token is looked up in CG-PRIMS. The bridge: checked-Forth source ->
\ ARM64 machine code -> native Mac executable.

require templ.fs
require regstack.fs                      \ abstract value stack (register allocation)
require opt.fs
require exec.fs
require cglocals.fs                      \ compile-time locals ({: a b :})
require cgquot.fs                        \ AOT quotation/combinator inlining
require cgloop.fs                        \ register-resident DO..LOOP mechanism

\ Non-primitive token hook: link.fs sets this to emit a BL to another caf word
\ (or RECURSE). Default: not a call.
defer EMIT-CALL   ( a u -- handled? )
:noname ( a u -- f ) 2drop false ;  is EMIT-CALL

: EMIT-PRIM ( a u -- )                  \ throws E-NO-ENC on an unmodeled word
   2dup CG-PRIMS search-wordlist ?dup if  drop nip nip execute  exit  then
   2dup EMIT-CALL if  2drop exit  then
   2drop E-NO-ENC throw ;

\ Float literal -> VS constant holding the f64's IEEE-754 bits (the native code
\ keeps floats as data-stack cells; FP prims move them X<->D). Reinterpret via a
\ scratch float store. FP-MARK? (checker.fs) gates so plain ints fall through.
create FBUF 1 floats allot
: EMIT-FLOAT ( a u -- f )
   2dup FP-MARK? 0= if 2drop false exit then
   2dup >float if  FBUF f!  2drop  FBUF @ v-pushc  true
              else 2drop false then ;

\ --- body cursor (module-level so the DO..LOOP scanner can look ahead). Saved /
\ restored across WALK-BODY so quotation inlining (re-entrant) is safe. ---
variable WB-CUR   variable WB-END
: WB-SKIP ( -- )  begin WB-CUR @ WB-END @ < WB-CUR @ c@ bl = and while 1 WB-CUR +! repeat ;
: WB-NEXT ( -- a u )                     \ next token, or ( cur 0 ) at end
   WB-SKIP  WB-CUR @ WB-END @ >= if  WB-CUR @ 0 exit then
   WB-CUR @ {: ts :}
   begin WB-CUR @ WB-END @ < WB-CUR @ c@ bl <> and while 1 WB-CUR +! repeat
   ts  WB-CUR @ ts - ;

\ A token is "VS-safe" inside a register loop iff emitting it never spills the VS:
\ a register-allocated primitive, the loop index I, or a numeric/float literal.
: NUMERIC? ( a u -- f )
   2dup NUMBER? if  2drop true exit then
   2dup FP-MARK? if  2dup >float if  fdrop 2drop true exit then  then
   2drop false ;
: VS-SAFE-TOK? ( a u -- f )
   2dup CG-VS find-name-in if  2drop true exit then
   2dup s" I" CI= if  2drop true exit then
   NUMERIC? ;

\ Scan from WB-CUR (just past DO/?DO) to the matching LOOP. Returns the body span
\ ( a u ) and true iff every token is VS-safe and there is no nested loop; else
\ restores WB-CUR and returns false.
: t= ( a u ca cu -- a u f )  {: ca cu :}  2dup ca cu CI= ;
: body-straight? ( -- a u true | false )
   WB-CUR @ {: bstart :}
   begin
      WB-CUR @ {: before :}
      WB-NEXT dup 0= if  2drop  bstart WB-CUR !  false exit then
      s" LOOP" t= if  2drop  bstart  before bstart -  true exit then
      s" DO"   t= if  2drop  bstart WB-CUR !  false exit then
      s" ?DO"  t= if  2drop  bstart WB-CUR !  false exit then
      VS-SAFE-TOK? 0= if  bstart WB-CUR !  false exit then
   again ;

\ A numeric literal pushes a VS constant; a VS primitive runs on the register
\ stack; anything else (control flow, calls, return-stack ops, unsupported words)
\ first SPILLS the VS to memory, then takes the proven memory path. LOOP-HOOK
\ (bound below) intercepts a register-eligible DO..LOOP before the spill.
defer LOOP-HOOK   ( a u -- f )
:noname ( a u -- f ) 2drop false ;  is LOOP-HOOK
: EMIT-TOKEN ( a u -- )
   2dup CHECK-QUOT-CG  if  2drop exit then                    \ [: … ;] capture / EXECUTE / DIP
   2dup CHECK-LOCAL-CG if  2drop exit then                    \ {: a b :} / local-name ref
   2dup LOOP-HOOK      if  2drop exit then                    \ register-resident DO..LOOP
   2dup EMIT-FLOAT     if  2drop exit then                    \ float literal -> f64-bits VS const
   2dup s>number? if  2>r 2drop 2r> d>s v-pushc  exit then    \ literal -> VS constant
   2drop
   2dup CG-VS search-wordlist ?dup if  drop nip nip execute  exit  then   \ VS primitive
   v-spill  EMIT-PRIM ;                                       \ spill, then memory-path op

: WALK-BODY {: a u | sc se :}
   WB-CUR @ to sc  WB-END @ to se         \ save cursor (re-entrant via quotation inlining)
   v-reset
   a u + WB-END !  a WB-CUR !
   begin WB-NEXT dup 0> while EMIT-TOKEN repeat 2drop
   v-spill                                \ materialise the register stack to memory
   sc WB-CUR !  se WB-END ! ;
' WALK-BODY is WALK-INLINE               \ let cgquot.fs inline quotation bodies

\ --- register-resident DO..LOOP ---
\ Emit a loop whose carry stays in registers across the back-edge. ( qdo? ba bu );
\ ba bu is the body span. Throws E-RLOOP if the body turns out to touch memory
\ below the VS (depth mismatch) — the caller then rolls back to the memory path.
: walk-span {: a u -- :}                 \ walk a token span with a local cursor (no WB-CUR)
   a u + {: e :}  a {: c :}
   begin
      begin c e < c c@ bl = and while c 1+ to c repeat  c e <
   while
      c {: ts :}  begin c e < c c@ bl <> and while c 1+ to c repeat  ts  c ts - EMIT-TOKEN
   repeat ;
: emit-rloop {: qdo? ba bu -- :}
   loop-save
   v-popr {: s :}  LIDX s MOV,  s r-free          \ index (start) -> LIDX
   v-popr {: l :}  LLIM l MOV,  l r-free          \ limit -> LLIM
   carry-snap                                     \ pin carry into homes (before the skip test)
   LIDX REG-PIN  LLIM REG-PIN                      \ loop-control + carry regs are loop-carried:
   CARRY-N @ 0 ?do  CARRY-R i cells + @ REG-PIN  loop   \ the optimizer must not touch them
   NEWLBL {: lexit :}
   qdo? if  LIDX LLIM CMP,  C-GE lexit BCOND,  then   \ ?DO: skip body if index>=limit
   NEWLBL {: ltop :}  ltop LBL,
   ba bu walk-span
   carry-recon                                    \ carry-out -> homes (or E-RLOOP)
   LIDX LIDX 1 ADDI,  LIDX LLIM CMP,  C-LT ltop BCOND,
   lexit LBL,  loop-rest  carry-restore ;

: CHECK-LOOP-CG ( a u -- f )
   2dup s" DO" CI=  >r  2dup s" ?DO" CI=  r> or  0= if  2drop false exit then
   WB-CUR @ {: savecur :}
   2dup s" ?DO" CI= {: qdo :}  2drop
   body-straight? 0= if  savecur WB-CUR !  false exit then    ( ba bu )
   cg-snapshot
   2>r  qdo if 1 else 0 then  2r>                 ( qdo ba bu )
   ['] emit-rloop catch ?dup if
      E-RLOOP = if  cg-rollback  savecur WB-CUR !  false exit then  throw
   then  true ;
' CHECK-LOOP-CG is LOOP-HOOK

\ Compile a body with one i64 input pushed first; the body's TOS becomes exit().
: COMPILE-WORD {: ba bu input -- :}
   ICODE-RESET  cf-reset  cgl-reset  q-reset  PIN-RESET
   512 g-prologue
   g-heap-init                          \ entry: mmap the bump heap (HP); callees inherit it
   input g-lit
   NEWLBL EPILOG !        \ EXIT branches here
   ba bu WALK-BODY
   EPILOG @ LBL,
   g-exit-tos
   OPTIMIZE ;             \ peephole the complete IR

: NATIVE-EVAL ( ba bu input -- exit-code )
   COMPILE-WORD  s" /tmp/caf-word" RUN-EXE ;
