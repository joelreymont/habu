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

\ --- compile-time constant folding -------------------------------------------
\ Numeric literals are DEFERRED onto a compile-time value stack instead of
\ emitted; a foldable op over two (or one) pending constants folds them at
\ compile time. Any other token first FLUSHES the pending constants (emits a
\ g-lit each, bottom-first) so the data stack is materialised before it runs —
\ so runtime values never get mixed up with deferred ones. Only ops whose gforth
\ semantics match the emitted ARM64 exactly are foldable (no /,MOD,2/: division
\ rounding / shift signedness differ).
32 constant MAXCTS
create CTS MAXCTS cells allot   variable #CTS
: CTS-RESET ( -- )  0 #CTS ! ;
: CTS-FLUSH ( -- )  #CTS @ 0 ?do  CTS i cells + @ g-lit  loop  CTS-RESET ;
: CTS-PUSH  ( n -- )  #CTS @ MAXCTS >= if CTS-FLUSH then
   CTS #CTS @ cells + !  1 #CTS +! ;
: CTS-POP   ( -- n )  -1 #CTS +!  CTS #CTS @ cells + @ ;

: FOLD2 ( xt -- f )   \ a b OP -> fold if both pending; else leave for normal emit
   #CTS @ 2 < if drop false exit then
   CTS-POP CTS-POP swap rot execute CTS-PUSH true ;
: FOLD1 ( xt -- f )   \ n OP -> fold if pending
   #CTS @ 1 < if drop false exit then
   CTS-POP swap execute CTS-PUSH true ;

\ Constant SHIFT amount with a runtime value below → emit an IMMEDIATE shift
\ (LSL/LSR #k) instead of materialising k and using a register shift (LSLV/LSRV).
\ One pending const = the shift amount (a runtime value sits on the data stack
\ below it; the checker guarantees it). Two pending = both const → plain fold.
: P-LSHI ( k -- )  T0 g-pop  T0 T0 rot LSLI,  T0 g-push ;
: P-RSHI ( k -- )  T0 g-pop  T0 T0 rot LSRI,  T0 g-push ;
: FOLD-SH ( imm-gen folding-xt -- f )
   #CTS @ 2 >= if  nip FOLD2  exit then        \ both const → fold the value
   #CTS @ 1  =  if  drop CTS-POP swap execute true  exit then  \ const shift → immediate
   2drop false ;                               \ runtime shift → normal (register) emit

wordlist constant CG-FOLD               \ foldable token -> folder (returns f)
get-current  CG-FOLD set-current
: + ['] + FOLD2 ;       : - ['] - FOLD2 ;       : * ['] * FOLD2 ;
: AND ['] and FOLD2 ;   : OR ['] or FOLD2 ;     : XOR ['] xor FOLD2 ;
: LSHIFT ['] P-LSHI ['] lshift FOLD-SH ;  : RSHIFT ['] P-RSHI ['] rshift FOLD-SH ;
: 1+ ['] 1+ FOLD1 ;     : 1- ['] 1- FOLD1 ;     : NEGATE ['] negate FOLD1 ;
: INVERT ['] invert FOLD1 ;  : 2* ['] 2* FOLD1 ;
set-current
: FOLD-OP ( a u -- f )  CG-FOLD search-wordlist if execute else false then ;

: EMIT-TOKEN ( a u -- )
   2dup s>number? if  2>r 2drop 2r> d>s CTS-PUSH  exit then   \ defer the constant
   2drop                                                      \ discard the failed-parse double
   2dup FOLD-OP if  2drop exit then                           \ folded a const op
   CTS-FLUSH  EMIT-PRIM ;                                     \ else materialise + emit

: WALK-BODY {: a u | end cur ts :}
   CTS-RESET
   a u + to end   a to cur
   begin
      begin cur end < cur c@ bl = and while cur 1+ to cur repeat
      cur end <
   while
      cur to ts
      begin cur end < cur c@ bl <> and while cur 1+ to cur repeat
      ts  cur ts -  EMIT-TOKEN
   repeat
   CTS-FLUSH ;                            \ emit any trailing constants

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
