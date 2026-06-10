\ link.fs — multi-word programs with a subroutine ABI. Each caf word becomes a
\ native subroutine over the shared data stack (Xds=x19, threaded through calls);
\ words call each other with BL, RECURSE calls self, non-leaf words save/restore
\ x30. BUILD-PROGRAM collects a root word's transitive callees, lays them out in
\ one __TEXT with a MAIN entry, and (via RUN-EXE) emits a runnable Mach-O.

require walk.fs
require rt.fs

30 constant LR

\ --- CODE-TABLE: name -> [ label | len | body-bytes ] ---
wordlist constant CODE-TABLE
: CG-RECORD ( na nu ba bu -- )
   2swap nextname
   get-current >r  CODE-TABLE set-current  create  r> set-current
   -1 ,                                    \ +0: label (assigned per build)
   dup ,                                   \ +1: body length
   here >r  dup allot  r> swap move ;      \ +2: body bytes
: WORD-PFA  ( a u -- pfa | 0 )  CODE-TABLE search-wordlist if execute else 0 then ;
: PFA>LABEL ( pfa -- addr )  ;
: PFA>BODY  ( pfa -- ba bu )  dup 2 cells + swap cell+ @ ;

\ --- token iteration (no emission) ---
: FOR-TOKENS {: a u xt | end cur ts -- :}
   a u + to end  a to cur
   begin
      begin cur end < cur c@ bl = and while cur 1+ to cur repeat
      cur end <
   while
      cur to ts
      begin cur end < cur c@ bl <> and while cur 1+ to cur repeat
      ts  cur ts -  xt execute
   repeat ;

\ --- transitive dependency collection ---
create DEPS 256 cells allot   variable #DEPS
: dep-has? ( pfa -- f )  #DEPS @ 0 ?do  dup DEPS i cells + @ = if drop true unloop exit then  loop drop false ;
: dep-add  ( pfa -- )  dup dep-has? if drop else DEPS #DEPS @ cells + !  1 #DEPS +! then ;
: scan-tok ( ta tu -- )
   2dup s" ." compare 0= if 2drop USES-DOT on exit then
   WORD-PFA ?dup if dep-add then ;
: COLLECT ( root-pfa -- )
   0 #DEPS !  dep-add
   0 begin dup #DEPS @ < while
      dup cells DEPS + @ PFA>BODY ['] scan-tok FOR-TOKENS  1+
   repeat drop ;

\ --- leaf detection (body has any call?) ---
variable LEAF?
: leaf-tok ( ta tu -- )            \ any token that emits a BL makes the word non-leaf
   2dup s" RECURSE" compare 0= if 2drop LEAF? off exit then
   2dup s" ."       compare 0= if 2drop LEAF? off exit then
   WORD-PFA if LEAF? off then ;
: NON-LEAF? ( pfa -- f )  LEAF? on  PFA>BODY ['] leaf-tok FOR-TOKENS  LEAF? @ 0= ;

\ --- call emission (drives walk.fs EMIT-CALL) ---
variable CUR-PFA
:noname ( a u -- handled? )
   2dup s" ." compare 0= if  2drop  DOT-LBL @ BL,  true  exit  then
   2dup s" RECURSE" compare 0= if
      2drop  CUR-PFA @ ?dup if PFA>LABEL @ else 0 then  BL,  true   \ 0 = placeholder (validation walk)
   else  WORD-PFA ?dup if  PFA>LABEL @ BL,  true  else  false  then  then ;
is EMIT-CALL

\ --- emit one word as a subroutine ---
: EMIT-WORD ( pfa -- )
   dup CUR-PFA !
   dup PFA>LABEL @ LBL,
   dup NON-LEAF? {: nl :}
   nl if  LR SP -16 STR-PRE,  then
   PFA>BODY WALK-BODY
   nl if  LR SP 16 LDR-POST,  then
   RET, ;

\ --- build a whole program rooted at `root`, with one i64 input ---
: BUILD-PROGRAM {: root input -- :}
   ICODE-RESET  cf-reset  USES-DOT off
   root COLLECT
   #DEPS @ 0 ?do  NEWLBL  DEPS i cells + @ PFA>LABEL !  loop
   USES-DOT @ if  NEWLBL DOT-LBL !  then
   SP SP 256 SUBI,  XDS SP 0 ADDI,        \ MAIN: data stack
   input g-lit
   root PFA>LABEL @ BL,                   \ call the root word
   0 g-pop  16 1 MOVZ,  $80 SVC,          \ exit(result)
   #DEPS @ 0 ?do  DEPS i cells + @ EMIT-WORD  loop
   USES-DOT @ if  EMIT-DOT  then ;

: RUN-NATIVE ( input "name" -- exit-code )
   parse-name WORD-PFA dup 0= if E-NO-ENC throw then
   swap BUILD-PROGRAM
   s" /tmp/caf-prog" RUN-EXE ;

\ --- standalone CLI: read argv[1], call the word, print the result, exit 0 ---
22 constant ARGV
: BUILD-CLI {: root -- :}
   ICODE-RESET  cf-reset  USES-DOT off
   root COLLECT
   #DEPS @ 0 ?do  NEWLBL  DEPS i cells + @ PFA>LABEL !  loop
   NEWLBL DOT-LBL !   NEWLBL ATOI-LBL !
   ARGV 1 0 ADDI,                         \ x22 = argv  (entry: x0=argc, x1=argv)
   SP SP 256 SUBI,  XDS SP 0 ADDI,        \ data stack
   9 ARGV 8 LDR,                          \ x9 = argv[1]
   ATOI-LBL @ BL,                         \ push atoi(argv[1])
   root PFA>LABEL @ BL,                   \ call the word
   DOT-LBL @ BL,                          \ print the result
   0 0 MOVZ,  16 1 MOVZ,  $80 SVC,        \ exit(0)
   #DEPS @ 0 ?do  DEPS i cells + @ EMIT-WORD  loop
   EMIT-DOT  EMIT-ATOI ;

\ Emit a standalone CLI executable `outfile` for a recorded word `name`.
\ Usage:  s" /tmp/sq" CAF-EXE SQUARE   then   ./sq 7
: CAF-EXE ( outfile-a outfile-u "name" -- )
   parse-name WORD-PFA dup 0= if E-NO-ENC throw then
   BUILD-CLI
   EMIT-EXE ;
