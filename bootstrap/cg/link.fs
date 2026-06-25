\ link.fs — multi-word programs with a subroutine ABI. Each habu word becomes a
\ native subroutine over the shared data stack (Xds=x19, threaded through calls);
\ words call each other with BL, RECURSE calls self, non-leaf words save/restore
\ x30. BUILD-PROGRAM collects a root word's transitive callees, lays them out in
\ one __TEXT with a MAIN entry, and (via RUN-EXE) emits a runnable Mach-O.

require walk.fs
require rt.fs

30 constant LR

\ --- CODE-TABLE: name -> [ label | arity | effect-flags | len | body-bytes ] ---
wordlist constant CODE-TABLE

: CG-RECORD ( na nu ba bu arity flags -- )
   {: ar flags :}
   2swap nextname
   get-current >r  CODE-TABLE set-current  create  r> set-current
   -1 ,  ar ,  flags ,  dup ,  here >r  dup allot  r> swap move ;

: WORD-PFA  ( a u -- pfa | 0 )  CODE-TABLE search-wordlist if execute else 0 then ;

: PFA>LABEL ( pfa -- addr )  ;

: PFA>ARITY ( pfa -- n )  cell+ @ ;

: PFA>EFLAGS ( pfa -- flags )  2 cells + @ ;

: PFA>BODY  ( pfa -- ba bu )  dup 4 cells + swap 3 cells + @ ;

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
256 constant MAX-DEPS
create DEPS MAX-DEPS cells allot   variable #DEPS

: DEP-HAS? ( pfa -- f )  #DEPS @ 0 ?do  dup DEPS i cells + @ = if drop true unloop exit then  loop drop false ;

: DEP-ROOM ( -- )
   #DEPS @ MAX-DEPS >= if 1 abort" cg: dependency table overflow" then ;

: DEP-ADD  ( pfa -- )  dup DEP-HAS? if drop else DEP-ROOM  DEPS #DEPS @ cells + !  1 #DEPS +! then ;

: SCAN-TOK ( ta tu -- )
   2dup s" ." compare 0= if 2drop USES-DOT on exit then
   WORD-PFA ?dup if DEP-ADD then ;

: COLLECT ( root-pfa -- )
   0 #DEPS !  DEP-ADD
   0 begin dup #DEPS @ < while
      dup cells DEPS + @ PFA>BODY ['] SCAN-TOK FOR-TOKENS  1+
   repeat drop ;

\ --- leaf detection (body has any call?) ---
variable LEAF?

: LEAF-TOK ( ta tu -- )            \ any token that emits a BL makes the word non-leaf
   2dup s" RECURSE" compare 0= if 2drop LEAF? off exit then
   2dup s" ."       compare 0= if 2drop LEAF? off exit then
   WORD-PFA if LEAF? off then ;

: NON-LEAF? ( pfa -- f )  LEAF? on  PFA>BODY ['] LEAF-TOK FOR-TOKENS  LEAF? @ 0= ;

\ --- call emission (drives walk.fs EMIT-CALL) ---
variable CUR-PFA
:noname ( a u -- handled? )
   2dup s" ." compare 0= if  2drop  DOT-LBL @ BL,  true  exit  then
   2dup s" RECURSE" compare 0= if         \ self-call (CUR-PFA unset during validation walk)
      2drop  CUR-PFA @ ?dup if PFA>LABEL @ else 0 then  BL,  true
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
   ICODE-RESET  CF-RESET  USES-DOT off
   root COLLECT
   #DEPS @ 0 ?do  LBL  DEPS i cells + @ PFA>LABEL !  loop
   USES-DOT @ if  LBL DOT-LBL !  then
   512 G-PROLOGUE                         \ MAIN: data + return stacks
   input G-LIT
   root PFA>LABEL @ BL,                   \ call the root word
   G-EXIT-TOS                             \ exit(result)
   #DEPS @ 0 ?do  DEPS i cells + @ EMIT-WORD  loop
   USES-DOT @ if  EMIT-DOT  then
   OPTIMIZE ;

: RUN-NATIVE ( input "name" -- exit-code )
   parse-name WORD-PFA dup 0= if E-NO-ENC throw then
   swap BUILD-PROGRAM
   s" /tmp/habu-prog" RUN-EXE ;

\ --- standalone CLI: read argv[1], call the word, print the result, exit 0 ---
22 constant ARGV

: BUILD-CLI {: root | ar -- :}
   root PFA>ARITY to ar
   ICODE-RESET  CF-RESET  USES-DOT off
   root COLLECT
   #DEPS @ 0 ?do  LBL  DEPS i cells + @ PFA>LABEL !  loop
   LBL DOT-LBL !   LBL ATOI-LBL !
   ARGV 1 0 ADDI,                         \ x22 = argv  (entry: x0=argc, x1=argv)
   512 G-PROLOGUE                         \ data + return stacks
   ar 0 ?do  9 ARGV i 1+ 8 *  LDR,  ATOI-LBL @ BL,  loop  \ push atoi(argv[1..ar])
   root PFA>LABEL @ BL,                   \ call the word
   DOT-LBL @ BL,                          \ print the result
   G-EXIT0
   #DEPS @ 0 ?do  DEPS i cells + @ EMIT-WORD  loop
   EMIT-DOT  EMIT-ATOI  OPTIMIZE ;

\ Emit a standalone CLI executable `outfile` for a recorded word `name`.
\ Usage:  s" /tmp/sq" HABU-EXE SQUARE   then   ./sq 7
: HABU-EXE ( outfile-a outfile-u "name" -- )
   parse-name WORD-PFA dup 0= if E-NO-ENC throw then
   BUILD-CLI
   EMIT-EXE ;
