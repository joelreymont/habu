\ repl-lint.f -- REPL-baked code must never exit the interactive session.
\ Run: bin/hb --load tools/lint/lib.f tools/argv.f tools/repl-lint.f -- [ROOT]
\ Load after tools/lint/lib.f and tools/argv.f.

$8000 constant REPL-FILE-CAP
$400 constant REPL-PATH-CAP
$80 constant REPL-PATH-MAX

create REPL-FB REPL-FILE-CAP allot
create REPL-PATH-BUF REPL-PATH-CAP allot
create REPL-PATH-IDS REPL-PATH-MAX cells allot
variable REPL-PATH#
variable REPL-BAD
variable REPL-FD
variable REPL-I
variable REPL-J
variable REPL-TMP
variable REPL-CMP
variable REPL-NUM-L
create REPL-NUM 32 allot
variable REPL-SRC-A
variable REPL-SRC-U
variable REPL-X
variable REPL-LINE
variable REPL-TOK-A
variable REPL-TOK-U
variable REPL-TOK-LINE
variable REPL-ROOT-A
variable REPL-ROOT-U

: NL ( -- )  10 emit ;
: EM-DASH ( -- )  $E2 emit $80 emit $94 emit ;
: UTYPE  ( u -- )
   0 REPL-NUM-L !
   dup 0 = if drop 48 emit exit then
   begin dup 0 > while
      dup 10 mod 48 +  REPL-NUM REPL-NUM-L @ + c!
      10 /  REPL-NUM-L @ 1+ REPL-NUM-L !
   repeat drop
   begin REPL-NUM-L @ 0 > while
      REPL-NUM-L @ 1- REPL-NUM-L !
      REPL-NUM REPL-NUM-L @ + c@ emit
   repeat ;

: FILE-EXISTS?  {: a u :}  ( -- f )
   a u PATHZ
   PATHBUF 0 0 open REPL-FD !
   REPL-FD @ 0 < if 0 exit then
   REPL-FD @ close  -1 ;

: REPL-ROOT! ( ptr u8 n -- ) {: a:ptr u :}
   a REPL-ROOT-A !
   u REPL-ROOT-U ! ;

: REPL-ROOT-SELF? ( -- bool )
   REPL-ROOT-U @ 0= if LINT-TRUE exit then
   REPL-ROOT-A @ REPL-ROOT-U @ s" ." STR= ;

: REPL-ROOTED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   REPL-ROOT-SELF? if a u exit then
   REPL-ROOT-U @ u + 1 + REPL-PATH-CAP > if s" repl-lint: root path too long" type NL s" " 1 die then
   REPL-ROOT-A @ REPL-PATH-BUF REPL-ROOT-U @ BMOVE
   REPL-ROOT-A @ REPL-ROOT-U @ 1- + c@ SLASH = if
      a REPL-PATH-BUF REPL-ROOT-U @ + u BMOVE
      REPL-PATH-BUF REPL-ROOT-U @ u +
      exit
   then
   SLASH REPL-PATH-BUF REPL-ROOT-U @ + c!
   a REPL-PATH-BUF REPL-ROOT-U @ 1 + + u BMOVE
   REPL-PATH-BUF REPL-ROOT-U @ 1 + u + ;

: TRIM-DQUOTE  {: a u :}  ( -- a u' )
   a  u 0 > a u 1- + c@ DQUOTE = and if u 1- else u then ;

: SQUOTE-TOK?  {: a u :}  ( -- f )
   u 2 <> if 0 exit then
   a c@ FOLD 115 =  a 1+ c@ DQUOTE = and ;

: SRC-PATH-TOK?  {: a u :}  ( -- f )
   a u TRIM-DQUOTE  P1U ! P1A !
   P1A @ P1U @ s" src/" STARTS-WITH?
   P1A @ P1U @ s" .f" HAS-EXT? and ;

: PATH-ID@  ( k -- id )  cells REPL-PATH-IDS + @ ;
: PATH-ID!  ( id k -- )  cells REPL-PATH-IDS + ! ;

: ADD-REPL-PATH  {: a u :}  ( -- )
   a u INTERN-FIND dup 0 >= if drop exit then drop
   REPL-PATH# @ REPL-PATH-MAX >= if s" repl-lint: too many source paths" type NL s" " 1 die then
   a u INTERN  REPL-PATH# @ PATH-ID!
   REPL-PATH# @ 1+ REPL-PATH# ! ;

: ADD-BACKSTOP-PATHS  ( -- )
   s" src/habu/repl.f" ADD-REPL-PATH
   s" src/habu/debug-watch.f" ADD-REPL-PATH
   s" src/habu/stepper.f" ADD-REPL-PATH
   s" src/habu/debug.f" ADD-REPL-PATH ;

: STR<  {: a u b v :}  ( -- f )
   0 REPL-CMP !
   begin REPL-CMP @ u <  REPL-CMP @ v < and while
      a REPL-CMP @ + c@  b REPL-CMP @ + c@
      2dup < if 2drop -1 exit then
      > if 0 exit then
      REPL-CMP @ 1+ REPL-CMP !
   repeat
   u v < ;

: PATH-ID<  {: left right :}  ( -- f )
   left INTERN$  right INTERN$  STR< ;

: SWAP-PATH-IDS  {: left right :}  ( -- )
   left PATH-ID@ REPL-TMP !
   right PATH-ID@ left PATH-ID!
   REPL-TMP @ right PATH-ID! ;

: SORT-REPL-PATHS  ( -- )
   0 REPL-I !
   begin REPL-I @ REPL-PATH# @ < while
      REPL-I @ 1+ REPL-J !
      begin REPL-J @ REPL-PATH# @ < while
         REPL-J @ PATH-ID@  REPL-I @ PATH-ID@  PATH-ID< if
            REPL-I @ REPL-J @ SWAP-PATH-IDS
         then
         REPL-J @ 1+ REPL-J !
      repeat
      REPL-I @ 1+ REPL-I !
   repeat ;

: ADD-STDIN-SOURCES  ( -- )
   s" src/habu/stdin.f" REPL-ROOTED$ REPL-FB REPL-FILE-CAP READ-FILE
   -1 PARENS? !  TOKENIZE
   0 REPL-I !
   begin REPL-I @ 2 + TN# @ < while
      REPL-I @ TOK s" -SRC" SUFFIX? if
         REPL-I @ 1+ TOK SQUOTE-TOK? if
            REPL-I @ 2 + TOK SRC-PATH-TOK? if
               P1A @ P1U @ ADD-REPL-PATH
            then
         then
      then
      REPL-I @ 1+ REPL-I !
   repeat ;

: COLLECT-REPL-PATHS  ( -- )
   INTERN-RESET
   0 REPL-PATH# !
   ADD-STDIN-SOURCES
   ADD-BACKSTOP-PATHS
   SORT-REPL-PATHS ;

: FATAL-TOK?  {: a u :}  ( -- f )
   a u s" die" STR=CI if -1 exit then
   a u s" bye" STR=CI ;

: R-END? ( -- f )
   REPL-X @ REPL-SRC-U @ >= ;

: R-C@ ( -- c )
   REPL-SRC-A @ REPL-X @ + c@ ;

: R-ADV ( -- c )
   R-C@
   REPL-X @ 1+ REPL-X !
   dup 10 = if REPL-LINE @ 1+ REPL-LINE ! then ;

: R-SKIP-LINE ( -- )
   begin R-END? 0= while
      R-C@ 10 = if exit then
      R-ADV drop
   repeat ;

: R-SKIP-PAREN ( -- )
   begin R-END? 0= while
      R-ADV 41 = if exit then
   repeat ;

: R-SKIP-QUOTE ( -- )
   begin R-END? 0= while
      R-ADV DQUOTE = if exit then
   repeat ;

: R-STRING-OPENER? {: a u :} ( -- f )
   u 2 <> if 0 exit then
   a 1+ c@ DQUOTE <> if 0 exit then
   a c@ FOLD dup 115 = swap 99 = or
   a c@ DOT = or ;

: R-SKIP-IGNORED ( -- )
   begin R-END? 0= while
      R-C@ WS? if
         R-ADV drop
      else R-C@ 92 = if
         R-SKIP-LINE
      else R-C@ 40 = if
         R-ADV drop
         R-SKIP-PAREN
      else
         exit
      then then then
   repeat ;

: R-NEXT-WORD ( -- f )
   R-SKIP-IGNORED
   R-END? if 0 exit then
   REPL-SRC-A @ REPL-X @ + REPL-TOK-A !
   REPL-X @ REPL-TMP !
   REPL-LINE @ REPL-TOK-LINE !
   begin R-END? 0= R-C@ WS? 0= and while
      R-ADV drop
   repeat
   REPL-X @ REPL-TMP @ - REPL-TOK-U !
   REPL-TOK-A @ REPL-TOK-U @ R-STRING-OPENER? if R-SKIP-QUOTE then
   -1 ;

: REPL-FINDING  {: fa fu ta tu line :}  ( -- )
   s" FATAL-IN-REPL " type fa fu type 58 emit line UTYPE s" : `" type
   ta tu type s" ` exits the session " type EM-DASH
   s"  use `throw` (the REPL recovers); `die` is for build-time makers only" type NL ;

: LINT-REPL-SOURCE  {: fa fu a u :}  ( -- )
   a REPL-SRC-A !  u REPL-SRC-U !  0 REPL-X !  1 REPL-LINE !
   begin R-NEXT-WORD while
      REPL-TOK-A @ REPL-TOK-U @ FATAL-TOK? if
         fa fu  REPL-TOK-A @ REPL-TOK-U @  REPL-TOK-LINE @  REPL-FINDING
         REPL-BAD @ 1+ REPL-BAD !
      then
   repeat ;

: LINT-REPL-FILE  {: pa pu :}  ( -- )
   pa pu REPL-ROOTED$ FILE-EXISTS? 0= if exit then
   pa pu REPL-ROOTED$ REPL-FB REPL-FILE-CAP READ-FILE  P2U ! P2A !
   pa pu  P2A @ P2U @  LINT-REPL-SOURCE ;

: REPL-LINT  ( -- )
   0 REPL-BAD !
   COLLECT-REPL-PATHS
   0 REPL-I !
   begin REPL-I @ REPL-PATH# @ < while
      REPL-I @ PATH-ID@ INTERN$ LINT-REPL-FILE
      REPL-I @ 1+ REPL-I !
   repeat
   s" repl-lint: " type REPL-BAD @ UTYPE 32 emit s" finding(s)" type NL
   REPL-BAD @ 0 > if s" " 1 die then ;

: REPL-CONFIG ( -- )
   s" tools/repl-lint.f [ROOT]" ARGV-USAGE!
   ARGV-PARSE
   0 1 ARGV-EXPECT-POS
   ARGV-POS# 0= if s" ." REPL-ROOT! exit then
   0 ARGV-POS$ REPL-ROOT! ;

: REPL-MAIN ( -- )
   REPL-CONFIG
   REPL-LINT ;

REPL-MAIN
