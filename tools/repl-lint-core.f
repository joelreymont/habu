\ repl-lint-core.f -- REPL-baked code must never exit the interactive session.
\ Load after lib/errors.f, lib/memory.f, lib/vector.f, tools/lint/text.f,
\ tools/lint/intern.f, tools/lint/token.f, and tools/lint/lib.f.

$8000 constant REPL-FILE-CAP
$400 constant REPL-PATH-CAP
$80 constant REPL-PATH-MAX

create REPL-FB REPL-FILE-CAP allot
create REPL-PATH-BUF REPL-PATH-CAP allot
create REPL-PATH-IDS REPL-PATH-MAX cells allot
create REPL-ONE 1 allot
variable REPL-PATH#
variable REPL-BAD
variable REPL-FD
variable REPL-OUT-FD
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

: REPL-ROOT-A-FIELD ( -- ptr ptr u8 )
   REPL-ROOT-A 0 ptr-field ;

: REPL-SRC-A-FIELD ( -- ptr ptr u8 )
   REPL-SRC-A 0 ptr-field ;

: REPL-TOK-A-FIELD ( -- ptr ptr u8 )
   REPL-TOK-A 0 ptr-field ;

: REPL-ROOT-A@ ( -- ptr u8 )
   REPL-ROOT-A-FIELD @ ;

: REPL-SRC-A@ ( -- ptr u8 )
   REPL-SRC-A-FIELD @ ;

: REPL-TOK-A@ ( -- ptr u8 )
   REPL-TOK-A-FIELD @ ;

: REPL-ROOT-A! ( ptr u8 -- )
   REPL-ROOT-A-FIELD ! ;

: REPL-SRC-A! ( ptr u8 -- )
   REPL-SRC-A-FIELD ! ;

: REPL-TOK-A! ( ptr u8 -- )
   REPL-TOK-A-FIELD ! ;

: REPL-OUT-FD! ( fd -- )
   REPL-OUT-FD ! ;

: REPL-OUT ( ptr u8 n -- )
   REPL-OUT-FD @ -rot LINT-OUT-WRITE ;

: REPL-C! ( n -- )
   REPL-ONE c! ;

: REPL-C ( n -- )
   REPL-C! REPL-ONE 1 REPL-OUT ;

: REPL-NL ( -- )
   10 REPL-C ;

: REPL-EM-DASH ( -- )
   $E2 REPL-C $80 REPL-C $94 REPL-C ;

: REPL-U  ( u -- )
   0 REPL-NUM-L !
   dup 0 = if drop 48 REPL-C exit then
   begin dup 0 > while
      dup 10 mod 48 +  REPL-NUM REPL-NUM-L @ + c!
      10 /  REPL-NUM-L @ 1+ REPL-NUM-L !
   repeat drop
   begin REPL-NUM-L @ 0 > while
      REPL-NUM-L @ 1- REPL-NUM-L !
      REPL-NUM REPL-NUM-L @ + c@ REPL-C
   repeat ;

: FILE-EXISTS?  ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u LINT-PATHZ
   PATHBUF 0 0 open REPL-FD !
   REPL-FD @ 0 < if LINT-FALSE exit then
   REPL-FD @ close  LINT-TRUE ;

: REPL-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a REPL-ROOT-A!
   u REPL-ROOT-U ! ;

: REPL-ROOT-SELF? ( -- bool )
   REPL-ROOT-U @ 0= if LINT-TRUE exit then
   REPL-ROOT-A@ REPL-ROOT-U @ s" ." LINT-STR= ;

: REPL-ROOTED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   REPL-ROOT-SELF? if a u exit then
   REPL-ROOT-U @ u + 1 + REPL-PATH-CAP > if s" repl-lint: root path too long" 1 die then
   REPL-ROOT-A@ REPL-PATH-BUF REPL-ROOT-U @ LINT-BMOVE
   REPL-ROOT-A@ REPL-ROOT-U @ 1- + c@ SLASH = if
      a REPL-PATH-BUF REPL-ROOT-U @ + u LINT-BMOVE
      REPL-PATH-BUF REPL-ROOT-U @ u +
      exit
   then
   SLASH REPL-PATH-BUF REPL-ROOT-U @ + c!
   a REPL-PATH-BUF REPL-ROOT-U @ 1 + + u LINT-BMOVE
   REPL-PATH-BUF REPL-ROOT-U @ 1 + u + ;

: TRIM-DQUOTE  ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0 <= if a u exit then
   a u 1- + c@ DQUOTE = if a u 1- exit then
   a u ;

: SQUOTE-TOK?  ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> if LINT-FALSE exit then
   a c@ LINT-FOLD 115 =  a 1+ c@ DQUOTE = and ;

: SRC-PATH-TRIMMED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" src/" LINT-STARTS-WITH?
   a u s" .f" HAS-EXT? and ;

: SRC-PATH-TOK? ( ptr u8 n -- bool )
   TRIM-DQUOTE SRC-PATH-TRIMMED? ;

: PATH-ID@  ( n -- n )  REPL-PATH-IDS swap cells + @ ;
: PATH-ID!  ( n n -- )  REPL-PATH-IDS swap cells + ! ;

: ADD-REPL-PATH  ( ptr u8 n -- ) {: a:ptr u:n :}
   a u INTERN-FIND dup 0 >= if drop exit then drop
   REPL-PATH# @ REPL-PATH-MAX >= if s" repl-lint: too many source paths" 1 die then
   a u INTERN  REPL-PATH# @ PATH-ID!
   REPL-PATH# @ 1+ REPL-PATH# ! ;

: ADD-BACKSTOP-PATHS  ( -- )
   s" src/habu/repl.f" ADD-REPL-PATH
   s" src/habu/debug-watch.f" ADD-REPL-PATH
   s" src/habu/stepper.f" ADD-REPL-PATH
   s" src/habu/debug.f" ADD-REPL-PATH ;

: STR<  ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   0 REPL-CMP !
   begin REPL-CMP @ u <  REPL-CMP @ v < and while
      a REPL-CMP @ + c@  b REPL-CMP @ + c@
      2dup < if 2drop LINT-TRUE exit then
      > if LINT-FALSE exit then
      REPL-CMP @ 1+ REPL-CMP !
   repeat
   u v < ;

: PATH-ID<  ( n n -- bool ) {: left:n right:n :}
   left INTERN$  right INTERN$  STR< ;

: SWAP-PATH-IDS  ( n n -- ) {: left:n right:n :}
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
      REPL-I @ TOK s" -SRC" LINT-SUFFIX? if
            REPL-I @ 1+ TOK SQUOTE-TOK? if
               REPL-I @ 2 + TOK SRC-PATH-TOK? if
                  REPL-I @ 2 + TOK TRIM-DQUOTE ADD-REPL-PATH
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

: FATAL-TOK?  ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" die" LINT-STR=CI if LINT-TRUE exit then
   a u s" bye" LINT-STR=CI ;

: R-END? ( -- bool )
   REPL-X @ REPL-SRC-U @ >= ;

: R-C@ ( -- n )
   REPL-SRC-A@ REPL-X @ + c@ ;

: R-ADV ( -- n )
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

: R-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> if LINT-FALSE exit then
   a 1+ c@ DQUOTE <> if LINT-FALSE exit then
   a c@ LINT-FOLD dup 115 = swap 99 = or
   a c@ DOT = or ;

: R-SKIP-IGNORED ( -- )
   begin R-END? 0= while
      R-C@ LINT-WS? if
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

: R-NEXT-WORD ( -- bool )
   R-SKIP-IGNORED
   R-END? if LINT-FALSE exit then
   REPL-SRC-A@ REPL-X @ + REPL-TOK-A!
   REPL-X @ REPL-TMP !
   REPL-LINE @ REPL-TOK-LINE !
   begin R-END? 0= R-C@ LINT-WS? 0= and while
      R-ADV drop
   repeat
   REPL-X @ REPL-TMP @ - REPL-TOK-U !
   REPL-TOK-A@ REPL-TOK-U @ R-STRING-OPENER? if R-SKIP-QUOTE then
   LINT-TRUE ;

: REPL-FINDING  ( ptr u8 n ptr u8 n n -- ) {: fa:ptr fu:n ta:ptr tu:n line:n :}
   s" FATAL-IN-REPL " REPL-OUT fa fu REPL-OUT 58 REPL-C line REPL-U s" : `" REPL-OUT
   ta tu REPL-OUT s" ` exits the session " REPL-OUT REPL-EM-DASH
   s"  use `throw` (the REPL recovers); `die` is for build-time makers only" REPL-OUT REPL-NL ;

: LINT-REPL-SOURCE  ( ptr u8 n ptr u8 n -- ) {: fa:ptr fu:n a:ptr u:n :}
   a REPL-SRC-A!  u REPL-SRC-U !  0 REPL-X !  1 REPL-LINE !
   begin R-NEXT-WORD while
      REPL-TOK-A@ REPL-TOK-U @ FATAL-TOK? if
         fa fu  REPL-TOK-A@ REPL-TOK-U @  REPL-TOK-LINE @  REPL-FINDING
         REPL-BAD @ 1+ REPL-BAD !
      then
   repeat ;

: LINT-REPL-FILE  ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu REPL-ROOTED$ FILE-EXISTS? 0= if exit then
   pa pu REPL-ROOTED$ REPL-FB REPL-FILE-CAP READ-FILE  P2U ! P2A!
   pa pu  P2A@ P2U @  LINT-REPL-SOURCE ;

: REPL-LINT-CHECK  ( -- n )
   0 REPL-BAD !
   COLLECT-REPL-PATHS
   0 REPL-I !
   begin REPL-I @ REPL-PATH# @ < while
      REPL-I @ PATH-ID@ INTERN$ LINT-REPL-FILE
      REPL-I @ 1+ REPL-I !
   repeat
   s" repl-lint: " REPL-OUT REPL-BAD @ REPL-U 32 REPL-C s" finding(s)" REPL-OUT REPL-NL
   REPL-BAD @ ;

: REPL-LINT  ( -- )
   REPL-LINT-CHECK 0 > if s" " 1 die then ;

1 >FD REPL-OUT-FD!
