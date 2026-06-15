\ argv.f -- shared argv parser for `bin/hb tool.f args...` scripts.

64 constant ARGV-E-USAGE
76 constant ARGV-E-INTERNAL
64 constant ARGV-MAX
1024 constant ARGV-MSG-CAP
1024 constant ARGV-PATH-CAP

variable ARGV-USE-MOCK?
variable ARGV-MOCK#
create ARGV-MOCK-A ARGV-MAX cells allot
create ARGV-MOCK-U ARGV-MAX cells allot

variable ARGV-I
variable ARGV-NPOS
create ARGV-POS-A ARGV-MAX cells allot
create ARGV-POS-U ARGV-MAX cells allot

variable ARGV-JSON
variable ARGV-STRICT-SIGNATURES
variable ARGV-ALL-ERRORS

variable ARGV-LABEL-A
variable ARGV-LABEL-U
variable ARGV-LABEL-SET
variable ARGV-LABEL-DEFAULT-A
variable ARGV-LABEL-DEFAULT-U

variable ARGV-OUT-A
variable ARGV-OUT-U
variable ARGV-OUT-SET
variable ARGV-OUT-DEFAULT-A
variable ARGV-OUT-DEFAULT-U

variable ARGV-USAGE-A
variable ARGV-USAGE-U
variable ARGV-QUIET
variable ARGV-MSG-L
create ARGV-MSG ARGV-MSG-CAP allot
create ARGV-PATH-BUF ARGV-PATH-CAP allot

: ARGV-BUF-FAIL ( -- )  s" argv: buffer overflow" ARGV-E-INTERNAL die ;

: ARGV-MSG+ {: a u :} ( a u -- )
   ARGV-MSG-L @ u + ARGV-MSG-CAP > IF ARGV-BUF-FAIL THEN
   0 BEGIN dup u < WHILE
      dup a + c@  ARGV-MSG ARGV-MSG-L @ + c!
      ARGV-MSG-L @ 1 + ARGV-MSG-L !
      1 +
   REPEAT drop ;

: ARGV-MSG-C+ {: c :} ( c -- )
   ARGV-MSG-L @ 1 + ARGV-MSG-CAP > IF ARGV-BUF-FAIL THEN
   c ARGV-MSG ARGV-MSG-L @ + c!
   ARGV-MSG-L @ 1 + ARGV-MSG-L ! ;

: ARGV-USAGE! {: a u :} ( a u -- )
   a ARGV-USAGE-A !  u ARGV-USAGE-U ! ;

: ARGV-QUIET! ( f -- )  ARGV-QUIET ! ;

: ARGV-FAIL-DONE ( -- )
   10 ARGV-MSG-C+
   s" usage: " ARGV-MSG+
   ARGV-USAGE-A @ ARGV-USAGE-U @ ARGV-MSG+
   10 ARGV-MSG-C+
   ARGV-QUIET @ 0 = IF 2 ARGV-MSG ARGV-MSG-L @ write drop THEN
   ARGV-E-USAGE throw ;

: ARGV-FAIL {: a u :} ( a u -- )
   0 ARGV-MSG-L !
   a u ARGV-MSG+
   ARGV-FAIL-DONE ;

: ARGV-UNKNOWN {: a u :} ( a u -- )
   0 ARGV-MSG-L !
   s" unknown option: " ARGV-MSG+
   a u ARGV-MSG+
   ARGV-FAIL-DONE ;

: ARGV-MISSING {: a u :} ( a u -- )
   0 ARGV-MSG-L !
   s" missing value for " ARGV-MSG+
   a u ARGV-MSG+
   ARGV-FAIL-DONE ;

: ARGV-RESET ( -- )
   0 ARGV-I !
   0 ARGV-NPOS !
   0 ARGV-JSON !
   0 ARGV-STRICT-SIGNATURES !
   0 ARGV-ALL-ERRORS !
   0 ARGV-LABEL-A !  0 ARGV-LABEL-U !  0 ARGV-LABEL-SET !
   0 ARGV-OUT-A !  0 ARGV-OUT-U !  0 ARGV-OUT-SET ! ;

: ARGV-USE-SCRIPT ( -- )  0 ARGV-USE-MOCK? ! ;

: ARGV-MOCK-CLEAR ( -- )
   -1 ARGV-USE-MOCK? !
   0 ARGV-MOCK# ! ;

: ARGV-MOCK+ {: a u :} ( a u -- )
   ARGV-MOCK# @ ARGV-MAX >= IF s" argv: mock argv overflow" ARGV-E-INTERNAL die THEN
   a ARGV-MOCK-A ARGV-MOCK# @ cells + !
   u ARGV-MOCK-U ARGV-MOCK# @ cells + !
   ARGV-MOCK# @ 1 + ARGV-MOCK# ! ;

: ARGV-COUNT ( -- n )
   ARGV-USE-MOCK? @ IF ARGV-MOCK# @ ELSE SCRIPT-ARGC THEN ;

: ARGV-TOK$ {: idx :} ( idx -- a u )
   ARGV-USE-MOCK? @ IF
      idx cells ARGV-MOCK-A + @
      idx cells ARGV-MOCK-U + @
   ELSE
      idx SCRIPT-ARGV$
   THEN ;

: ARGV-TOK= {: idx a u :} ( idx a u -- f )
   idx ARGV-TOK$ a u STR= ;

: ARGV-DASH? {: a u :} ( a u -- f )
   u 1 > IF a c@ 45 = ELSE 0 THEN ;

: ARGV-POS+ {: a u :} ( a u -- )
   ARGV-NPOS @ ARGV-MAX >= IF s" too many positional arguments" ARGV-FAIL THEN
   a ARGV-POS-A ARGV-NPOS @ cells + !
   u ARGV-POS-U ARGV-NPOS @ cells + !
   ARGV-NPOS @ 1 + ARGV-NPOS ! ;

: ARGV-POS# ( -- n )  ARGV-NPOS @ ;

: ARGV-POS$ {: idx :} ( idx -- a u )
   idx 0 <  idx ARGV-NPOS @ >= or IF s" positional index out of range" ARGV-FAIL THEN
   idx cells ARGV-POS-A + @
   idx cells ARGV-POS-U + @ ;

: ARGV-LABEL! {: a u :} ( a u -- )
   a ARGV-LABEL-A !  u ARGV-LABEL-U !  -1 ARGV-LABEL-SET ! ;

: ARGV-LABEL-DEFAULT! {: a u :} ( a u -- )
   a ARGV-LABEL-DEFAULT-A !  u ARGV-LABEL-DEFAULT-U ! ;

: ARGV-LABEL? ( -- f )  ARGV-LABEL-SET @ 0 <> ;

: ARGV-LABEL$ ( -- a u )
   ARGV-LABEL? IF
      ARGV-LABEL-A @  ARGV-LABEL-U @
   ELSE
      ARGV-LABEL-DEFAULT-A @  ARGV-LABEL-DEFAULT-U @
   THEN ;

: ARGV-OUT! {: a u :} ( a u -- )
   a ARGV-OUT-A !  u ARGV-OUT-U !  -1 ARGV-OUT-SET ! ;

: ARGV-OUT-DEFAULT! {: a u :} ( a u -- )
   a ARGV-OUT-DEFAULT-A !  u ARGV-OUT-DEFAULT-U ! ;

: ARGV-OUT? ( -- f )  ARGV-OUT-SET @ 0 <> ;

: ARGV-OUT$ ( -- a u )
   ARGV-OUT? IF
      ARGV-OUT-A @  ARGV-OUT-U @
   ELSE
      ARGV-OUT-DEFAULT-A @  ARGV-OUT-DEFAULT-U @
   THEN ;

: ARGV-JSON? ( -- f )  ARGV-JSON @ 0 <> ;

: ARGV-STRICT-SIGNATURES? ( -- f )  ARGV-STRICT-SIGNATURES @ 0 <> ;

: ARGV-ALL-ERRORS? ( -- f )  ARGV-ALL-ERRORS @ 0 <> ;

: ARGV-TAKE-NEXT {: a u :} ( a u -- va vu )
   ARGV-I @ 1 + ARGV-COUNT >= IF a u ARGV-MISSING THEN
   ARGV-I @ 1 + ARGV-I !
   ARGV-I @ ARGV-TOK$ ;

: ARGV-PARSE-OPT {: a u :} ( a u -- )
   a u s" --json" STR= IF -1 ARGV-JSON ! EXIT THEN
   a u s" --label" STR= IF a u ARGV-TAKE-NEXT ARGV-LABEL! EXIT THEN
   a u s" --strict-signatures" STR= IF -1 ARGV-STRICT-SIGNATURES ! EXIT THEN
   a u s" --all-errors" STR= IF -1 ARGV-ALL-ERRORS ! EXIT THEN
   a u s" -o" STR= IF a u ARGV-TAKE-NEXT ARGV-OUT! EXIT THEN
   a u ARGV-DASH? IF a u ARGV-UNKNOWN ELSE a u ARGV-POS+ THEN ;

: ARGV-COLLECT-REST ( -- )
   BEGIN ARGV-I @ ARGV-COUNT < WHILE
      ARGV-I @ ARGV-TOK$ ARGV-POS+
      ARGV-I @ 1 + ARGV-I !
   REPEAT ;

: ARGV-PARSE ( -- )
   ARGV-RESET
   BEGIN ARGV-I @ ARGV-COUNT < WHILE
      ARGV-I @ s" --" ARGV-TOK= IF
         ARGV-I @ 1 + ARGV-I !
         ARGV-COLLECT-REST
         EXIT
      THEN
      ARGV-I @ ARGV-TOK$ ARGV-PARSE-OPT
      ARGV-I @ 1 + ARGV-I !
   REPEAT ;

: ARGV-EXPECT-POS {: lo hi :} ( lo hi -- )
   ARGV-NPOS @ lo < IF s" wrong number of positional arguments" ARGV-FAIL THEN
   hi 0 >= IF
      ARGV-NPOS @ hi > IF s" wrong number of positional arguments" ARGV-FAIL THEN
   THEN ;

: ARGV-EXPECT-POS-EXACT {: n :} ( n -- )
   n n ARGV-EXPECT-POS ;

: ARGV-REQUIRE-OUT ( -- )
   ARGV-OUT? 0= IF s" missing -o OUT" ARGV-FAIL THEN ;

: ARGV-REQUIRE-LABEL ( -- )
   ARGV-LABEL? 0= IF s" missing --label NAME" ARGV-FAIL THEN ;

: ARGV-ZCOPY {: a u dst cap :} ( a u dst cap -- z )
   u 1 + cap > IF s" argv: path too long" ARGV-E-INTERNAL die THEN
   0 BEGIN dup u < WHILE
      dup a + c@  over dst + c!
      1 +
   REPEAT drop
   0 dst u + c!
   dst ;

: ARGV-PATHZ ( a u -- z )
   ARGV-PATH-BUF ARGV-PATH-CAP ARGV-ZCOPY ;

: ARGV-POSZ ( idx -- z )
   ARGV-POS$ ARGV-PATHZ ;

: ARGV-OUTZ ( -- z )
   ARGV-OUT$ ARGV-PATHZ ;

: ARGV-INIT ( -- )
   ARGV-USE-SCRIPT
   ARGV-RESET
   0 0 ARGV-LABEL-DEFAULT!
   0 0 ARGV-OUT-DEFAULT!
   0 ARGV-QUIET!
   s" hb tool.f [options] file ..." ARGV-USAGE! ;

ARGV-INIT
