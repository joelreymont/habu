\ argv.f -- checked argv parser for `bin/hb script.f args...` scripts.

64 constant ARGV-E-USAGE
76 constant ARGV-E-INTERNAL
64 constant ARGV-MAX
1024 constant ARGV-MSG-CAP
1024 constant ARGV-PATH-CAP
$0A constant ARGV-CHAR-LF
$2D constant ARGV-CHAR-DASH

variable ARGV-USE-MOCK?
variable ARGV-MOCK#
create ARGV-MOCK-A ARGV-MAX cells allot
create ARGV-MOCK-U ARGV-MAX cells allot

variable ARGV-I
variable ARGV-NPOS
create ARGV-POS-A ARGV-MAX cells allot
create ARGV-POS-U ARGV-MAX cells allot

variable ARGV-JSON

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

: ARGV-FALSE ( -- bool )  0 0= 0= ;

: ARGV-TRUE ( -- bool )  0 0= ;

: ARGV-BYTES= {: a:ptr u b:ptr v :} ( ptr u8 n ptr u8 n -- bool )
   u v <> if ARGV-FALSE exit then
   0 begin dup u < while
      dup a + c@  over b + c@  <> if drop ARGV-FALSE exit then
      1 +
   repeat drop ARGV-TRUE ;

: ARGV-BUF-FAIL ( -- )  ARGV-E-INTERNAL throw ;

: ARGV-MSG+ {: a:ptr u :} ( ptr u8 n -- )
   ARGV-MSG-L @ u + ARGV-MSG-CAP > if ARGV-BUF-FAIL then
   0 begin dup u < while
      dup a + c@  ARGV-MSG ARGV-MSG-L @ + c!
      ARGV-MSG-L @ 1 + ARGV-MSG-L !
      1 +
   repeat drop ;

: ARGV-MSG-C+ {: c :} ( c -- )
   ARGV-MSG-L @ 1 + ARGV-MSG-CAP > if ARGV-BUF-FAIL then
   c ARGV-MSG ARGV-MSG-L @ + c!
   ARGV-MSG-L @ 1 + ARGV-MSG-L ! ;

: ARGV-USAGE! {: a:ptr u :} ( ptr u8 n -- )
   a ARGV-USAGE-A !  u ARGV-USAGE-U ! ;

: ARGV-QUIET! ( n -- )  ARGV-QUIET ! ;

: ARGV-FAIL-DONE ( -- )
   ARGV-CHAR-LF ARGV-MSG-C+
   s" usage: " ARGV-MSG+
   ARGV-USAGE-A @ ARGV-USAGE-U @ ARGV-MSG+
   ARGV-CHAR-LF ARGV-MSG-C+
   ARGV-QUIET @ 0 = if 2 ARGV-MSG ARGV-MSG-L @ write drop then
   ARGV-E-USAGE throw ;

: ARGV-FAIL {: a:ptr u :} ( ptr u8 n -- )
   0 ARGV-MSG-L !
   a u ARGV-MSG+
   ARGV-FAIL-DONE ;

: ARGV-UNKNOWN {: a:ptr u :} ( ptr u8 n -- )
   0 ARGV-MSG-L !
   s" unknown option: " ARGV-MSG+
   a u ARGV-MSG+
   ARGV-FAIL-DONE ;

: ARGV-MISSING {: a:ptr u :} ( ptr u8 n -- )
   0 ARGV-MSG-L !
   s" missing value for " ARGV-MSG+
   a u ARGV-MSG+
   ARGV-FAIL-DONE ;

: ARGV-RESET ( -- )
   0 ARGV-I !
   0 ARGV-NPOS !
   0 ARGV-JSON !
   NULL$ drop ARGV-OUT-A !  0 ARGV-OUT-U !  0 ARGV-OUT-SET ! ;

: ARGV-USE-SCRIPT ( -- )  0 ARGV-USE-MOCK? ! ;

: ARGV-MOCK-CLEAR ( -- )
   -1 ARGV-USE-MOCK? !
   0 ARGV-MOCK# ! ;

: ARGV-MOCK+ {: a:ptr u :} ( ptr u8 n -- )
   ARGV-MOCK# @ ARGV-MAX >= if ARGV-E-INTERNAL throw then
   a ARGV-MOCK-A ARGV-MOCK# @ cells + !
   u ARGV-MOCK-U ARGV-MOCK# @ cells + !
   ARGV-MOCK# @ 1 + ARGV-MOCK# ! ;

: ARGV-COUNT ( -- n )
   ARGV-USE-MOCK? @ if ARGV-MOCK# @ else SCRIPT-ARGC then ;

: ARGV-TOK$ {: idx :} ( n -- ptr u8 n )
   ARGV-USE-MOCK? @ if
      idx cells ARGV-MOCK-A + @
      idx cells ARGV-MOCK-U + @
   else
      idx SCRIPT-ARGV$
   then ;

: ARGV-TOK= {: idx a:ptr u :} ( n ptr u8 n -- bool )
   idx ARGV-TOK$ a u ARGV-BYTES= ;

: ARGV-DASH? {: a:ptr u :} ( ptr u8 n -- bool )
   u 1 > if a c@ ARGV-CHAR-DASH = else ARGV-FALSE then ;

: ARGV-POS+ {: a:ptr u :} ( ptr u8 n -- )
   ARGV-NPOS @ ARGV-MAX >= if s" too many positional arguments" ARGV-FAIL then
   a ARGV-POS-A ARGV-NPOS @ cells + !
   u ARGV-POS-U ARGV-NPOS @ cells + !
   ARGV-NPOS @ 1 + ARGV-NPOS ! ;

: ARGV-POS# ( -- n )  ARGV-NPOS @ ;

: ARGV-POS$ {: idx :} ( n -- ptr u8 n )
   idx 0 <  idx ARGV-NPOS @ >= or if s" positional index out of range" ARGV-FAIL then
   idx cells ARGV-POS-A + @
   idx cells ARGV-POS-U + @ ;

: ARGV-OUT! {: a:ptr u :} ( ptr u8 n -- )
   a ARGV-OUT-A !  u ARGV-OUT-U !  -1 ARGV-OUT-SET ! ;

: ARGV-OUT-DEFAULT! {: a:ptr u :} ( ptr u8 n -- )
   a ARGV-OUT-DEFAULT-A !  u ARGV-OUT-DEFAULT-U ! ;

: ARGV-OUT? ( -- bool )  ARGV-OUT-SET @ 0 <> ;

: ARGV-OUT$ ( -- ptr u8 n )
   ARGV-OUT? if
      ARGV-OUT-A @  ARGV-OUT-U @
   else
      ARGV-OUT-DEFAULT-A @  ARGV-OUT-DEFAULT-U @
   then ;

: ARGV-JSON? ( -- bool )  ARGV-JSON @ 0 <> ;

: ARGV-TAKE-NEXT {: a:ptr u :} ( ptr u8 n -- ptr u8 n )
   ARGV-I @ 1 + ARGV-COUNT >= if a u ARGV-MISSING then
   ARGV-I @ 1 + ARGV-I !
   ARGV-I @ ARGV-TOK$ ;

: ARGV-PARSE-OPT {: a:ptr u :} ( ptr u8 n -- )
   a u s" --json" ARGV-BYTES= if -1 ARGV-JSON ! exit then
   a u s" -o" ARGV-BYTES= if a u ARGV-TAKE-NEXT ARGV-OUT! exit then
   a u ARGV-DASH? if a u ARGV-UNKNOWN else a u ARGV-POS+ then ;

: ARGV-COLLECT-REST ( -- )
   begin ARGV-I @ ARGV-COUNT < while
      ARGV-I @ ARGV-TOK$ ARGV-POS+
      ARGV-I @ 1 + ARGV-I !
   repeat ;

: ARGV-PARSE ( -- )
   ARGV-RESET
   begin ARGV-I @ ARGV-COUNT < while
      ARGV-I @ s" --" ARGV-TOK= if
         ARGV-I @ 1 + ARGV-I !
         ARGV-COLLECT-REST
         exit
      then
      ARGV-I @ ARGV-TOK$ ARGV-PARSE-OPT
      ARGV-I @ 1 + ARGV-I !
   repeat ;

: ARGV-EXPECT-POS {: lo hi :} ( n n -- )
   ARGV-NPOS @ lo < if s" wrong number of positional arguments" ARGV-FAIL then
   hi 0 >= if
      ARGV-NPOS @ hi > if s" wrong number of positional arguments" ARGV-FAIL then
   then ;

: ARGV-EXPECT-POS-EXACT {: n :} ( n -- )
   n n ARGV-EXPECT-POS ;

: ARGV-REQUIRE-OUT ( -- )
   ARGV-OUT? 0= if s" missing -o OUT" ARGV-FAIL then ;

: ARGV-ZCOPY {: a:ptr u dst:ptr cap :} ( ptr u8 n ptr u8 n -- ptr u8 )
   u 1 + cap > if ARGV-E-INTERNAL throw then
   0 begin dup u < while
      dup a + c@  over dst + c!
      1 +
   repeat drop
   0 dst u + c!
   dst ;

: ARGV-PATHZ ( ptr u8 n -- ptr u8 )
   ARGV-PATH-BUF ARGV-PATH-CAP ARGV-ZCOPY ;

: ARGV-POSZ ( n -- ptr u8 )
   ARGV-POS$ ARGV-PATHZ ;

: ARGV-OUTZ ( -- ptr u8 )
   ARGV-OUT$ ARGV-PATHZ ;

: ARGV-INIT ( -- )
   ARGV-USE-SCRIPT
   ARGV-RESET
   NULL$ ARGV-OUT-DEFAULT!
   0 ARGV-QUIET!
   s" hb script.f [options] file ..." ARGV-USAGE! ;

ARGV-INIT
