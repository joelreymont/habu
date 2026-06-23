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
variable ARGV-STRICT-SIGNATURES
variable ARGV-ALL-ERRORS
variable ARGV-STRICT-BOUNDARY

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

: ARGV-FALSE ( -- bool )  0 0= 0= ;

: ARGV-TRUE ( -- bool )  0 0= ;

: ARGV-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: ARGV-PTR-U8@ ( ptr a -- ptr u8 )
   ARGV-PTR-U8-FIELD @ ;

: ARGV-PTR-U8! ( ptr u8 ptr a -- )
   ARGV-PTR-U8-FIELD ! ;

: ARGV-PTR-U8-SLOT ( n ptr a -- ptr ptr u8 )
   swap cells + ARGV-PTR-U8-FIELD ;

: ARGV-PTR-U8-SLOT@ ( n ptr a -- ptr u8 )
   ARGV-PTR-U8-SLOT @ ;

: ARGV-PTR-U8-SLOT! ( ptr u8 n ptr a -- )
   ARGV-PTR-U8-SLOT ! ;

: ARGV-BYTES= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if ARGV-FALSE exit then
   0 begin dup u < while
      dup a + c@  over b + c@  <> if drop ARGV-FALSE exit then
      1 +
   repeat drop ARGV-TRUE ;

: ARGV-BUF-FAIL ( -- )  ARGV-E-INTERNAL throw ;

: ARGV-MSG+ ( ptr u8 n -- ) {: a:ptr u :}
   ARGV-MSG-L @ u + ARGV-MSG-CAP > if ARGV-BUF-FAIL then
   0 begin dup u < while
      dup a + c@  ARGV-MSG ARGV-MSG-L @ + c!
      ARGV-MSG-L @ 1 + ARGV-MSG-L !
      1 +
   repeat drop ;

: ARGV-MSG-C+ ( c -- ) {: c :}
   ARGV-MSG-L @ 1 + ARGV-MSG-CAP > if ARGV-BUF-FAIL then
   c ARGV-MSG ARGV-MSG-L @ + c!
   ARGV-MSG-L @ 1 + ARGV-MSG-L ! ;

: ARGV-USAGE! ( ptr u8 n -- ) {: a:ptr u :}
   a ARGV-USAGE-A ARGV-PTR-U8!  u ARGV-USAGE-U ! ;

: ARGV-QUIET! ( n -- )  ARGV-QUIET ! ;

: ARGV-FAIL-DONE ( -- )
   ARGV-CHAR-LF ARGV-MSG-C+
   s" usage: " ARGV-MSG+
   ARGV-USAGE-A ARGV-PTR-U8@ ARGV-USAGE-U @ ARGV-MSG+
   ARGV-CHAR-LF ARGV-MSG-C+
   ARGV-QUIET @ 0 = if 2 ARGV-MSG ARGV-MSG-L @ write drop then
   ARGV-E-USAGE throw ;

: ARGV-FAIL ( ptr u8 n -- ) {: a:ptr u :}
   0 ARGV-MSG-L !
   a u ARGV-MSG+
   ARGV-FAIL-DONE ;

: ARGV-UNKNOWN ( ptr u8 n -- ) {: a:ptr u :}
   0 ARGV-MSG-L !
   s" unknown option: " ARGV-MSG+
   a u ARGV-MSG+
   ARGV-FAIL-DONE ;

: ARGV-MISSING ( ptr u8 n -- ) {: a:ptr u :}
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
   0 ARGV-STRICT-BOUNDARY !
   NULL$ drop ARGV-LABEL-A ARGV-PTR-U8!  0 ARGV-LABEL-U !  0 ARGV-LABEL-SET !
   NULL$ drop ARGV-OUT-A ARGV-PTR-U8!  0 ARGV-OUT-U !  0 ARGV-OUT-SET ! ;

: ARGV-USE-SCRIPT ( -- )  0 ARGV-USE-MOCK? ! ;

: ARGV-MOCK-CLEAR ( -- )
   -1 ARGV-USE-MOCK? !
   0 ARGV-MOCK# ! ;

: ARGV-MOCK+ ( ptr u8 n -- ) {: a:ptr u :}
   ARGV-MOCK# @ ARGV-MAX >= if ARGV-E-INTERNAL throw then
   a ARGV-MOCK# @ ARGV-MOCK-A ARGV-PTR-U8-SLOT!
   u ARGV-MOCK-U ARGV-MOCK# @ cells + !
   ARGV-MOCK# @ 1 + ARGV-MOCK# ! ;

: ARGV-COUNT ( -- n )
   ARGV-USE-MOCK? @ if ARGV-MOCK# @ else SCRIPT-ARGC then ;

: ARGV-TOK$ ( n -- ptr u8 n ) {: idx :}
   ARGV-USE-MOCK? @ if
      idx ARGV-MOCK-A ARGV-PTR-U8-SLOT@
      idx cells ARGV-MOCK-U + @
   else
      idx SCRIPT-ARGV$
   then ;

: ARGV-TOK= ( n ptr u8 n -- bool ) {: idx a:ptr u :}
   idx ARGV-TOK$ a u ARGV-BYTES= ;

: ARGV-DASH? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 1 > if a c@ ARGV-CHAR-DASH = else ARGV-FALSE then ;

: ARGV-POS+ ( ptr u8 n -- ) {: a:ptr u :}
   ARGV-NPOS @ ARGV-MAX >= if s" too many positional arguments" ARGV-FAIL then
   a ARGV-NPOS @ ARGV-POS-A ARGV-PTR-U8-SLOT!
   u ARGV-POS-U ARGV-NPOS @ cells + !
   ARGV-NPOS @ 1 + ARGV-NPOS ! ;

: ARGV-POS# ( -- n )  ARGV-NPOS @ ;

: ARGV-POS$ ( n -- ptr u8 n ) {: idx :}
   idx 0 <  idx ARGV-NPOS @ >= or if s" positional index out of range" ARGV-FAIL then
   idx ARGV-POS-A ARGV-PTR-U8-SLOT@
   idx cells ARGV-POS-U + @ ;

: ARGV-LABEL! ( ptr u8 n -- ) {: a:ptr u :}
   a ARGV-LABEL-A ARGV-PTR-U8!  u ARGV-LABEL-U !  -1 ARGV-LABEL-SET ! ;

: ARGV-LABEL-DEFAULT! ( ptr u8 n -- ) {: a:ptr u :}
   a ARGV-LABEL-DEFAULT-A ARGV-PTR-U8!  u ARGV-LABEL-DEFAULT-U ! ;

: ARGV-LABEL? ( -- bool )  ARGV-LABEL-SET @ 0 <> ;

: ARGV-LABEL$ ( -- ptr u8 n )
   ARGV-LABEL? if
      ARGV-LABEL-A ARGV-PTR-U8@  ARGV-LABEL-U @
   else
      ARGV-LABEL-DEFAULT-A ARGV-PTR-U8@  ARGV-LABEL-DEFAULT-U @
   then ;

: ARGV-OUT! ( ptr u8 n -- ) {: a:ptr u :}
   a ARGV-OUT-A ARGV-PTR-U8!  u ARGV-OUT-U !  -1 ARGV-OUT-SET ! ;

: ARGV-OUT-DEFAULT! ( ptr u8 n -- ) {: a:ptr u :}
   a ARGV-OUT-DEFAULT-A ARGV-PTR-U8!  u ARGV-OUT-DEFAULT-U ! ;

: ARGV-OUT? ( -- bool )  ARGV-OUT-SET @ 0 <> ;

: ARGV-OUT$ ( -- ptr u8 n )
   ARGV-OUT? if
      ARGV-OUT-A ARGV-PTR-U8@  ARGV-OUT-U @
   else
      ARGV-OUT-DEFAULT-A ARGV-PTR-U8@  ARGV-OUT-DEFAULT-U @
   then ;

: ARGV-JSON? ( -- bool )  ARGV-JSON @ 0 <> ;

: ARGV-STRICT-SIGNATURES? ( -- bool )  ARGV-STRICT-SIGNATURES @ 0 <> ;

: ARGV-ALL-ERRORS? ( -- bool )  ARGV-ALL-ERRORS @ 0 <> ;

: ARGV-STRICT-BOUNDARY? ( -- bool )  ARGV-STRICT-BOUNDARY @ 0 <> ;

: ARGV-TAKE-NEXT ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   ARGV-I @ 1 + ARGV-COUNT >= if a u ARGV-MISSING then
   ARGV-I @ 1 + ARGV-I !
   ARGV-I @ ARGV-TOK$ ;

: ARGV-PARSE-OPT ( ptr u8 n -- ) {: a:ptr u :}
   a u s" --json" ARGV-BYTES= if -1 ARGV-JSON ! exit then
   a u s" --json-errors" ARGV-BYTES= if -1 ARGV-JSON ! exit then
   a u s" --label" ARGV-BYTES= if a u ARGV-TAKE-NEXT ARGV-LABEL! exit then
   a u s" --strict-signatures" ARGV-BYTES= if -1 ARGV-STRICT-SIGNATURES ! exit then
   a u s" --all-errors" ARGV-BYTES= if -1 ARGV-ALL-ERRORS ! exit then
   a u s" --strict-boundary" ARGV-BYTES= if -1 ARGV-STRICT-BOUNDARY ! exit then
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

: ARGV-EXPECT-POS ( n n -- ) {: lo hi :}
   ARGV-NPOS @ lo < if s" wrong number of positional arguments" ARGV-FAIL then
   hi 0 >= if
      ARGV-NPOS @ hi > if s" wrong number of positional arguments" ARGV-FAIL then
   then ;

: ARGV-EXPECT-POS-EXACT ( n -- ) {: n :}
   n n ARGV-EXPECT-POS ;

: ARGV-REQUIRE-OUT ( -- )
   ARGV-OUT? 0= if s" missing -o OUT" ARGV-FAIL then ;

: ARGV-REQUIRE-LABEL ( -- )
   ARGV-LABEL? 0= if s" missing --label NAME" ARGV-FAIL then ;

: ARGV-ZCOPY ( ptr u8 n ptr u8 n -- ptr u8 ) {: a:ptr u dst:ptr cap :}
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
   NULL$ ARGV-LABEL-DEFAULT!
   NULL$ ARGV-OUT-DEFAULT!
   0 ARGV-QUIET!
   s" hb script.f [options] file ..." ARGV-USAGE! ;

ARGV-INIT
