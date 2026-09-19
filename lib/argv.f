\ argv.f -- checked argv parser for `bin/hb script.f args...` scripts.
\
\ The module lives in `package ARGV`; the tool CLIs and the stdlib share this one
\ packaged module. External callers drive
\ it through the qualified public API: ARGV:USAGE! / ARGV:QUIET! configure the
\ usage text and diagnostic writes, ARGV:PARSE reads SCRIPT-ARGC / SCRIPT-ARGV$
\ (or a mock set installed with ARGV:MOCK-CLEAR / ARGV:MOCK+ / ARGV:USE-SCRIPT),
\ and the accessors ARGV:POS# / ARGV:POS$ / ARGV:LABEL$ / ARGV:OUT$ / ARGV:JSON?
\ and the other flag predicates read the parsed result. ARGV:EXPECT-POS /
\ ARGV:EXPECT-POS-EXACT / ARGV:REQUIRE-OUT / ARGV:REQUIRE-LABEL validate arity and
\ required options, ARGV:FAIL throws a caller-worded usage failure, ARGV:RESET
\ clears parser state, and ARGV:POSZ / ARGV:OUTZ / ARGV:PATHZ / ARGV:ZCOPY produce
\ NUL-terminated path copies in the module path buffer. A usage failure throws the
\ public code ARGV:E-USAGE after emitting the usage text, and a capacity failure
\ throws ARGV:E-INTERNAL. Every buffer, parser-state cell, and internal helper
\ (ARGV-FAIL-DONE, ARGV-PARSE-OPT, ARGV-TAKE-NEXT, ...) is package-private.

package ARGV

public

64 constant E-USAGE
76 constant E-INTERNAL

private

64 constant ARGV-MAX
1024 constant ARGV-MSG-CAP
1024 constant ARGV-PATH-CAP
$0A constant ARGV-CHAR-LF
$2D constant ARGV-CHAR-DASH

STRUCTURE span 0 FIELD base ptr u8 FIELD size len ;STRUCTURE
ENUM configured-value 0
   VARIANT defaulted FIELD value span ;VARIANT
   VARIANT explicit FIELD value span ;VARIANT
;ENUM

variable ARGV-USE-MOCK?
variable ARGV-MOCK#
ARGV-MAX LAYOUT-BUFFER ARGV-MOCK span

variable ARGV-I
variable ARGV-NPOS
variable ARGV-SCAN-NPOS
ARGV-MAX LAYOUT-BUFFER ARGV-POS span

TYPED-VARIABLE ARGV-JSON bool
TYPED-VARIABLE ARGV-ALL-ERRORS bool
TYPED-VARIABLE ARGV-STRICT-BOUNDARY bool

TYPED-VARIABLE ARGV-LABEL configured-value
TYPED-VARIABLE ARGV-LABEL-DEFAULT span

TYPED-VARIABLE ARGV-OUT configured-value
TYPED-VARIABLE ARGV-OUT-DEFAULT span

TYPED-VARIABLE ARGV-USAGE span
variable ARGV-QUIET
variable ARGV-MSG-L
create ARGV-MSG ARGV-MSG-CAP allot
create ARGV-PATH-BUF ARGV-PATH-CAP allot

: ARGV-FALSE ( -- bool )  0 0= 0= ;

: ARGV-TRUE ( -- bool )  0 0= ;

: >SPAN ( ptr u8 n -- span )
   dup 0 < if E-INTERNAL throw then
   >LEN SPAN-MAKE ;

: SPAN$ ( span -- ptr u8 n )
   SPAN-UNMAKE LEN>N ;

: EXPLICIT? ( configured-value -- bool )
   MATCH configured-value
      defaulted OF drop ARGV-FALSE ENDOF
      explicit OF drop ARGV-TRUE ENDOF
   ;MATCH ;

: VALUE$ ( configured-value -- ptr u8 n )
   MATCH configured-value
      defaulted OF SPAN$ ENDOF
      explicit OF SPAN$ ENDOF
   ;MATCH ;

: ARGV-BYTES= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if ARGV-FALSE exit then
   0 begin dup u < while
      dup a + c@  over b + c@  <> if drop ARGV-FALSE exit then
      1 +
   repeat drop ARGV-TRUE ;

: ARGV-BUF-FAIL ( -- )  E-INTERNAL throw ;

: ARGV-MSG+ ( ptr u8 n -- ) {: a:ptr u :}
   ARGV-MSG-L @ u + ARGV-MSG-CAP > if ARGV-BUF-FAIL then
   0 begin dup u < while
      dup a + c@  ARGV-MSG ARGV-MSG-L @ + c!
      ARGV-MSG-L @ 1 + ARGV-MSG-L !
      1 +
   repeat drop ;

: ARGV-MSG-C+ ( n -- ) {: c :}
   ARGV-MSG-L @ 1 + ARGV-MSG-CAP > if ARGV-BUF-FAIL then
   c ARGV-MSG ARGV-MSG-L @ + c!
   ARGV-MSG-L @ 1 + ARGV-MSG-L ! ;

public

: USAGE! ( ptr u8 n -- )
   >SPAN ARGV-USAGE ! ;

: QUIET! ( n -- )  ARGV-QUIET ! ;

private

: ARGV-FAIL-DONE ( -- )
   ARGV-CHAR-LF ARGV-MSG-C+
   s" usage: " ARGV-MSG+
   ARGV-USAGE @ SPAN$ ARGV-MSG+
   ARGV-CHAR-LF ARGV-MSG-C+
   ARGV-QUIET @ 0 = if 2 ARGV-MSG ARGV-MSG-L @ write drop then
   E-USAGE throw ;

public

: FAIL ( ptr u8 n -- ) {: a:ptr u:n :}
   0 ARGV-MSG-L !
   a u ARGV-MSG+
   ARGV-FAIL-DONE ;

private

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

public

: RESET ( -- )
   0 ARGV-I !
   0 ARGV-NPOS !
   ARGV-FALSE ARGV-JSON !
   ARGV-FALSE ARGV-ALL-ERRORS !
   ARGV-FALSE ARGV-STRICT-BOUNDARY !
   ARGV-LABEL-DEFAULT @ construct configured-value defaulted ARGV-LABEL !
   ARGV-OUT-DEFAULT @ construct configured-value defaulted ARGV-OUT ! ;

: USE-SCRIPT ( -- )  0 ARGV-USE-MOCK? ! ;

: MOCK-CLEAR ( -- )
   -1 ARGV-USE-MOCK? !
   0 ARGV-MOCK# ! ;

: MOCK+ ( ptr u8 n -- )
   ARGV-MOCK# @ ARGV-MAX >= if E-INTERNAL throw then
   >SPAN ARGV-MOCK# @ ARGV-MOCK !
   ARGV-MOCK# @ 1 + ARGV-MOCK# ! ;

: COUNT ( -- n )
   ARGV-USE-MOCK? @ if ARGV-MOCK# @ else SCRIPT-ARGC then ;

private

: ARGV-TOKEN ( n -- span ) {: idx:n :}
   idx 0 < idx COUNT >= or if s" token index out of range" FAIL then
   ARGV-USE-MOCK? @ if
      idx ARGV-MOCK @
   else
      idx SCRIPT-ARGV$ >SPAN
   then ;

public

: TOK$ ( n -- ptr u8 n )
   ARGV-TOKEN SPAN$ ;

: TOK= ( n ptr u8 n -- bool ) {: idx:n a:ptr u:n :}
   idx TOK$ a u ARGV-BYTES= ;

private

: ARGV-DASH? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 1 > if a c@ ARGV-CHAR-DASH = else ARGV-FALSE then ;

: ARGV-POS+ ( span bool -- ) {: arg:span dry:bool :}
   ARGV-SCAN-NPOS @ ARGV-MAX >= if s" too many positional arguments" FAIL then
   dry 0= if arg ARGV-SCAN-NPOS @ ARGV-POS ! then
   ARGV-SCAN-NPOS @ 1+ ARGV-SCAN-NPOS ! ;

public

: POS# ( -- n )  ARGV-NPOS @ ;

: POS$ ( n -- ptr u8 n ) {: idx:n :}
   idx 0 <  idx ARGV-NPOS @ >= or if s" positional index out of range" FAIL then
   idx ARGV-POS @ SPAN$ ;

: LABEL! ( ptr u8 n -- )
   >SPAN construct configured-value explicit ARGV-LABEL ! ;

: LABEL? ( -- bool )
   ARGV-LABEL @ EXPLICIT? ;

: LABEL-DEFAULT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u >SPAN {: value:span :}
   value ARGV-LABEL-DEFAULT !
   LABEL? 0= if value construct configured-value defaulted ARGV-LABEL ! then ;

: LABEL$ ( -- ptr u8 n )
   ARGV-LABEL @ VALUE$ ;

: OUT! ( ptr u8 n -- )
   >SPAN construct configured-value explicit ARGV-OUT ! ;

: OUT? ( -- bool )
   ARGV-OUT @ EXPLICIT? ;

: OUT-DEFAULT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u >SPAN {: value:span :}
   value ARGV-OUT-DEFAULT !
   OUT? 0= if value construct configured-value defaulted ARGV-OUT ! then ;

: OUT$ ( -- ptr u8 n )
   ARGV-OUT @ VALUE$ ;

: JSON? ( -- bool )  ARGV-JSON @ ;

: ALL-ERRORS? ( -- bool )  ARGV-ALL-ERRORS @ ;

: STRICT-BOUNDARY? ( -- bool )  ARGV-STRICT-BOUNDARY @ ;

private

: ARGV-TAKE-NEXT ( span -- span ) {: arg:span :}
   ARGV-I @ 1 + COUNT >= if arg SPAN$ ARGV-MISSING then
   ARGV-I @ 1 + ARGV-I !
   ARGV-I @ ARGV-TOKEN ;

: ARGV-PARSE-OPT ( span bool -- ) {: arg:span dry:bool :}
   arg SPAN$ s" --json" ARGV-BYTES= arg SPAN$ s" --json-errors" ARGV-BYTES= or if
      dry 0= if ARGV-TRUE ARGV-JSON ! then exit
   then
   arg SPAN$ s" --label" ARGV-BYTES= if
      arg ARGV-TAKE-NEXT
      dry if drop else construct configured-value explicit ARGV-LABEL ! then exit
   then
   arg SPAN$ s" --all-errors" ARGV-BYTES= if
      dry 0= if ARGV-TRUE ARGV-ALL-ERRORS ! then exit
   then
   arg SPAN$ s" --strict-boundary" ARGV-BYTES= if
      dry 0= if ARGV-TRUE ARGV-STRICT-BOUNDARY ! then exit
   then
   arg SPAN$ s" -o" ARGV-BYTES= if
      arg ARGV-TAKE-NEXT
      dry if drop else construct configured-value explicit ARGV-OUT ! then exit
   then
   arg SPAN$ ARGV-DASH? if arg SPAN$ ARGV-UNKNOWN else arg dry ARGV-POS+ then ;

: ARGV-COLLECT-REST ( bool -- ) {: dry:bool :}
   begin ARGV-I @ COUNT < while
      ARGV-I @ ARGV-TOKEN dry ARGV-POS+
      ARGV-I @ 1 + ARGV-I !
   repeat ;

: ARGV-SCAN ( bool -- ) {: dry:bool :}
   0 ARGV-I !  0 ARGV-SCAN-NPOS !
   begin ARGV-I @ COUNT < while
      ARGV-I @ s" --" TOK= if
         ARGV-I @ 1 + ARGV-I !
         dry ARGV-COLLECT-REST
         exit
      then
      ARGV-I @ ARGV-TOKEN dry ARGV-PARSE-OPT
      ARGV-I @ 1 + ARGV-I !
   repeat ;

public

\ Validate with the same parser before replacing any published result. A bad
\ option or full positional list therefore leaves the prior parse intact.
: PARSE ( -- )
   ARGV-TRUE ARGV-SCAN
   RESET
   ARGV-FALSE ARGV-SCAN
   ARGV-SCAN-NPOS @ ARGV-NPOS ! ;

: EXPECT-POS ( n n -- ) {: lo:n hi:n :}
   ARGV-NPOS @ lo < if s" wrong number of positional arguments" FAIL then
   hi 0 >= if
      ARGV-NPOS @ hi > if s" wrong number of positional arguments" FAIL then
   then ;

: EXPECT-POS-EXACT ( n -- ) {: n:n :}
   n n EXPECT-POS ;

: REQUIRE-OUT ( -- )
   OUT? 0= if s" missing -o OUT" FAIL then ;

: REQUIRE-LABEL ( -- )
   LABEL? 0= if s" missing --label NAME" FAIL then ;

: ZCOPY ( ptr u8 n ptr u8 n -- ptr u8 ) {: a:ptr u:n dst:ptr cap:n :}
   u 0 < if E-INTERNAL throw then
   cap 0 <= if E-INTERNAL throw then
   u cap >= if E-INTERNAL throw then
   0 begin dup u < while
      dup a + c@  over dst + c!
      1 +
   repeat drop
   0 dst u + c!
   dst ;

: PATHZ ( ptr u8 n -- ptr u8 )
   ARGV-PATH-BUF ARGV-PATH-CAP ZCOPY ;

: POSZ ( n -- ptr u8 )
   POS$ PATHZ ;

: OUTZ ( -- ptr u8 )
   OUT$ PATHZ ;

private

: ARGV-INIT ( -- )
   USE-SCRIPT
   NULL$ >SPAN ARGV-LABEL-DEFAULT !
   NULL$ >SPAN ARGV-OUT-DEFAULT !
   RESET
   0 QUIET!
   s" hb script.f [options] file ..." USAGE! ;

ARGV-INIT

;package
