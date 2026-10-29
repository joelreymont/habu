\ argv.f - checked argv parser for `bin/hb script.f args...` scripts (ARGV).
\ PARSE reads SCRIPT-ARGC / SCRIPT-ARGV$, which start after the `--` ending
\ `bin/hb --load a.f -- args` (file-source mode leaves non-tty stdin to the
\ tool), or the MOCK+ set until USE-SCRIPT. A refusal throws E-USAGE (64, the
\ exit code when uncaught).
\ STORAGE CLASS. PROCESS-WIDE: static parser state; one task parses at a time.

package ARGV

public

64 constant E-USAGE
76 constant E-INTERNAL

private

$2D constant ARGV-CHAR-DASH

STRUCTURE span FIELD base ptr u8 FIELD size len ;STRUCTURE
ENUM configured-value
   VARIANT defaulted FIELD value span ;VARIANT
   VARIANT explicit FIELD value span ;VARIANT
;ENUM

variable ARGV-USE-MOCK?
variable ARGV-MOCK#
DYNAMIC-BUFFER ARGV-MOCK span

variable ARGV-I
variable ARGV-NPOS
variable ARGV-SCAN-NPOS
DYNAMIC-BUFFER ARGV-POS span

TYPED-VARIABLE ARGV-JSON bool
TYPED-VARIABLE ARGV-ALL-ERRORS bool
TYPED-VARIABLE ARGV-STRICT-BOUNDARY bool

TYPED-VARIABLE ARGV-LABEL configured-value
TYPED-VARIABLE ARGV-LABEL-DEFAULT span

TYPED-VARIABLE ARGV-OUT configured-value
TYPED-VARIABLE ARGV-OUT-DEFAULT span

TYPED-VARIABLE ARGV-USAGE span
variable ARGV-QUIET

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

: ARGV-ERR ( ptr u8 n -- ) {: a:ptr u:n :}   \ one piece of a refusal line
   ARGV-QUIET @ 0 = if 2 a u write drop then ;

public

: USAGE! ( ptr u8 n -- )
   >SPAN ARGV-USAGE ! ;

: QUIET! ( n -- )  ARGV-QUIET ! ;

private

: ARGV-FAIL-DONE ( -- )
   S\" \nusage: " ARGV-ERR
   ARGV-USAGE @ SPAN$ ARGV-ERR
   S\" \n" ARGV-ERR
   E-USAGE throw ;

public

: FAIL ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-INTERNAL throw then
   a u ARGV-ERR
   ARGV-FAIL-DONE ;

private

: ARGV-UNKNOWN ( ptr u8 n -- ) {: a:ptr u :}
   s" unknown option: " ARGV-ERR
   a u ARGV-ERR
   ARGV-FAIL-DONE ;

: ARGV-MISSING ( ptr u8 n -- ) {: a:ptr u :}
   s" missing value for " ARGV-ERR
   a u ARGV-ERR
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
   >SPAN {: arg:span :}
   ARGV-MOCK# @ 1+ ARGV-MOCK-RESERVE
   arg ARGV-MOCK# @ ARGV-MOCK !
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

\ The dry scan validates with the same parser before any published result is
\ replaced, and counts the positionals so the table grows before RESET.
: PARSE ( -- )
   ARGV-TRUE ARGV-SCAN
   ARGV-SCAN-NPOS @ ARGV-POS-RESERVE
   RESET
   ARGV-FALSE ARGV-SCAN
   ARGV-SCAN-NPOS @ ARGV-NPOS ! ;

\ hi < 0 means unbounded.
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
