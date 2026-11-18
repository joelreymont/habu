split-str
\ argv.f - checked argv parser for `bin/hb script.f args...` scripts (ARGV).
\ PARSE reads SCRIPT-ARGC / SCRIPT-ARGV$, which start after the `--` ending
\ `bin/hb --load a.f -- args` (file-source mode leaves non-tty stdin to the
\ tool), or the MOCK+ set until USE-SCRIPT. A rejection throws EXIT-STATUS:USAGE,
\ the exit status when uncaught.
\ STORAGE CLASS. PROCESS-WIDE: static parser state; one task parses at a time.

require src/core/exit-status.f

package ARGV

$2D constant ARGV-CHAR-DASH

STRUCTURE span FIELD base ^u8 FIELD size len ;STRUCTURE
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

TYPED-VARIABLE ARGV-LABEL configured-value
TYPED-VARIABLE ARGV-OUT configured-value

TYPED-VARIABLE ARGV-USAGE span
variable ARGV-QUIET

: ARGV-FALSE ( -- bool )  0 0= 0= ;

: ARGV-TRUE ( -- bool )  0 0= ;

: >SPAN ( str -- span )
   dup 0 < if EXIT-STATUS:INTERNAL throw then
   >LEN SPAN-MAKE ;

: SPAN$ ( span -- ^u8 n )
   SPAN-UNMAKE LEN>N ;

: EXPLICIT? ( configured-value -- bool )
   MATCH configured-value
      defaulted OF drop ARGV-FALSE ENDOF
      explicit OF drop ARGV-TRUE ENDOF
   ;MATCH ;

: VALUE$ ( configured-value -- str )
   MATCH configured-value
      defaulted OF SPAN$ ENDOF
      explicit OF SPAN$ ENDOF
   ;MATCH ;

: ARGV-BYTES= ( str str -- bool ) {: a:^ u b:^ v :}
   u v <> if ARGV-FALSE exit then
   0 begin dup u < while
      dup a + c@  over b + c@  <> if drop ARGV-FALSE exit then
      1 +
   repeat drop ARGV-TRUE ;

: ARGV-ERR ( str -- ) {: a:^ u:n :}   \ one piece of a refusal line
   ARGV-QUIET @ 0 = if 2 a u write drop then ;

public

: USAGE! ( str -- )
   >SPAN ARGV-USAGE ! ;

: QUIET! ( n -- )  ARGV-QUIET ! ;

private

: ARGV-FAIL-DONE ( -- )
   S\" \nusage: " ARGV-ERR
   ARGV-USAGE @ SPAN$ ARGV-ERR
   S\" \n" ARGV-ERR
   EXIT-STATUS:USAGE throw ;

public

: FAIL ( ^u8 n -- ) {: a:^ u:n :}
   u 0 < if EXIT-STATUS:INTERNAL throw then
   a u ARGV-ERR
   ARGV-FAIL-DONE ;

private

: ARGV-UNKNOWN ( str -- ) {: a:^ u :}
   s" unknown option: " ARGV-ERR
   a u ARGV-ERR
   ARGV-FAIL-DONE ;

: ARGV-MISSING ( str -- ) {: a:^ u :}
   s" missing value for " ARGV-ERR
   a u ARGV-ERR
   ARGV-FAIL-DONE ;

public

: RESET ( -- )
   0 ARGV-I !
   0 ARGV-NPOS !
   ARGV-FALSE ARGV-JSON !
   NULL$ >SPAN construct configured-value defaulted ARGV-LABEL !
   NULL$ >SPAN construct configured-value defaulted ARGV-OUT ! ;

: USE-SCRIPT ( -- )  0 ARGV-USE-MOCK? ! ;

: MOCK-CLEAR ( -- )
   -1 ARGV-USE-MOCK? !
   0 ARGV-MOCK# ! ;

: MOCK+ ( str -- )
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

: TOK$ ( n -- str )
   ARGV-TOKEN SPAN$ ;

: TOK= ( n str -- bool ) {: idx:n a:^ u:n :}
   idx TOK$ a u ARGV-BYTES= ;

private

: ARGV-DASH? ( str -- bool ) {: a:^ u :}
   u 1 > if a c@ ARGV-CHAR-DASH = else ARGV-FALSE then ;

: ARGV-POS+ ( span bool -- ) {: arg:span dry:bool :}
   dry 0= if arg ARGV-SCAN-NPOS @ ARGV-POS ! then
   ARGV-SCAN-NPOS @ 1+ ARGV-SCAN-NPOS ! ;

public

: POS# ( -- n )  ARGV-NPOS @ ;

: POS$ ( n -- str ) {: idx:n :}
   idx 0 <  idx ARGV-NPOS @ >= or if s" positional index out of range" FAIL then
   idx ARGV-POS @ SPAN$ ;

: LABEL! ( ^u8 n -- )
   >SPAN construct configured-value explicit ARGV-LABEL ! ;

: LABEL? ( -- bool )
   ARGV-LABEL @ EXPLICIT? ;

: LABEL$ ( -- str )
   ARGV-LABEL @ VALUE$ ;

: OUT! ( str -- )
   >SPAN construct configured-value explicit ARGV-OUT ! ;

: OUT? ( -- bool )
   ARGV-OUT @ EXPLICIT? ;

: OUT$ ( -- ^u8 n )
   ARGV-OUT @ VALUE$ ;

: JSON? ( -- bool )  ARGV-JSON @ ;

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

: REQUIRE-LABEL ( -- )
   LABEL? 0= if s" missing --label NAME" FAIL then ;

private

: ARGV-INIT ( -- )
   USE-SCRIPT
   RESET
   0 QUIET!
   s" hb script.f [options] file ..." USAGE! ;

ARGV-INIT

;package
