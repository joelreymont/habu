\ bootstrap-refresh-doc.f - validate the documented native refresh command.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f

package BOOTSTRAP-REFRESH-DOC
using LINT-SPLIT

0 constant TOKEN-ENGINE
1 constant TOKEN-LOAD
2 constant TOKEN-ENTRY
3 constant TOKEN-SEPARATOR
4 constant TOKEN-VERB
5 constant TOKEN-COUNT

1 constant COMMAND-LINE-OFF
2 constant CLOSE-FENCE-OFF

variable HEADING-INDEX
variable HEADING-COUNT

: HEADING? ( ptr u8 n -- bool )
   LINT-TRIM s" ## Refresh `bin/hb`" LINT-STR= ;

: SECTION? ( ptr u8 n -- bool )
   LINT-TRIM s" ## " LINT-STARTS-WITH? ;

: OPEN-FENCE? ( ptr u8 n -- bool )
   LINT-TRIM s" ```sh" LINT-STR= ;

: CLOSE-FENCE? ( ptr u8 n -- bool )
   LINT-TRIM s" ```" LINT-STR= ;

\ The generated bin/hb is the exact executable token but is not a tracked load
\ source. Resolve every Forth source token, including any unexpected extra.
: SOURCE-PATH? ( ptr u8 n -- bool )
   2dup s" .f" LINT-ENDS-WITH? if 2drop LINT-TRUE exit then
   s" .fs" LINT-ENDS-WITH? ;

: NOTE-HEADING ( n -- ) {: index:n :}
   index HEADING-INDEX !
   HEADING-COUNT @ 1 + HEADING-COUNT ! ;

: FIND-HEADING ( -- n )
   0 HEADING-COUNT !
   0 begin dup SN# @ < while
      dup S@ HEADING? if dup NOTE-HEADING then
      1+
   repeat drop
   HEADING-COUNT @ 1 <> if E-BUILD-BOOT-DRIFT throw then
   HEADING-INDEX @ ;

: FIND-FENCE ( n -- n )
   1 +
   begin dup SN# @ < while
      dup S@ OPEN-FENCE? if exit then
      dup S@ SECTION? if E-BUILD-BOOT-DRIFT throw then
      1 +
   repeat
   drop E-BUILD-BOOT-DRIFT throw ;

: COMMAND$ ( -- ptr u8 n )
   FIND-HEADING FIND-FENCE {: fence:n :}
   fence CLOSE-FENCE-OFF + SN# @ >= if E-BUILD-BOOT-DRIFT throw then
   fence CLOSE-FENCE-OFF + S@ CLOSE-FENCE? 0= if E-BUILD-BOOT-DRIFT throw then
   fence COMMAND-LINE-OFF + S@ LINT-TRIM
   dup 0= if 2drop E-BUILD-BOOT-DRIFT throw then ;

: RESOLVE-TOKEN ( n -- ) {: index:n :}
   index S@ 2dup SOURCE-PATH? 0= if 2drop exit then
   FILE? 0= if E-BUILD-PATH throw then ;

: RESOLVE-TOKENS ( -- )
   0 begin dup SN# @ < while
      dup RESOLVE-TOKEN
      1 +
   repeat drop ;

: EXPECTED$ ( n -- ptr u8 n )
   dup TOKEN-ENGINE = if drop s" bin/hb" exit then
   dup TOKEN-LOAD = if drop s" --load" exit then
   dup TOKEN-ENTRY = if drop s" tools/build-fixpoint-refresh.f" exit then
   dup TOKEN-SEPARATOR = if drop s" --" exit then
   dup TOKEN-VERB = if drop s" install" exit then
   drop E-TBL-BOUNDS throw ;

: CHECK-TOKEN ( n -- ) {: index:n :}
   index S@ index EXPECTED$ LINT-STR= 0= if
      E-BUILD-BOOT-DRIFT throw
   then ;

: CHECK-TOKENS ( -- )
   SN# @ TOKEN-COUNT <> if E-BUILD-BOOT-DRIFT throw then
   0 begin dup SN# @ < while
      dup CHECK-TOKEN
      1 +
   repeat drop ;

: CHECK-COMMAND$ ( ptr u8 n -- )
   SPLIT-WHITESPACE
   RESOLVE-TOKENS
   CHECK-TOKENS ;

public

: VALIDATE$ ( ptr u8 n -- )
   SPLIT-LINES
   COMMAND$ CHECK-COMMAND$ ;

: VALIDATE ( -- )
   s" docs/bootstrap.md" LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT VALIDATE$ ;

;package
