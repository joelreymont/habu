\ utf8-scalar-test.f - direct and property tests for UTF8:NEXT, the
\ whole-span check UTF8:VALID? built on it, the boundary cut UTF8:FIT and the
\ cut-character check UTF8:CUT?.

require src/lib/test/assert.f
require src/lib/property.f
require src/lib/utf8-scalar.f

package UTF8-SCALAR-TEST

$A5 constant CANARY
$80 constant CONTINUATION-TAG
$C0 constant CONTINUATION-MASK
$3F constant PAYLOAD-MASK
$1F constant TWO-LEAD-MASK
$0F constant THREE-LEAD-MASK
$07 constant FOUR-LEAD-MASK
6 constant CONTINUATION-SHIFT

$80 constant ASCII-LIMIT
$C2 constant TWO-FIRST
$E0 constant THREE-FIRST
$F0 constant FOUR-FIRST
$F5 constant FOUR-LAST-EXCLUSIVE
$A0 constant E0-SECOND-FIRST
$9F constant ED-SECOND-LAST
$90 constant F0-SECOND-FIRST
$8F constant F4-SECOND-LAST
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST
$10FFFF constant SCALAR-LAST

create CASE-BUF 6 allot

: YES ( -- bool )
   0 0= ;

: NO ( -- bool )
   0 0= 0= ;

: FAIL ( -- )
   1 0 T= ;

: PREP4 ( n n n n -- ^u8 )
   CANARY CASE-BUF c!
   CANARY CASE-BUF 5 + c!
   CASE-BUF 4 + c!  CASE-BUF 3 + c!
   CASE-BUF 2 + c!  CASE-BUF 1+ c!
   CASE-BUF 1+ ;

: CANARIES ( -- )
   CASE-BUF c@ CANARY T=
   CASE-BUF 5 + c@ CANARY T= ;

: ASSERT-SCALAR ( str n n n -- )
   {: source:str cursor:n want-scalar:n want-next:n :}
   source cursor UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         want-next T= want-scalar T=
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

: ASSERT-RAW ( str n n n -- )
   {: source:str cursor:n want-byte:n want-next:n :}
   source cursor UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         2drop FAIL
      ENDOF
      raw-byte OF
         want-next T= want-byte T=
      ENDOF
   ;MATCH ;

: DISCARD-STEP ( UTF8:scalar-step -- )
   MATCH UTF8:scalar-step
      scalar OF
         2drop
      ENDOF
      raw-byte OF
         2drop
      ENDOF
   ;MATCH ;

: WIDTH-ONE ( -- )
   $00 0 0 0 PREP4 1 >str 0 $00 1 ASSERT-SCALAR
   $7F 0 0 0 PREP4 1 >str 0 $7F 1 ASSERT-SCALAR ;

: WIDTH-TWO ( -- )
   $C2 $80 0 0 PREP4 2 >str 0 $80 2 ASSERT-SCALAR
   $DF $BF 0 0 PREP4 2 >str 0 $7FF 2 ASSERT-SCALAR ;

: WIDTH-THREE ( -- )
   $E0 $A0 $80 0 PREP4 3 >str 0 $800 3 ASSERT-SCALAR
   $EF $BF $BF 0 PREP4 3 >str 0 $FFFF 3 ASSERT-SCALAR ;

: WIDTH-FOUR ( -- )
   $F0 $90 $80 $80 PREP4 4 >str 0 $10000 4 ASSERT-SCALAR
   $F4 $8F $BF $BF PREP4 4 >str 0 $10FFFF 4 ASSERT-SCALAR ;

: WIDTH-BOUNDARIES ( -- )
   WIDTH-ONE WIDTH-TWO WIDTH-THREE WIDTH-FOUR CANARIES ;

: OVERLONG ( -- )
   $C0 $80 0 0 PREP4 2 >str 0 $C0 1 ASSERT-RAW
   $C1 $BF 0 0 PREP4 2 >str 0 $C1 1 ASSERT-RAW
   $E0 $80 $80 0 PREP4 3 >str 0 $E0 1 ASSERT-RAW
   $F0 $80 $80 $80 PREP4 4 >str 0 $F0 1 ASSERT-RAW
   CANARIES ;

: RAW-REMAINDER ( -- )
   $C0 $80 0 0 PREP4 2 >str 0 $C0 1 ASSERT-RAW
   $C0 $80 0 0 PREP4 2 >str 1 $80 2 ASSERT-RAW
   CANARIES ;

: LONE-CONTINUATIONS ( -- )
   $80 0 0 0 PREP4 1 >str 0 $80 1 ASSERT-RAW
   $BF 0 0 0 PREP4 1 >str 0 $BF 1 ASSERT-RAW
   CANARIES ;

: TRUNCATED-TWO ( -- )
   $C2 $80 0 0 PREP4 1 >str 0 $C2 1 ASSERT-RAW
   CANARIES ;

: TRUNCATED-THREE ( -- )
   $E0 $A0 $80 0 PREP4 1 >str 0 $E0 1 ASSERT-RAW
   $E0 $A0 $80 0 PREP4 2 >str 0 $E0 1 ASSERT-RAW
   CANARIES ;

: TRUNCATED-FOUR ( -- )
   $F0 $90 $80 $80 PREP4 1 >str 0 $F0 1 ASSERT-RAW
   $F0 $90 $80 $80 PREP4 2 >str 0 $F0 1 ASSERT-RAW
   $F0 $90 $80 $80 PREP4 3 >str 0 $F0 1 ASSERT-RAW
   CANARIES ;

: TRUNCATIONS ( -- )
   TRUNCATED-TWO TRUNCATED-THREE TRUNCATED-FOUR ;

: FINAL-LEAD ( -- )
   $41 $E0 $A0 $80 PREP4 2 >str 1 $E0 2 ASSERT-RAW
   CANARIES ;

: BAD-CONTINUATION-TWO ( -- )
   $C2 $41 0 0 PREP4 2 >str 0 $C2 1 ASSERT-RAW ;

: BAD-CONTINUATION-THREE ( -- )
   $E0 $41 $80 0 PREP4 3 >str 0 $E0 1 ASSERT-RAW
   $E0 $A0 $41 0 PREP4 3 >str 0 $E0 1 ASSERT-RAW ;

: BAD-CONTINUATION-FOUR ( -- )
   $F0 $41 $80 $80 PREP4 4 >str 0 $F0 1 ASSERT-RAW
   $F0 $90 $41 $80 PREP4 4 >str 0 $F0 1 ASSERT-RAW
   $F0 $90 $80 $41 PREP4 4 >str 0 $F0 1 ASSERT-RAW ;

: BAD-CONTINUATIONS ( -- )
   BAD-CONTINUATION-TWO
   BAD-CONTINUATION-THREE
   BAD-CONTINUATION-FOUR
   CANARIES ;

: SURROGATES ( -- )
   $ED $A0 $80 0 PREP4 3 >str 0 $ED 1 ASSERT-RAW
   $ED $BF $BF 0 PREP4 3 >str 0 $ED 1 ASSERT-RAW
   CANARIES ;

: ABOVE-SCALAR-LIMIT ( -- )
   $F4 $90 $80 $80 PREP4 4 >str 0 $F4 1 ASSERT-RAW
   $F7 $BF $BF $BF PREP4 4 >str 0 $F7 1 ASSERT-RAW
   $F8 $80 $80 $80 PREP4 4 >str 0 $F8 1 ASSERT-RAW
   $FF 0 0 0 PREP4 1 >str 0 $FF 1 ASSERT-RAW
   CANARIES ;

create ADJACENT-BUF $C2 c, $80 c, $80 c,

: ASSERT-EXCESS ( n -- )
   ADJACENT-BUF 3 >str swap UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         2drop FAIL
      ENDOF
      raw-byte OF
         3 T= $80 T=
      ENDOF
   ;MATCH ;

: ADJACENT-CALLS ( -- )
   ADJACENT-BUF 3 >str 0 UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         dup 2 T= swap $80 T= ASSERT-EXCESS
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

create NESTED-OUTER $C2 c, $A2 c,
create NESTED-INNER $F0 c, $9F c, $98 c, $80 c,

: ASSERT-INNER ( -- )
   NESTED-INNER 4 >str 0 $1F600 4 ASSERT-SCALAR ;

: NESTED-CALL ( -- )
   NESTED-OUTER 2 >str 0 UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         ASSERT-INNER 2 T= $A2 T=
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

create STATE-A-BUF $C2 c, $A2 c, $41 c,
create STATE-B-BUF $F0 c, $9F c, $98 c, $80 c, $42 c,
variable STATE-A-CURSOR
variable STATE-B-CURSOR

: STATE-A-FIRST ( -- )
   STATE-A-BUF 3 >str STATE-A-CURSOR @ UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         STATE-A-CURSOR ! $A2 T=
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

: STATE-A-SECOND ( -- )
   STATE-A-BUF 3 >str STATE-A-CURSOR @ UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         STATE-A-CURSOR ! $41 T=
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

: STATE-B-FIRST ( -- )
   STATE-B-BUF 5 >str STATE-B-CURSOR @ UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         STATE-B-CURSOR ! $1F600 T=
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

: STATE-B-SECOND ( -- )
   STATE-B-BUF 5 >str STATE-B-CURSOR @ UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         STATE-B-CURSOR ! $42 T=
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

: INTERLEAVED-STATES ( -- )
   0 STATE-A-CURSOR !  0 STATE-B-CURSOR !
   STATE-A-FIRST STATE-B-FIRST STATE-A-SECOND STATE-B-SECOND
   STATE-A-CURSOR @ 3 T=
   STATE-B-CURSOR @ 5 T= ;

: BAD-NEGATIVE-LENGTH ( -- )
   CASE-BUF 1+ -1 >str 0 UTF8:NEXT DISCARD-STEP ;

: BAD-NEGATIVE-CURSOR ( -- )
   CASE-BUF 1+ 1 >str -1 UTF8:NEXT DISCARD-STEP ;

: BAD-END-CURSOR ( -- )
   CASE-BUF 1+ 1 >str 1 UTF8:NEXT DISCARD-STEP ;

: BAD-PAST-CURSOR ( -- )
   CASE-BUF 1+ 1 >str 2 UTF8:NEXT DISCARD-STEP ;

: CURSOR-BOUNDS ( -- )
   $41 0 0 0 PREP4 drop
   [: BAD-NEGATIVE-LENGTH ;] E-STR-BOUNDS TTHROWSQ
   [: BAD-NEGATIVE-CURSOR ;] E-STR-BOUNDS TTHROWSQ
   [: BAD-END-CURSOR ;] E-STR-BOUNDS TTHROWSQ
   [: BAD-PAST-CURSOR ;] E-STR-BOUNDS TTHROWSQ
   [: CASE-BUF 1+ -1 >str UTF8:VALID? drop ;] E-STR-BOUNDS TTHROWSQ
   CANARIES ;

: CONTINUATION? ( n -- bool )
   CONTINUATION-MASK and CONTINUATION-TAG = ;

: CONTINUATION-AT? ( str n -- bool )
   {: source:str index:n :}
   index source STR:LEN >= if
      NO exit
   then
   source STR:DATA index + c@ CONTINUATION? ;

: SECOND-IN? ( str n n -- bool )
   {: source:str first:n last:n :}
   source STR:LEN 2 < if
      NO exit
   then
   source STR:DATA 1+ c@ first >=
   source STR:DATA 1+ c@ last <= and ;

: ORACLE-THREE? ( str n -- bool )
   {: source:str lead:n :}
   source 1 CONTINUATION-AT?
   source 2 CONTINUATION-AT? and
   lead THREE-FIRST = if
      source E0-SECOND-FIRST $BF SECOND-IN? and
   then
   lead $ED = if
      source $80 ED-SECOND-LAST SECOND-IN? and
   then ;

: ORACLE-FOUR? ( str n -- bool )
   {: source:str lead:n :}
   source 1 CONTINUATION-AT?
   source 2 CONTINUATION-AT? and
   source 3 CONTINUATION-AT? and
   lead FOUR-FIRST = if
      source F0-SECOND-FIRST $BF SECOND-IN? and
   then
   lead $F4 = if
      source $80 F4-SECOND-LAST SECOND-IN? and
   then ;

: ORACLE-VALID? ( str -- bool ) {: source:str :}
   source STR:LEN 0 <= if
      NO exit
   then
   source STR:DATA c@ {: lead:n :}
   lead ASCII-LIMIT < if
      YES exit
   then
   lead TWO-FIRST < if
      NO exit
   then
   lead THREE-FIRST < if
      source 1 CONTINUATION-AT? exit
   then
   lead FOUR-FIRST < if
      source lead ORACLE-THREE? exit
   then
   lead FOUR-LAST-EXCLUSIVE < if
      source lead ORACLE-FOUR? exit
   then
   NO ;

: REF-BYTE ( str n -- n ) {: source:str index:n :}
   index source STR:LEN >= if
      E-STR-BOUNDS throw
   then
   source STR:DATA index + c@ ;

: REF-TWO ( str -- n ) {: source:str :}
   source 0 REF-BYTE TWO-LEAD-MASK and CONTINUATION-SHIFT lshift
   source 1 REF-BYTE PAYLOAD-MASK and or ;

: REF-THREE ( str -- n ) {: source:str :}
   source 0 REF-BYTE THREE-LEAD-MASK and CONTINUATION-SHIFT lshift
   source 1 REF-BYTE PAYLOAD-MASK and or CONTINUATION-SHIFT lshift
   source 2 REF-BYTE PAYLOAD-MASK and or ;

: REF-FOUR ( str -- n ) {: source:str :}
   source 0 REF-BYTE FOUR-LEAD-MASK and CONTINUATION-SHIFT lshift
   source 1 REF-BYTE PAYLOAD-MASK and or CONTINUATION-SHIFT lshift
   source 2 REF-BYTE PAYLOAD-MASK and or CONTINUATION-SHIFT lshift
   source 3 REF-BYTE PAYLOAD-MASK and or ;

: REFERENCE-SCALAR ( str n -- n )
   {: source:str width:n :}
   width 1 = if
      source 0 REF-BYTE exit
   then
   width 2 = if
      source REF-TWO exit
   then
   width 3 = if
      source REF-THREE exit
   then
   width 4 = if
      source REF-FOUR exit
   then
   E-STR-BOUNDS throw ;

: TEST-SCALAR? ( n -- bool ) {: scalar:n :}
   scalar 0 >=
   scalar SCALAR-LAST <= and
   scalar SURROGATE-FIRST < scalar SURROGATE-LAST > or and ;

: PROPERTY-SCALAR ( n n str -- )
   {: scalar:n next:n source:str :}
   source ORACLE-VALID? TTRUE
   next 1 >= TTRUE
   next source STR:LEN <= TTRUE
   source next REFERENCE-SCALAR scalar T=
   scalar TEST-SCALAR? TTRUE ;

: PROPERTY-RAW ( n n str -- )
   {: byte:n next:n source:str :}
   source ORACLE-VALID? TFALSE
   next 1 T=
   byte source STR:DATA c@ T= ;

: PROPERTY-CASE ( str -- ) {: source:str :}
   source 0 UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         source PROPERTY-SCALAR
      ENDOF
      raw-byte OF
         source PROPERTY-RAW
      ENDOF
   ;MATCH ;

: PROPERTY-WIDTHS ( -- )
   $00 0 0 0 PREP4 1 >str PROPERTY-CASE
   $C2 $80 0 0 PREP4 2 >str PROPERTY-CASE
   $E0 $A0 $80 0 PREP4 3 >str PROPERTY-CASE
   $F0 $90 $80 $80 PREP4 4 >str PROPERTY-CASE ;

: RANDOM-CASE ( -- str )
   256 PROP:RND% 256 PROP:RND%
   256 PROP:RND% 256 PROP:RND% PREP4
   4 PROP:RND% 1+ >str ;

: RANDOM-PROPERTY ( -- )
   8317 2048 PROP:RUN-RESET
   PROP:COUNT@ 0 ?do
      RANDOM-CASE PROPERTY-CASE
      CANARIES
   loop ;

\ The span's length bounds the check: the $FF past it is never read.
: VALID-ACCEPTS ( -- )
   CASE-BUF 1+ 0 >str UTF8:VALID? TTRUE
   $41 $42 $43 $7F PREP4 4 >str UTF8:VALID? TTRUE
   $41 $FF 0 0 PREP4 1 >str UTF8:VALID? TTRUE
   $41 0 $C2 $80 PREP4 4 >str UTF8:VALID? TTRUE
   $C2 $80 $DF $BF PREP4 4 >str UTF8:VALID? TTRUE
   CANARIES ;

\ A bad lead, a bad byte after valid scalars, and a span that cuts a scalar.
: VALID-REFUSES ( -- )
   $FF 0 0 0 PREP4 1 >str UTF8:VALID? TFALSE
   $41 $C2 $80 $FF PREP4 4 >str UTF8:VALID? TFALSE
   $41 0 $FF 0 PREP4 3 >str UTF8:VALID? TFALSE
   $41 $42 $C2 $80 PREP4 3 >str UTF8:VALID? TFALSE
   $E0 $A0 $80 0 PREP4 2 >str UTF8:VALID? TFALSE
   CANARIES ;

\ A cap that falls inside a character backs up to its lead; a stray
\ continuation byte is a character of its own, so a cap before one stays.
: FIT-CUTS ( -- )
   $41 $42 0 0 PREP4 2 >str 4 UTF8:FIT 2 T=
   $41 $C3 $A9 $41 PREP4 4 >str 2 UTF8:FIT 1 T=
   $41 $C3 $A9 $41 PREP4 4 >str 3 UTF8:FIT 3 T=
   $F0 $9F $98 $80 PREP4 4 >str 3 UTF8:FIT 0 T=
   $41 $C3 $A9 $41 PREP4 4 >str 0 UTF8:FIT 0 T=
   S\" A\x80\x80\x80\x80\x80" 5 UTF8:FIT 5 T=
   S\" AAAAAAAAAAAAAAAAAAAAAAAAAAAA\xF0\x9F\x98\x80\x80B" 32 UTF8:FIT 32 T=
   S\" \xE0\x80\x80" 1 UTF8:FIT 1 T=
   \ The byte before the span leads a character that crosses the cap; FIT
   \ never reads it.
   S\" \xE2\x80\x80" STR:DATA 1+ 2 >str 1 UTF8:FIT 1 T=
   [: CASE-BUF 1+ 2 >str -1 UTF8:FIT drop ;] E-STR-BOUNDS TTHROWSQ
   [: CASE-BUF 1+ -1 >str 2 UTF8:FIT drop ;] E-STR-BOUNDS TTHROWSQ
   CANARIES ;

\ A lead and fewer continuations than its width are cut only when some ending
\ decodes: the second-byte ranges after $E0, $ED, $F0 and $F4 decide.
: CUT-CHECKS ( -- )
   S\" \xC3" 0 UTF8:CUT? TTRUE
   S\" \xC3\xA9" 0 UTF8:CUT? TFALSE
   S\" \xC0" 0 UTF8:CUT? TFALSE
   S\" \xE0" 0 UTF8:CUT? TTRUE
   S\" \xE0\x80" 0 UTF8:CUT? TFALSE
   S\" \xE0\xA0" 0 UTF8:CUT? TTRUE
   S\" \xED\xA0" 0 UTF8:CUT? TFALSE
   S\" \xED\x9F" 0 UTF8:CUT? TTRUE
   S\" \xF0\x8F" 0 UTF8:CUT? TFALSE
   S\" \xF4\x8F\xBF" 0 UTF8:CUT? TTRUE
   S\" \xF4\x90" 0 UTF8:CUT? TFALSE
   S\" \xF5" 0 UTF8:CUT? TFALSE
   S\" \x80" 0 UTF8:CUT? TFALSE
   S\" A" 0 UTF8:CUT? TFALSE
   S\" A" 1 UTF8:CUT? TFALSE
   S\" A\xC3" 1 UTF8:CUT? TTRUE
   [: CASE-BUF 1+ -1 >str -1 UTF8:CUT? drop ;] E-STR-BOUNDS TTHROWSQ
   [: CASE-BUF 1+ 1 >str 2 UTF8:CUT? drop ;] E-STR-BOUNDS TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   WIDTH-BOUNDARIES OVERLONG RAW-REMAINDER LONE-CONTINUATIONS
   TRUNCATIONS FINAL-LEAD BAD-CONTINUATIONS SURROGATES ABOVE-SCALAR-LIMIT
   ADJACENT-CALLS NESTED-CALL
   INTERLEAVED-STATES CURSOR-BOUNDS PROPERTY-WIDTHS RANDOM-PROPERTY
   VALID-ACCEPTS VALID-REFUSES FIT-CUTS CUT-CHECKS ;

;package

UTF8-SCALAR-TEST:RUN
T-REPORT
