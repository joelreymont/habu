\ utf8-scalar-test.f - direct and property tests for UTF8:NEXT.

require lib/test.f
require lib/property.f
require lib/utf8-scalar.f

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

: PREP4 ( n n n n -- ptr u8 )
   CANARY CASE-BUF c!
   CANARY CASE-BUF 5 + c!
   CASE-BUF 4 + c!  CASE-BUF 3 + c!
   CASE-BUF 2 + c!  CASE-BUF 1+ c!
   CASE-BUF 1+ ;

: CANARIES ( -- )
   CASE-BUF c@ CANARY T=
   CASE-BUF 5 + c@ CANARY T= ;

: ASSERT-SCALAR ( ptr u8 n n n n -- )
   {: source:ptr length:n cursor:n want-scalar:n want-next:n :}
   source length cursor UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         want-next T= want-scalar T=
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

: ASSERT-RAW ( ptr u8 n n n n -- )
   {: source:ptr length:n cursor:n want-byte:n want-next:n :}
   source length cursor UTF8:NEXT
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
   $00 0 0 0 PREP4 1 0 $00 1 ASSERT-SCALAR
   $7F 0 0 0 PREP4 1 0 $7F 1 ASSERT-SCALAR ;

: WIDTH-TWO ( -- )
   $C2 $80 0 0 PREP4 2 0 $80 2 ASSERT-SCALAR
   $DF $BF 0 0 PREP4 2 0 $7FF 2 ASSERT-SCALAR ;

: WIDTH-THREE ( -- )
   $E0 $A0 $80 0 PREP4 3 0 $800 3 ASSERT-SCALAR
   $EF $BF $BF 0 PREP4 3 0 $FFFF 3 ASSERT-SCALAR ;

: WIDTH-FOUR ( -- )
   $F0 $90 $80 $80 PREP4 4 0 $10000 4 ASSERT-SCALAR
   $F4 $8F $BF $BF PREP4 4 0 $10FFFF 4 ASSERT-SCALAR ;

: WIDTH-BOUNDARIES ( -- )
   WIDTH-ONE WIDTH-TWO WIDTH-THREE WIDTH-FOUR CANARIES ;

: OVERLONG ( -- )
   $C0 $80 0 0 PREP4 2 0 $C0 1 ASSERT-RAW
   $C1 $BF 0 0 PREP4 2 0 $C1 1 ASSERT-RAW
   $E0 $80 $80 0 PREP4 3 0 $E0 1 ASSERT-RAW
   $F0 $80 $80 $80 PREP4 4 0 $F0 1 ASSERT-RAW
   CANARIES ;

: RAW-REMAINDER ( -- )
   $C0 $80 0 0 PREP4 2 0 $C0 1 ASSERT-RAW
   $C0 $80 0 0 PREP4 2 1 $80 2 ASSERT-RAW
   CANARIES ;

: LONE-CONTINUATIONS ( -- )
   $80 0 0 0 PREP4 1 0 $80 1 ASSERT-RAW
   $BF 0 0 0 PREP4 1 0 $BF 1 ASSERT-RAW
   CANARIES ;

: TRUNCATED-TWO ( -- )
   $C2 $80 0 0 PREP4 1 0 $C2 1 ASSERT-RAW
   CANARIES ;

: TRUNCATED-THREE ( -- )
   $E0 $A0 $80 0 PREP4 1 0 $E0 1 ASSERT-RAW
   $E0 $A0 $80 0 PREP4 2 0 $E0 1 ASSERT-RAW
   CANARIES ;

: TRUNCATED-FOUR ( -- )
   $F0 $90 $80 $80 PREP4 1 0 $F0 1 ASSERT-RAW
   $F0 $90 $80 $80 PREP4 2 0 $F0 1 ASSERT-RAW
   $F0 $90 $80 $80 PREP4 3 0 $F0 1 ASSERT-RAW
   CANARIES ;

: TRUNCATIONS ( -- )
   TRUNCATED-TWO TRUNCATED-THREE TRUNCATED-FOUR ;

: FINAL-LEAD ( -- )
   $41 $E0 $A0 $80 PREP4 2 1 $E0 2 ASSERT-RAW
   CANARIES ;

: BAD-CONTINUATION-TWO ( -- )
   $C2 $41 0 0 PREP4 2 0 $C2 1 ASSERT-RAW ;

: BAD-CONTINUATION-THREE ( -- )
   $E0 $41 $80 0 PREP4 3 0 $E0 1 ASSERT-RAW
   $E0 $A0 $41 0 PREP4 3 0 $E0 1 ASSERT-RAW ;

: BAD-CONTINUATION-FOUR ( -- )
   $F0 $41 $80 $80 PREP4 4 0 $F0 1 ASSERT-RAW
   $F0 $90 $41 $80 PREP4 4 0 $F0 1 ASSERT-RAW
   $F0 $90 $80 $41 PREP4 4 0 $F0 1 ASSERT-RAW ;

: BAD-CONTINUATIONS ( -- )
   BAD-CONTINUATION-TWO
   BAD-CONTINUATION-THREE
   BAD-CONTINUATION-FOUR
   CANARIES ;

: SURROGATES ( -- )
   $ED $A0 $80 0 PREP4 3 0 $ED 1 ASSERT-RAW
   $ED $BF $BF 0 PREP4 3 0 $ED 1 ASSERT-RAW
   CANARIES ;

: ABOVE-SCALAR-LIMIT ( -- )
   $F4 $90 $80 $80 PREP4 4 0 $F4 1 ASSERT-RAW
   $F7 $BF $BF $BF PREP4 4 0 $F7 1 ASSERT-RAW
   $F8 $80 $80 $80 PREP4 4 0 $F8 1 ASSERT-RAW
   $FF 0 0 0 PREP4 1 0 $FF 1 ASSERT-RAW
   CANARIES ;

create ADJACENT-BUF $C2 c, $80 c, $80 c,

: ASSERT-EXCESS ( n -- )
   ADJACENT-BUF 3 rot UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         2drop FAIL
      ENDOF
      raw-byte OF
         3 T= $80 T=
      ENDOF
   ;MATCH ;

: ADJACENT-CALLS ( -- )
   ADJACENT-BUF 3 0 UTF8:NEXT
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
   NESTED-INNER 4 0 $1F600 4 ASSERT-SCALAR ;

: NESTED-CALL ( -- )
   NESTED-OUTER 2 0 UTF8:NEXT
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
   STATE-A-BUF 3 STATE-A-CURSOR @ UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         STATE-A-CURSOR ! $A2 T=
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

: STATE-A-SECOND ( -- )
   STATE-A-BUF 3 STATE-A-CURSOR @ UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         STATE-A-CURSOR ! $41 T=
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

: STATE-B-FIRST ( -- )
   STATE-B-BUF 5 STATE-B-CURSOR @ UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         STATE-B-CURSOR ! $1F600 T=
      ENDOF
      raw-byte OF
         2drop FAIL
      ENDOF
   ;MATCH ;

: STATE-B-SECOND ( -- )
   STATE-B-BUF 5 STATE-B-CURSOR @ UTF8:NEXT
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
   CASE-BUF 1+ -1 0 UTF8:NEXT DISCARD-STEP ;

: BAD-NEGATIVE-CURSOR ( -- )
   CASE-BUF 1+ 1 -1 UTF8:NEXT DISCARD-STEP ;

: BAD-END-CURSOR ( -- )
   CASE-BUF 1+ 1 1 UTF8:NEXT DISCARD-STEP ;

: BAD-PAST-CURSOR ( -- )
   CASE-BUF 1+ 1 2 UTF8:NEXT DISCARD-STEP ;

: CURSOR-BOUNDS ( -- )
   $41 0 0 0 PREP4 drop
   [: BAD-NEGATIVE-LENGTH ;] E-STR-BOUNDS TTHROWSQ
   [: BAD-NEGATIVE-CURSOR ;] E-STR-BOUNDS TTHROWSQ
   [: BAD-END-CURSOR ;] E-STR-BOUNDS TTHROWSQ
   [: BAD-PAST-CURSOR ;] E-STR-BOUNDS TTHROWSQ
   CANARIES ;

: CONTINUATION? ( n -- bool )
   CONTINUATION-MASK and CONTINUATION-TAG = ;

: CONTINUATION-AT? ( ptr u8 n n -- bool )
   {: source:ptr length:n index:n :}
   index length >= if
      NO exit
   then
   source index + c@ CONTINUATION? ;

: SECOND-IN? ( ptr u8 n n n -- bool )
   {: source:ptr length:n first:n last:n :}
   length 2 < if
      NO exit
   then
   source 1+ c@ first >=
   source 1+ c@ last <= and ;

: ORACLE-THREE? ( ptr u8 n n -- bool )
   {: source:ptr length:n lead:n :}
   source length 1 CONTINUATION-AT?
   source length 2 CONTINUATION-AT? and
   lead THREE-FIRST = if
      source length E0-SECOND-FIRST $BF SECOND-IN? and
   then
   lead $ED = if
      source length $80 ED-SECOND-LAST SECOND-IN? and
   then ;

: ORACLE-FOUR? ( ptr u8 n n -- bool )
   {: source:ptr length:n lead:n :}
   source length 1 CONTINUATION-AT?
   source length 2 CONTINUATION-AT? and
   source length 3 CONTINUATION-AT? and
   lead FOUR-FIRST = if
      source length F0-SECOND-FIRST $BF SECOND-IN? and
   then
   lead $F4 = if
      source length $80 F4-SECOND-LAST SECOND-IN? and
   then ;

: ORACLE-VALID? ( ptr u8 n -- bool ) {: source:ptr length:n :}
   length 0 <= if
      NO exit
   then
   source c@ {: lead:n :}
   lead ASCII-LIMIT < if
      YES exit
   then
   lead TWO-FIRST < if
      NO exit
   then
   lead THREE-FIRST < if
      source length 1 CONTINUATION-AT? exit
   then
   lead FOUR-FIRST < if
      source length lead ORACLE-THREE? exit
   then
   lead FOUR-LAST-EXCLUSIVE < if
      source length lead ORACLE-FOUR? exit
   then
   NO ;

: REF-BYTE ( ptr u8 n n -- n ) {: source:ptr length:n index:n :}
   index length >= if
      E-STR-BOUNDS throw
   then
   source index + c@ ;

: REF-TWO ( ptr u8 n -- n ) {: source:ptr length:n :}
   source length 0 REF-BYTE TWO-LEAD-MASK and CONTINUATION-SHIFT lshift
   source length 1 REF-BYTE PAYLOAD-MASK and or ;

: REF-THREE ( ptr u8 n -- n ) {: source:ptr length:n :}
   source length 0 REF-BYTE THREE-LEAD-MASK and CONTINUATION-SHIFT lshift
   source length 1 REF-BYTE PAYLOAD-MASK and or CONTINUATION-SHIFT lshift
   source length 2 REF-BYTE PAYLOAD-MASK and or ;

: REF-FOUR ( ptr u8 n -- n ) {: source:ptr length:n :}
   source length 0 REF-BYTE FOUR-LEAD-MASK and CONTINUATION-SHIFT lshift
   source length 1 REF-BYTE PAYLOAD-MASK and or CONTINUATION-SHIFT lshift
   source length 2 REF-BYTE PAYLOAD-MASK and or CONTINUATION-SHIFT lshift
   source length 3 REF-BYTE PAYLOAD-MASK and or ;

: REFERENCE-SCALAR ( ptr u8 n n -- n )
   {: source:ptr length:n width:n :}
   width 1 = if
      source length 0 REF-BYTE exit
   then
   width 2 = if
      source length REF-TWO exit
   then
   width 3 = if
      source length REF-THREE exit
   then
   width 4 = if
      source length REF-FOUR exit
   then
   E-STR-BOUNDS throw ;

: TEST-SCALAR? ( n -- bool ) {: scalar:n :}
   scalar 0 >=
   scalar SCALAR-LAST <= and
   scalar SURROGATE-FIRST < scalar SURROGATE-LAST > or and ;

: PROPERTY-SCALAR ( n n ptr u8 n -- )
   {: scalar:n next:n source:ptr length:n :}
   source length ORACLE-VALID? TTRUE
   next 1 >= TTRUE
   next length <= TTRUE
   source length next REFERENCE-SCALAR scalar T=
   scalar TEST-SCALAR? TTRUE ;

: PROPERTY-RAW ( n n ptr u8 n -- )
   {: byte:n next:n source:ptr length:n :}
   source length ORACLE-VALID? TFALSE
   next 1 T=
   byte source c@ T= ;

: PROPERTY-CASE ( ptr u8 n -- ) {: source:ptr length:n :}
   source length 0 UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF
         source length PROPERTY-SCALAR
      ENDOF
      raw-byte OF
         source length PROPERTY-RAW
      ENDOF
   ;MATCH ;

: PROPERTY-WIDTHS ( -- )
   $00 0 0 0 PREP4 1 PROPERTY-CASE
   $C2 $80 0 0 PREP4 2 PROPERTY-CASE
   $E0 $A0 $80 0 PREP4 3 PROPERTY-CASE
   $F0 $90 $80 $80 PREP4 4 PROPERTY-CASE ;

: RANDOM-CASE ( -- ptr u8 n )
   256 PROP:RND% 256 PROP:RND%
   256 PROP:RND% 256 PROP:RND% PREP4
   4 PROP:RND% 1+ ;

: RANDOM-PROPERTY ( -- )
   8317 2048 PROP:RUN-RESET
   PROP:COUNT@ 0 ?do
      RANDOM-CASE PROPERTY-CASE
      CANARIES
   loop ;

public

: RUN ( -- )
   T-RESET
   WIDTH-BOUNDARIES OVERLONG RAW-REMAINDER LONE-CONTINUATIONS
   TRUNCATIONS FINAL-LEAD BAD-CONTINUATIONS SURROGATES ABOVE-SCALAR-LIMIT
   ADJACENT-CALLS NESTED-CALL
   INTERLEAVED-STATES CURSOR-BOUNDS PROPERTY-WIDTHS RANDOM-PROPERTY ;

;package

UTF8-SCALAR-TEST:RUN
T-REPORT
