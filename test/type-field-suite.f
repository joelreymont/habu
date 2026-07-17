\ type-field-suite.f - shared field arena behavior and type-safety.
\ Run by the native engine gate on both the candidate and bin/hb.

require lib/test/assert.f
require test/checker-assert.f
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package TYPE-FIELD-TEST
public

PRODUCT field-parse-owner 0 FIELD legacy n ;PRODUCT
PRODUCT field-owner 0 FIELD legacy n ;PRODUCT
PRODUCT field-bad-owner 0 FIELD legacy n ;PRODUCT
PRODUCT field-reuse-owner 0 FIELD legacy n ;PRODUCT
PRODUCT field-grow-owner 0 FIELD legacy n ;PRODUCT
PRODUCT field-nest-owner 0 FIELD legacy n ;PRODUCT

SUMTYPE field-enum 0
   VARIANT payload n ;VARIANT
;SUMTYPE

ENUM field-color red blue ;ENUM

private

PRODUCT field-private-owner 0 FIELD legacy n ;PRODUCT

variable BASE
create NAME-BUF 64 allot

\ White-box calls compile direct references to friend-only engine words. Their
\ declared effects let the focused suite test the private builder without
\ publishing any mutation word through TYPE-FIELD.
TRUSTED: B-OPEN ( TYPE-FIELD:family-id -- TYPE-FIELD:field-tx ) FIELD-OPEN ;
TRUSTED: B-START
   ( TYPE-FIELD:field-tx ptr u8 n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   FIELD-START ;
TRUSTED: B-START-VARIANT
   ( TYPE-FIELD:field-tx TYPE-FIELD:variant-id ptr u8 n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   FIELD-START-VARIANT ;
TRUSTED: B-PARSE-START
   ( TYPE-FIELD:field-tx -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   FIELD-PARSE-START ;
TRUSTED: B-SCHEMA
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft TYPE-FIELD:schema-id TYPE-FIELD:alignment TYPE-FIELD:field-flags -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   FIELD-SCHEMA ;
TRUSTED: B-LAYOUT
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft TYPE-FIELD:slot TYPE-FIELD:cell-count TYPE-FIELD:byte-off TYPE-FIELD:byte-size -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   FIELD-LAYOUT ;
TRUSTED: B-SOURCE
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft TYPE-FIELD:source-id TYPE-FIELD:source-off TYPE-FIELD:source-len -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   FIELD-SOURCE ;
TRUSTED: B-ADD
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft -- TYPE-FIELD:field-tx )
   FIELD-ADD ;
TRUSTED: B-COMMIT ( TYPE-FIELD:field-tx -- ) FIELD-COMMIT ;
TRUSTED: B-ROLLBACK ( TYPE-FIELD:field-tx -- ) FIELD-ROLLBACK ;

: PARSE-OWNER ( -- TYPE-FIELD:family-id )
   s" field-parse-owner" TYPE-FIELD:FAMILY ;
: OWNER ( -- TYPE-FIELD:family-id )
   s" field-owner" TYPE-FIELD:FAMILY ;
: BAD-OWNER ( -- TYPE-FIELD:family-id )
   s" field-bad-owner" TYPE-FIELD:FAMILY ;
: REUSE-OWNER ( -- TYPE-FIELD:family-id )
   s" field-reuse-owner" TYPE-FIELD:FAMILY ;
: GROW-OWNER ( -- TYPE-FIELD:family-id )
   s" field-grow-owner" TYPE-FIELD:FAMILY ;
: NEST-OWNER ( -- TYPE-FIELD:family-id )
   s" field-nest-owner" TYPE-FIELD:FAMILY ;
: SUM-OWNER ( -- TYPE-FIELD:family-id )
   s" field-enum" TYPE-FIELD:FAMILY ;
: PRIVATE-OWNER ( -- TYPE-FIELD:family-id )
   s" field-private-owner" TYPE-FIELD:FAMILY ;
: COLOR-OWNER ( -- TYPE-FIELD:family-id )
   s" field-color" TYPE-FIELD:FAMILY ;
: PAYLOAD ( -- TYPE-FIELD:variant-id )
   SUM-OWNER s" payload" TYPE-FIELD:VARIANT-ID ;
: RED ( -- TYPE-FIELD:variant-id )
   COLOR-OWNER s" red" TYPE-FIELD:VARIANT-ID ;
: X-ID ( -- TYPE-FIELD:field-id )
   OWNER s" x" TYPE-FIELD:FIND ;
: HIDDEN ( -- TYPE-FIELD:field-id )
   OWNER s" hidden" TYPE-FIELD:FIND ;

: SCHEMA-PUBLIC
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   0 TYPE-FIELD:SCHEMA-ID
   CELL TYPE-FIELD:ALIGNMENT
   TYPE-FIELD:PUBLIC-FLAGS
   B-SCHEMA ;

: SCHEMA-PRIVATE
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   0 TYPE-FIELD:SCHEMA-ID
   CELL TYPE-FIELD:ALIGNMENT
   TYPE-FIELD:PRIVATE-FLAGS
   B-SCHEMA ;

: LAYOUT-RAW
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft n n n n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   {: slot:n cells:n off:n size:n :}
   slot TYPE-FIELD:SLOT-ID
   cells TYPE-FIELD:CELL-COUNT
   off TYPE-FIELD:BYTE-OFF
   size TYPE-FIELD:BYTE-SIZE
   B-LAYOUT ;

: LAYOUT-CELL
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   {: slot:n :}
   slot 1 slot CELL * CELL LAYOUT-RAW ;

: SOURCE-SPAN
   ( TYPE-FIELD:field-tx TYPE-FIELD:field-draft n -- TYPE-FIELD:field-tx TYPE-FIELD:field-draft )
   {: off:n :}
   7 TYPE-FIELD:SOURCE-ID
   off TYPE-FIELD:SOURCE-OFF
   3 TYPE-FIELD:SOURCE-LEN
   B-SOURCE ;

: PUBLIC-ROW
   ( TYPE-FIELD:field-tx ptr u8 n n -- TYPE-FIELD:field-tx )
   {: a:ptr u:n slot:n :}
   a u B-START
   SCHEMA-PUBLIC
   slot LAYOUT-CELL
   slot SOURCE-SPAN
   B-ADD ;

: PRIVATE-ROW
   ( TYPE-FIELD:field-tx ptr u8 n n -- TYPE-FIELD:field-tx )
   {: a:ptr u:n slot:n :}
   a u B-START
   SCHEMA-PRIVATE
   slot LAYOUT-CELL
   slot SOURCE-SPAN
   B-ADD ;

: PUBLIC-RAW
   ( TYPE-FIELD:field-tx ptr u8 n n n n n -- TYPE-FIELD:field-tx )
   {: a:ptr u:n slot:n cells:n off:n size:n :}
   a u B-START
   SCHEMA-PUBLIC
   slot cells off size LAYOUT-RAW
   off SOURCE-SPAN
   B-ADD ;

: PARSE-BUILD ( -- )
   PARSE-OWNER B-OPEN
   B-PARSE-START
   SCHEMA-PUBLIC
   0 LAYOUT-CELL
   0 SOURCE-SPAN
   B-ADD
   B-COMMIT ;

: BUILD-OWNER ( -- )
   OWNER B-OPEN
   s" x" 0 PUBLIC-ROW
   s" hidden" 1 PRIVATE-ROW
   B-COMMIT ;

: BUILD-VARIANT ( -- )
   SUM-OWNER B-OPEN
   PAYLOAD s" value" B-START-VARIANT
   SCHEMA-PUBLIC
   0 LAYOUT-CELL
   20 SOURCE-SPAN
   B-ADD
   B-COMMIT ;

: BAD-ENUM-FIELD ( -- )
   COLOR-OWNER B-OPEN
   RED s" code" B-START-VARIANT
   SCHEMA-PUBLIC
   0 LAYOUT-CELL
   30 SOURCE-SPAN
   B-ADD
   B-ROLLBACK ;

: BUILD-PRIVATE-FAMILY ( -- )
   PRIVATE-OWNER B-OPEN
   s" owned" 0 PUBLIC-ROW
   B-COMMIT ;

: BAD-DUP-STAGE ( -- )
   BAD-OWNER B-OPEN
   s" dup-stage" B-START
   SCHEMA-PUBLIC
   SCHEMA-PUBLIC
   0 LAYOUT-CELL
   0 SOURCE-SPAN
   B-ADD
   B-ROLLBACK ;

: BAD-ALIGN ( -- )
   BAD-OWNER B-OPEN
   s" bad-align" B-START
   0 TYPE-FIELD:SCHEMA-ID
   CELL 2 * TYPE-FIELD:ALIGNMENT
   TYPE-FIELD:PUBLIC-FLAGS
   B-SCHEMA
   0 LAYOUT-CELL
   0 SOURCE-SPAN
   B-ADD
   B-ROLLBACK ;

: BAD-FIRST-SLOT ( -- )
   BAD-OWNER B-OPEN
   s" first-slot" 1 1 0 CELL PUBLIC-RAW
   B-ROLLBACK ;

: BAD-FIRST-BYTE ( -- )
   BAD-OWNER B-OPEN
   s" first-byte" 0 1 CELL CELL PUBLIC-RAW
   B-ROLLBACK ;

: BAD-SLOT-GAP ( -- )
   BAD-OWNER B-OPEN
   s" first" 0 PUBLIC-ROW
   s" slot-gap" 2 1 CELL CELL PUBLIC-RAW
   B-ROLLBACK ;

: BAD-BYTE-GAP ( -- )
   BAD-OWNER B-OPEN
   s" first" 0 PUBLIC-ROW
   s" byte-gap" 1 1 CELL 2 * CELL PUBLIC-RAW
   B-ROLLBACK ;

: BAD-SIZE ( -- )
   BAD-OWNER B-OPEN
   s" bad-size" 0 1 0 CELL 2 * PUBLIC-RAW
   B-ROLLBACK ;

: BAD-ZERO-WIDTH ( -- )
   BAD-OWNER B-OPEN
   s" zero-width" 0 0 0 CELL PUBLIC-RAW
   B-ROLLBACK ;

: BAD-MULTI-WIDTH ( -- )
   BAD-OWNER B-OPEN
   s" multi-width" 0 2 0 CELL 2 * PUBLIC-RAW
   B-ROLLBACK ;

: BAD-ZERO-BYTES ( -- )
   BAD-OWNER B-OPEN
   s" zero-bytes" 0 1 0 0 PUBLIC-RAW
   B-ROLLBACK ;

: BAD-DUP ( -- )
   OWNER B-OPEN
   s" x" 2 PUBLIC-ROW
   B-ROLLBACK ;

: BAD-RESERVED ( -- )
   BAD-OWNER B-OPEN
   s" make" 0 PUBLIC-ROW
   B-ROLLBACK ;

: BAD-LIFO ( -- )
   NEST-OWNER B-OPEN
   NEST-OWNER B-OPEN
   swap B-COMMIT
   B-ROLLBACK ;

: ROLLBACK-REUSE ( -- )
   REUSE-OWNER B-OPEN
   s" reuse" 0 PUBLIC-ROW
   B-ROLLBACK ;

: COMMIT-REUSE ( -- )
   REUSE-OWNER B-OPEN
   s" reuse" 0 PUBLIC-ROW
   B-COMMIT ;

: GROW ( -- )
   GROW-OWNER B-OPEN
   s" alpha-long" 0 PUBLIC-ROW
   s" bravo-long" 1 PUBLIC-ROW
   s" charlie-long" 2 PUBLIC-ROW
   s" delta-long" 3 PUBLIC-ROW
   s" echo-long" 4 PUBLIC-ROW
   s" foxtrot-long" 5 PUBLIC-ROW
   B-COMMIT ;

: RANGE-FAIL ( -- )
   PARSE-OWNER B-OPEN
   s" late" 1 PUBLIC-ROW
   B-ROLLBACK ;

: NAME-SMALL ( -- )
   X-ID NAME-BUF 0 TYPE-FIELD:BYTE-SIZE TYPE-FIELD:NAME drop ;

: FIND-MISSING ( -- )
   OWNER s" missing" TYPE-FIELD:FIND drop ;

: FIND-ROLLED-BACK ( -- )
   REUSE-OWNER s" reuse" TYPE-FIELD:FIND drop ;

: EACH+ ( n TYPE-FIELD:field-id -- n ) drop 1+ ;

: ASSERT-X ( -- )
   X-ID
   dup TYPE-FIELD:FAMILY@ OWNER TYPE-FIELD:FAMILY= TTRUE
   dup TYPE-FIELD:VARIANT? TFALSE
   dup TYPE-FIELD:SCHEMA@ 0 TYPE-FIELD:SCHEMA-ID TYPE-FIELD:SCHEMA= TTRUE
   dup TYPE-FIELD:SLOT@ 0 TYPE-FIELD:SLOT-ID TYPE-FIELD:SLOT= TTRUE
   dup TYPE-FIELD:CELLS@ 1 TYPE-FIELD:CELL-COUNT TYPE-FIELD:CELL-COUNT= TTRUE
   dup TYPE-FIELD:BYTE-OFF@ 0 TYPE-FIELD:BYTE-OFF TYPE-FIELD:BYTE-OFF= TTRUE
   dup TYPE-FIELD:BYTE-SIZE@ CELL TYPE-FIELD:BYTE-SIZE TYPE-FIELD:BYTE-SIZE= TTRUE
   dup TYPE-FIELD:ALIGN@ CELL TYPE-FIELD:ALIGNMENT TYPE-FIELD:ALIGNMENT= TTRUE
   dup TYPE-FIELD:FLAGS@ TYPE-FIELD:PUBLIC-BYTE-FLAGS TYPE-FIELD:FLAGS= TTRUE
   dup TYPE-FIELD:VIS@ TYPE-FIELD:PUBLIC-VIS TYPE-FIELD:VISIBILITY= TTRUE
   dup NAME-BUF 64 TYPE-FIELD:BYTE-SIZE TYPE-FIELD:NAME
      1 TYPE-FIELD:BYTE-SIZE TYPE-FIELD:BYTE-SIZE= TTRUE
   NAME-BUF 1 s" x" T$=
   TYPE-FIELD:SOURCE@
   3 TYPE-FIELD:SOURCE-LEN TYPE-FIELD:SOURCE-LEN= TTRUE
   0 TYPE-FIELD:SOURCE-OFF TYPE-FIELD:SOURCE-OFF= TTRUE
   7 TYPE-FIELD:SOURCE-ID TYPE-FIELD:SOURCE-ID= TTRUE ;

: ASSERT-VARIANT ( -- )
   SUM-OWNER PAYLOAD s" value" TYPE-FIELD:FIND-VARIANT
   dup TYPE-FIELD:FAMILY@ SUM-OWNER TYPE-FIELD:FAMILY= TTRUE
   dup TYPE-FIELD:VARIANT? TTRUE
   dup TYPE-FIELD:VARIANT@ PAYLOAD TYPE-FIELD:VARIANT= TTRUE
   TYPE-FIELD:SLOT@ 0 TYPE-FIELD:SLOT-ID TYPE-FIELD:SLOT= TTRUE ;

: ASSERT-PARSE ( -- )
   PARSE-OWNER s" parser-token" TYPE-FIELD:FIND
   NAME-BUF 64 TYPE-FIELD:BYTE-SIZE TYPE-FIELD:NAME
      12 TYPE-FIELD:BYTE-SIZE TYPE-FIELD:BYTE-SIZE= TTRUE
   NAME-BUF 12 s" parser-token" T$= ;

: ASSERT-ROLES ( -- )
   s" BAD-FAMILY-VARIANT ( TYPE-FIELD:family-id TYPE-FIELD:variant-id -- TYPE-FIELD:family-id TYPE-FIELD:variant-id ) swap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-SCHEMA-SLOT ( TYPE-FIELD:schema-id TYPE-FIELD:slot -- TYPE-FIELD:schema-id TYPE-FIELD:slot ) swap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-SLOT ( TYPE-FIELD:slot TYPE-FIELD:cell-count -- TYPE-FIELD:slot TYPE-FIELD:cell-count ) swap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-BYTE ( TYPE-FIELD:byte-off TYPE-FIELD:byte-size -- TYPE-FIELD:byte-off TYPE-FIELD:byte-size ) swap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-SOURCE-ID ( TYPE-FIELD:source-id TYPE-FIELD:source-off -- TYPE-FIELD:source-id TYPE-FIELD:source-off ) swap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-SOURCE ( TYPE-FIELD:source-off TYPE-FIELD:source-len -- TYPE-FIELD:source-off TYPE-FIELD:source-len ) swap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ID ( n -- TYPE-FIELD:field-id )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NO-FRIEND ( TYPE-FIELD:family-id -- TYPE-FIELD:field-tx ) FIELD-OPEN"
      CHECK-QUIET-CANDIDATE! 1 T=
   s" MISSING-OPEN ( TYPE-FIELD:family-id -- ) TYPE-FIELD:OPEN"
      CHECK-QUIET-CANDIDATE! 1 T= ;

2048 constant IO-CAP
10000 constant TIMEOUT-MS
70 constant REJECT-RC

variable IN-U
variable OUT-U
variable ERR-U
variable EXITED
variable RC

create IN-BUF IO-CAP allot
create OUT-BUF IO-CAP allot
create ERR-BUF IO-CAP allot

: IN$ ( -- ptr u8 n ) IN-BUF IN-U @ ;
: ERR$ ( -- ptr u8 n ) ERR-BUF ERR-U @ ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: INPUT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1 + IO-CAP > if E-FS-CAPACITY throw then
   a IN-BUF u BYTE-COPY
   10 IN-BUF u + c!
   u 1 + IN-U ! ;

: RESULT! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !
   LEN>N OUT-U ! ;

: RUN ( ptr u8 n -- )
   INPUT!
   PROC-ARGV-RESET
   HB$ >LEN IN$ >LEN OUT-BUF IO-CAP >LEN ERR-BUF IO-CAP >LEN
   TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME
   RESULT! ;

: ASSERT-REJECT ( -- )
   EXITED @ TTRUE
   RC @ REJECT-RC T= ;

: EXPECT-INTERNAL ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n word:ptr wordu:n :}
   src srcu RUN
   ASSERT-REJECT
   ERR$ s" hb: internal engine word: " CONTAINS? TTRUE
   ERR$ word wordu CONTAINS? TTRUE ;

: EXPECT-UNDEFINED ( ptr u8 n -- ) {: word:ptr wordu:n :}
   word wordu RUN
   ASSERT-REJECT
   ERR$ s" E-UNDEFINED: " CONTAINS? TTRUE
   ERR$ word wordu CONTAINS? TTRUE ;

: ASSERT-MUTATION-SEALED ( -- )
   s" 1 0 FIELD-OPEN" s" FIELD-OPEN" EXPECT-INTERNAL
   s" ' FIELD-OPEN" s" FIELD-OPEN" EXPECT-INTERNAL
   s" 1 0 0 0 FIELD-START" s" FIELD-START" EXPECT-INTERNAL
   s" 1 0 0 0 0 0 FIELD-SCHEMA" s" FIELD-SCHEMA" EXPECT-INTERNAL
   s" 1 0 0 0 0 0 0 FIELD-LAYOUT" s" FIELD-LAYOUT" EXPECT-INTERNAL
   s" 1 0 0 0 0 0 FIELD-SOURCE" s" FIELD-SOURCE" EXPECT-INTERNAL
   s" 1 0 0 FIELD-ADD" s" FIELD-ADD" EXPECT-INTERNAL
   s" 1 0 FIELD-COMMIT" s" FIELD-COMMIT" EXPECT-INTERNAL
   s" TYPE-FIELD:OPEN" EXPECT-UNDEFINED
   s" TYPE-FIELD:START" EXPECT-UNDEFINED
   s" TYPE-FIELD:SCHEMA" EXPECT-UNDEFINED
   s" TYPE-FIELD:LAYOUT" EXPECT-UNDEFINED
   s" TYPE-FIELD:SOURCE" EXPECT-UNDEFINED
   s" TYPE-FIELD:ADD" EXPECT-UNDEFINED
   s" TYPE-FIELD:COMMIT" EXPECT-UNDEFINED ;

: PREPARE ( -- )
   BUILD-OWNER
   ASSERT-X
   HIDDEN drop
   [: NAME-SMALL ;] TYPE-FIELD:E-CAPACITY TTHROWSQ
   [: FIND-MISSING ;] TYPE-FIELD:E-ID TTHROWSQ

   [: BAD-DUP-STAGE ;] TYPE-FIELD:E-DRAFT TTHROWSQ
   [: BAD-ALIGN ;] TYPE-FIELD:E-LAYOUT TTHROWSQ
   [: BAD-FIRST-SLOT ;] TYPE-FIELD:E-LAYOUT TTHROWSQ
   [: BAD-FIRST-BYTE ;] TYPE-FIELD:E-LAYOUT TTHROWSQ
   [: BAD-SLOT-GAP ;] TYPE-FIELD:E-LAYOUT TTHROWSQ
   [: BAD-BYTE-GAP ;] TYPE-FIELD:E-LAYOUT TTHROWSQ
   [: BAD-SIZE ;] TYPE-FIELD:E-LAYOUT TTHROWSQ
   [: BAD-ZERO-WIDTH ;] TYPE-FIELD:E-LAYOUT TTHROWSQ
   [: BAD-MULTI-WIDTH ;] TYPE-FIELD:E-LAYOUT TTHROWSQ
   [: BAD-ZERO-BYTES ;] TYPE-FIELD:E-LAYOUT TTHROWSQ
   [: BAD-DUP ;] TYPE-FIELD:E-DUP TTHROWSQ
   [: BAD-RESERVED ;] TYPE-FIELD:E-NAME TTHROWSQ
   [: BAD-LIFO ;] TYPE-FIELD:E-TX TTHROWSQ

   ROLLBACK-REUSE
   [: FIND-ROLLED-BACK ;] TYPE-FIELD:E-ID TTHROWSQ
   COMMIT-REUSE
   REUSE-OWNER s" reuse" TYPE-FIELD:FIND drop

   BUILD-VARIANT
   ASSERT-VARIANT
   [: BAD-ENUM-FIELD ;] TYPE-FIELD:E-FAMILY TTHROWSQ
   BUILD-PRIVATE-FAMILY
   GROW
   [: RANGE-FAIL ;] TYPE-FIELD:E-RANGE TTHROWSQ

   0 GROW-OWNER [: EACH+ ;] TYPE-FIELD:EACH 6 T=
   COLOR-OWNER s" red" TYPE-FIELD:VARIANT-ID drop ;

public

: HIDDEN-ID ( -- TYPE-FIELD:field-id ) HIDDEN ;

private

: FINAL-ASSERT ( -- )
   TYPE-FIELD:COUNT TYPE-FIELD:FIELD-COUNT>N BASE @ 12 + T=
   ASSERT-PARSE
   ASSERT-X
   ASSERT-VARIANT
   GROW-OWNER s" foxtrot-long" TYPE-FIELD:FIND
   TYPE-FIELD:SLOT@ 5 TYPE-FIELD:SLOT-ID TYPE-FIELD:SLOT= TTRUE
   PRIVATE-OWNER s" owned" TYPE-FIELD:FIND drop ;

T-RESET
TYPE-FIELD:COUNT TYPE-FIELD:FIELD-COUNT>N BASE !
PARSE-BUILD parser-token
PREPARE
ASSERT-ROLES
ASSERT-MUTATION-SEALED

;package

package TYPE-FIELD-OUTSIDER
private

: OWNER ( -- TYPE-FIELD:family-id )
   s" field-owner" TYPE-FIELD:FAMILY ;

: HIDDEN ( -- )
   OWNER s" hidden" TYPE-FIELD:FIND drop ;

: HIDDEN-REFLECT ( -- )
   TYPE-FIELD-TEST:HIDDEN-ID TYPE-FIELD:FAMILY@ drop ;

: PRIVATE-FAMILY ( -- )
   s" field-private-owner" TYPE-FIELD:FAMILY drop ;

: EACH+ ( n TYPE-FIELD:field-id -- n ) drop 1+ ;

: ASSERT-EACH ( -- )
   0 OWNER [: EACH+ ;] TYPE-FIELD:EACH 1 T= ;

OWNER s" x" TYPE-FIELD:FIND drop
' HIDDEN TYPE-FIELD:E-VISIBILITY TTHROWS
' HIDDEN-REFLECT TYPE-FIELD:E-VISIBILITY TTHROWS
' PRIVATE-FAMILY TYPE-FIELD:E-FAMILY TTHROWS
ASSERT-EACH

;package

package TYPE-FIELD-TEST
private

FINAL-ASSERT
T-REPORT

;package
