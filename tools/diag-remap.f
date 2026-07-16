\ diag-remap.f - authenticated, byte-preserving composed diagnostic remapping.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/float.f
require lib/json-read.f
require lib/json-write.f
require tools/source-map.f

package DIAG-REMAP
private

6 constant PATCH-CAP
INCLUDE-MAX-DEPTH 1+ constant CHAIN-CAP
$400 constant KEY-CAP
$10000 constant DECODE-CAP
$09 constant TAB
$0A constant LF
$3A constant COLON

ENUM patch-kind file-field line-field column-field start-field end-field chain-field ;ENUM

PATCH-CAP LAYOUT-BUFFER PATCH-KIND-V patch-kind
create PATCH-OFF-V PATCH-CAP cells allot
create PATCH-END-V PATCH-CAP cells allot
create KEY-BUF KEY-CAP allot
create DIAG-FILE-BUF FS-PATH-CAP allot
create IN-CHAIN-BUF CHAIN-CAP FS-PATH-CAP * allot
create IN-CHAIN-U CHAIN-CAP cells allot

PTR-VARIABLE DECODE-A
PTR-VARIABLE SRC-PATH-A
variable SRC-PATH-U
PTR-VARIABLE DIAG-A
variable DIAG-U
variable DIAG-I
PTR-VARIABLE DIAG-LINE-A
variable DIAG-LINE-U
variable DIAG-HAS-LF

variable DIAG-FILE-U
variable IN-HAS-FILE
variable IN-HAS-LINE
variable IN-HAS-COL
variable IN-HAS-START
variable IN-HAS-END
variable IN-HAS-CHAIN
variable IN-CHAIN-N
variable DIAG-LINE
variable DIAG-COL
variable DIAG-START
variable DIAG-END
variable DIAG-KEY-U
variable JSON-CLOSE-I
variable LEAK-FILE
variable PATCH-N
variable PATCH-CURSOR

variable REM-FILE
variable REM-CHAIN
variable REM-LINE
variable REM-COL
variable REM-START
variable REM-END
variable REM-HAS-END

variable TEXT-PATH-I
variable TEXT-PATH-U
variable TEXT-LINE-I
variable TEXT-COL-I
variable TEXT-SUFFIX-I

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: COMPOSED-LABEL$ ( -- ptr u8 n )
   s" <habu-composed>" ;

: FAIL-SCHEMA ( -- )
   E-DIAG-SCHEMA throw ;

: FAIL-ORIGIN ( -- )
   E-DIAG-ORIGIN throw ;

: SRC-PATH-A-FIELD ( -- ptr ptr u8 )
   SRC-PATH-A 0 ptr-field ;

: SRC-PATH-A@ ( -- ptr u8 )
   SRC-PATH-A-FIELD @ ;

: SRC-PATH-A! ( ptr u8 -- )
   SRC-PATH-A-FIELD ! ;

: SRC-PATH$ ( -- ptr u8 n )
   SRC-PATH-A@ SRC-PATH-U @ ;

: DIAG-A-FIELD ( -- ptr ptr u8 )
   DIAG-A 0 ptr-field ;

: DIAG-A@ ( -- ptr u8 )
   DIAG-A-FIELD @ ;

: DIAG-A! ( ptr u8 -- )
   DIAG-A-FIELD ! ;

: DIAG-LINE-A-FIELD ( -- ptr ptr u8 )
   DIAG-LINE-A 0 ptr-field ;

: DIAG-LINE-A@ ( -- ptr u8 )
   DIAG-LINE-A-FIELD @ ;

: DIAG-LINE-A! ( ptr u8 -- )
   DIAG-LINE-A-FIELD ! ;

: DIAG-LINE$ ( -- ptr u8 n )
   DIAG-LINE-A@ DIAG-LINE-U @ ;

: DECODE-BUF ( -- ptr u8 )
   DECODE-A @ 0= if
      DECODE-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop DECODE-A !
   then
   DECODE-A @ ;

: N$-CHECK ( ptr u8 n -- )
   STR>NUMBER? MATCH option
      none OF FAIL-SCHEMA ENDOF
      some OF dup 0 < if drop FAIL-SCHEMA else drop then ENDOF
   ;MATCH ;

: N$>N ( ptr u8 n -- n )
   2dup N$-CHECK
   STR>NUMBER? MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

: PATCH-KIND-PTR ( n -- ptr patch-kind )
   PATCH-KIND-V ;

: PATCH-KIND! ( patch-kind n -- ) {: kind:patch-kind idx:n :}
   kind idx PATCH-KIND-PTR ! ;

: PATCH-KIND@ ( n -- patch-kind )
   PATCH-KIND-PTR @ ;

: PATCH+ ( patch-kind n n -- ) {: kind:patch-kind off:n end:n :}
   PATCH-N @ PATCH-CAP >= if FAIL-SCHEMA then
   PATCH-N @ 0 > if off PATCH-END-V PATCH-N @ 1- cells + @ < if FAIL-SCHEMA then then
   kind PATCH-N @ PATCH-KIND!
   off PATCH-OFF-V PATCH-N @ cells + !
   end PATCH-END-V PATCH-N @ cells + !
   PATCH-N @ 1+ PATCH-N ! ;

: SPAN-BOUNDS ( -- n n )
   JR-SPAN$ {: a:ptr u:n :}
   a DIAG-LINE-A@ < if FAIL-SCHEMA then
   a u + DIAG-LINE-A@ DIAG-LINE-U @ + > if FAIL-SCHEMA then
   a DIAG-LINE-A@ - dup u + ;

: STRING-BOUNDS ( -- n n )
   SPAN-BOUNDS {: off:n end:n :}
   off 0= end DIAG-LINE-U @ >= or if FAIL-SCHEMA then
   off 1- end 1+ ;

: SCALAR? ( n -- bool ) {: kind:n :}
   kind JT-STR = if TRUE exit then
   kind JT-INT = if TRUE exit then
   kind JT-FLOAT = if TRUE exit then
   kind JT-TRUE = if TRUE exit then
   kind JT-FALSE = if TRUE exit then
   kind JT-NULL = ;

: OPENER? ( n -- bool )
   dup JT-OBJ = swap JT-ARR = or ;

: VALUE-BOUNDS ( n -- n n ) {: kind:n :}
   kind JT-STR = if STRING-BOUNDS exit then
   kind SCALAR? if SPAN-BOUNDS exit then
   kind OPENER? if
      SPAN-BOUNDS drop {: off:n :}
      JR-SKIP-VALUE
      SPAN-BOUNDS nip
      off swap
      exit
   then
   0 0 FAIL-SCHEMA ;

: DIAG-RESET-FIELDS ( -- )
   0 DIAG-FILE-U !
   0 IN-HAS-FILE !
   0 IN-HAS-LINE !
   0 IN-HAS-COL !
   0 IN-HAS-START !
   0 IN-HAS-END !
   0 IN-HAS-CHAIN !
   0 IN-CHAIN-N !
   0 PATCH-N ! ;

: DIAG-KEY! ( -- )
   KEY-BUF KEY-CAP JR-STR DIAG-KEY-U ! ;

: DIAG-KEY=? ( ptr u8 n -- bool )
   KEY-BUF DIAG-KEY-U @ 2swap STR= ;

: JSON-KIND-CHECK ( n n -- )
   <> if FAIL-SCHEMA then ;

: JSON-FILE ( n -- ) {: kind:n :}
   IN-HAS-FILE @ if FAIL-SCHEMA then
   kind JT-STR JSON-KIND-CHECK
   DIAG-FILE-BUF FS-PATH-CAP JR-STR DIAG-FILE-U !
   -1 IN-HAS-FILE !
   kind VALUE-BOUNDS construct patch-kind file-field -rot PATCH+ ;

: JSON-INT ( n -- n ) {: kind:n :}
   kind JT-INT JSON-KIND-CHECK
   JR-INT ;

: JSON-LINE ( n -- ) {: kind:n :}
   IN-HAS-LINE @ if FAIL-SCHEMA then
   kind JSON-INT DIAG-LINE !
   -1 IN-HAS-LINE !
   kind VALUE-BOUNDS construct patch-kind line-field -rot PATCH+ ;

: JSON-COLUMN ( n -- ) {: kind:n :}
   IN-HAS-COL @ if FAIL-SCHEMA then
   kind JSON-INT DIAG-COL !
   -1 IN-HAS-COL !
   kind VALUE-BOUNDS construct patch-kind column-field -rot PATCH+ ;

: JSON-START ( n -- ) {: kind:n :}
   IN-HAS-START @ if FAIL-SCHEMA then
   kind JSON-INT DIAG-START !
   -1 IN-HAS-START !
   kind VALUE-BOUNDS construct patch-kind start-field -rot PATCH+ ;

: JSON-END ( n -- ) {: kind:n :}
   IN-HAS-END @ if FAIL-SCHEMA then
   kind JSON-INT DIAG-END !
   -1 IN-HAS-END !
   kind VALUE-BOUNDS construct patch-kind end-field -rot PATCH+ ;

: IN-CHAIN-SLOT ( n -- ptr u8 )
   FS-PATH-CAP * IN-CHAIN-BUF + ;

: IN-CHAIN-U! ( n n -- )
   cells IN-CHAIN-U + ! ;

: IN-CHAIN-U@ ( n -- n )
   cells IN-CHAIN-U + @ ;

: JSON-CHAIN-ITEM ( n -- ) {: kind:n :}
   kind JT-STR JSON-KIND-CHECK
   IN-CHAIN-N @ CHAIN-CAP >= if FAIL-SCHEMA then
   IN-CHAIN-N @ {: idx:n :}
   idx IN-CHAIN-SLOT FS-PATH-CAP JR-STR {: u:n :}
   u 0= if FAIL-SCHEMA then
   u idx IN-CHAIN-U!
   idx 1+ IN-CHAIN-N ! ;

: JSON-CHAIN-ITEMS ( -- )
   begin
      JR-NEXT dup JT-ARR-END <>
   while
      JSON-CHAIN-ITEM
   repeat drop
   IN-CHAIN-N @ 0= if FAIL-SCHEMA then ;

: JSON-CHAIN ( n -- ) {: kind:n :}
   IN-HAS-CHAIN @ if FAIL-SCHEMA then
   kind JT-ARR JSON-KIND-CHECK
   SPAN-BOUNDS drop {: off:n :}
   JSON-CHAIN-ITEMS
   SPAN-BOUNDS nip {: end:n :}
   -1 IN-HAS-CHAIN !
   construct patch-kind chain-field off end PATCH+ ;

: JSON-COLLECT-VALUE ( n -- ) {: kind:n :}
   s" file" DIAG-KEY=? if kind JSON-FILE exit then
   s" line" DIAG-KEY=? if kind JSON-LINE exit then
   s" column" DIAG-KEY=? if kind JSON-COLUMN exit then
   s" byte_start" DIAG-KEY=? if kind JSON-START exit then
   s" byte_end" DIAG-KEY=? if kind JSON-END exit then
   s" include_chain" DIAG-KEY=? if kind JSON-CHAIN exit then
   JR-SKIP-VALUE ;

: JSON-COLLECT ( -- )
   DIAG-RESET-FIELDS
   DIAG-LINE$ JR-INIT
   JR-NEXT JT-OBJ JSON-KIND-CHECK
   begin
      JR-NEXT dup JT-OBJ-END <>
   while
      dup JT-KEY JSON-KIND-CHECK
      drop
      DIAG-KEY!
      JR-NEXT JSON-COLLECT-VALUE
   repeat drop
   SPAN-BOUNDS drop JSON-CLOSE-I !
   JR-NEXT JT-END JSON-KIND-CHECK ;

: DIAG-COMPOSED? ( -- bool )
   IN-HAS-FILE @ 0= if FALSE exit then
   DIAG-FILE-BUF DIAG-FILE-U @ SRC-PATH$ STR= if TRUE exit then
   DIAG-FILE-BUF DIAG-FILE-U @ COMPOSED-LABEL$ STR= ;

: REMEMBER-START ( -- )
   SOURCE-MAP:ORIGIN-FILE-ID REM-FILE !
   SOURCE-MAP:ORIGIN-CHAIN-ID REM-CHAIN !
   SOURCE-MAP:ORIGIN-LINE REM-LINE !
   SOURCE-MAP:ORIGIN-COLUMN REM-COL !
   SOURCE-MAP:ORIGIN-BYTE REM-START ! ;

: REMAP-START ( n -- )
   SOURCE-MAP:ORIGIN!
   REMEMBER-START ;

: REMAP-LINE-COLUMN ( n n -- )
   SOURCE-MAP:ORIGIN-LINE-COLUMN!
   SOURCE-MAP:ORIGIN-OUTPUT-BYTE DIAG-START !
   REMEMBER-START ;

: INPUT-LOC-CHECK ( -- )
   IN-HAS-LINE @ 0= if
      IN-HAS-COL @ if FAIL-ORIGIN then
      exit
   then
   IN-HAS-COL @ 0= if FAIL-ORIGIN then ;

: START-LOC-CHECK ( -- )
   IN-HAS-LINE @ 0= if exit then
   DIAG-LINE @ DIAG-COL @ SOURCE-MAP:ORIGIN-LINE-COLUMN!
   SOURCE-MAP:ORIGIN-OUTPUT-BYTE DIAG-START @ <> if FAIL-ORIGIN then
   DIAG-START @ REMAP-START ;

: INPUT-CHAIN-ITEM-CHECK ( n -- ) {: idx:n :}
   idx IN-CHAIN-SLOT idx IN-CHAIN-U@
   idx SOURCE-MAP:ORIGIN-CHAIN-FILE$ STR= 0= if FAIL-ORIGIN then ;

: INPUT-CHAIN-CHECK ( -- )
   IN-HAS-CHAIN @ 0= if exit then
   IN-CHAIN-N @ SOURCE-MAP:ORIGIN-CHAIN-N <> if FAIL-ORIGIN then
   0 begin dup IN-CHAIN-N @ < while
      dup INPUT-CHAIN-ITEM-CHECK
      1+
   repeat drop ;

: REMAP-END ( n -- ) {: end:n :}
   end DIAG-START @ < if FAIL-ORIGIN then
   DIAG-START @ end SOURCE-MAP:ORIGIN-SPAN!
   end DIAG-START @ - REM-START @ + dup REM-START @ < if drop FAIL-ORIGIN then
   REM-END ! ;

: REMAP-JSON-ORIGIN ( -- )
   INPUT-LOC-CHECK
   IN-HAS-START @ if
      DIAG-START @ REMAP-START
      START-LOC-CHECK
   else
      IN-HAS-LINE @ 0= if FAIL-ORIGIN then
      DIAG-LINE @ DIAG-COL @ REMAP-LINE-COLUMN
   then
   INPUT-CHAIN-CHECK
   IN-HAS-END @ REM-HAS-END !
   IN-HAS-END @ if DIAG-END @ REMAP-END then ;

: WRITE-CHAIN ( -- )
   JW-ARRAY-START
   0 begin dup SOURCE-MAP:ORIGIN-CHAIN-N < while
      dup 0 > if JW-COMMA then
      dup SOURCE-MAP:ORIGIN-CHAIN-FILE$ JW-STRING
      1+
   repeat drop
   JW-ARRAY-END ;

: WRITE-PATCH ( patch-kind -- )
   MATCH patch-kind
      file-field OF SOURCE-MAP:ORIGIN-FILE$ JW-STRING ENDOF
      line-field OF REM-LINE @ JW-U ENDOF
      column-field OF REM-COL @ JW-U ENDOF
      start-field OF REM-START @ JW-U ENDOF
      end-field OF REM-HAS-END @ 0= if FAIL-ORIGIN then REM-END @ JW-U ENDOF
      chain-field OF WRITE-CHAIN ENDOF
   ;MATCH ;

: WRITE-PATCHES ( -- n )
   0 PATCH-CURSOR !
   0 begin dup PATCH-N @ < while
      dup PATCH-OFF-V swap cells + @ {: off:n :}
      DIAG-LINE-A@ PATCH-CURSOR @ + off PATCH-CURSOR @ - JW-RAW
      dup PATCH-KIND@ WRITE-PATCH
      dup PATCH-END-V swap cells + @ PATCH-CURSOR !
      1+
   repeat drop
   PATCH-CURSOR @ ;

: WRITE-MISSING ( -- )
   IN-HAS-LINE @ 0= if JW-COMMA s" line" JW-KEY REM-LINE @ JW-U then
   IN-HAS-COL @ 0= if JW-COMMA s" column" JW-KEY REM-COL @ JW-U then
   IN-HAS-START @ 0= if JW-COMMA s" byte_start" JW-KEY REM-START @ JW-U then
   IN-HAS-CHAIN @ 0= if JW-COMMA s" include_chain" JW-KEY WRITE-CHAIN then ;

: REWRITE-JSON ( -- )
   WRITE-PATCHES {: cursor:n :}
   cursor JSON-CLOSE-I @ < if
      DIAG-LINE-A@ cursor + JSON-CLOSE-I @ cursor - JW-RAW
   then
   WRITE-MISSING
   DIAG-LINE-A@ JSON-CLOSE-I @ + DIAG-LINE-U @ JSON-CLOSE-I @ - JW-RAW ;

: RAW-LINE ( -- )
   DIAG-LINE$ JW-RAW ;

: COMPOSED-ID? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u SRC-PATH$ CONTAINS? if TRUE exit then
   a u COMPOSED-LABEL$ CONTAINS? ;

: JSON-STRING-CHECK ( -- )
   DECODE-BUF DECODE-CAP JR-STR {: u:n :}
   DECODE-A @ u COMPOSED-ID? if FAIL-ORIGIN then ;

: JSON-LEAK-KEY ( -- )
   DECODE-BUF DECODE-CAP JR-STR {: u:n :}
   DECODE-A @ u COMPOSED-ID? if FAIL-ORIGIN then
   JR-DEPTH @ 1 = DECODE-A @ u s" file" STR= and LEAK-FILE ! ;

: JSON-LEAK-TOKEN ( n -- ) {: kind:n :}
   kind JT-KEY = if JSON-LEAK-KEY exit then
   LEAK-FILE @ if 0 LEAK-FILE ! exit then
   kind JT-STR = if JSON-STRING-CHECK then ;

: JSON-LEAK-CHECK ( -- )
   DIAG-LINE$ JR-INIT
   0 LEAK-FILE !
   begin JR-NEXT dup JT-END <> while
      JSON-LEAK-TOKEN
   repeat drop ;

: RAW-LINE-CHECK ( -- )
   DIAG-LINE$ COMPOSED-ID? if FAIL-ORIGIN then ;

: PROCESS-JSON ( -- )
   JSON-LEAK-CHECK
   JSON-COLLECT
   DIAG-COMPOSED? if REMAP-JSON-ORIGIN REWRITE-JSON else RAW-LINE-CHECK RAW-LINE then ;

: TEXT-MATCH! ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   DIAG-LINE$ path pathu FIND-SUB MATCH option
      none OF FALSE ENDOF
      some OF
         IDX>N TEXT-PATH-I !
         pathu TEXT-PATH-U !
         TRUE
      ENDOF
   ;MATCH ;

: TEXT-PATH-CHECK ( -- )
   SRC-PATH$ TEXT-MATCH! if exit then
   COMPOSED-LABEL$ TEXT-MATCH! if exit then
   -1 TEXT-PATH-I ! ;

: TEXT-DIGIT? ( n -- bool )
   dup $30 >= swap $39 <= and ;

: TEXT-DIGITS-END ( n -- n )
   begin dup DIAG-LINE-U @ < while
      DIAG-LINE-A@ over + c@ TEXT-DIGIT? 0= if exit then
      1+
   repeat ;

: TEXT-LOC-CHECK ( -- )
   TEXT-PATH-I @ 0 < if exit then
   TEXT-PATH-I @ TEXT-PATH-U @ + {: sep:n :}
   sep DIAG-LINE-U @ >= if FAIL-ORIGIN then
   DIAG-LINE-A@ sep + c@ COLON <> if FAIL-ORIGIN then
   sep 1+ TEXT-LINE-I !
   TEXT-LINE-I @ TEXT-DIGITS-END TEXT-COL-I !
   TEXT-COL-I @ TEXT-LINE-I @ = if FAIL-ORIGIN then
   TEXT-COL-I @ DIAG-LINE-U @ >= if FAIL-ORIGIN then
   DIAG-LINE-A@ TEXT-COL-I @ + c@ COLON <> if FAIL-ORIGIN then
   TEXT-COL-I @ 1+ TEXT-COL-I !
   TEXT-COL-I @ TEXT-DIGITS-END TEXT-SUFFIX-I !
   TEXT-SUFFIX-I @ TEXT-COL-I @ = if FAIL-ORIGIN then ;

: TEXT-N ( n n -- n ) {: start:n end:n :}
   DIAG-LINE-A@ start + end start - N$>N ;

: TEXT-ORIGIN ( -- )
   TEXT-LINE-I @ TEXT-COL-I @ 1- TEXT-N
   TEXT-COL-I @ TEXT-SUFFIX-I @ TEXT-N
   REMAP-LINE-COLUMN
   0 REM-HAS-END ! ;

: WRITE-TEXT-CHAIN ( -- )
   s" include chain: " JW-RAW
   0 begin dup SOURCE-MAP:ORIGIN-CHAIN-N < while
      dup 0 > if s"  -> " JW-RAW then
      dup SOURCE-MAP:ORIGIN-CHAIN-FILE$ JW-STRING
      1+
   repeat drop ;

: REWRITE-TEXT ( -- )
   DIAG-LINE-A@ TEXT-PATH-I @ JW-RAW
   SOURCE-MAP:ORIGIN-FILE$ JW-STRING
   COLON JW-C REM-LINE @ JW-U
   COLON JW-C REM-COL @ JW-U
   s"  byte " JW-RAW REM-START @ JW-U
   DIAG-LINE-A@ TEXT-SUFFIX-I @ + DIAG-LINE-U @ TEXT-SUFFIX-I @ - JW-RAW
   LF JW-C
   WRITE-TEXT-CHAIN ;

: PROCESS-TEXT ( -- )
   TEXT-PATH-CHECK
   TEXT-LOC-CHECK
   TEXT-PATH-I @ 0 < if RAW-LINE-CHECK RAW-LINE exit then
   TEXT-ORIGIN
   REWRITE-TEXT ;

: JSON-LINE? ( -- bool )
   0 begin dup DIAG-LINE-U @ < while
      DIAG-LINE-A@ over + c@ dup $20 = swap TAB = or if
         1+
      else
         DIAG-LINE-A@ swap + c@ $7B = exit
      then
   repeat drop FALSE ;

: PROCESS-LINE ( -- )
   JSON-LINE? if PROCESS-JSON else PROCESS-TEXT then ;

: NEXT-DIAG-LINE? ( -- bool )
   DIAG-I @ DIAG-U @ >= if FALSE exit then
   DIAG-I @ {: start:n :}
   begin DIAG-I @ DIAG-U @ < while
      DIAG-A@ DIAG-I @ + c@ LF = if
         DIAG-A@ start + DIAG-LINE-A!
         DIAG-I @ start - DIAG-LINE-U !
         -1 DIAG-HAS-LF !
         DIAG-I @ 1+ DIAG-I !
         TRUE exit
      then
      DIAG-I @ 1+ DIAG-I !
   repeat
   DIAG-A@ start + DIAG-LINE-A!
   DIAG-U @ start - DIAG-LINE-U !
   0 DIAG-HAS-LF !
   TRUE ;

: OUTPUT-CHECK ( -- )
   JW$ COMPOSED-ID? if FAIL-ORIGIN then ;

: REMAP ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: map:ptr mapu:n src:ptr srcu:n label:ptr labelu:n src-dg:ptr src-dgu:n map-dg:ptr map-dgu:n diag:ptr diagu:n :}
   diagu DECODE-CAP > if E-DIAG-CAPACITY throw then
   label SRC-PATH-A!  labelu SRC-PATH-U !
   map mapu src srcu src-dg src-dgu map-dg map-dgu SOURCE-MAP:OPEN
   diag DIAG-A!
   diagu DIAG-U !
   0 DIAG-I !
   JW-RESET
   begin NEXT-DIAG-LINE? while
      PROCESS-LINE
      DIAG-HAS-LF @ if LF JW-C then
   repeat
   OUTPUT-CHECK
   JW$ ;

public

: REMAP$ ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   REMAP ;

;package
