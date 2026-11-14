\ rejection.f - every byte a diagnostic writes: the growable text a record is
\ built in, its JSON escaping and numbers, and the sink that sends a finished
\ record to a file descriptor or to a caller's buffer. Loads before the checker,
\ where no emit exists, so bytes go out through a one-byte buffer.
package REJECTION
private

create BYTE-BUF 1 allot
\ TEXT, mapped on first use, holds the text being recorded, TEXT-LEN bytes of it;
\ FLUSH zeroes TEXT-LEN.
16384 constant TEXT-FIRST                \ bytes of the first mapping
PERSISTED-PTR-VARIABLE TEXT-P   NULL-PTR TEXT-P !
variable TEXT-CAP   0 TEXT-CAP !        \ bytes mapped at TEXT-P, 0 for none
variable IN-JSON   0 IN-JSON !          \ inside a JSON string: EMIT1 escapes
variable BUF-ON
variable BUF-FD   2 BUF-FD !
PTR-VARIABLE BUF-A
variable BUF-CAP
variable BUF-U
variable BUF-I
create NUM-BUF 20 allot  variable NUM-V  variable NUM-N

public
variable DEST   0 DEST !                 \ 0 = stdout, 1 = TEXT (sig recording)
variable TEXT-LEN

\ E-DIAG-CAPACITY rejects a record that does not fit the caller's buffer.
\ src/lib/error.f owns it but compiles after this file, so the same (code, name)
\ pair is re-registered here, the one form tools/error-code-lint.f admits.
-2901 constant E-DIAG-CAPACITY

private

: TEXT-MAP ( -- )
   TEXT-FIRST ARENA-ALLOC-BYTES TEXT-P !  TEXT-FIRST TEXT-CAP ! ;


public
: TEXT ( -- ptr u8 )
   TEXT-CAP @ 0= IF TEXT-MAP THEN
   TEXT-P @ ;
private


\ Doubles TEXT, keeping its bytes; a span taken from it before is stale.
: TEXT-GROW ( -- )
   TEXT-CAP @ 0= IF TEXT-MAP EXIT THEN
   TEXT-CAP @ {: oc:n :}
   TEXT-P @ oc oc 2 * ARENA-BYTES-GROW TEXT-P !
   oc 2 * TEXT-CAP ! ;


public
: EMIT-RAW ( n -- )
   {: c:n :}
   DEST @ IF
     TEXT-LEN @ TEXT-CAP @ 2 - > IF TEXT-GROW THEN
     c TEXT TEXT-LEN @ + c!  TEXT-LEN @ 1 + TEXT-LEN !
   ELSE c BYTE-BUF c! BYTE-BUF 1 type THEN ;
private


: HEX-DIGIT ( n -- ) {: d:n :}
   d 10 < IF d 48 + ELSE d 55 + THEN EMIT-RAW ;

\ Escapes per RFC 8259 section 7: \n \r \t \" \\ by letter, every other control
\ byte as \u00XX in uppercase hex.
: JSON-CHAR {: c :}
   c case
      10 of 92 EMIT-RAW 110 EMIT-RAW endof
      13 of 92 EMIT-RAW 114 EMIT-RAW endof
      9 of 92 EMIT-RAW 116 EMIT-RAW endof
      34 of 92 EMIT-RAW c EMIT-RAW endof
      92 of 92 EMIT-RAW c EMIT-RAW endof
      c 32 < IF
         92 EMIT-RAW 117 EMIT-RAW 48 EMIT-RAW 48 EMIT-RAW
         c 4 rshift HEX-DIGIT  c 15 and HEX-DIGIT
      ELSE c EMIT-RAW THEN
   endcase ;

public
\ Between JSON-OPEN and JSON-CLOSE every byte is escaped: a package may be named `\`.
: EMIT1 {: c :}
   IN-JSON @ IF c JSON-CHAR ELSE c EMIT-RAW THEN ;
: JSON-OPEN ( -- )   34 EMIT-RAW  -1 IN-JSON ! ;
: JSON-CLOSE ( -- )  0 IN-JSON !  34 EMIT-RAW ;

: BUFFER! ( ptr u8 n -- )
   {: a:ptr cap:n :}
   a BUF-A !
   cap BUF-CAP !
   0 BUF-U !
   -1 BUF-ON ! ;

: BUFFER-OFF ( -- )
   0 BUF-ON !
   0 BUF-U ! ;

: FD! ( n -- )
   BUF-FD ! ;

\ BUF-A is a pointer cell, so a plain fetch keeps the ptr u8 view the byte
\ store in BUF-COPY needs.
: BUFFER$ ( -- ptr u8 n )
   BUF-A @ BUF-U @ ;
private

: BUF-COPY ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 BUF-I !
   BEGIN BUF-I @ u < WHILE
      a BUF-I @ + c@
      BUF-A @ BUF-U @ + BUF-I @ + c!
      BUF-I @ 1 + BUF-I !
   REPEAT ;

\ The caller's buffer cannot grow: a record that does not fit is rejected whole by
\ a throw, and the buffer keeps every record before it.
: BUF-APPEND ( ptr u8 n -- )
   {: a:ptr u:n :}
   BUF-ON @ 0= IF BUF-FD @ a u write drop EXIT THEN
   BUF-U @ u + BUF-CAP @ > IF E-DIAG-CAPACITY throw THEN
   a u BUF-COPY
   BUF-U @ u + BUF-U ! ;

public
\ Rendering returns to stdout before the append, so a rejected record leaves the
\ renderer as a delivered one does.
: FLUSH ( -- )
   TEXT TEXT-LEN @
   0 DEST !  0 TEXT-LEN !
   BUF-APPEND ;

: EMIT-STR ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

: EMIT-U
   NUM-V !  0 NUM-N !
   NUM-V @ 0= IF 48 EMIT1 EXIT THEN
   BEGIN NUM-V @ 0 > WHILE
      NUM-V @ 10 mod 48 +  NUM-BUF NUM-N @ + c!
      NUM-N @ 1 + NUM-N !
      NUM-V @ 10 / NUM-V !
   REPEAT
   NUM-N @ BEGIN dup 0 > WHILE
      1 - dup NUM-BUF + c@ EMIT1
   REPEAT drop ;

\ A signed number in decimal, as EMIT-U prints one from 0 up.
: EMIT-N ( n -- ) {: v:n :}
   v 0 >= IF v EMIT-U EXIT THEN
   45 EMIT1
   v 1 63 lshift = IF s" 9223372036854775808" EMIT-STR EXIT THEN
   v negate EMIT-U ;
: JSON-STRING ( ptr u8 n -- )
   JSON-OPEN EMIT-STR JSON-CLOSE ;
: JSON-KEY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u JSON-STRING  58 EMIT1 ;
;package
