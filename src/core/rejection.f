\ rejection.f - every byte a diagnostic writes: the growable text a record is
\ built in, its JSON escaping and numbers, and the sink that sends a finished
\ record to a file descriptor or to a capture it grows. It also holds where a
\ diagnostic's source sits: the armed source, its map and the loader's file
\ path. Loads before the checker, where no emit exists, so bytes go out through
\ a one-byte buffer.
\ A rejection is printed in parts, called in this order: OPEN, or WARN for a
\ warning, at most one AT, any NOTEs, HELP, then STOP or KEEP. The caller
\ writes each part's text with EMIT-STR, EMIT-U or EMIT1 before the next part
\ word, which ends it. The record is built in TEXT, so a renderer that resets
\ TEXT-LEN cannot run inside one.

\ The status a rejection stops with.
70 constant CHECKER-REJECT-RC

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
\ The capture, mapped on first use and grown as TEXT is, holds HELD-U bytes;
\ CAPTURE empties it.
PERSISTED-PTR-VARIABLE HELD-P   NULL-PTR HELD-P !
variable HELD-CAP   0 HELD-CAP !        \ bytes mapped at HELD-P, 0 for none
variable HELD-U   0 HELD-U !
create NUM-BUF 20 allot  variable NUM-V  variable NUM-N

public
variable DEST   0 DEST !                 \ 0 = stdout, 1 = TEXT (sig recording)
variable TEXT-LEN
\ The main DATA's REJECTION-JSON-CELL from any thread: a task's region is not
\ the main one, and main reads data-base because a cross build's host is not
\ mapped at DATA-VA. No signature: `ptr n` would refuse a stored flag, and the
\ inferred row keeps data-base's kind, which takes a flag and refuses an address.
: JSON
   data-base TASK-TCB-CELL + @ 0= IF data-base REJECTION-JSON-CELL + EXIT THEN
   data-base  DATA-VA data-base NULL-PTR - -  +  REJECTION-JSON-CELL + ;
\ QUIET counts nested quiet-check scopes. It is created before the check
\ hook exists, so no effect is published for it.
variable QUIET
variable TALLY   0 TALLY !               \ rejections counted this load

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

\ Grows the capture, keeping its bytes, until n more fit.
: HELD-ROOM ( n -- ) {: u:n :}
   HELD-CAP @ 0= IF
      TEXT-FIRST ARENA-ALLOC-BYTES HELD-P !  TEXT-FIRST HELD-CAP !
   THEN
   HELD-CAP @ BEGIN dup HELD-U @ u + < WHILE 2 * REPEAT {: nc:n :}
   nc HELD-CAP @ > IF
      HELD-P @ HELD-CAP @ nc ARENA-BYTES-GROW HELD-P !  nc HELD-CAP !
   THEN ;


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

: CAPTURE ( -- )
   0 HELD-U !
   -1 BUF-ON ! ;

\ Records go to the file descriptor again; BUFFER$ still answers the capture.
: BUFFER-OFF ( -- )
   0 BUF-ON ! ;

: FD! ( n -- )
   BUF-FD ! ;

: BUFFER$ ( -- ptr u8 n )
   HELD-P @ HELD-U @ ;
private

: BUF-APPEND ( ptr u8 n -- )
   {: a:ptr u:n :}
   BUF-ON @ 0= IF BUF-FD @ a u write drop EXIT THEN
   u HELD-ROOM
   a HELD-P @ HELD-U @ + u ARENA-COPY
   HELD-U @ u + HELD-U ! ;

public
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

\ Where a top step's token sits: the active file's path, which an inner
\ `evaluate` never changes. FILE-PATH is private so no program writes it.
private
SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL FILE-PATH
public

\ The active file's path; empty when no file is being read.
: PATH$ ( -- ptr u8 n )
   FILE-PATH @ data-base SRCLOC:PATHLEN-CELL + @ ;

\ The file a rejection names when no file holds its source.
: NO-FILE$ ( -- ptr u8 n ) s" <input>" ;

private
create FILE-BUF PATH-CAP 1 + allot   variable FILE-U
public
variable ORIGIN-LINE  variable ORIGIN-COLUMN  variable ORIGIN-BYTE
: FILE$ ( -- ptr u8 n ) FILE-BUF FILE-U @ ;
private
\ A tool's label names one file, the one the load was reading when the tool set
\ it; a label set with no file open names the steps that have no file. LABEL-BUF
\ holds that file's path, empty for none; a top-level step in it names the
\ label, and one in any other file names its own path.
\ The boot's NO-FILE$ is no label.
variable LABELED
create LABEL-BUF PATH-CAP allot   variable LABEL-U
: COPY ( ptr u8 ptr u8 n -- ) {: a:ptr d:ptr u:n :}
   0 BEGIN dup u < WHILE
      dup a + c@
      over d + c!
      1 +
   REPEAT drop ;
public
: LABELS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   LABELED @ 0= IF 0 0= 0= EXIT THEN
   a u LABEL-BUF LABEL-U @ CORE-STR= ;
: ORIGIN! {: line col byte :}
   line ORIGIN-LINE !  col ORIGIN-COLUMN !  byte ORIGIN-BYTE ! ;
\ The file line, column and byte of the byte p points at in a buffer whose first
\ byte sits at file line bl, column bc and byte bb. LF ends a line, a column
\ counts bytes, and only the buffer's first line starts at column bc.
private
variable POS-OFF  variable POS-LINE  variable POS-COLUMN  variable POS-I
public
: POSITION ( ptr u8 ptr u8 n n n -- n n n )
   {: base:ptr p:ptr bl:n bc:n bb:n :}
   p base - POS-OFF !
   1 POS-LINE !  1 POS-COLUMN !  0 POS-I !
   BEGIN POS-I @ POS-OFF @ < WHILE
      base POS-I @ + c@ 10 = IF
         POS-LINE @ 1 + POS-LINE !  1 POS-COLUMN !
      ELSE
         POS-COLUMN @ 1 + POS-COLUMN !
      THEN
      POS-I @ 1 + POS-I !
   REPEAT
   bl POS-LINE @ + 1 -
   POS-LINE @ 1 = IF bc POS-COLUMN @ + 1 - ELSE POS-COLUMN @ THEN
   bb POS-OFF @ + ;

private
PTR-VARIABLE SOURCE-A  variable SOURCE-U
variable SOURCE-LINE  variable SOURCE-COLUMN
public
variable SOURCE-BYTE
private
PTR-VARIABLE MAP-A  variable MAP-U
PTR-VARIABLE ROW-A  variable ROW-N
\ The rows' source offset of the armed source's first byte, subtracted from
\ each row's: the engine's rows count from the input buffer's start.
variable ROW-BIAS
PTR-VARIABLE CLOSER-A  variable CLOSER-U  PTR-VARIABLE CLOSER-BASE
public
: CLOSER-OFF ( -- )
   NULL-PTR CLOSER-A !  0 CLOSER-U !  NULL-PTR CLOSER-BASE ! ;
private
: MAP-OFF ( -- )
   NULL-PTR MAP-A !  0 MAP-U !  NULL-PTR ROW-A !  0 ROW-N !  0 ROW-BIAS !
   CLOSER-OFF ;
public
: SOURCE-OFF ( -- )
   NULL-PTR SOURCE-A !  0 SOURCE-U !
   MAP-OFF ;
: SOURCE! ( ptr u8 n n n n -- )
   {: a:ptr u:n line:n col:n byte:n :}
   a SOURCE-A !  u SOURCE-U !
   line SOURCE-LINE !  col SOURCE-COLUMN !  byte SOURCE-BYTE !
   MAP-OFF ;
: MAP! ( ptr u8 n ptr n n -- )
   {: a:ptr u:n rows:ptr count:n :}
   a MAP-A !  u MAP-U !  rows ROW-A !  count ROW-N !  0 ROW-BIAS !
   CLOSER-OFF ;
\ Set after MAP!, which clears it.
: MAP-BIAS! ( n -- )
   ROW-BIAS ! ;
\ The token a u that closes the text whose first byte is base.
: CLOSER! ( ptr u8 n ptr u8 -- )
   {: a:ptr u:n base:ptr :}
   a CLOSER-A !  u CLOSER-U !  base CLOSER-BASE ! ;
: SOURCE$ ( -- ptr u8 n ) SOURCE-A @ SOURCE-U @ ;
\ Whether a closer was given for the text whose first byte is base.
: CLOSES? ( ptr u8 -- bool ) CLOSER-BASE @ - 0= CLOSER-U @ 0 > and ;
: CLOSER$ ( -- ptr u8 n ) CLOSER-A @ CLOSER-U @ ;
private
: ROW@ ( n -- n )
   ROW-A @ {: i:n rows:ptr :}
   i cells rows + @ ;
\ The first cell of the last of count two-cell rows at or before text offset x.
: ROW-AT ( ptr n n n -- n )
   {: rows:ptr count:n x:n :}
   0 BEGIN
      dup 1 + count < IF dup 1 + 2 * cells rows + @ x <= ELSE 0 0= 0= THEN
   WHILE 1 + REPEAT
   2 * ;
: MAP-ROW ( n -- n )
   {: x:n :}
   ROW-A @ ROW-N @ x ROW-AT ;
\ Whether text offset x lies in a run with no source.
: MAP-NONE? ( n -- bool )
   MAP-ROW 1 + ROW@ BODY-SOURCE:NONE = ;
\ The separator after a run maps to the byte after the run, where its last
\ token ends.
: MAP>SOURCE ( n -- n )
   {: x:n :}
   x MAP-ROW {: row:n :}
   row 1 + ROW@  x +  row ROW@ -  ROW-BIAS @ - ;
: IN-SPAN? ( n n -- bool )  \ an end pointer sits at len
   {: off:n len:n :}
   off 0 >=  off len <= and ;
: SOURCE-POSITION ( n -- n n n )
   {: off:n :}
   SOURCE-A @  SOURCE-A @ off +
   SOURCE-LINE @ SOURCE-COLUMN @ SOURCE-BYTE @  POSITION ;
public
: >SOURCE ( ptr u8 -- n bool )
   {: p:ptr :}
   SOURCE-U @ 0 = IF 0 0 0= 0= EXIT THEN
   ROW-N @ 0 >  p MAP-A @ -  MAP-U @ IN-SPAN?  and IF
      p MAP-A @ - MAP-NONE? IF 0 0 0= 0= EXIT THEN
      p MAP-A @ - MAP>SOURCE 0 0= EXIT
   THEN
   p SOURCE-A @ -  SOURCE-U @ IN-SPAN? IF
      p SOURCE-A @ - 0 0= EXIT
   THEN
   0 0 0= 0= ;
\ 0 0 0 false when there is no map for p.
: LOCATE ( ptr u8 -- n n n bool )
   >SOURCE 0= IF drop 0 0 0 0 0= 0= EXIT THEN
   SOURCE-POSITION 0 0= ;
1 1 0 ORIGIN!

\ The file a top-level step names; empty when no file or label names it.
: LOAD-FILE$ ( -- ptr u8 n )
   PATH$ {: pa:ptr pu:n :}
   pa pu LABELS? IF FILE$ EXIT THEN
   pa pu ;

private
\ Which text of the record is open.
0 constant NO-PART
1 constant MESSAGE-PART
2 constant LABEL-PART
3 constant VALUE-PART
4 constant HELP-PART
variable PART   NO-PART PART !
variable NOTES   0 NOTES !              \ the JSON notes object is open
variable GUTTER   1 GUTTER !            \ digits of AT's line, 1 unplaced
SRCLOC:OWNED-CELL RESERVED-PTR-U8-CELL OWNED
SRCLOC:INB-CELL RESERVED-PTR-U8-CELL INPUT-START
INE-CELL RESERVED-PTR-U8-CELL INPUT-END
BODY-TEXT:BASE-CELL RESERVED-PTR-U8-CELL BODY-BASE
PENDTKA-CELL RESERVED-PTR-U8-CELL PEND-NAME
\ AT's place: whether a buffer holds the token, its file, line, column and
\ byte, the byte after it, and the source line with the bytes before the token.
variable PLACED
PTR-VARIABLE PLACE-FILE-A  variable PLACE-FILE-U
variable AT-LINE  variable AT-COLUMN  variable AT-BYTE  variable AT-END
PTR-VARIABLE LINE-A  variable LINE-U  variable LEAD-U
\ A token in an evaluated text is placed at the token INVOKED-A INVOKED-U where
\ the evaluation was run; the label is followed by the evaluated place, as rustc
\ follows a macro's call with its expansion: the line EVALUATED-A EVALUATED-U of
\ that text that holds the token EVAL-TOKEN-A EVAL-TOKEN-U, EVAL-LEAD-U bytes in.
variable EVALUATED   0 EVALUATED !
PTR-VARIABLE EVALUATED-A  variable EVALUATED-U
PTR-VARIABLE EVAL-TOKEN-A  variable EVAL-TOKEN-U  variable EVAL-LEAD-U
PTR-VARIABLE INVOKED-A  variable INVOKED-U
EVAL-TOP-CELL RESERVED-PTR-U8-CELL EVAL-TOP

: END-TEXT ( -- )
   PART @ NO-PART = IF EXIT THEN
   JSON @ IF JSON-CLOSE ELSE 10 EMIT1 THEN ;
: BLANKS ( n -- ) {: k:n :}
   0 BEGIN dup k < WHILE 32 EMIT1 1 + REPEAT drop ;
: DIGITS ( n -- n ) {: v:n :}
   1 v BEGIN dup 9 > WHILE 10 / swap 1 + swap REPEAT drop ;
: FIELD ( ptr u8 n -- )   44 EMIT1 JSON-KEY ;
: MARGIN ( -- )   GUTTER @ 1 + BLANKS ;

: PLACE-FILE! ( ptr u8 n -- ) {: a:ptr u:n :}
   a PLACE-FILE-A !  u PLACE-FILE-U ! ;
: PLACE-FILE$ ( -- ptr u8 n )   PLACE-FILE-A @ PLACE-FILE-U @ ;
: OWNED$ ( -- ptr u8 n )   OWNED @ data-base SRCLOC:OWNEDLEN-CELL + @ ;
: INPUT$ ( -- ptr u8 n )
   INPUT-START @ {: b:ptr :}
   b  INPUT-END @ b - ;
: HOLDS? ( ptr u8 n ptr u8 n -- bool ) {: b:ptr bu:n t:ptr tu:n :}
   t b - {: at:n :}
   at 0 >=  at tu + bu <= and ;
\ Whether the armed source holds the n bytes at p: its map's text, outside a
\ run with no source, else its own bytes.
: ARMED? ( ptr u8 n -- bool ) {: p:ptr n:n :}
   SOURCE-U @ 0 = IF 0 0= 0= EXIT THEN
   ROW-N @ 0 >  MAP-A @ MAP-U @ p n HOLDS?  and IF p MAP-A @ - MAP-NONE? 0= EXIT THEN
   SOURCE$ p n HOLDS? ;
public
\ Whether one source a rejection places from holds the token p n whole, for
\ good: the armed source, the given closer's bytes or the loader's owned bytes.
\ A zero-length token is held at any offset from 0 to the buffer's length.
: HELD? ( ptr u8 n -- bool ) {: p:ptr n:n :}
   p n ARMED? IF 0 0= EXIT THEN
   CLOSER$ p n HOLDS? IF 0 0= EXIT THEN
   OWNED$ p n HOLDS? ;
private
\ The line of b's bu bytes that holds t: from the byte after the last LF before
\ t up to the next LF, or the buffer's edge.
: LINE! ( ptr u8 n ptr u8 -- ) {: b:ptr bu:n t:ptr :}
   t b - {: at:n :}
   at BEGIN dup 0 > IF dup 1 - b + c@ 10 <> ELSE 0 0= 0= THEN WHILE 1 - REPEAT
   {: s:n :}
   at BEGIN dup bu < IF dup b + c@ 10 <> ELSE 0 0= 0= THEN WHILE 1 + REPEAT
   {: e:n :}
   b s + LINE-A !  e s - LINE-U !  at s - LEAD-U ! ;
\ A body's source is armed from its name on, so a file or the buffer read,
\ which hold the bytes before the name, cut the line first.
: CUT ( ptr u8 -- ) {: t:ptr :}
   OWNED$ t 0 HOLDS? IF OWNED$ t LINE! EXIT THEN
   INPUT$ t 0 HOLDS? IF INPUT$ t LINE! EXIT THEN
   SOURCE$ t LINE! ;
: POSITION! ( n n n -- )
   AT-BYTE !  AT-COLUMN !  AT-LINE !  -1 PLACED ! ;
: SOURCE-PLACE ( n ptr u8 n -- ) {: off:n t:ptr tu:n :}
   off SOURCE-POSITION POSITION!
   t tu + >SOURCE IF SOURCE-BYTE @ + ELSE drop AT-BYTE @ tu + THEN AT-END !
   SOURCE-A @ off + {: s:ptr :}
   s CUT
   OWNED$ s 0 HOLDS? IF LOAD-FILE$ ELSE FILE$ THEN PLACE-FILE! ;
: BUFFER-PLACE ( ptr u8 n ptr u8 n -- ) {: b:ptr bu:n t:ptr tu:n :}
   b t 1 1 0 POSITION POSITION!
   AT-BYTE @ tu + AT-END !
   b bu t LINE! ;
\ The bytes of the buffer read that the pending definition's body text
\ copied the token t tu from, by the rows the copy records (layout.f
\ BODY-SOURCE); else t. Rows count from the buffer's first byte, and they
\ name this buffer only while the definition's name lies in it: a REPL line
\ refills the buffer that held the line before, and OPENER-KEEP moves the
\ name out of it.
: BODY>INPUT ( ptr u8 n -- ptr u8 ) {: t:ptr tu:n :}
   BODY-BASE @ {: base:ptr :}
   data-base PEND-CELL + @ 0= IF t EXIT THEN
   base NULL-PTR = IF t EXIT THEN
   t base - {: x:n :}
   x 0 <  x data-base BODYLEN-CELL + @ >= or IF t EXIT THEN
   data-base BODY-TEXT:ROWS-CELL + @ {: count:n :}
   count 0 <= IF t EXIT THEN
   INPUT$ PEND-NAME @ 1 HOLDS? 0= IF t EXIT THEN
   base data-base BODY-TEXT:CAP-CELL + @ + cell-view {: rows:ptr :}
   rows count x ROW-AT {: r:n :}
   r 1 + cells rows + @ {: src:n :}
   src BODY-SOURCE:NONE = IF t EXIT THEN
   INPUT-START @ src + x + r cells rows + @ - {: p:ptr :}
   INPUT$ p tu HOLDS? IF p EXIT THEN
   t ;
\ Each evaluate frame saved the input it interrupted, its start, end and read
\ position (layout.f EVAL-INB), and links to the frame it runs in (EVAL-PREV).
\ A loaded file and a REPL line run in frames of their own.
: FRAME@ ( ptr u8 n -- ptr u8 ) + 0 ptr-field @ ;
: OUTER ( ptr u8 -- ptr u8 )   EVAL-PREV FRAME@ ;
: SAVED$ ( ptr u8 -- ptr u8 n ) {: f:ptr :}
   f EVAL-INB FRAME@ {: b:ptr :}
   b  f EVAL-INE FRAME@ b - ;
\ Whether frame f is the first evaluation its input ran: f interrupted the
\ active file's bytes, or the frame it runs in is the outermost, a REPL line's.
: INVOKED? ( ptr u8 -- bool ) {: f:ptr :}
   OWNED$ f EVAL-INB FRAME@ 0 HOLDS?
   f OUTER OUTER NULL-PTR =  or ;
\ The frame of the first evaluation, outward from the innermost; NULL-PTR when
\ the input read is no evaluated text.
: INVOKER ( -- ptr u8 )
   EVAL-TOP @
   dup NULL-PTR = IF EXIT THEN
   dup OUTER NULL-PTR = IF drop NULL-PTR EXIT THEN
   BEGIN dup INVOKED? 0= WHILE OUTER REPEAT ;
\ Records the line LINE! found as the evaluated place of the token t tu.
: EVALUATED! ( ptr u8 n -- ) {: t:ptr tu:n :}
   LINE-A @ EVALUATED-A !  LINE-U @ EVALUATED-U !  LEAD-U @ EVAL-LEAD-U !
   t EVAL-TOKEN-A !  tu EVAL-TOKEN-U ! ;
\ Whether an evaluated text inside the invoker inv holds the token t tu: the
\ buffer read, or one an inner frame interrupted. Records the token's evaluated
\ place, and answers the token in that text.
: IN-EVALUATED? ( ptr u8 n ptr u8 -- ptr u8 bool ) {: t:ptr tu:n inv:ptr :}
   t tu BODY>INPUT {: s:ptr :}
   INPUT$ s tu HOLDS? IF
      INPUT$ s LINE!  s tu EVALUATED!
      s 0 0= EXIT
   THEN
   EVAL-TOP @ BEGIN dup inv = 0= WHILE
      dup SAVED$ t tu HOLDS? IF
         SAVED$ t LINE!  t tu EVALUATED!
         t 0 0= EXIT
      THEN
      OUTER
   REPEAT drop
   t 0 0= 0= ;
\ The token in b that ends at or before p: back over blanks, then over the
\ token's bytes, never before b.
: READ-BACK ( ptr u8 ptr u8 -- ptr u8 n ) {: b:ptr p:ptr :}
   p b - BEGIN dup 0 > IF dup 1 - b + c@ 33 < ELSE 0 0= 0= THEN WHILE 1 - REPEAT
   {: e:n :}
   e BEGIN dup 0 > IF dup 1 - b + c@ 32 > ELSE 0 0= 0= THEN WHILE 1 - REPEAT
   {: s:n :}
   b s +  e s - ;
\ Places at the token the invoker's input had read as the evaluation began, in
\ the active file or else the line it interrupted.
: INVOKE-PLACE ( ptr u8 -- ) {: inv:ptr :}
   inv SAVED$ {: b:ptr bu:n :}
   b inv EVAL-INP FRAME@ READ-BACK {: a:ptr u:n :}
   u 0= IF EXIT THEN
   -1 EVALUATED !  a INVOKED-A !  u INVOKED-U !
   OWNED$ a u HOLDS? IF OWNED$ a u BUFFER-PLACE LOAD-FILE$ PLACE-FILE! EXIT THEN
   b bu a u BUFFER-PLACE NO-FILE$ PLACE-FILE! ;
public
\ Whether PLACE finds the byte p points at in a source it knows.
: PLACES? ( ptr u8 -- bool ) {: p:ptr :}
   p >SOURCE nip IF 0 0= EXIT THEN
   p 1 BODY>INPUT {: s:ptr :}
   OWNED$ s 1 HOLDS? IF 0 0= EXIT THEN
   INPUT$ s 1 HOLDS? IF 0 0= EXIT THEN
   INVOKER {: inv:ptr :}
   inv NULL-PTR = IF 0 0= 0= EXIT THEN
   p 1 inv IN-EVALUATED? nip ;
private
\ The armed source holding the token's first byte, else the loader's owned
\ bytes, else an evaluated text, which places it where the evaluation was run,
\ else the buffer read, holding the whole token, as itself or as the bytes a
\ body's text copied it from; else no place.
: PLACE ( ptr u8 n -- ) {: t:ptr tu:n :}
   0 PLACED !  0 EVALUATED !
   t >SOURCE IF t tu SOURCE-PLACE EXIT THEN drop
   t tu BODY>INPUT {: s:ptr :}
   OWNED$ s tu HOLDS? IF OWNED$ s tu BUFFER-PLACE LOAD-FILE$ PLACE-FILE! EXIT THEN
   INVOKER {: inv:ptr :}
   inv NULL-PTR = 0= IF
      t tu inv IN-EVALUATED? nip IF
         inv INVOKE-PLACE  PLACED @ IF EXIT THEN
         0 EVALUATED !
      THEN
   THEN
   INPUT$ s tu HOLDS? IF INPUT$ s tu BUFFER-PLACE NO-FILE$ PLACE-FILE! EXIT THEN
   LOAD-FILE$ {: fa:ptr fu:n :}
   fu 0 > IF fa fu ELSE NO-FILE$ THEN PLACE-FILE! ;

: PLACE-JSON ( ptr u8 n -- ) {: t:ptr tu:n :}
   s" file" FIELD  PLACE-FILE$ JSON-STRING
   PLACED @ IF
      s" line" FIELD  AT-LINE @ EMIT-U
      s" column" FIELD  AT-COLUMN @ EMIT-U
      s" byte_start" FIELD  AT-BYTE @ EMIT-U
      s" byte_end" FIELD  AT-END @ EMIT-U
   THEN
   s" token" FIELD  t tu JSON-STRING
   PLACED @ IF s" source_line" FIELD  LINE-A @ LINE-U @ JSON-STRING THEN
   s" label" FIELD  JSON-OPEN ;
\ The caret line copies the tabs in the line's first n bytes, before the token,
\ so the carets line up.
: LEAD ( ptr u8 n -- ) {: a:ptr k:n :}
   0 BEGIN dup k < WHILE
      dup a + c@ 9 = IF 9 ELSE 32 THEN EMIT1
      1 +
   REPEAT drop ;
: CARETS ( n -- ) {: tu:n :}
   tu 0= IF 1 ELSE tu THEN {: k:n :}
   0 BEGIN dup k < WHILE 94 EMIT1 1 + REPEAT drop ;
: PLACE-PROSE ( ptr u8 n -- ) {: t:ptr tu:n :}
   GUTTER @ BLANKS  s" --> " EMIT-STR  PLACE-FILE$ EMIT-STR
   PLACED @ 0= IF
      10 EMIT1  MARGIN  s" = at `" EMIT-STR  t tu EMIT-STR  s" `: " EMIT-STR
      EXIT
   THEN
   58 EMIT1  AT-LINE @ EMIT-U  58 EMIT1  AT-COLUMN @ EMIT-U  10 EMIT1
   MARGIN 124 EMIT1 10 EMIT1
   AT-LINE @ EMIT-U  s"  | " EMIT-STR  LINE-A @ LINE-U @ EMIT-STR  10 EMIT1
   MARGIN s" | " EMIT-STR  LINE-A @ LEAD-U @ LEAD  tu CARETS  32 EMIT1 ;

\ A note's key; its value follows.
: NOTE-KEY ( ptr u8 n -- ) {: k:ptr ku:n :}
   JSON @ IF
      NOTES @ IF 44 EMIT1 ELSE s" notes" FIELD  123 EMIT1  -1 NOTES ! THEN
      k ku JSON-KEY  JSON-OPEN
   ELSE
      MARGIN  s" = " EMIT-STR  k ku EMIT-STR  s" : " EMIT-STR
   THEN
   VALUE-PART PART ! ;
\ The evaluated place: in JSON an object after the label, in prose its line
\ with carets under the token.
: EVALUATED-JSON ( -- )
   s" evaluated" FIELD  123 EMIT1
   s" source_line" JSON-KEY  EVALUATED-A @ EVALUATED-U @ JSON-STRING
   s" column" FIELD  EVAL-LEAD-U @ 1 + EMIT-U
   s" token" FIELD  EVAL-TOKEN-A @ EVAL-TOKEN-U @ JSON-STRING
   125 EMIT1 ;
: EVALUATED-PROSE ( -- )
   s" = evaluated: " {: k:ptr ku:n :}
   MARGIN  k ku EMIT-STR  EVALUATED-A @ EVALUATED-U @ EMIT-STR  10 EMIT1
   MARGIN  ku BLANKS  EVALUATED-A @ EVAL-LEAD-U @ LEAD  EVAL-TOKEN-U @ CARETS
   10 EMIT1 ;
\ Ends the open text; the label's end writes the evaluated place.
: CLOSE-PART ( -- )
   END-TEXT
   PART @ LABEL-PART = 0= IF EXIT THEN
   EVALUATED @ 0= IF EXIT THEN
   0 EVALUATED !
   JSON @ IF EVALUATED-JSON ELSE EVALUATED-PROSE THEN ;

\ Closes a notes object no HELP closed, then the record; sends the record,
\ or drops it under QUIET.
: END-RECORD ( -- )
   CLOSE-PART
   JSON @ IF  NOTES @ IF 125 EMIT1 THEN  125 EMIT1  THEN  10 EMIT1
   NO-PART PART !
   QUIET @ 0= IF FLUSH ELSE 0 TEXT-LEN !  0 DEST ! THEN ;

variable WARNING   0 WARNING !           \ the open record is a warning
: BEGIN-RECORD ( ptr u8 n ptr u8 n -- ) {: sev:ptr su:n code:ptr cu:n :}
   1 DEST !  0 TEXT-LEN !  0 NOTES !  1 GUTTER !  0 EVALUATED !
   JSON @ IF
      123 EMIT1  s" severity" JSON-KEY  sev su JSON-STRING
      s" code" FIELD  code cu JSON-STRING
      s" message" FIELD  JSON-OPEN
   ELSE
      sev su EMIT-STR  91 EMIT1  code cu EMIT-STR  s" ]: " EMIT-STR
   THEN
   MESSAGE-PART PART ! ;

public
\ Opens a record with its code; the message follows.
: OPEN ( ptr u8 n -- ) {: code:ptr cu:n :}
   0 WARNING !  s" error" code cu BEGIN-RECORD ;
\ Opens a warning with its code, which KEEP does not count; the message follows.
: WARN ( ptr u8 n -- ) {: code:ptr cu:n :}
   -1 WARNING !  s" warning" code cu BEGIN-RECORD ;
\ Places the token t tu; its label follows.
: AT ( ptr u8 n -- ) {: t:ptr tu:n :}
   END-TEXT
   t tu PLACE
   PLACED @ IF AT-LINE @ DIGITS ELSE 1 THEN GUTTER !
   EVALUATED @ IF INVOKED-A @ INVOKED-U @ ELSE t tu THEN {: p:ptr pu:n :}
   JSON @ IF p pu PLACE-JSON ELSE p pu PLACE-PROSE THEN
   LABEL-PART PART ! ;
\ A note's key; its value follows.
: NOTE ( ptr u8 n -- )
   CLOSE-PART NOTE-KEY ;
\ The help text follows.
: HELP ( -- )
   CLOSE-PART
   JSON @ IF
      NOTES @ IF 125 EMIT1  0 NOTES ! THEN
      s" help" FIELD  JSON-OPEN
   ELSE
      MARGIN  s" = help: " EMIT-STR
   THEN
   HELP-PART PART ! ;
\ Ends the record and throws status n.
: STOP ( n -- )
   END-RECORD throw ;
\ Ends the record and returns; a sent rejection counts in TALLY.
: KEEP ( -- )
   END-RECORD
   QUIET @ 0=  WARNING @ 0=  and IF TALLY @ 1 + TALLY ! THEN ;

\ The definer kw ku has no name of one token: the record stands at t tu, which
\ why labels. It is written through its help, and the caller stops it.
: MISSING-NAME ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: kw:ptr ku:n t:ptr tu:n why:ptr whyu:n :}
   s" E-MISSING-NAME" OPEN
   96 EMIT1  kw ku EMIT-STR  s" ` needs a name" EMIT-STR
   t tu AT  why whyu EMIT-STR
   HELP
   s" give `" EMIT-STR  kw ku EMIT-STR
   s" ` one name, a token with no blanks" EMIT-STR ;

private
\ FILE! takes any path a tool names, so a long one is rejected here, after the
\ words that print a rejection.
: PATH-COPY ( ptr u8 n ptr u8 -- n ) {: a:ptr u:n d:ptr :}
   u PATH-CAP > IF
      s" E-PATH-RANGE" OPEN
      s" `REJECTION:FILE!` got a path " EMIT-STR  u EMIT-U
      s"  bytes long, more than the " EMIT-STR  PATH-CAP EMIT-U
      s"  bytes a path may have" EMIT-STR
      HELP  s" name the file by a path of at most " EMIT-STR
      PATH-CAP EMIT-U  s"  bytes" EMIT-STR
      CHECKER-REJECT-RC STOP
   THEN
   a d u COPY  u ;
: FILE-COPY ( ptr u8 n -- )
   FILE-BUF PATH-COPY FILE-U ! ;
NO-FILE$ FILE-COPY
public
: FILE! ( ptr u8 n -- )
   FILE-COPY
   PATH$ LABEL-BUF PATH-COPY LABEL-U !
   -1 LABELED ! ;
;package
