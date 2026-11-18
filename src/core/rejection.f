split-str
\ rejection.f - every byte a diagnostic writes: the growable text a record is
\ built in, its JSON escaping and numbers, and the sink that sends a finished
\ record to a file descriptor or to a capture it grows. It also holds where a
\ diagnostic's source sits: the armed source, its map and the loader's file
\ path. Loads before the checker, where no emit exists, so bytes go out through
\ a one-byte buffer. Every cell, buffer and first room it writes is the running
\ task's own: its block, or its region's carry stamp.
\ A rejection is printed in parts, called in this order: OPEN, or WARN for a
\ warning, at most one AT, any NOTEs, HELP, then STOP or KEEP. The caller
\ writes each part's text with EMIT-STR, EMIT-U or EMIT1 before the next part
\ word, which ends it. The record is built in TEXT, so a renderer that resets
\ TEXT-LEN cannot run inside one.

package REJECTION
private

\ Rounds n bytes up to whole cells.
: ROUND ( n -- n ) 1 cells 1 - +  1 cells negate and ;
\ TEXT holds the record being built, TEXT-LEN bytes of it. Each growable room
\ here starts in TEXT-FIRST bytes of the block, so a record is written after
\ the system refuses memory; a larger room is mapped.
16384 constant TEXT-FIRST                \ bytes of each first room
\ The block holds every cell, buffer and first room of a record; the cells
\ come first, each zero at its start, so FREE zeroes them as one run.
0
dup constant TEXT-P-OFF  1 cells +
dup constant TEXT-CAP-OFF  1 cells +
dup constant TEXT-LEN-OFF  1 cells +
dup constant DEST-OFF  1 cells +
dup constant IN-JSON-OFF  1 cells +
dup constant PART-OFF  1 cells +
dup constant BUF-ON-OFF  1 cells +
dup constant BUF-FD-OFF  1 cells +
dup constant HELD-P-OFF  1 cells +
dup constant HELD-CAP-OFF  1 cells +
dup constant HELD-U-OFF  1 cells +
dup constant SAVED-P-OFF  1 cells +
dup constant SAVED-CAP-OFF  1 cells +
dup constant SAVED-U-OFF  1 cells +
dup constant NUM-V-OFF  1 cells +
dup constant NUM-N-OFF  1 cells +
dup constant QUIET-OFF  1 cells +
dup constant TALLY-OFF  1 cells +
dup constant FILE-U-OFF  1 cells +
dup constant LABELED-OFF  1 cells +
dup constant NAMED-OFF  1 cells +
dup constant LABEL-U-OFF  1 cells +
dup constant LABEL-NAME-U-OFF  1 cells +
dup constant SOURCE-A-OFF  1 cells +
dup constant SOURCE-U-OFF  1 cells +
dup constant SOURCE-LINE-OFF  1 cells +
dup constant SOURCE-COLUMN-OFF  1 cells +
dup constant SOURCE-BYTE-OFF  1 cells +
dup constant MAP-A-OFF  1 cells +
dup constant MAP-U-OFF  1 cells +
dup constant ROW-A-OFF  1 cells +
dup constant ROW-N-OFF  1 cells +
dup constant ROW-BIAS-OFF  1 cells +
dup constant CLOSER-A-OFF  1 cells +
dup constant CLOSER-U-OFF  1 cells +
dup constant CLOSER-BASE-OFF  1 cells +
dup constant NOTES-OFF  1 cells +
dup constant GUTTER-OFF  1 cells +
dup constant PLACED-OFF  1 cells +
dup constant PLACE-FILE-A-OFF  1 cells +
dup constant PLACE-FILE-U-OFF  1 cells +
dup constant AT-LINE-OFF  1 cells +
dup constant AT-COLUMN-OFF  1 cells +
dup constant AT-BYTE-OFF  1 cells +
dup constant AT-END-OFF  1 cells +
dup constant LINE-A-OFF  1 cells +
dup constant LINE-U-OFF  1 cells +
dup constant LEAD-U-OFF  1 cells +
dup constant LAST-U-OFF  1 cells +
dup constant LAST-AT-OFF  1 cells +
dup constant EVALUATED-OFF  1 cells +
dup constant EVALUATED-A-OFF  1 cells +
dup constant EVALUATED-U-OFF  1 cells +
dup constant EVAL-TOKEN-A-OFF  1 cells +
dup constant EVAL-TOKEN-U-OFF  1 cells +
dup constant EVAL-LEAD-U-OFF  1 cells +
dup constant READER-A-OFF  1 cells +
dup constant READER-U-OFF  1 cells +
dup constant WORD-A-OFF  1 cells +
dup constant WORD-U-OFF  1 cells +
dup constant ROOT-A-OFF  1 cells +
dup constant ROOT-U-OFF  1 cells +
dup constant WARNING-OFF  1 cells +
dup constant LABELS-P-OFF  1 cells +
dup constant LABELS-CAP-OFF  1 cells +
dup constant LABELS-U-OFF  1 cells +
dup constant CELLS-BYTES
dup constant BYTE-BUF-OFF  1 ROUND +
dup constant NUM-BUF-OFF  20 ROUND +
dup constant FILE-BUF-OFF  PATH-CAP 1 + ROUND +
dup constant LABEL-BUF-OFF  PATH-CAP ROUND +
dup constant LABEL-NAME-BUF-OFF  PATH-CAP ROUND +
dup constant LAST-BUF-OFF  PATH-CAP ROUND +
dup constant TEXT-BASE-OFF  TEXT-FIRST ROUND +
dup constant HELD-BASE-OFF  TEXT-FIRST ROUND +
dup constant SAVED-BASE-OFF  TEXT-FIRST ROUND +
dup constant LABELS-BASE-OFF  TEXT-FIRST ROUND +
public
constant BLOCK-BYTES
private
create MAIN-BLOCK BLOCK-BYTES SCRATCH-ALLOT
\ The running task's block, at the start of its user area.
: BLOCK ( -- ^u8 )
   USER-BAND:AREA-OFF POINTER-STORAGE:DATA-CELL @
   dup NULL-PTR = IF drop MAIN-BLOCK THEN ;
: CELL-AT ( n -- ^n ) BLOCK + cell-view ;
: PTR-AT ( n -- ^^u8 ) BLOCK + 0 ptr-field ;

: BYTE-BUF ( -- ^u8 ) BLOCK BYTE-BUF-OFF + ;
: TEXT-BASE ( -- ^u8 ) BLOCK TEXT-BASE-OFF + ;
: HELD-BASE ( -- ^u8 ) BLOCK HELD-BASE-OFF + ;
: SAVED-BASE ( -- ^u8 ) BLOCK SAVED-BASE-OFF + ;
: TEXT-P ( -- ^^u8 ) TEXT-P-OFF PTR-AT ;
: TEXT-CAP ( -- ^n ) TEXT-CAP-OFF CELL-AT ;               \ bytes at TEXT-P, 0 for none
: IN-JSON ( -- ^n ) IN-JSON-OFF CELL-AT ;          \ inside a JSON string: EMIT1 escapes
\ Which text of the record is open. A prose AT opens GAP-PART: the label's
\ first byte writes the space after the carets and opens LABEL-PART. An AT no
\ source holds opens DROP-PART, which drops the label: no place carries it.
\ Its end writes nothing and leaves LABEL-PART, so the notes after it print.
0 constant NO-PART
1 constant MESSAGE-PART
2 constant LABEL-PART
3 constant VALUE-PART
4 constant HELP-PART
5 constant GAP-PART
6 constant DROP-PART
: PART ( -- ^n ) PART-OFF CELL-AT ;
: BUF-ON ( -- ^n ) BUF-ON-OFF CELL-AT ;
: BUF-FD ( -- ^n ) BUF-FD-OFF CELL-AT ;   \ the descriptor records go to, xor 2
\ The capture, grown as TEXT is, holds HELD-U bytes; CAPTURE empties it.
: HELD-P ( -- ^^u8 ) HELD-P-OFF PTR-AT ;
: HELD-CAP ( -- ^n ) HELD-CAP-OFF CELL-AT ;        \ bytes at HELD-P, 0 for none
: HELD-U ( -- ^n ) HELD-U-OFF CELL-AT ;
\ The record SAVE kept, SAVED-U bytes, in a buffer grown as the capture is.
: SAVED-P ( -- ^^u8 ) SAVED-P-OFF PTR-AT ;
: SAVED-CAP ( -- ^n ) SAVED-CAP-OFF CELL-AT ;
: SAVED-U ( -- ^n ) SAVED-U-OFF CELL-AT ;
: NUM-BUF ( -- ^u8 ) BLOCK NUM-BUF-OFF + ;
: NUM-V ( -- ^n ) NUM-V-OFF CELL-AT ;
: NUM-N ( -- ^n ) NUM-N-OFF CELL-AT ;

public
: DEST ( -- ^n ) DEST-OFF CELL-AT ;   \ 0 = stdout, 1 = TEXT (sig recording)
: TEXT-LEN ( -- ^n ) TEXT-LEN-OFF CELL-AT ;
\ The main DATA's REJECTION-JSON-CELL from any thread: a task's region is not
\ the main one, and main reads data-base because a cross build's host is not
\ mapped at DATA-VA. No signature: `^n` would reject a stored flag, and the
\ inferred row keeps data-base's kind, which takes a flag and rejects an address.
: JSON
   data-base TASK-TCB-CELL + @ 0= IF data-base REJECTION-JSON-CELL + EXIT THEN
   data-base  DATA-VA data-base NULL-PTR - -  +  REJECTION-JSON-CELL + ;
\ QUIET counts nested quiet-check scopes.
: QUIET ( -- ^n ) QUIET-OFF CELL-AT ;
: TALLY ( -- ^n ) TALLY-OFF CELL-AT ;   \ rejections counted this load

private
: TEXT-MAP ( -- )
   TEXT-BASE TEXT-P !  TEXT-FIRST TEXT-CAP ! ;


public
: TEXT ( -- ^u8 )
   TEXT-CAP @ 0= IF TEXT-MAP THEN
   TEXT-P @ ;
private


\ Moves the room of c bytes at p, whose first room is f, to nc fresh bytes that
\ hold its c bytes. The first room is the block's: it is zeroed, not unmapped.
: ROOM-GROW ( ^u8 ^u8 n n -- ^u8 ) {: p:^ f:^ c:n nc:n :}
   \ The record writer cannot report a refusal of its own room.
   nc map-anon 0 <> IF
      drop s" rejection: a record room's mmap failed" EXIT-STATUS:SYSTEM die
   THEN {: q:^ :}
   p q c ARENA-COPY
   p f = IF p c ARENA-ZERO ELSE p c ARENA-UNMAP THEN
   q ;

\ Doubles TEXT, keeping its bytes; a span taken from it before is stale.
: TEXT-GROW ( -- )
   TEXT-CAP @ 0= IF TEXT-MAP EXIT THEN
   TEXT-CAP @ {: oc:n :}
   TEXT-P @ TEXT-BASE oc oc 2 * ROOM-GROW TEXT-P !
   oc 2 * TEXT-CAP ! ;

\ The cap bytes at p, or the first room f when cap is 0, grown by doubling and
\ keeping their bytes until u more fit after the first used, and their new
\ capacity.
: FIT ( ^u8 ^u8 n n n -- ^u8 n ) {: p:^ f:^ cap:n used:n u:n :}
   cap 0= IF f TEXT-FIRST ELSE p cap THEN
   {: q:^ c:n :}
   c BEGIN dup used u + < WHILE 2 * REPEAT {: nc:n :}
   nc c > IF q f c nc ROOM-GROW nc EXIT THEN
   q c ;

\ Grows the capture, keeping its bytes, until n more fit.
: HELD-ROOM ( n -- ) {: u:n :}
   HELD-P @ HELD-BASE HELD-CAP @ HELD-U @ u FIT  HELD-CAP !  HELD-P ! ;

: SAVED-ROOM ( n -- )
   {: u:n :}
   SAVED-P @ SAVED-BASE SAVED-CAP @ 0 u FIT  SAVED-CAP !  SAVED-P ! ;


public
: EMIT-RAW ( n -- )
   {: c:n :}
   DEST @ 0= 0= IF
     TEXT-LEN @ TEXT-CAP @ 2 - > IF TEXT-GROW THEN
     c TEXT TEXT-LEN @ + c!  TEXT-LEN @ 1 + TEXT-LEN !
   ELSE c BYTE-BUF c! BYTE-BUF 1 type THEN ;
private


: HEX-CHAR ( n -- n ) {: d:n :} d 10 < IF d 48 + ELSE d 55 + THEN ;

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
         c 4 rshift HEX-CHAR EMIT-RAW  c 15 and HEX-CHAR EMIT-RAW
      ELSE c EMIT-RAW THEN
   endcase ;

public
\ Between JSON-OPEN and JSON-CLOSE every byte is escaped: a package may be named `\`.
: EMIT1 {: c :}
   PART @ DROP-PART = IF EXIT THEN
   PART @ GAP-PART = IF  LABEL-PART PART !  32 EMIT-RAW  THEN
   IN-JSON @ 0= 0= IF c JSON-CHAR ELSE c EMIT-RAW THEN ;
: JSON-OPEN ( -- )   34 EMIT-RAW  -1 IN-JSON ! ;
: JSON-CLOSE ( -- )  0 IN-JSON !  34 EMIT-RAW ;

private
: FD ( -- n ) BUF-FD @ 2 xor ;
public

: CAPTURE ( -- )
   0 HELD-U !
   -1 BUF-ON ! ;

\ Records go to the file descriptor again; BUFFER$ still answers the capture.
: BUFFER-OFF ( -- )
   0 BUF-ON ! ;

\ Ends the capture and sends what it holds: a run that a throw or a fatal
\ rejection ends sends every record it made.
: RELEASE ( -- )
   BUF-ON @ 0= IF EXIT THEN
   0 BUF-ON !
   FD HELD-P @ HELD-U @ write drop
   0 HELD-U ! ;

: FD! ( n -- ) 2 xor BUF-FD ! ;

: BUFFER$ ( -- ^u8 n )
   HELD-P @ HELD-U @ ;
private

: BUF-APPEND ( str -- )
   {: a:^ u:n :}
   BUF-ON @ 0= IF FD a u write drop EXIT THEN
   u HELD-ROOM
   a HELD-P @ HELD-U @ + u ARENA-COPY
   HELD-U @ u + HELD-U ! ;

: FLUSH ( -- )
   TEXT TEXT-LEN @
   0 DEST !  0 TEXT-LEN !
   BUF-APPEND ;

public
: EMIT-STR ( ^u8 n -- ) {: a:^ u:n :}
   0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

: EMIT-U
   NUM-V !  0 NUM-N !
   NUM-V @ 0= IF 48 EMIT1 EXIT THEN
   NUM-V @ 0 < IF
      NUM-V @ 1 rshift 5 /
      NUM-V @ over 10 * - 48 +  NUM-BUF c!
      1 NUM-N !  NUM-V !
   THEN
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
   v negate EMIT-U ;
private
: JSON-STRING ( str -- )
   JSON-OPEN EMIT-STR JSON-CLOSE ;
: JSON-KEY ( str -- ) {: a:^ u:n :}
   a u JSON-STRING  58 EMIT1 ;

\ Where a top step's token sits: the active file's path, which an inner
\ `evaluate` never changes. FILE-PATH is private so no program writes it.
SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL FILE-PATH
public

\ The active file's path; empty when no file is being read.
: PATH$ ( -- ^u8 n )
   FILE-PATH @ data-base SRCLOC:PATHLEN-CELL + @ ;

\ The file a rejection names when no file holds its source.
: NO-FILE$ ( -- str ) s" <input>" ;

private
: FILE-BUF ( -- ^u8 ) BLOCK FILE-BUF-OFF + ;
: FILE-U ( -- ^n ) FILE-U-OFF CELL-AT ;   \ 0 when no file is named
public
\ The file FILE! named, NO-FILE$ when none is.
: FILE$ ( -- str )
   FILE-U @ 0= IF NO-FILE$ EXIT THEN
   FILE-BUF FILE-U @ ;
private
\ LABEL-BUF holds the path of the file LABEL! ran in, empty for none, and
\ LABEL-NAME-BUF the label a top-level step in that file is placed at.
: LABELED ( -- ^n ) LABELED-OFF CELL-AT ;
\ Whether a tool named a file by FILE!.
: NAMED ( -- ^n ) NAMED-OFF CELL-AT ;
: LABEL-BUF ( -- ^u8 ) BLOCK LABEL-BUF-OFF + ;
: LABEL-U ( -- ^n ) LABEL-U-OFF CELL-AT ;
: LABEL-NAME-BUF ( -- ^u8 ) BLOCK LABEL-NAME-BUF-OFF + ;
: LABEL-NAME-U ( -- ^n ) LABEL-NAME-U-OFF CELL-AT ;
: LABEL-NAME$ ( -- str ) LABEL-NAME-BUF LABEL-NAME-U @ ;
: COPY ( ^u8 ^u8 n -- ) {: a:^ d:^ u:n :}
   0 BEGIN dup u < WHILE
      dup a + c@
      over d + c!
      1 +
   REPEAT drop ;
: LABELS? ( str -- bool ) {: a:^ u:n :}
   LABELED @ 0= IF 0 0= 0= EXIT THEN
   a u LABEL-BUF LABEL-U @ CORE-STR= ;
\ Line, column and byte of p in a buffer whose first byte is at line bl, column
\ bc, byte bb; LF ends a line, a column counts bytes; tasks may call it.
public
: POSITION ( ^u8 ^u8 n n n -- n n n )
   {: base:^ p:^ bl:n bc:n bb:n :}
   p base - {: off:n :}
   1 1 0                                    \ line, column, index
   BEGIN dup off < WHILE
      base over + c@ 10 = IF
         rot 1 + rot drop 1 rot
      ELSE
         swap 1 + swap
      THEN
      1 +
   REPEAT drop {: line:n column:n :}
   bl line + 1 -
   line 1 = IF bc column + 1 - ELSE column THEN
   bb off + ;

private
: SOURCE-A ( -- ^^u8 ) SOURCE-A-OFF PTR-AT ;
: SOURCE-U ( -- ^n ) SOURCE-U-OFF CELL-AT ;
: SOURCE-LINE ( -- ^n ) SOURCE-LINE-OFF CELL-AT ;
: SOURCE-COLUMN ( -- ^n ) SOURCE-COLUMN-OFF CELL-AT ;
public
: SOURCE-BYTE ( -- ^n ) SOURCE-BYTE-OFF CELL-AT ;
private
: MAP-A ( -- ^^u8 ) MAP-A-OFF PTR-AT ;
: MAP-U ( -- ^n ) MAP-U-OFF CELL-AT ;
: ROW-A ( -- ^^n ) BLOCK ROW-A-OFF + 0 ptr-field ;
: ROW-N ( -- ^n ) ROW-N-OFF CELL-AT ;
\ The rows' source offset of the armed source's first byte, subtracted from
\ each row's: the engine's rows count from the input buffer's start.
: ROW-BIAS ( -- ^n ) ROW-BIAS-OFF CELL-AT ;
: CLOSER-A ( -- ^^u8 ) CLOSER-A-OFF PTR-AT ;
: CLOSER-U ( -- ^n ) CLOSER-U-OFF CELL-AT ;
: CLOSER-BASE ( -- ^^u8 ) CLOSER-BASE-OFF PTR-AT ;
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
: SOURCE! ( ^u8 n n n n -- )
   {: a:^ u:n line:n col:n byte:n :}
   a SOURCE-A !  u SOURCE-U !
   line SOURCE-LINE !  col SOURCE-COLUMN !  byte SOURCE-BYTE !
   MAP-OFF ;
: MAP! ( str ^n n -- )
   {: a:^ u:n rows:^ count:n :}
   a MAP-A !  u MAP-U !  rows ROW-A !  count ROW-N !  0 ROW-BIAS !
   CLOSER-OFF ;
\ Set after MAP!, which clears it.
: MAP-BIAS! ( n -- )
   ROW-BIAS ! ;
\ The token a u that closes the text whose first byte is base, which the text
\ does not hold: a structure still open there is rejected at it, as the load
\ rejects it.
: CLOSER! ( str ^u8 -- )
   {: a:^ u:n base:^ :}
   a CLOSER-A !  u CLOSER-U !  base CLOSER-BASE ! ;
: SOURCE$ ( -- str ) SOURCE-A @ SOURCE-U @ ;
\ Whether a closer was given for the text whose first byte is base.
: CLOSES? ( ^u8 -- bool ) CLOSER-BASE @ - 0= CLOSER-U @ 0 > and ;
: CLOSER$ ( -- str ) CLOSER-A @ CLOSER-U @ ;
private
: ROW@ ( n -- n )
   ROW-A @ {: i:n rows:^ :}
   i cells rows + @ ;
\ The first cell of the last of count two-cell rows at or before text offset x,
\ row 0 when none is. The rows ascend by text offset.
: ROW-AT ( ^n n n -- n )
   {: rows:^ count:n x:n :}
   1 count [: 2 * cells rows + @ x <= ;] KERNEL-SEARCH:PARTITION-POINT 1 - 2 * ;
: MAP-ROW ( n -- n )
   {: x:n :}
   ROW-A @ ROW-N @ x ROW-AT ;
\ The length of the token at s: its bytes up to a blank or e.
: TOKEN-LEN ( ^u8 ^u8 -- n ) {: s:^ e:^ :}
   0 BEGIN dup s + e - 0 < IF dup s + c@ 32 > ELSE 0 0= 0= THEN WHILE 1 + REPEAT ;
\ The source of the run text offset x lies in (layout.f BODY-SOURCE).
: MAP-RUN ( n -- n )
   MAP-ROW 1 + ROW@ ;
\ Whether text offset x lies in a run its source does not copy: one with no
\ source, or one a definer built.
: MAP-NONE? ( n -- bool )
   MAP-RUN {: src:n :}
   src BODY-SOURCE:NONE =  src BODY-SOURCE:BUILT? or ;
\ The token in the armed source that built the run text offset x lies in;
\ empty when no definer built the run.
: MAP-BUILDER ( n -- str )
   MAP-RUN {: src:n :}
   src BODY-SOURCE:BUILT? 0= IF NULL-PTR 0 EXIT THEN
   SOURCE-A @ src BODY-SOURCE:BUILT + ROW-BIAS @ - {: s:^ :}
   s  s SOURCE-A @ SOURCE-U @ + TOKEN-LEN ;
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
: >SOURCE ( ^u8 -- n bool )
   {: p:^ :}
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
: LOCATE ( ^u8 -- n n n bool )
   >SOURCE 0= IF drop 0 0 0 0 0= 0= EXIT THEN
   SOURCE-POSITION 0 0= ;

private
\ The name a record gives the file at path pa pu: its label, else its path.
: FILE-NAME$ ( str -- str ) {: pa:^ pu:n :}
   pa pu LABELS? IF LABEL-NAME$ EXIT THEN
   pa pu ;
\ The file a top-level step names; empty when no file or label names it.
: LOAD-FILE$ ( -- str )   PATH$ FILE-NAME$ ;

: NOTES ( -- ^n ) NOTES-OFF CELL-AT ;              \ the JSON notes object is open
: GUTTER ( -- ^n ) GUTTER-OFF CELL-AT ;            \ digits of AT's line, 1 unplaced
SRCLOC:OWNED-CELL RESERVED-PTR-U8-CELL OWNED
SRCLOC:INB-CELL RESERVED-PTR-U8-CELL INPUT-START
INE-CELL RESERVED-PTR-U8-CELL INPUT-END
BODY-TEXT:BASE-CELL RESERVED-PTR-U8-CELL BODY-BASE
PENDTKA-CELL RESERVED-PTR-U8-CELL PEND-NAME
\ AT's place: whether a buffer holds the token, its file, line, column and
\ byte, the byte after it, and the source line with the bytes before the token.
: PLACED ( -- ^n ) PLACED-OFF CELL-AT ;
: PLACE-FILE-A ( -- ^^u8 ) PLACE-FILE-A-OFF PTR-AT ;
: PLACE-FILE-U ( -- ^n ) PLACE-FILE-U-OFF CELL-AT ;
: AT-LINE ( -- ^n ) AT-LINE-OFF CELL-AT ;
: AT-COLUMN ( -- ^n ) AT-COLUMN-OFF CELL-AT ;
: AT-BYTE ( -- ^n ) AT-BYTE-OFF CELL-AT ;
: AT-END ( -- ^n ) AT-END-OFF CELL-AT ;
: LINE-A ( -- ^^u8 ) LINE-A-OFF PTR-AT ;
: LINE-U ( -- ^n ) LINE-U-OFF CELL-AT ;
: LEAD-U ( -- ^n ) LEAD-U-OFF CELL-AT ;
\ The newest record's place as AT copies it: its file's path, PATH-CAP bytes at
\ most, and its first byte plus one, 0 for none.
: LAST-BUF ( -- ^u8 ) BLOCK LAST-BUF-OFF + ;
: LAST-U ( -- ^n ) LAST-U-OFF CELL-AT ;
: LAST-AT ( -- ^n ) LAST-AT-OFF CELL-AT ;
\ A token in an evaluated text is placed where the evaluation ran, followed by
\ the line EVALUATED-A EVALUATED-U, the token EVAL-LEAD-U bytes in.
: EVALUATED ( -- ^n ) EVALUATED-OFF CELL-AT ;
: EVALUATED-A ( -- ^^u8 ) EVALUATED-A-OFF PTR-AT ;
: EVALUATED-U ( -- ^n ) EVALUATED-U-OFF CELL-AT ;
: EVAL-TOKEN-A ( -- ^^u8 ) EVAL-TOKEN-A-OFF PTR-AT ;
: EVAL-TOKEN-U ( -- ^n ) EVAL-TOKEN-U-OFF CELL-AT ;
: EVAL-LEAD-U ( -- ^n ) EVAL-LEAD-U-OFF CELL-AT ;
EVAL-TOP-CELL RESERVED-PTR-U8-CELL EVAL-TOP
\ A token no source holds is placed at the token READER-A READER-U the reader
\ walking the subject is on, an empty one for none, else at RUNNING, the token
\ the engine read last; the label is followed by a `word` note naming the token
\ WORD-A WORD-U; WORD-U is 0 for no note.
: READER-A ( -- ^^u8 ) READER-A-OFF PTR-AT ;
: READER-U ( -- ^n ) READER-U-OFF CELL-AT ;
TKA-CELL RESERVED-PTR-U8-CELL RUNNING
: WORD-A ( -- ^^u8 ) WORD-A-OFF PTR-AT ;
: WORD-U ( -- ^n ) WORD-U-OFF CELL-AT ;
\ Bytes of a prose line or quoted text shown: rustc's width when its output is
\ not a terminal. A longer one shows a window, `...` in place of each cut end.
140 constant WIDTH
40 constant ROOM                        \ bytes shown before a windowed token

\ Whether byte c continues a UTF-8 character, taking no column of its own.
: CONT? ( n -- bool )  192 and 128 = ;
\ The offset at or before n of the bytes at a where a UTF-8 character starts.
: CHAR-START ( str -- n ) {: a:^ n:n :}
   n BEGIN dup 0 > WHILE
      dup a + c@ CONT? 0= IF EXIT THEN
      1 -
   REPEAT ;
\ The offset of the first byte c in the u bytes at a, else u.
: BYTE-AT ( str n -- n ) {: a:^ u:n c:n :}
   0 BEGIN dup u < WHILE
      dup a + c@ c = IF EXIT THEN
      1 +
   REPEAT ;
: ESC-BYTE ( n -- ) {: c:n :}
   c 92 = IF 92 EMIT1 92 EMIT1 EXIT THEN
   c 10 = IF 92 EMIT1 110 EMIT1 EXIT THEN
   c 13 = IF 92 EMIT1 114 EMIT1 EXIT THEN
   c 9 = IF 92 EMIT1 116 EMIT1 EXIT THEN
   c 32 <  c 127 =  or IF
      92 EMIT1 120 EMIT1  c 4 rshift HEX-CHAR EMIT1  c 15 and HEX-CHAR EMIT1 EXIT
   THEN
   c EMIT1 ;
public
\ Emits the program text a u a note quotes. In prose it shows one line of at
\ most WIDTH bytes: past its first line or its first WIDTH - 3 bytes, the rest
\ becomes `...`. Other text a note holds, a path or a type, shows in full.
: EMIT-CUT ( str -- ) {: a:^ u:n :}
   JSON @ IF a u EMIT-STR EXIT THEN
   a u 10 BYTE-AT {: f:n :}
   f u =  u WIDTH <= and IF a u EMIT-STR EXIT THEN
   a  a f WIDTH 3 - min CHAR-START  EMIT-STR
   s" ..." EMIT-STR ;
\ Text a record quotes from outside the source, such as a path, shows on one line.
: EMIT-ESC ( str -- ) {: a:^ u:n :}
   JSON @ IF a u EMIT-STR EXIT THEN
   0 BEGIN dup u < WHILE dup a + c@ ESC-BYTE 1 + REPEAT drop ;
private
: END-TEXT ( -- )
   PART @ GAP-PART = IF LABEL-PART PART ! THEN
   PART @ DROP-PART = IF LABEL-PART PART ! EXIT THEN
   JSON @ IF JSON-CLOSE EXIT THEN
   10 EMIT1 ;
: BLANKS ( n -- ) {: k:n :}
   0 BEGIN dup k < WHILE 32 EMIT1 1 + REPEAT drop ;
: DIGITS ( n -- n ) {: v:n :}
   1 v BEGIN dup 9 > WHILE 10 / swap 1 + swap REPEAT drop ;
: FIELD ( ^u8 n -- )   44 EMIT1 JSON-KEY ;
: MARGIN ( -- )   GUTTER @ 1 + BLANKS ;

: PLACE-FILE! ( str -- ) {: a:^ u:n :}
   a PLACE-FILE-A !  u PLACE-FILE-U ! ;
: PLACE-FILE$ ( -- str )   PLACE-FILE-A @ PLACE-FILE-U @ ;
: ROOT-A ( -- ^^u8 ) ROOT-A-OFF PTR-AT ;
: ROOT-U ( -- ^n ) ROOT-U-OFF CELL-AT ;
public
\ The bytes the loader owns for the active file.
: OWNED$ ( -- str )   OWNED @ data-base SRCLOC:OWNEDLEN-CELL + @ ;
\ The root stream's text, set while the loader runs it; empty outside it.
: ROOT-TEXT! ( str -- ) {: a:^ u:n :}   a ROOT-A !  u ROOT-U ! ;
: ROOT-TEXT$ ( -- str )   ROOT-A @ ROOT-U @ ;
\ The buffer the engine reads.
: INPUT$ ( -- str )
   INPUT-START @ {: b:^ :}
   b  INPUT-END @ b - ;
private
: HOLDS? ( str str -- bool ) {: b:^ bu:n t:^ tu:n :}
   t b - {: at:n :}
   at 0 >=  at tu + bu <= and ;
public
\ Whether the engine reads file text: an `evaluate` of a slice of the open file
\ does, of any other string does not.
: FILE-BUFFER? ( -- bool )
   OWNED$ {: o:^ ou:n :}
   o NULL-PTR <>  o ou INPUT$ HOLDS?  and ;
private
\ Whether the armed source holds the n bytes at p: its map's text, outside a
\ run it does not copy, else its own bytes.
: ARMED? ( str -- bool ) {: p:^ n:n :}
   SOURCE-U @ 0 = IF 0 0= 0= EXIT THEN
   ROW-N @ 0 >  MAP-A @ MAP-U @ p n HOLDS?  and IF p MAP-A @ - MAP-NONE? 0= EXIT THEN
   SOURCE$ p n HOLDS? ;
\ The line of b's bu bytes that holds t: from the byte after the last LF before
\ t up to the next LF, or the buffer's edge.
: LINE! ( str ^u8 -- ) {: b:^ bu:n t:^ :}
   t b - {: at:n :}
   at BEGIN dup 0 > IF dup 1 - b + c@ 10 <> ELSE 0 0= 0= THEN WHILE 1 - REPEAT
   {: s:n :}
   at BEGIN dup bu < IF dup b + c@ 10 <> ELSE 0 0= 0= THEN WHILE 1 + REPEAT
   {: e:n :}
   b s + LINE-A !  e s - LINE-U !  at s - LEAD-U ! ;
\ A body's source is armed from its name on, so a file or the buffer read,
\ which hold the bytes before the name, cut the line first.
: CUT ( ^u8 -- ) {: t:^ :}
   OWNED$ t 0 HOLDS? IF OWNED$ t LINE! EXIT THEN
   INPUT$ t 0 HOLDS? IF INPUT$ t LINE! EXIT THEN
   SOURCE$ t LINE! ;
: POSITION! ( n n n -- )
   AT-BYTE !  AT-COLUMN !  AT-LINE !  -1 PLACED ! ;
: SOURCE-PLACE ( n str -- ) {: off:n t:^ tu:n :}
   off SOURCE-POSITION POSITION!
   t tu + >SOURCE IF SOURCE-BYTE @ + ELSE drop AT-BYTE @ tu + THEN AT-END !
   SOURCE-A @ off + {: s:^ :}
   s CUT
   OWNED$ s 0 HOLDS? IF LOAD-FILE$ ELSE FILE$ THEN PLACE-FILE! ;
: BUFFER-PLACE ( str str -- ) {: b:^ bu:n t:^ tu:n :}
   b t 1 1 0 POSITION POSITION!
   AT-BYTE @ tu + AT-END !
   b bu t LINE! ;
\ The run of the pending definition's body text that t lies in, by the rows
\ the copy records (layout.f BODY-SOURCE): its first byte and its source;
\ false when t is in no run with a row there.
\ Rows count from the buffer's first byte, and they name this buffer only
\ while the definition's name lies in it: a REPL line refills the buffer that
\ held the line before, and OPENER-KEEP moves the name out of it.
: INPUT-RUN ( ^u8 -- ^u8 n bool ) {: t:^ :}
   BODY-BASE @ {: base:^ :}
   data-base PEND-CELL + @ 0=  base NULL-PTR = or IF t 0 0 0= 0= EXIT THEN
   t base - {: x:n :}
   x 0 <  x data-base BODYLEN-CELL + @ >= or IF t 0 0 0= 0= EXIT THEN
   data-base BODY-TEXT:ROWS-CELL + @ {: count:n :}
   count 0 <=  INPUT$ PEND-NAME @ 1 HOLDS? 0= or IF t 0 0 0= 0= EXIT THEN
   base data-base BODY-TEXT:CAP-CELL + @ + cell-view {: rows:^ :}
   rows count x ROW-AT {: r:n :}
   base r cells rows + @ +  r 1 + cells rows + @  0 0= ;
\ The bytes of the buffer read that the pending definition's body text
\ copied the token t tu from; else t.
: BODY>INPUT ( str -- ^u8 ) {: t:^ tu:n :}
   t INPUT-RUN 0= IF 2drop t EXIT THEN {: run:^ src:n :}
   src BODY-SOURCE:NONE =  src BODY-SOURCE:BUILT? or IF t EXIT THEN
   INPUT-START @ src +  t run - + {: p:^ :}
   INPUT$ p tu HOLDS? IF p EXIT THEN
   t ;
\ The token that built the run t lies in, in the armed source or the buffer
\ read; empty when no definer built the run.
: BUILDER ( ^u8 -- ^u8 n ) {: t:^ :}
   SOURCE-U @ 0 >  ROW-N @ 0 > and  t MAP-A @ - MAP-U @ IN-SPAN? and IF
      t MAP-A @ - MAP-BUILDER EXIT
   THEN
   t INPUT-RUN 0= IF 2drop NULL-PTR 0 EXIT THEN nip {: src:n :}
   src BODY-SOURCE:BUILT? 0= IF NULL-PTR 0 EXIT THEN
   INPUT-START @ src BODY-SOURCE:BUILT + {: s:^ :}
   s  s INPUT-END @ TOKEN-LEN ;
\ Each evaluate frame saved the input it interrupted, its start, end and read
\ position (layout.f EVAL-INB), and links to the frame it runs in (EVAL-PREV).
\ A loaded file and a REPL line run in frames of their own.
: FRAME@ ( ^u8 n -- ^u8 ) + 0 ptr-field @ ;
: OUTER ( ^u8 -- ^u8 )   EVAL-PREV FRAME@ ;
: SAVED$ ( ^u8 -- str ) {: f:^ :}
   f EVAL-INB FRAME@ {: b:^ :}
   b  f EVAL-INE FRAME@ b - ;
\ The loader's frames, innermost first, one per open file: each links to the
\ frame of the file it interrupted.
SRCLOC:SOURCE-CELL RESERVED-PTR-U8-CELL LOADER
\ The bytes the open file of frame fr runs; empty until they are read.
: FRAME-BYTES$ ( ^u8 -- str ) {: fr:^ :}
   fr SRCLOC:FR-SIZE + cell-view @ {: u:n :}
   u 0 < IF NULL-PTR 0 EXIT THEN
   fr SRCLOC:FR-EXEC-COPY FRAME@ {: c:^ :}
   c NULL-PTR = IF fr SRCLOC:HEADER-BYTES + u EXIT THEN
   c u ;
\ The name a record gives the file of frame fr.
: FRAME-FILE$ ( ^u8 -- str ) {: fr:^ :}
   fr SRCLOC:FR-PATH +  fr SRCLOC:FR-PATHLEN + cell-view @ FILE-NAME$ ;
\ The frame of the innermost open file whose bytes hold the n bytes at t;
\ NULL-PTR when none does.
: HOLDER ( ^u8 n -- ^u8 ) {: t:^ tu:n :}
   LOADER @ dup NULL-PTR = IF EXIT THEN
   SRCLOC:REC-TOP FRAME@
   BEGIN dup NULL-PTR = 0= WHILE
      dup FRAME-BYTES$ t tu HOLDS? IF EXIT THEN
      SRCLOC:FR-PREV FRAME@
   REPEAT ;
\ Places the token t tu in the open file holding it; answers whether one does.
: FILE-PLACE ( str -- bool ) {: t:^ tu:n :}
   t tu HOLDER {: fr:^ :}
   fr NULL-PTR = IF 0 0= 0= EXIT THEN
   fr FRAME-BYTES$ t tu BUFFER-PLACE  fr FRAME-FILE$ PLACE-FILE!  0 0= ;
\ Whether frame f is the first evaluation its input ran: f interrupted an open
\ file's bytes, or the frame it runs in is the outermost, a REPL line's.
: INVOKED? ( ^u8 -- bool ) {: f:^ :}
   f EVAL-INB FRAME@ 0 HOLDER NULL-PTR = 0=
   f OUTER OUTER NULL-PTR =  or ;
\ The frame of the first evaluation, outward from the innermost; NULL-PTR when
\ the input read is no evaluated text.
: INVOKER ( -- ^u8 )
   EVAL-TOP @
   dup NULL-PTR = IF EXIT THEN
   dup OUTER NULL-PTR = IF drop NULL-PTR EXIT THEN
   BEGIN dup INVOKED? 0= WHILE OUTER REPEAT ;
\ Records the line LINE! found as the evaluated place of the token t tu.
: EVALUATED! ( str -- ) {: t:^ tu:n :}
   LINE-A @ EVALUATED-A !  LINE-U @ EVALUATED-U !  LEAD-U @ EVAL-LEAD-U !
   t EVAL-TOKEN-A !  tu EVAL-TOKEN-U ! ;
\ Whether an evaluated text inside the invoker inv holds the token t tu: the
\ buffer read, or one an inner frame interrupted. Records the token's evaluated
\ place, and answers the token in that text.
: IN-EVALUATED? ( str ^u8 -- ^u8 bool ) {: t:^ tu:n inv:^ :}
   t tu BODY>INPUT {: s:^ :}
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
\ token's bytes, never before b; empty when only blanks stand between them.
: TOKEN-BEFORE ( ^u8 ^u8 -- str ) {: b:^ p:^ :}
   p b - BEGIN dup 0 > IF dup 1 - b + c@ 33 < ELSE 0 0= 0= THEN WHILE 1 - REPEAT
   {: e:n :}
   e BEGIN dup 0 > IF dup 1 - b + c@ 32 > ELSE 0 0= 0= THEN WHILE 1 - REPEAT
   {: s:n :}
   b s +  e s - ;
\ Places at the token the invoker's input had read as the evaluation began, in
\ the open file holding it or else the line it interrupted.
: INVOKE-PLACE ( ^u8 -- ) {: inv:^ :}
   inv SAVED$ {: b:^ bu:n :}
   b inv EVAL-INP FRAME@ TOKEN-BEFORE {: a:^ u:n :}
   u 0= IF EXIT THEN
   -1 EVALUATED !
   a u FILE-PLACE IF EXIT THEN
   b bu a u BUFFER-PLACE NO-FILE$ PLACE-FILE! ;
public
\ Whether one source a rejection places from holds the token p n whole while
\ the rejection is written: the armed source, the given closer's bytes, an open
\ file's bytes, the root stream's text or an evaluated text being run, each of
\ which stays put while it is open or runs. A zero-length token is held at any
\ offset from 0 to the buffer's length.
: HELD? ( ^u8 n -- bool ) {: p:^ n:n :}
   p n ARMED? IF 0 0= EXIT THEN
   CLOSER$ p n HOLDS? IF 0 0= EXIT THEN
   p n HOLDER NULL-PTR = 0= IF 0 0= EXIT THEN
   ROOT-TEXT$ p n HOLDS? IF 0 0= EXIT THEN
   INVOKER {: inv:^ :}
   inv NULL-PTR = IF 0 0= 0= EXIT THEN
   p n inv IN-EVALUATED? nip ;
private
\ Places the token t tu in the armed source holding its first byte, else an
\ open file's bytes, else an evaluated text, which places it where the
\ evaluation was run, else the buffer read, holding the whole token, as itself
\ or as the bytes a body's text copied it from; answers whether one did. A
\ token of a run a definer built is placed by none of them.
: PLACE-IN ( str -- bool ) {: t:^ tu:n :}
   t >SOURCE IF t tu SOURCE-PLACE 0 0= EXIT THEN drop
   t BUILDER nip 0 > IF 0 0= 0= EXIT THEN
   t tu BODY>INPUT {: s:^ :}
   s tu FILE-PLACE IF 0 0= EXIT THEN
   INVOKER {: inv:^ :}
   inv NULL-PTR = 0= IF
      t tu inv IN-EVALUATED? nip IF
         inv INVOKE-PLACE  PLACED @ 0= 0= IF 0 0= EXIT THEN
         0 EVALUATED !
      THEN
   THEN
   INPUT$ s tu HOLDS? IF INPUT$ s tu BUFFER-PLACE NO-FILE$ PLACE-FILE!  0 0= EXIT THEN
   0 0= 0= ;
public
\ Whether a record at the byte p underlines that byte in its source, never the
\ token that built its run.
: PLACES-IN? ( ^u8 -- bool ) {: p:^ :}
   p >SOURCE nip IF 0 0= EXIT THEN
   p BUILDER nip 0 > IF 0 0= 0= EXIT THEN
   p 1 BODY>INPUT {: s:^ :}
   s 1 HOLDER NULL-PTR = 0= IF 0 0= EXIT THEN
   INPUT$ s 1 HOLDS? IF 0 0= EXIT THEN
   INVOKER {: inv:^ :}
   inv NULL-PTR = IF 0 0= 0= EXIT THEN
   p 1 inv IN-EVALUATED? nip ;
private
\ Whether a tool named a file while the file being read is not labelled: that
\ file is then the tool's own.
: TOOL-OWN? ( -- bool )
   NAMED @ 0= IF 0 0= 0= EXIT THEN
   PATH$ LABELS? 0= ;
public
\ Whether PLACE places the byte p points at, at itself or at the token that
\ built its run.
: PLACES? ( ^u8 -- bool ) {: p:^ :}
   p PLACES-IN? IF 0 0= EXIT THEN
   p BUILDER {: b:^ bu:n :}
   bu 0 > 0= IF 0 0= 0= EXIT THEN
   TOOL-OWN? IF b >SOURCE nip ELSE b PLACES-IN? THEN ;
private
\ Places the token t tu, else the token that built its run, else the reader's
\ token when it is on one or the running token when not, naming t tu in a
\ `word` note, else nowhere.
\ While the file being read is the tool's own, that token places only in an
\ armed source.
: PLACE ( ^u8 n -- ) {: t:^ tu:n :}
   0 PLACED !  0 EVALUATED !  0 WORD-U !
   t tu PLACE-IN IF EXIT THEN
   t WORD-A !  tu WORD-U !
   t BUILDER {: b:^ bu:n :}
   bu 0 > IF b bu ELSE
      READER-U @ 0 > IF READER-A @ READER-U @ ELSE RUNNING @ data-base TKL-CELL + @ THEN
   THEN
   {: r:^ ru:n :}
   TOOL-OWN? IF
      r >SOURCE IF r ru SOURCE-PLACE ELSE drop THEN
   ELSE
      r ru PLACE-IN drop
   THEN ;

: PLACE-JSON ( str -- ) {: t:^ tu:n :}
   s" file" FIELD  PLACE-FILE$ JSON-STRING
   s" line" FIELD  AT-LINE @ EMIT-U
   s" column" FIELD  AT-COLUMN @ EMIT-U
   s" byte_start" FIELD  AT-BYTE @ EMIT-U
   s" byte_end" FIELD  AT-END @ EMIT-U
   s" token" FIELD  t tu JSON-STRING
   s" source_line" FIELD  LINE-A @ LINE-U @ JSON-STRING
   s" label" FIELD  JSON-OPEN ;
\ The caret line copies the tabs in the line's first n bytes, before the token,
\ and a blank for each other character, so the carets line up.
: LEAD ( ^u8 n -- ) {: a:^ k:n :}
   0 BEGIN dup k < WHILE
      dup a + c@ {: c:n :}
      c CONT? 0= IF c 9 = IF 9 ELSE 32 THEN EMIT1 THEN
      1 +
   REPEAT drop ;
\ A caret under each character of the token t tu, one under an empty token.
: CARETS ( str -- ) {: t:^ tu:n :}
   tu 0= IF 94 EMIT1 EXIT THEN
   0 BEGIN dup tu < WHILE
      dup t + c@ CONT? 0= IF 94 EMIT1 THEN
      1 +
   REPEAT drop ;
\ The bytes s to e shown of the line a u whose token starts k bytes in: all of
\ a line of at most WIDTH, else from ROOM bytes before the token, at most WIDTH
\ with the `...` of each cut end; neither cut splits a UTF-8 character.
: WINDOW ( ^u8 n n -- n n ) {: a:^ u:n k:n :}
   u WIDTH <= IF 0 u EXIT THEN
   k ROOM > IF k ROOM - ELSE 0 THEN {: s0:n :}
   a s0 CHAR-START {: s:n :}
   s 0 > IF WIDTH 6 - ELSE WIDTH 3 - THEN {: w:n :}
   u s - w 3 + <= IF s u EXIT THEN
   s  a s w + CHAR-START ;
\ The line a u, whose token starts k bytes in, as its window.
: SHOW-LINE ( str n -- ) {: a:^ u:n k:n :}
   a u k WINDOW {: s:n e:n :}
   s 0 > IF s" ..." EMIT-STR THEN
   a s + e s - EMIT-STR
   e u < IF s" ..." EMIT-STR THEN ;
\ The carets under the token tu bytes long that starts k bytes into the line
\ a u, up to its window's end.
: SHOW-CARETS ( str n n -- ) {: a:^ u:n k:n tu:n :}
   a u k WINDOW {: s:n e:n :}
   s 0 > IF 3 BLANKS THEN
   a s + k s - LEAD
   a k +  tu e k - > IF e k - ELSE tu THEN CARETS ;
: PLACE-PROSE ( str -- ) {: t:^ tu:n :}
   GUTTER @ BLANKS  s" --> " EMIT-STR  PLACE-FILE$ EMIT-STR
   58 EMIT1  AT-LINE @ EMIT-U  58 EMIT1  AT-COLUMN @ EMIT-U  10 EMIT1
   MARGIN 124 EMIT1 10 EMIT1
   AT-LINE @ EMIT-U  s"  | " EMIT-STR  LINE-A @ LINE-U @ LEAD-U @ SHOW-LINE  10 EMIT1
   MARGIN s" | " EMIT-STR  LINE-A @ LINE-U @ LEAD-U @ tu SHOW-CARETS ;

\ A note's key; its value follows.
: NOTE-KEY ( str -- ) {: k:^ ku:n :}
   JSON @ IF
      NOTES @ 0= 0= IF 44 EMIT1 ELSE s" notes" FIELD  123 EMIT1  -1 NOTES ! THEN
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
   s" = evaluated: " {: k:^ ku:n :}
   EVALUATED-A @ EVALUATED-U @ EVAL-LEAD-U @ {: a:^ u:n lead:n :}
   MARGIN  k ku EMIT-STR  a u lead SHOW-LINE  10 EMIT1
   MARGIN  ku BLANKS  a u lead EVAL-TOKEN-U @ SHOW-CARETS ;
\ Ends the open text; the label's end, or a dropped label's, writes the
\ evaluated place JSON still owes, then the `word` note.
: CLOSE-PART ( -- )
   END-TEXT
   PART @ LABEL-PART = 0= IF EXIT THEN
   EVALUATED @ 0= 0= IF  0 EVALUATED !  EVALUATED-JSON  THEN
   WORD-U @ 0= IF EXIT THEN
   s" word" NOTE-KEY  WORD-A @ WORD-U @ EMIT-STR  0 WORD-U !  END-TEXT ;

\ Closes a notes object no HELP closed, then the record.
: CLOSE-RECORD ( -- )
   CLOSE-PART
   JSON @ IF  NOTES @ 0= 0= IF 125 EMIT1 THEN  125 EMIT1  THEN  10 EMIT1
   NO-PART PART ! ;

\ Ends the record and sends it, or drops it under QUIET.
: END-RECORD ( -- )
   CLOSE-RECORD
   QUIET @ 0= IF FLUSH ELSE 0 TEXT-LEN !  0 DEST ! THEN ;

: WARNING ( -- ^n ) WARNING-OFF CELL-AT ;           \ the open record is a warning
: BEGIN-RECORD ( str str -- ) {: sev:^ su:n code:^ cu:n :}
   1 DEST !  0 TEXT-LEN !  0 NOTES !  1 GUTTER !  0 EVALUATED !  0 WORD-U !
   0 LAST-U !  0 LAST-AT !
   NO-PART PART !
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
: OPEN ( ^u8 n -- ) {: code:^ cu:n :}
   0 WARNING !  s" error" code cu BEGIN-RECORD ;
\ Opens a warning with its code, which KEEP does not count; the message follows.
: WARN ( ^u8 n -- ) {: code:^ cu:n :}
   -1 WARNING !  s" warning" code cu BEGIN-RECORD ;
\ Places the token t tu as PLACE does; its label follows. With no place the
\ record writes none and drops the label. In prose an evaluated token's call is
\ labelled as such, and the label follows the evaluated place.
: AT ( ^u8 n -- ) {: t:^ tu:n :}
   END-TEXT
   t tu PLACE
   PLACED @ 0= IF DROP-PART PART ! EXIT THEN
   PLACE-FILE$ {: f:^ fu:n :}
   f LAST-BUF fu COPY  fu LAST-U !  AT-BYTE @ 1 + LAST-AT !
   AT-LINE @ DIGITS GUTTER !
   \ The placed bytes as the file holds them: a checked copy stands its tokens
   \ one blank apart, so a span across blanks is not the copy's length.
   LINE-A @ LEAD-U @ +  AT-END @ AT-BYTE @ - {: p:^ pu:n :}
   JSON @ IF p pu PLACE-JSON  LABEL-PART PART !  EXIT THEN
   p pu PLACE-PROSE
   EVALUATED @ 0= 0= IF
      0 EVALUATED !
      s"  evaluates the text below" EMIT-STR  10 EMIT1  EVALUATED-PROSE
   THEN
   GAP-PART PART ! ;
\ The path and first byte of the newest record's place, the byte -1 when it
\ had none. The path is borrowed until the next record is placed.
: LAST-PLACE ( -- str n )   LAST-BUF LAST-U @ LAST-AT @ 1 - ;
\ Drops the `word` note AT added to its place.
: NO-WORD ( -- )   0 WORD-U ! ;
\ A note's key; its value follows.
: NOTE ( ^u8 n -- )
   CLOSE-PART NOTE-KEY ;
\ Opens a record with code c and message m, and places the token t tu with
\ label l; the label may go on.
: OPEN-AT ( str str str str -- )
   {: c:^ cu:n m:^ mu:n t:^ tu:n l:^ lu:n :}
   c cu OPEN  m mu EMIT-STR  t tu AT  l lu EMIT-STR ;
\ The limit note: the number n, then the unit w wu if it has one.
: LIMIT ( n ^u8 n -- ) {: n:n w:^ wu:n :}
   s" limit" NOTE  n EMIT-U
   wu 0 > IF s"  " EMIT-STR  w wu EMIT-STR THEN ;
\ The token a u if HELD? holds its source, else its copy c cu.
: KEPT ( ^u8 n ^u8 n -- ^u8 n ) {: a:^ u:n c:^ cu:n :}
   a u HELD? IF a u EXIT THEN
   c cu ;
\ The help text follows.
: HELP ( -- )
   CLOSE-PART
   JSON @ IF
      NOTES @ 0= 0= IF 125 EMIT1  0 NOTES ! THEN
      s" help" FIELD  JSON-OPEN
   ELSE
      MARGIN  s" = help: " EMIT-STR
   THEN
   HELP-PART PART ! ;
\ Ends the record and returns; a sent rejection counts in TALLY.
: KEEP ( -- )
   END-RECORD
   QUIET @ 0=  WARNING @ 0=  and IF TALLY @ 1 + TALLY ! THEN ;
\ Ends the record, counts it as KEEP does and throws status n.
: STOP ( n -- )
   KEEP throw ;

\ Ends the record and keeps it, in place of the one kept before, for
\ SEND-SAVED; under QUIET it keeps none.
: SAVE ( -- )
   CLOSE-RECORD
   0 SAVED-U !
   QUIET @ 0= IF
      TEXT-LEN @ SAVED-ROOM
      TEXT SAVED-P @ TEXT-LEN @ ARENA-COPY
      TEXT-LEN @ SAVED-U !
   THEN
   0 data-base STAMP-CELL + !  0 TEXT-LEN !  0 DEST ! ;

\ Sends the record SAVE kept, and keeps none.
: SEND-SAVED ( -- )
   0 data-base STAMP-CELL + !
   SAVED-U @ 0= IF EXIT THEN
   SAVED-P @ SAVED-U @  0 SAVED-U !  BUF-APPEND ;

\ Throws n as the throw that carries the kept record, if SAVE kept one.
: PASS ( n -- )
   SAVED-U @ 0 <> IF
      data-base THROWS-CELL + @ 1 +  data-base STAMP-CELL + !
   THEN
   throw ;

\ Whether the newest throw this thread began carries the kept record.
: CARRIED? ( -- bool )
   data-base STAMP-CELL + @  data-base THROWS-CELL + @  = ;

\ The word kw ku has no operand after it, which what names with its article
\ ("a name"): the record stands at t tu, which why labels. It is written
\ through the start of its help: the caller writes the help and stops it.
: MISSING-OPERAND ( str str str str -- )
   {: kw:^ ku:n what:^ whatu:n t:^ tu:n why:^ whyu:n :}
   s" E-MISSING-NAME" OPEN
   96 EMIT1  kw ku EMIT-STR  s" ` needs " EMIT-STR  what whatu EMIT-STR
   t tu AT  why whyu EMIT-STR
   HELP ;

\ The definer kw ku has no name of one token: the record stands at t tu, which
\ why labels. It is written through its help, and the caller stops it.
: MISSING-NAME ( ^u8 n ^u8 n ^u8 n -- )
   {: kw:^ ku:n t:^ tu:n why:^ whyu:n :}
   kw ku s" a name" t tu why whyu MISSING-OPERAND
   s" give `" EMIT-STR  kw ku EMIT-STR
   s" ` one name, a token with no blanks" EMIT-STR ;

\ The checker reads a `(` as a signature and a `\` as a comment wherever either
\ stands, so the source token t tu names nothing when definer kw ku takes it as
\ a name. Neither byte has a case.
: NOT-A-NAME ( str str -- ) {: kw:^ ku:n t:^ tu:n :}
   t tu s" (" CORE-STR= IF
      kw ku t tu s" a signature, not a name" MISSING-NAME
      EXIT-STATUS:REJECTED STOP
   THEN
   t tu s" \" CORE-STR= IF
      kw ku t tu s" a comment, not a name" MISSING-NAME
      EXIT-STATUS:REJECTED STOP
   THEN ;

\ The counted string the literal's opener t tu opens is longer than the max
\ bytes its count byte holds. It is written through its help, and the caller
\ stops it.
: COUNTED-RANGE ( str n -- ) {: t:^ tu:n max:n :}
   s" E-COUNTED-STRING-TOO-LONG" OPEN
   s" the counted string `" EMIT-STR  t tu EMIT-STR
   s" ` opens is longer than its count byte holds" EMIT-STR
   t tu AT  s" too long" EMIT-STR
   max s" bytes" LIMIT
   HELP  s" shorten the text, or write it with " EMIT-STR
   S\" `s\"`" EMIT-STR  s" , which has no count byte" EMIT-STR ;

private
\ The range the form of the number token t tu reads: a `$` one's, one with a
\ `.`, or a decimal integer's.
: RANGE-NOTE ( str -- ) {: t:^ tu:n :}
   s" range" NOTE
   t tu 36 BYTE-AT tu < IF
      s" a cell holds `$0` to `$FFFFFFFFFFFFFFFF`" EMIT-STR EXIT
   THEN
   t tu 46 BYTE-AT tu < IF
      s" a number with a `.` holds a whole part up to `9223372036854775807` and at most 18 digits after the `.`" EMIT-STR EXIT
   THEN
   s" a cell holds `-9223372036854775808` to `9223372036854775807`" EMIT-STR ;
public

\ The number token t tu does not fit in a cell. It is written through its
\ help, and the caller stops or keeps it.
: LITERAL-RANGE ( str -- ) {: t:^ tu:n :}
   s" E-LITERAL-RANGE" OPEN
   s" the number `" EMIT-STR  t tu EMIT-STR  s" ` is out of range" EMIT-STR
   t tu AT  s" does not fit in a cell" EMIT-STR
   t tu RANGE-NOTE
   HELP  s" write a number in that range" EMIT-STR ;

\ Writes the record of word w given a path len bytes long, empty, below 0 or
\ past cap, the longest length the failing test allows, through its message:
\ the caller may write one AT, then writes the help and ends the record. A w of
\ length 0 names no word: the message's subject is the path.
: PATH-RANGE ( str n n -- ) {: w:^ wu:n len:n cap:n :}
   s" E-PATH-RANGE" OPEN
   wu 0= IF
      len 0= IF s" a path is empty" EMIT-STR  EXIT THEN
      len 0 < IF
         s" a path's length is " EMIT-STR  len EMIT-N  s" , below 0" EMIT-STR
         EXIT
      THEN
      s" a path is " EMIT-STR
   ELSE
      96 EMIT1  w wu EMIT-STR
      len 0= IF s" ` got an empty path" EMIT-STR  EXIT THEN
      len 0 < IF
         s" ` got a path whose length is " EMIT-STR  len EMIT-N
         s" , below 0" EMIT-STR  EXIT
      THEN
      s" ` got a path " EMIT-STR
   THEN
   len EMIT-U  s"  bytes long, more than the " EMIT-STR
   cap EMIT-U  s"  bytes a path may have" EMIT-STR ;

\ Writes the record of word w given a path that resolves to more than cap
\ bytes; the caller's part is as for PATH-RANGE.
: PATH-RESOLVED-RANGE ( str n -- ) {: w:^ wu:n cap:n :}
   s" E-PATH-RANGE" OPEN
   wu 0= IF
      s" a path resolves to more than the " EMIT-STR
   ELSE
      96 EMIT1  w wu EMIT-STR
      s" ` got a path that resolves to more than the " EMIT-STR
   THEN
   cap EMIT-U  s"  bytes a path may have" EMIT-STR ;

\ Throws E-MEM-MAP carrying the record of n bytes the system refused for what
\ the first str names, while loading the file the second names, if any.
: MAP-PASS-FILE ( n str str -- ) {: bytes:n a:^ u:n f:^ fu:n :}
   s" E-MEM-MAP" OPEN
   s" the system refused " EMIT-STR  bytes EMIT-U
   s"  bytes of storage" EMIT-STR
   u 0 > if s"  for " EMIT-STR  a u EMIT-STR then
   fu 0 > if s" file" NOTE  f fu EMIT-STR then
   HELP
   s" raise the process's address-space limit (ulimit -v), or reserve less storage"
   EMIT-STR
   SAVE  E-MEM-MAP PASS ;

\ Throws E-MEM-MAP carrying the record of n bytes the system refused for what
\ a u names, if anything.
: MAP-PASS ( n str -- )
   s" " MAP-PASS-FILE ;

private
: DUPLICATE-OPEN ( str str -- ) {: a:^ u:n t:^ tu:n :}
   s" E-DUPLICATE-DEFINITION" OPEN
   96 EMIT1  a u EMIT-STR  s" ` is already defined in this wordlist" EMIT-STR
   t tu AT  s" already defined" EMIT-STR ;
public

\ The name a u is already defined where it is defined again: the record stands
\ at t tu. It is written through its help, and the caller stops it.
: DUPLICATE ( ^u8 n ^u8 n -- )
   DUPLICATE-OPEN
   HELP  s" rename this word, or remove the earlier definition" EMIT-STR ;

\ DUPLICATE's record when the name defined again is one the type f fu generates.
: DUPLICATE-GENERATED ( str str str -- ) {: a:^ u:n t:^ tu:n f:^ fu:n :}
   a u t tu DUPLICATE-OPEN
   s" generated by" NOTE  s" type `" EMIT-STR  f fu EMIT-STR  96 EMIT1
   HELP  s" rename this word, or the type `" EMIT-STR  f fu EMIT-STR
   s" `, which generates it" EMIT-STR ;

\ The does> clause of definer stem, named stem then suffix, takes a name
\ already defined: the record stands at t tu. It is written through its help,
\ and the caller stops it.
: CLAUSE-TAKEN ( str str str -- )
   {: stem:^ stemu:n sfx:^ sfxu:n t:^ tu:n :}
   s" E-DUPLICATE-DEFINITION" OPEN
   96 EMIT1  stem stemu EMIT-STR  sfx sfxu EMIT-STR
   s" ` is already defined in this wordlist" EMIT-STR
   t tu AT  s" its clause's name is taken" EMIT-STR
   HELP  s" rename `" EMIT-STR  stem stemu EMIT-STR
   s" `, or the word already named `" EMIT-STR
   stem stemu EMIT-STR  sfx sfxu EMIT-STR  96 EMIT1 ;

\ Rejects a definition of NAME when the dictionary has no record left, placed
\ at PLACE.
: DICTIONARY-FULL ( str str -- ) {: a:^ u:n t:^ tu:n :}
   s" E-DICTIONARY-FULL" OPEN
   s" the dictionary has no record left for `" EMIT-STR  a u EMIT-STR  96 EMIT1
   t tu AT  s" no record left" EMIT-STR
   HELP  s" define fewer words in this load" EMIT-STR
   EXIT-STATUS:FULL STOP ;

\ The name a u would change a sealed package: the record stands at the source
\ token t tu, which is not a u when the name is generated. It is written
\ through its help, and the caller stops it.
: SEALED ( ^u8 n ^u8 n -- ) {: a:^ u:n t:^ tu:n :}
   s" E-SEALED-PACKAGE" OPEN
   96 EMIT1  a u EMIT-STR  96 EMIT1  s"  would change a sealed package" EMIT-STR
   t tu AT  s" sealed" EMIT-STR
   HELP
   s" put this in a package of your own: a sealed package takes no new words"
   EMIT-STR ;

\ The package name t tu that `using` takes holds a colon. It is written through
\ its help, and the caller stops it.
: USING-BAD-NAME ( str -- ) {: t:^ tu:n :}
   s" E-USING-BAD-NAME" OPEN
   s" the package name `" EMIT-STR  t tu EMIT-STR  s" ` contains a colon" EMIT-STR
   t tu AT  s" not a package name" EMIT-STR
   HELP  s" use a top-level package, named without `:`" EMIT-STR ;

\ The `;using` token t tu has no open using to close. It is written through its
\ help, and the caller stops it.
: USING-UNBALANCED ( str -- ) {: t:^ tu:n :}
   s" E-USING-UNBALANCED" OPEN
   96 EMIT1  t tu EMIT-STR  s" ` has no open using to close" EMIT-STR
   t tu AT  s" nothing to close" EMIT-STR
   HELP  s" remove this `" EMIT-STR  t tu EMIT-STR
   s" `, or open a using before it" EMIT-STR ;

\ The `;using` token t tu would close a using opened outside w wu, a package or
\ a file. It is written through its help, and the caller stops it.
: USING-OUTER ( str str -- ) {: t:^ tu:n w:^ wu:n :}
   s" E-USING-OUTER" OPEN
   96 EMIT1  t tu EMIT-STR  s" ` would close a using opened outside this " EMIT-STR
   w wu EMIT-STR
   t tu AT  s" using opened outside" EMIT-STR
   HELP  s" close each using in the " EMIT-STR  w wu EMIT-STR
   s"  that opens it" EMIT-STR ;

private
\ FILE! takes any path a tool names, so a long one is rejected here, after the
\ words that print a rejection. The record stands at the path where a source
\ holds it; one from a command line or the environment is in none.
: PATH-COPY ( str ^u8 -- n ) {: a:^ u:n d:^ :}
   u PATH-CAP > IF
      s" REJECTION:FILE!" u PATH-CAP PATH-RANGE
      a PLACES? IF  a u AT  s" too long" EMIT-STR  THEN
      HELP  s" name the file by a path of at most " EMIT-STR
      PATH-CAP EMIT-U  s"  bytes" EMIT-STR
      EXIT-STATUS:REJECTED STOP
   THEN
   a d u COPY  u ;
: FILE-COPY ( str -- )
   FILE-BUF PATH-COPY FILE-U ! ;
public
: FILE! ( ^u8 n -- )
   FILE-COPY  -1 NAMED ! ;
\ Places in the bytes of the file p pu name label a u; the files it loads are the program's.
: FILE-LABEL! ( ^u8 n ^u8 n -- ) {: a:^ u:n p:^ pu:n :}
   a u LABEL-NAME-BUF PATH-COPY LABEL-NAME-U !
   p pu LABEL-BUF PATH-COPY LABEL-U !
   -1 LABELED ! ;
\ Places in the bytes of the file being read name this file; the files it
\ loads are the program's, not the tool's own.
: LABEL! ( ^u8 n -- )
   PATH$ FILE-LABEL! ;

private
\ The bytes of the labels PRESERVE-LABEL holds, innermost last: LABELS-U bytes
\ at LABELS-P, grown to fit. A save is found by its offset, since a nested
\ save may move them.
: LABELS-BASE ( -- ^u8 ) BLOCK LABELS-BASE-OFF + ;
: LABELS-P ( -- ^^u8 ) LABELS-P-OFF PTR-AT ;
: LABELS-CAP ( -- ^n ) LABELS-CAP-OFF CELL-AT ;
: LABELS-U ( -- ^n ) LABELS-U-OFF CELL-AT ;

: LABELS-PUSH ( str -- ) {: a:^ u:n :}
   LABELS-P @ LABELS-BASE LABELS-CAP @ LABELS-U @ u FIT  LABELS-CAP !  LABELS-P !
   a LABELS-P @ LABELS-U @ + u COPY
   LABELS-U @ u + LABELS-U ! ;

\ The label's bytes are saved; where they start, their lengths, LABELED and
\ NAMED stay on the stack for LABEL-RESTORE.
: LABEL-SAVE ( -- n n n n n n )
   LABELS-U @  FILE-BUF FILE-U @ LABELS-PUSH  LABEL-BUF LABEL-U @ LABELS-PUSH
   LABEL-NAME$ LABELS-PUSH
   FILE-U @ LABEL-U @ LABEL-NAME-U @ LABELED @ NAMED @ ;

: LABEL-RESTORE ( n n n n n n -- )
   {: at:n fu:n lu:n nu:n on:n nm:n :}
   LABELS-P @ at + {: k:^ :}
   k FILE-BUF fu COPY  fu FILE-U !
   k fu + LABEL-BUF lu COPY  lu LABEL-U !
   k fu + lu + LABEL-NAME-BUF nu COPY  nu LABEL-NAME-U !
   on LABELED !  nm NAMED !  at LABELS-U ! ;
public

\ Runs q, then puts back the label it found, on a throw too.
: PRESERVE-LABEL ( [ -- ] -- ) {: q :}
   LABEL-SAVE  q catch {: rc:n :}
   LABEL-RESTORE  rc 0 <> IF rc throw THEN ;

\ Runs q with a u as the token the reader walking the subject is on, then puts
\ back the one it found, on a throw too.
: READING ( ^u8 n [ -- ] -- ) {: a:^ u:n q :}
   READER-A @ READER-U @ {: was:^ wu:n :}
   a READER-A !  u READER-U !
   q catch {: rc:n :}
   was READER-A !  wu READER-U !  rc 0 <> IF rc throw THEN ;

private
\ Unmaps the room of c bytes at p unless it is the first room f or none.
: ROOM-FREE ( ^u8 ^u8 n -- ) {: p:^ f:^ c:n :}
   c 0=  p f =  or IF EXIT THEN
   p c ARENA-UNMAP ;
public

\ Sends the task's capture, unmaps the rooms it grew and zeroes the block's
\ cells, so none names an unmapped room; TASK calls it as each activation ends.
: FREE ( -- )
   RELEASE
   TEXT-P @ TEXT-BASE TEXT-CAP @ ROOM-FREE
   HELD-P @ HELD-BASE HELD-CAP @ ROOM-FREE
   SAVED-P @ SAVED-BASE SAVED-CAP @ ROOM-FREE
   LABELS-P @ LABELS-BASE LABELS-CAP @ ROOM-FREE
   BLOCK CELLS-BYTES ARENA-ZERO ;
;package
