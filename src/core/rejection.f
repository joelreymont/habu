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
\ TEXT holds the record being built, TEXT-LEN bytes of it. Each growable room
\ here starts in TEXT-FIRST bytes of its own scratch storage, so a record is
\ written after the system refuses memory; a larger room is mapped.
16384 constant TEXT-FIRST                \ bytes of each first room
create TEXT-BASE TEXT-FIRST SCRATCH-ALLOT
create HELD-BASE TEXT-FIRST SCRATCH-ALLOT
create SAVED-BASE TEXT-FIRST SCRATCH-ALLOT
PTR-VARIABLE TEXT-P
SCRATCH-VARIABLE TEXT-CAP               \ bytes at TEXT-P, 0 for none
variable IN-JSON   0 IN-JSON !          \ inside a JSON string: EMIT1 escapes
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
variable PART   NO-PART PART !
variable BUF-ON
variable BUF-FD   2 BUF-FD !
\ The capture, grown as TEXT is, holds HELD-U bytes; CAPTURE empties it.
PERSISTED-PTR-VARIABLE HELD-P   NULL-PTR HELD-P !
variable HELD-CAP   0 HELD-CAP !        \ bytes at HELD-P, 0 for none
variable HELD-U   0 HELD-U !
\ The record SAVE kept, SAVED-U bytes, in a buffer grown as the capture is.
PERSISTED-PTR-VARIABLE SAVED-P   NULL-PTR SAVED-P !
variable SAVED-CAP   0 SAVED-CAP !
variable SAVED-U   0 SAVED-U !
create NUM-BUF 20 allot  variable NUM-V  variable NUM-N

public
variable DEST   0 DEST !                 \ 0 = stdout, 1 = TEXT (sig recording)
variable TEXT-LEN
\ The main DATA's REJECTION-JSON-CELL from any thread: a task's region is not
\ the main one, and main reads data-base because a cross build's host is not
\ mapped at DATA-VA. No signature: `^n` would reject a stored flag, and the
\ inferred row keeps data-base's kind, which takes a flag and rejects an address.
: JSON
   data-base TASK-TCB-CELL + @ 0= IF data-base REJECTION-JSON-CELL + EXIT THEN
   data-base  DATA-VA data-base NULL-PTR - -  +  REJECTION-JSON-CELL + ;
\ QUIET counts nested quiet-check scopes. It is created before the check
\ hook exists, so no effect is published for it.
variable QUIET
variable TALLY   0 TALLY !               \ rejections counted this load

private
: TEXT-MAP ( -- )
   TEXT-BASE TEXT-P !  TEXT-FIRST TEXT-CAP ! ;


public
: TEXT ( -- ^u8 )
   TEXT-CAP @ 0= IF TEXT-MAP THEN
   TEXT-P @ ;
private


\ Moves the room of c bytes at p, whose first room is f, to nc fresh bytes that
\ hold its c bytes. The first room is image storage: it is zeroed, not unmapped.
: ROOM-GROW ( ^u8 ^u8 n n -- ^u8 ) {: p:^ f:^ c:n nc:n :}
   nc ARENA-ALLOC-BYTES {: q:^ :}
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
   PART @ DROP-PART = IF EXIT THEN
   PART @ GAP-PART = IF  LABEL-PART PART !  32 EMIT-RAW  THEN
   IN-JSON @ IF c JSON-CHAR ELSE c EMIT-RAW THEN ;
: JSON-OPEN ( -- )   34 EMIT-RAW  -1 IN-JSON ! ;
: JSON-CLOSE ( -- )  0 IN-JSON !  34 EMIT-RAW ;

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
   BUF-FD @ HELD-P @ HELD-U @ write drop
   0 HELD-U ! ;

: FD! ( n -- )
   BUF-FD ! ;

: BUFFER$ ( -- ^u8 n )
   HELD-P @ HELD-U @ ;
private

: BUF-APPEND ( ^u8 n -- )
   {: a:^ u:n :}
   BUF-ON @ 0= IF BUF-FD @ a u write drop EXIT THEN
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
: JSON-STRING ( ^u8 n -- )
   JSON-OPEN EMIT-STR JSON-CLOSE ;
: JSON-KEY ( ^u8 n -- ) {: a:^ u:n :}
   a u JSON-STRING  58 EMIT1 ;

\ Where a top step's token sits: the active file's path, which an inner
\ `evaluate` never changes. FILE-PATH is private so no program writes it.
SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL FILE-PATH
public

\ The active file's path; empty when no file is being read.
: PATH$ ( -- ^u8 n )
   FILE-PATH @ data-base SRCLOC:PATHLEN-CELL + @ ;

\ The file a rejection names when no file holds its source.
: NO-FILE$ ( -- ^u8 n ) s" <input>" ;

private
create FILE-BUF PATH-CAP 1 + allot   variable FILE-U
public
: FILE$ ( -- ^u8 n ) FILE-BUF FILE-U @ ;
private
\ LABEL! labels one file, the one the load was reading when it ran; a label set
\ with no file open names the steps that have no file. LABEL-BUF holds that
\ file's path, empty for none; a top-level step in it names the label, and one
\ in any other file names its own path.
\ The boot's NO-FILE$ is no label.
variable LABELED
\ Whether a tool named a file by FILE!.
variable NAMED
create LABEL-BUF PATH-CAP allot   variable LABEL-U
: COPY ( ^u8 ^u8 n -- ) {: a:^ d:^ u:n :}
   0 BEGIN dup u < WHILE
      dup a + c@
      over d + c!
      1 +
   REPEAT drop ;
: LABELS? ( ^u8 n -- bool ) {: a:^ u:n :}
   LABELED @ 0= IF 0 0= 0= EXIT THEN
   a u LABEL-BUF LABEL-U @ CORE-STR= ;
\ The file line, column and byte of the byte p points at in a buffer whose first
\ byte sits at file line bl, column bc and byte bb. LF ends a line, a column
\ counts bytes, and only the buffer's first line starts at column bc.
private
variable POS-OFF  variable POS-LINE  variable POS-COLUMN  variable POS-I
public
: POSITION ( ^u8 ^u8 n n n -- n n n )
   {: base:^ p:^ bl:n bc:n bb:n :}
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
: SOURCE! ( ^u8 n n n n -- )
   {: a:^ u:n line:n col:n byte:n :}
   a SOURCE-A !  u SOURCE-U !
   line SOURCE-LINE !  col SOURCE-COLUMN !  byte SOURCE-BYTE !
   MAP-OFF ;
: MAP! ( ^u8 n ^n n -- )
   {: a:^ u:n rows:^ count:n :}
   a MAP-A !  u MAP-U !  rows ROW-A !  count ROW-N !  0 ROW-BIAS !
   CLOSER-OFF ;
\ Set after MAP!, which clears it.
: MAP-BIAS! ( n -- )
   ROW-BIAS ! ;
\ The token a u that closes the text whose first byte is base, which the text
\ does not hold: a structure still open there is rejected at it, as the load
\ rejects it.
: CLOSER! ( ^u8 n ^u8 -- )
   {: a:^ u:n base:^ :}
   a CLOSER-A !  u CLOSER-U !  base CLOSER-BASE ! ;
: SOURCE$ ( -- ^u8 n ) SOURCE-A @ SOURCE-U @ ;
\ Whether a closer was given for the text whose first byte is base.
: CLOSES? ( ^u8 -- bool ) CLOSER-BASE @ - 0= CLOSER-U @ 0 > and ;
: CLOSER$ ( -- ^u8 n ) CLOSER-A @ CLOSER-U @ ;
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
: MAP-BUILDER ( n -- ^u8 n )
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
\ The file a top-level step names; empty when no file or label names it.
: LOAD-FILE$ ( -- ^u8 n )
   PATH$ {: pa:^ pu:n :}
   pa pu LABELS? IF FILE$ EXIT THEN
   pa pu ;

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
\ The newest record's place as AT copies it: its file's path and first byte.
\ FILE$ and a loading file's path fit PATH-CAP bytes.
create LAST-BUF PATH-CAP allot   variable LAST-U   0 LAST-U !   variable LAST-AT   -1 LAST-AT !
\ A token in an evaluated text is placed where the evaluation ran, followed by
\ the line EVALUATED-A EVALUATED-U, the token EVAL-LEAD-U bytes in.
variable EVALUATED   0 EVALUATED !
PTR-VARIABLE EVALUATED-A  variable EVALUATED-U
PTR-VARIABLE EVAL-TOKEN-A  variable EVAL-TOKEN-U  variable EVAL-LEAD-U
EVAL-TOP-CELL RESERVED-PTR-U8-CELL EVAL-TOP
\ A token no source holds is placed at the token READER-A READER-U the reader
\ walking the subject is on, an empty one for none, else at RUNNING, the token
\ the engine read last; the label is followed by a `word` note naming the token
\ WORD-A WORD-U; WORD-U is 0 for no note.
PTR-VARIABLE READER-A  variable READER-U   0 READER-U !
TKA-CELL RESERVED-PTR-U8-CELL RUNNING
PTR-VARIABLE WORD-A  variable WORD-U   0 WORD-U !
\ Bytes of a prose line or quoted text shown: rustc's width when its output is
\ not a terminal. A longer one shows a window, `...` in place of each cut end.
140 constant WIDTH
40 constant ROOM                        \ bytes shown before a windowed token

\ Whether byte c continues a UTF-8 character, taking no column of its own.
: CONT? ( n -- bool )  192 and 128 = ;
\ The offset at or before n of the bytes at a where a UTF-8 character starts.
: CHAR-START ( ^u8 n -- n ) {: a:^ n:n :}
   n BEGIN dup 0 > WHILE
      dup a + c@ CONT? 0= IF EXIT THEN
      1 -
   REPEAT ;
\ The offset of the first byte c in the u bytes at a, else u.
: BYTE-AT ( ^u8 n n -- n ) {: a:^ u:n c:n :}
   0 BEGIN dup u < WHILE
      dup a + c@ c = IF EXIT THEN
      1 +
   REPEAT ;
public
\ Emits the program text a u a note quotes. In prose it shows one line of at
\ most WIDTH bytes: past its first line or its first WIDTH - 3 bytes, the rest
\ becomes `...`. Other text a note holds, a path or a type, shows in full.
: EMIT-CUT ( ^u8 n -- ) {: a:^ u:n :}
   JSON @ IF a u EMIT-STR EXIT THEN
   a u 10 BYTE-AT {: f:n :}
   f u =  u WIDTH <= and IF a u EMIT-STR EXIT THEN
   a  a f WIDTH 3 - min CHAR-START  EMIT-STR
   s" ..." EMIT-STR ;
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

: PLACE-FILE! ( ^u8 n -- ) {: a:^ u:n :}
   a PLACE-FILE-A !  u PLACE-FILE-U ! ;
: PLACE-FILE$ ( -- ^u8 n )   PLACE-FILE-A @ PLACE-FILE-U @ ;
: OWNED$ ( -- ^u8 n )   OWNED @ data-base SRCLOC:OWNEDLEN-CELL + @ ;
PTR-VARIABLE ROOT-A  variable ROOT-U   0 ROOT-U !
public
\ The root stream's text, set while the loader runs it; empty outside it.
: ROOT-TEXT! ( ^u8 n -- ) {: a:^ u:n :}   a ROOT-A !  u ROOT-U ! ;
: ROOT-TEXT$ ( -- ^u8 n )   ROOT-A @ ROOT-U @ ;
private
: INPUT$ ( -- ^u8 n )
   INPUT-START @ {: b:^ :}
   b  INPUT-END @ b - ;
: HOLDS? ( ^u8 n ^u8 n -- bool ) {: b:^ bu:n t:^ tu:n :}
   t b - {: at:n :}
   at 0 >=  at tu + bu <= and ;
\ Whether the armed source holds the n bytes at p: its map's text, outside a
\ run it does not copy, else its own bytes.
: ARMED? ( ^u8 n -- bool ) {: p:^ n:n :}
   SOURCE-U @ 0 = IF 0 0= 0= EXIT THEN
   ROW-N @ 0 >  MAP-A @ MAP-U @ p n HOLDS?  and IF p MAP-A @ - MAP-NONE? 0= EXIT THEN
   SOURCE$ p n HOLDS? ;
\ The line of b's bu bytes that holds t: from the byte after the last LF before
\ t up to the next LF, or the buffer's edge.
: LINE! ( ^u8 n ^u8 -- ) {: b:^ bu:n t:^ :}
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
: SOURCE-PLACE ( n ^u8 n -- ) {: off:n t:^ tu:n :}
   off SOURCE-POSITION POSITION!
   t tu + >SOURCE IF SOURCE-BYTE @ + ELSE drop AT-BYTE @ tu + THEN AT-END !
   SOURCE-A @ off + {: s:^ :}
   s CUT
   OWNED$ s 0 HOLDS? IF LOAD-FILE$ ELSE FILE$ THEN PLACE-FILE! ;
: BUFFER-PLACE ( ^u8 n ^u8 n -- ) {: b:^ bu:n t:^ tu:n :}
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
: BODY>INPUT ( ^u8 n -- ^u8 ) {: t:^ tu:n :}
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
: SAVED$ ( ^u8 -- ^u8 n ) {: f:^ :}
   f EVAL-INB FRAME@ {: b:^ :}
   b  f EVAL-INE FRAME@ b - ;
\ Whether frame f is the first evaluation its input ran: f interrupted the
\ active file's bytes, or the frame it runs in is the outermost, a REPL line's.
: INVOKED? ( ^u8 -- bool ) {: f:^ :}
   OWNED$ f EVAL-INB FRAME@ 0 HOLDS?
   f OUTER OUTER NULL-PTR =  or ;
\ The frame of the first evaluation, outward from the innermost; NULL-PTR when
\ the input read is no evaluated text.
: INVOKER ( -- ^u8 )
   EVAL-TOP @
   dup NULL-PTR = IF EXIT THEN
   dup OUTER NULL-PTR = IF drop NULL-PTR EXIT THEN
   BEGIN dup INVOKED? 0= WHILE OUTER REPEAT ;
\ Records the line LINE! found as the evaluated place of the token t tu.
: EVALUATED! ( ^u8 n -- ) {: t:^ tu:n :}
   LINE-A @ EVALUATED-A !  LINE-U @ EVALUATED-U !  LEAD-U @ EVAL-LEAD-U !
   t EVAL-TOKEN-A !  tu EVAL-TOKEN-U ! ;
\ Whether an evaluated text inside the invoker inv holds the token t tu: the
\ buffer read, or one an inner frame interrupted. Records the token's evaluated
\ place, and answers the token in that text.
: IN-EVALUATED? ( ^u8 n ^u8 -- ^u8 bool ) {: t:^ tu:n inv:^ :}
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
: TOKEN-BEFORE ( ^u8 ^u8 -- ^u8 n ) {: b:^ p:^ :}
   p b - BEGIN dup 0 > IF dup 1 - b + c@ 33 < ELSE 0 0= 0= THEN WHILE 1 - REPEAT
   {: e:n :}
   e BEGIN dup 0 > IF dup 1 - b + c@ 32 > ELSE 0 0= 0= THEN WHILE 1 - REPEAT
   {: s:n :}
   b s +  e s - ;
\ Places at the token the invoker's input had read as the evaluation began, in
\ the active file or else the line it interrupted.
: INVOKE-PLACE ( ^u8 -- ) {: inv:^ :}
   inv SAVED$ {: b:^ bu:n :}
   b inv EVAL-INP FRAME@ TOKEN-BEFORE {: a:^ u:n :}
   u 0= IF EXIT THEN
   -1 EVALUATED !
   OWNED$ a u HOLDS? IF OWNED$ a u BUFFER-PLACE LOAD-FILE$ PLACE-FILE! EXIT THEN
   b bu a u BUFFER-PLACE NO-FILE$ PLACE-FILE! ;
public
\ Whether one source a rejection places from holds the token p n whole while
\ the rejection is written: the armed source, the given closer's bytes or the
\ loader's owned bytes, which stay put for good, or the root stream's text or
\ an evaluated text being run, which stays put while it runs. A zero-length
\ token is held at any offset from 0 to the buffer's length.
: HELD? ( ^u8 n -- bool ) {: p:^ n:n :}
   p n ARMED? IF 0 0= EXIT THEN
   CLOSER$ p n HOLDS? IF 0 0= EXIT THEN
   OWNED$ p n HOLDS? IF 0 0= EXIT THEN
   ROOT-TEXT$ p n HOLDS? IF 0 0= EXIT THEN
   INVOKER {: inv:^ :}
   inv NULL-PTR = IF 0 0= 0= EXIT THEN
   p n inv IN-EVALUATED? nip ;
private
\ Places the token t tu in the armed source holding its first byte, else the
\ loader's owned bytes, else an evaluated text, which places it where the
\ evaluation was run, else the buffer read, holding the whole token, as itself
\ or as the bytes a body's text copied it from; answers whether one did. A
\ token of a run a definer built is placed by none of them.
: PLACE-IN ( ^u8 n -- bool ) {: t:^ tu:n :}
   t >SOURCE IF t tu SOURCE-PLACE 0 0= EXIT THEN drop
   t BUILDER nip 0 > IF 0 0= 0= EXIT THEN
   t tu BODY>INPUT {: s:^ :}
   OWNED$ s tu HOLDS? IF
      OWNED$ s tu BUFFER-PLACE LOAD-FILE$ PLACE-FILE!  0 0= EXIT
   THEN
   INVOKER {: inv:^ :}
   inv NULL-PTR = 0= IF
      t tu inv IN-EVALUATED? nip IF
         inv INVOKE-PLACE  PLACED @ IF 0 0= EXIT THEN
         0 EVALUATED !
      THEN
   THEN
   INPUT$ s tu HOLDS? IF INPUT$ s tu BUFFER-PLACE NO-FILE$ PLACE-FILE!  0 0= EXIT THEN
   0 0= 0= ;
\ Whether PLACE-IN places the byte p points at.
: PLACES-IN? ( ^u8 -- bool ) {: p:^ :}
   p >SOURCE nip IF 0 0= EXIT THEN
   p BUILDER nip 0 > IF 0 0= 0= EXIT THEN
   p 1 BODY>INPUT {: s:^ :}
   OWNED$ s 1 HOLDS? IF 0 0= EXIT THEN
   INPUT$ s 1 HOLDS? IF 0 0= EXIT THEN
   INVOKER {: inv:^ :}
   inv NULL-PTR = IF 0 0= 0= EXIT THEN
   p 1 inv IN-EVALUATED? nip ;
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

: PLACE-JSON ( ^u8 n -- ) {: t:^ tu:n :}
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
: CARETS ( ^u8 n -- ) {: t:^ tu:n :}
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
: SHOW-LINE ( ^u8 n n -- ) {: a:^ u:n k:n :}
   a u k WINDOW {: s:n e:n :}
   s 0 > IF s" ..." EMIT-STR THEN
   a s + e s - EMIT-STR
   e u < IF s" ..." EMIT-STR THEN ;
\ The carets under the token tu bytes long that starts k bytes into the line
\ a u, up to its window's end.
: SHOW-CARETS ( ^u8 n n n -- ) {: a:^ u:n k:n tu:n :}
   a u k WINDOW {: s:n e:n :}
   s 0 > IF 3 BLANKS THEN
   a s + k s - LEAD
   a k +  tu e k - > IF e k - ELSE tu THEN CARETS ;
: PLACE-PROSE ( ^u8 n -- ) {: t:^ tu:n :}
   GUTTER @ BLANKS  s" --> " EMIT-STR  PLACE-FILE$ EMIT-STR
   58 EMIT1  AT-LINE @ EMIT-U  58 EMIT1  AT-COLUMN @ EMIT-U  10 EMIT1
   MARGIN 124 EMIT1 10 EMIT1
   AT-LINE @ EMIT-U  s"  | " EMIT-STR  LINE-A @ LINE-U @ LEAD-U @ SHOW-LINE  10 EMIT1
   MARGIN s" | " EMIT-STR  LINE-A @ LINE-U @ LEAD-U @ tu SHOW-CARETS ;

\ A note's key; its value follows.
: NOTE-KEY ( ^u8 n -- ) {: k:^ ku:n :}
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
   s" = evaluated: " {: k:^ ku:n :}
   EVALUATED-A @ EVALUATED-U @ EVAL-LEAD-U @ {: a:^ u:n lead:n :}
   MARGIN  k ku EMIT-STR  a u lead SHOW-LINE  10 EMIT1
   MARGIN  ku BLANKS  a u lead EVAL-TOKEN-U @ SHOW-CARETS ;
\ Ends the open text; the label's end, or a dropped label's, writes the
\ evaluated place JSON still owes, then the `word` note.
: CLOSE-PART ( -- )
   END-TEXT
   PART @ LABEL-PART = 0= IF EXIT THEN
   EVALUATED @ IF  0 EVALUATED !  EVALUATED-JSON  THEN
   WORD-U @ 0= IF EXIT THEN
   s" word" NOTE-KEY  WORD-A @ WORD-U @ EMIT-STR  0 WORD-U !  END-TEXT ;

\ Closes a notes object no HELP closed, then the record.
: CLOSE-RECORD ( -- )
   CLOSE-PART
   JSON @ IF  NOTES @ IF 125 EMIT1 THEN  125 EMIT1  THEN  10 EMIT1
   NO-PART PART ! ;

\ Ends the record and sends it, or drops it under QUIET.
: END-RECORD ( -- )
   CLOSE-RECORD
   QUIET @ 0= IF FLUSH ELSE 0 TEXT-LEN !  0 DEST ! THEN ;

variable WARNING   0 WARNING !           \ the open record is a warning
: BEGIN-RECORD ( ^u8 n ^u8 n -- ) {: sev:^ su:n code:^ cu:n :}
   1 DEST !  0 TEXT-LEN !  0 NOTES !  1 GUTTER !  0 EVALUATED !  0 WORD-U !
   0 LAST-U !  -1 LAST-AT !
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
\ Places the token t tu; its label follows. A token no source holds has no
\ place, so the record writes none and drops the label. In prose an evaluated
\ token's call is labelled as such, and the label follows the evaluated place.
: AT ( ^u8 n -- ) {: t:^ tu:n :}
   END-TEXT
   t tu PLACE
   PLACED @ 0= IF DROP-PART PART ! EXIT THEN
   PLACE-FILE$ {: f:^ fu:n :}
   f LAST-BUF fu COPY  fu LAST-U !  AT-BYTE @ LAST-AT !
   AT-LINE @ DIGITS GUTTER !
   \ The placed bytes as the file holds them: a checked copy stands its tokens
   \ one blank apart, so a span across blanks is not the copy's length.
   LINE-A @ LEAD-U @ +  AT-END @ AT-BYTE @ - {: p:^ pu:n :}
   JSON @ IF p pu PLACE-JSON  LABEL-PART PART !  EXIT THEN
   p pu PLACE-PROSE
   EVALUATED @ IF
      0 EVALUATED !
      s"  evaluates the text below" EMIT-STR  10 EMIT1  EVALUATED-PROSE
   THEN
   GAP-PART PART ! ;
\ The path and first byte of the newest record's place, the byte -1 when it
\ had none. The path is borrowed until the next record is placed.
: LAST-PLACE ( -- ^u8 n n )   LAST-BUF LAST-U @ LAST-AT @ ;
\ Drops the `word` note AT added to its place.
: NO-WORD ( -- )   0 WORD-U ! ;
\ A note's key; its value follows.
: NOTE ( ^u8 n -- )
   CLOSE-PART NOTE-KEY ;
\ Opens a record with code c and message m, and places the token t tu with
\ label l; the label may go on.
: OPEN-AT ( ^u8 n ^u8 n ^u8 n ^u8 n -- )
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
      NOTES @ IF 125 EMIT1  0 NOTES ! THEN
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

\ Throws n as the throw that carries the kept record.
: PASS ( n -- )
   data-base THROWS-CELL + @ 1 +  data-base STAMP-CELL + !  throw ;

\ Whether the newest throw this thread began carries the kept record.
: CARRIED? ( -- bool )
   data-base STAMP-CELL + @  data-base THROWS-CELL + @  = ;

\ The word kw ku has no operand after it, which what names with its article
\ ("a name"): the record stands at t tu, which why labels. It is written
\ through the start of its help: the caller writes the help and stops it.
: MISSING-OPERAND ( ^u8 n ^u8 n ^u8 n ^u8 n -- )
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
: NOT-A-NAME ( ^u8 n ^u8 n -- ) {: kw:^ ku:n t:^ tu:n :}
   t tu s" (" CORE-STR= IF
      kw ku t tu s" a signature, not a name" MISSING-NAME
      CHECKER-REJECT-RC STOP
   THEN
   t tu s" \" CORE-STR= IF
      kw ku t tu s" a comment, not a name" MISSING-NAME
      CHECKER-REJECT-RC STOP
   THEN ;

\ The counted string the literal's opener t tu opens is longer than the max
\ bytes its count byte holds. It is written through its help, and the caller
\ stops it.
: COUNTED-RANGE ( ^u8 n n -- ) {: t:^ tu:n max:n :}
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
: RANGE-NOTE ( ^u8 n -- ) {: t:^ tu:n :}
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
: LITERAL-RANGE ( ^u8 n -- ) {: t:^ tu:n :}
   s" E-LITERAL-RANGE" OPEN
   s" the number `" EMIT-STR  t tu EMIT-STR  s" ` is out of range" EMIT-STR
   t tu AT  s" does not fit in a cell" EMIT-STR
   t tu RANGE-NOTE
   HELP  s" write a number in that range" EMIT-STR ;

\ Writes the record of word w given a path len bytes long, past cap, the
\ longest length the failing test allows, through its message: the caller may
\ write one AT, then writes the help and ends the record. len is 0 for an empty
\ path and -1 when only the path it resolves to is longer. A w of length 0
\ names no word: the message's subject is the path.
: PATH-RANGE ( ^u8 n n n -- ) {: w:^ wu:n len:n cap:n :}
   s" E-PATH-RANGE" OPEN
   wu 0= IF
      len 0= IF s" a path is empty" EMIT-STR  EXIT THEN
      len 0 < IF
         s" a path resolves to more than the " EMIT-STR
      ELSE
         s" a path is " EMIT-STR  len EMIT-U
         s"  bytes long, more than the " EMIT-STR
      THEN
   ELSE
      96 EMIT1  w wu EMIT-STR
      len 0= IF s" ` got an empty path" EMIT-STR  EXIT THEN
      len 0 < IF
         s" ` got a path that resolves to more than the " EMIT-STR
      ELSE
         s" ` got a path " EMIT-STR  len EMIT-U
         s"  bytes long, more than the " EMIT-STR
      THEN
   THEN
   cap EMIT-U  s"  bytes a path may have" EMIT-STR ;

\ The name a u is already defined where it is defined again: the record stands
\ at t tu. It is written through its help, and the caller stops it.
: DUPLICATE ( ^u8 n ^u8 n -- ) {: a:^ u:n t:^ tu:n :}
   s" E-DUPLICATE-DEFINITION" OPEN
   96 EMIT1  a u EMIT-STR  s" ` is already defined in this wordlist" EMIT-STR
   t tu AT  s" already defined" EMIT-STR
   HELP  s" rename this word, or remove the earlier definition" EMIT-STR ;

\ The does> clause of definer stem, named stem then suffix, takes a name
\ already defined: the record stands at t tu. It is written through its help,
\ and the caller stops it.
: CLAUSE-TAKEN ( ^u8 n ^u8 n ^u8 n -- )
   {: stem:^ stemu:n sfx:^ sfxu:n t:^ tu:n :}
   s" E-DUPLICATE-DEFINITION" OPEN
   96 EMIT1  stem stemu EMIT-STR  sfx sfxu EMIT-STR
   s" ` is already defined in this wordlist" EMIT-STR
   t tu AT  s" its clause's name is taken" EMIT-STR
   HELP  s" rename `" EMIT-STR  stem stemu EMIT-STR
   s" `, or the word already named `" EMIT-STR
   stem stemu EMIT-STR  sfx sfxu EMIT-STR  96 EMIT1 ;

\ The status a load stops with when the dictionary has no record left.
77 constant DICT-FULL-RC

\ Rejects a definition of NAME when the dictionary has no record left, placed
\ at PLACE.
: DICTIONARY-FULL ( ^u8 n ^u8 n -- ) {: a:^ u:n t:^ tu:n :}
   s" E-DICTIONARY-FULL" OPEN
   s" the dictionary has no record left for `" EMIT-STR  a u EMIT-STR  96 EMIT1
   t tu AT  s" no record left" EMIT-STR
   HELP  s" define fewer words in this load" EMIT-STR
   DICT-FULL-RC STOP ;

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
: USING-BAD-NAME ( ^u8 n -- ) {: t:^ tu:n :}
   s" E-USING-BAD-NAME" OPEN
   s" the package name `" EMIT-STR  t tu EMIT-STR  s" ` contains a colon" EMIT-STR
   t tu AT  s" not a package name" EMIT-STR
   HELP  s" use a top-level package, named without `:`" EMIT-STR ;

\ The `;using` token t tu has no open using to close. It is written through its
\ help, and the caller stops it.
: USING-UNBALANCED ( ^u8 n -- ) {: t:^ tu:n :}
   s" E-USING-UNBALANCED" OPEN
   96 EMIT1  t tu EMIT-STR  s" ` has no open using to close" EMIT-STR
   t tu AT  s" nothing to close" EMIT-STR
   HELP  s" remove this `" EMIT-STR  t tu EMIT-STR
   s" `, or open a using before it" EMIT-STR ;

\ The `;using` token t tu would close a using opened outside w wu, a package or
\ a file. It is written through its help, and the caller stops it.
: USING-OUTER ( ^u8 n ^u8 n -- ) {: t:^ tu:n w:^ wu:n :}
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
: PATH-COPY ( ^u8 n ^u8 -- n ) {: a:^ u:n d:^ :}
   u PATH-CAP > IF
      s" REJECTION:FILE!" u PATH-CAP PATH-RANGE
      a PLACES? IF  a u AT  s" too long" EMIT-STR  THEN
      HELP  s" name the file by a path of at most " EMIT-STR
      PATH-CAP EMIT-U  s"  bytes" EMIT-STR
      CHECKER-REJECT-RC STOP
   THEN
   a d u COPY  u ;
: FILE-COPY ( ^u8 n -- )
   FILE-BUF PATH-COPY FILE-U ! ;
NO-FILE$ FILE-COPY
public
: FILE! ( ^u8 n -- )
   FILE-COPY  -1 NAMED ! ;
\ Places in the bytes of the file being read name this file; the files it
\ loads are the program's, not the tool's own.
: LABEL! ( ^u8 n -- )
   FILE-COPY
   PATH$ LABEL-BUF PATH-COPY LABEL-U !
   -1 LABELED ! ;

private
\ The bytes of the labels PRESERVE-LABEL holds, innermost last: LABELS-U bytes
\ at LABELS-P, grown to fit. A save is found by its offset, since a nested
\ save may move them.
create LABELS-BASE TEXT-FIRST SCRATCH-ALLOT
PERSISTED-PTR-VARIABLE LABELS-P   NULL-PTR LABELS-P !
variable LABELS-CAP   0 LABELS-CAP !
variable LABELS-U   0 LABELS-U !

: LABELS-PUSH ( ^u8 n -- ) {: a:^ u:n :}
   LABELS-P @ LABELS-BASE LABELS-CAP @ LABELS-U @ u FIT  LABELS-CAP !  LABELS-P !
   a LABELS-P @ LABELS-U @ + u COPY
   LABELS-U @ u + LABELS-U ! ;

\ The label's bytes are saved; where they start, their lengths, LABELED and
\ NAMED stay on the stack for LABEL-RESTORE.
: LABEL-SAVE ( -- n n n n n )
   LABELS-U @  FILE$ LABELS-PUSH  LABEL-BUF LABEL-U @ LABELS-PUSH
   FILE-U @ LABEL-U @ LABELED @ NAMED @ ;

: LABEL-RESTORE ( n n n n n -- )
   {: at:n fu:n lu:n on:n nm:n :}
   LABELS-P @ at + {: k:^ :}
   k FILE-BUF fu COPY  fu FILE-U !
   k fu + LABEL-BUF lu COPY  lu LABEL-U !
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
;package
