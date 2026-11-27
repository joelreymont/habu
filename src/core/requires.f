\ requires.f - the require check: a file calls its own words, the words of
\ the files it requires directly and the words of no file. A file read by a
\ require or as an entry has an id from a counter that only goes up (MINT);
\ text it includes is its own. Id 0 is no file: a provided file's records, the
\ REPL's and a kernel primitive's, callable from anywhere. Text a word evaluates
\ is no file's: its calls are not checked, nor is a call read while no file or
\ id 0 is current, and a require it makes grants nothing, so a verdict rests on
\ a file's own text. A record such text makes is the file's being read.
\ Under the design seal (POLICY-NDICT-CELL) a record older than the seal is the
\ seal's alone to judge, and a sealed file's own words follow the rule above.
\ A file under the policy's open root can require no file outside it, so it
\ calls those files' words with no require.
\ The image's records carry their files' ids, 1 to IMAGE-N, which the
\ build's end file states (SPAN, PERSIST); MINT goes on above them.
\
\ Each dictionary slot holds its record's file id. The loader names the file
\ being read (CURRENT!) as a frame opens and closes; STAMP gives the slots
\ made since the last change the id that was current, and a slot at or above
\ the stamped count is the current file's. Where the engine lowered its count
\ (a build's end, a replay) the stamped count follows it down. The
\ ids are one table of DICT-CAP cells, mapped by the first STAMP, which fills
\ the image's records from its spans: a frame's close, which an uncaught throw
\ under an address-space limit runs, maps nothing.
\
\ A diagnostic spells the missing require for its reader: an engine root's file
\ by its name there, unless that root holds that name; any other as the
\ reader's require of the file nearest it is spelled, where that spelling names
\ a directory above both and, read under the root the reader's next require
\ resolves under, still names that file, as one a SOURCE-ROOT:WITH body made
\ under another root does not; else by its path from the root the reader's next
\ require resolves under, through `..` where it is not below it, or by its
\ canonical path where the reader has no root. `require` reads one
\ token, so a path with a blank is spelled for `required`.
\
\ The edges, (requirer, required) pairs, are an open-addressed set of cells
\ at most half full: each pair is one key, the requirer's id above bit 32. A
\ row per edge holds its key and, in EDGE-TEXTS, the require's spelling.
\
\ CHECK records each record it judges, with ASKS, the requires files made so
\ far, as a compile read, so a link can judge the part's calls again.

package REQUIRES
private

EXPORT BOOT:data-base
PTR-VARIABLE FILE-IDS                \ one id per dictionary slot
SCRATCH-VARIABLE STAMPED             \ slots FILE-IDS holds stamped
SCRATCH-VARIABLE READING             \ the reader: CURRENT@
SCRATCH-VARIABLE MINTED              \ the newest id
PTR-VARIABLE EDGE-SET                \ 0 for an empty cell
SCRATCH-VARIABLE EDGE-CAP            \ in cells, a power of 2
SCRATCH-VARIABLE EDGE-N
PTR-VARIABLE EDGE-KEYS               \ per edge, its key
SCRATCH-VARIABLE EDGE-KEYS-CAP       \ in cells
PTR-VARIABLE EDGE-TEXTS              \ per edge, its spelling's, as PATH-ROWS
SCRATCH-VARIABLE EDGE-TEXTS-CAP      \ in cells
SCRATCH-VARIABLE ASKED               \ the requires files made so far (EDGE)
PTR-VARIABLE PATH-ROWS               \ per id, its path's offset and length
SCRATCH-VARIABLE PATH-ROWS-CAP       \ in cells
PTR-VARIABLE ROOT-ROWS               \ per id, its root's, as PATH-ROWS
SCRATCH-VARIABLE ROOT-ROWS-CAP       \ in cells
PTR-VARIABLE PATH-TEXT               \ the paths', roots' and spellings' bytes
SCRATCH-VARIABLE PATH-TEXT-CAP       \ in cells
SCRATCH-VARIABLE PATH-TEXT-U         \ in bytes
PERSISTED-PTR-VARIABLE CWD-BYTES     \ the working directory (CWD!)
PERSISTED-PTR-VARIABLE CWD-LEN
PERSISTED-PTR-VARIABLE ENGINE-BYTES  \ the engine root (ENGINE!)
PERSISTED-PTR-VARIABLE ENGINE-LEN
PERSISTED-PTR-VARIABLE POLICY-BYTES  \ the policy's root (POLICY-ROOT!)
PERSISTED-PTR-VARIABLE POLICY-LEN
PTR-VARIABLE JOINED                  \ the bytes of UNDER
SCRATCH-VARIABLE JOINED-CAP          \ in bytes
\ The image's: PATH-ROWS for ids to IMAGE-N, then their path bytes; and SPAN-N
\ spans of records, each its first, its end and their file's id.
PERSISTED-PTR-VARIABLE IMAGE-PATHS
variable IMAGE-N
PERSISTED-PTR-VARIABLE IMAGE-SPANS
variable SPAN-N
PTR-VARIABLE SPAN-ROWS               \ the spans an end file states
SCRATCH-VARIABLE SPAN-ROWS-CAP       \ in cells

: UNMAP-CELLS ( ^n n -- ) {: t:^ n:n :}
   t n cells ARENA-UNMAP ;

\ Grows the table at pv, capv cells, to hold need cells; it keeps its cells.
: GROW-CELLS ( ^^n ^n n -- ) {: pv:^ capv:^ need:n :}
   need capv @ <= if exit then
   need capv @ 2 * max 1024 max {: cap:n :}
   cap cells ARENA-ALLOC {: fresh:^ :}
   pv @ {: old:^ :}
   old fresh capv @ ARENA-FRESH-COPY
   old capv @ UNMAP-CELLS
   fresh pv !  cap capv ! ;

: STAMP-SYNC ( -- )
   ndict@ STAMPED @ < if ndict@ STAMPED ! then ;

: SPAN-AT ( n -- ^n ) {: k:n :}
   IMAGE-SPANS @ k 3 * cells + ;

\ Gives span k's records its id; answers its end.
: SPAN-FILL ( n -- n )
   SPAN-AT {: s:^ :}
   s @ {: first:n :}
   s CELL + @ {: end:n :}
   s 2 cells + @ {: id:n :}
   end first ?do id FILE-IDS @ i cells + ! loop
   end ;

\ A fresh table holds the image's spans; the stamped count starts past them.
: IMAGE-STAMP ( -- )
   0 SPAN-N @ 0 ?do i SPAN-FILL max loop
   ndict@ min STAMPED ! ;

public

\ The id of the file that records made now are; 0 for none.
: FILE@ ( -- n ) READING @ {: n:n :} n n negate max ;

private

: STAMP ( -- )
   STAMP-SYNC
   FILE-IDS @ NULL-PTR = if
      DICT-CAP cells ARENA-ALLOC FILE-IDS !
      IMAGE-STAMP
   then
   ndict@ {: n:n :}
   FILE@ {: id:n :}
   n STAMPED @ ?do id FILE-IDS @ i cells + ! loop
   n STAMPED ! ;

\ The id of the file record idx came from.
: FILE-OF ( n -- n ) {: idx:n :}
   idx STAMPED @ >= if FILE@ exit then
   FILE-IDS @ idx cells + @ ;

: EDGE-KEY ( n n -- n ) {: from:n to:n :}
   from 32 lshift to or ;

\ Fibonacci hashing: the product's high bits pick the first cell.
: EDGE-HOME ( n n -- n ) {: key:n cap:n :}
   key $9E3779B97F4A7C15 * 32 rshift cap 1 - and ;

: EDGE-ENDS? ( ^n n n -- bool ) {: t:^ i:n key:n :}
   t i cells + @ {: v:n :}
   v 0=  v key =  or ;

\ The cell of key in the set t of cap cells, else the empty cell after its run.
: EDGE-PROBE ( ^n n n -- n ) {: t:^ cap:n key:n :}
   key cap EDGE-HOME
   begin t over key EDGE-ENDS? 0= while 1 + cap 1 - and repeat ;

: EDGE-PUT ( n ^n n -- ) {: key:n t:^ cap:n :}
   key  t  t cap key EDGE-PROBE cells +  ! ;

: EDGE-REHASH ( n -- ) {: cap:n :}
   cap cells ARENA-ALLOC {: fresh:^ :}
   EDGE-SET @ {: old:^ :}
   EDGE-CAP @ 0 ?do
      old i cells + @ dup 0 = if drop else fresh cap EDGE-PUT then
   loop
   old EDGE-CAP @ UNMAP-CELLS
   fresh EDGE-SET !  cap EDGE-CAP ! ;

: EDGE? ( n n -- bool ) {: from:n to:n :}
   EDGE-CAP @ 0= if 0 0= 0= exit then
   from to EDGE-KEY {: key:n :}
   EDGE-SET @ {: t:^ :}
   t  t EDGE-CAP @ key EDGE-PROBE cells +  @ key = ;

: TEXT! ( str n ^^n ^n -- ) {: s:str id:n rows:^ cap:^ :}
   rows cap id 1 + 2 * GROW-CELLS
   PATH-TEXT PATH-TEXT-CAP PATH-TEXT-U @ s STR:LEN + CELL / 1 + GROW-CELLS
   PATH-TEXT-U @ {: off:n :}
   s STR:DATA  PATH-TEXT @ BYTE-VIEW off +  s STR:LEN >span ARENA-COPY
   off rows @ id 2 * cells + !
   s STR:LEN rows @ id 2 * 1 + cells + !
   off s STR:LEN + PATH-TEXT-U ! ;

: IMAGE-PATH$ ( n -- str ) {: id:n :}
   IMAGE-PATHS @ {: t:^ :}
   t id 2 * cells + @ {: off:n :}
   t IMAGE-N @ 1 + 2 * cells + BYTE-VIEW off +
   t id 2 * 1 + cells + @ >str ;

: TEXT$ ( n ^n -- str ) {: id:n rows:^ :}
   rows id 2 * cells + @ {: off:n :}
   PATH-TEXT @ BYTE-VIEW off +  rows id 2 * 1 + cells + @ >str ;

: PATH$ ( n -- str ) {: id:n :}
   id IMAGE-N @ <= if id IMAGE-PATH$ exit then
   id PATH-ROWS @ TEXT$ ;

: ROOT$ ( n -- str ) {: id:n :}
   id IMAGE-N @ <= if s" " exit then
   id ROOT-ROWS @ TEXT$ ;

: SLASH? ( n str -- bool ) {: k:n s:str :}
   k s STR:LEN >= if 0 0= 0= exit then
   s STR:DATA k + c@ 47 = ;

: DIR$ ( str -- str ) {: p:str :}
   p STR:LEN begin 1 - dup p SLASH? until {: k:n :}
   p STR:DATA k >str ;

: PORTABLE? ( str -- bool ) {: s:str :} 0 s SLASH? 0= ;

: SAME-AT? ( n str str -- bool ) {: k:n a:str b:str :}
   k a STR:LEN >=  k b STR:LEN >=  or if 0 0= 0= exit then
   a STR:DATA k + c@  b STR:DATA k + c@ = ;

\ The bytes of path p, its last `/` one, that name directory r or one above it.
: SHARED ( str str -- n ) {: p:str r:str :}
   0 begin dup p r SAME-AT? while 1 + repeat {: k:n :}
   k r STR:LEN =  k p SLASH?  and if k 1 + exit then
   k begin dup 1 - p SLASH? 0= while 1 - repeat ;

\ The directories of r past its first k bytes.
: UPS ( str n -- n ) {: r:str k:n :}
   k r STR:LEN >= if 0 exit then
   1  r STR:LEN k ?do i r SLASH? if 1 + then loop ;

: PAST ( str n -- str ) {: s:str k:n :}
   s STR:DATA k +  s STR:LEN k - >str ;

: CWD$ ( -- str ) CWD-BYTES @ BYTE-VIEW  CWD-LEN @ @ >str ;

\ The name a record gives file id.
: NAME$ ( n -- str ) {: id:n :}
   id PATH$ {: p:str :}
   CWD$ {: d:str :}
   p PORTABLE?  d STR:LEN 0=  or if p exit then
   p d SHARED {: k:n :}
   d k UPS 0 > if p exit then
   p k PAST ;

: ENGINE$ ( -- str )
   ENGINE-LEN @ @ {: u:n :}
   u 0= if CWD$ exit then
   ENGINE-BYTES @ BYTE-VIEW u >str ;

\ Path p in directory d, a NUL after it, until the next.
: UNDER ( str str -- str ) {: d:str p:str :}
   d STR:LEN 1 + {: at:n :}
   at p STR:LEN + {: u:n :}
   JOINED JOINED-CAP u 1 + u 1 + ARENA-ROOM
   JOINED @ BYTE-VIEW {: b:^ :}
   d STR:DATA b d STR:LEN >span ARENA-COPY
   47 b d STR:LEN + c!
   p STR:DATA  b at +  p STR:LEN >span  ARENA-COPY
   0 b u + c!
   b u >str ;

: HOLDS? ( str str -- bool )
   UNDER STR:DATA 0 access 0= ;

\ The path p of a file, canonical where root r holds an engine file's name.
: FROM-ROOT ( str str -- str ) {: p:str r:str :}
   p PORTABLE? 0= if p exit then
   r p HOLDS? 0= if p exit then
   ENGINE$ p UNDER ;

: POLICY$ ( -- str ) POLICY-BYTES @ BYTE-VIEW  POLICY-LEN @ @ >str ;

create REAL-BUF PATH-CAP 1 + SCRATCH-ALLOT

\ The path p of a file, an engine file's canonical where it resolves.
: REAL$ ( str -- str ) {: p:str :}
   p PORTABLE? 0= if p exit then
   ENGINE$ p UNDER {: q:str :}
   q STR:DATA REAL-BUF PATH-CAP 1 + >span realpath {: n:n :}
   n 0 > if REAL-BUF n >str else q then ;

: OUTSIDE? ( n -- bool )
   PATH$ REAL$ {: q:str :}
   POLICY$ {: r:str :}
   r  q r SHARED  UPS 0 > ;

: HOSTED? ( n n -- bool ) {: cur:n id:n :}
   data-base POLICY-REQUIRE-CELL + @ 0= if 0 0= 0= exit then
   id OUTSIDE? 0= if 0 0= 0= exit then
   cur OUTSIDE? 0= ;

EXPORT POINTER-STORAGE:RESERVED-PTR-U8-CELL
SRCLOC:SOURCE-CELL RESERVED-PTR-U8-CELL LOADER

\ The root a require in file id's text resolves under now.
: BASE$ ( n -- str ) {: id:n :}
   LOADER @ {: rec:^ :}
   rec NULL-PTR = if id ROOT$ exit then
   rec SRCLOC:REC-ROOTLEN + CELL-VIEW @ {: u:n :}
   u 0= if id ROOT$ exit then
   rec SRCLOC:REC-ROOT + 0 ptr-field @  u >str ;

: ENDS? ( str str -- bool ) {: s:str e:str :}
   s STR:LEN e STR:LEN - {: h:n :}
   h 0 < if 0 0= 0= exit then
   e STR:LEN 0 ?do
      i  s h PAST  e SAME-AT? 0= if 0 0= 0= unloop exit then
   loop
   0 0= ;

: EDGE-FROM ( n -- n ) cells EDGE-KEYS @ + @ 32 rshift ;
: EDGE-PATH$ ( n -- str ) cells EDGE-KEYS @ + @ $FFFFFFFF and PATH$ ;
: EDGE-TEXT$ ( n -- str ) EDGE-TEXTS @ TEXT$ ;

create NAMED-BUF PATH-CAP 1 + SCRATCH-ALLOT

\ Whether edge i's spelling, read under its requirer's root now, names its
\ file: one read under another root, as a SOURCE-ROOT:WITH body's, does not.
: NAMES? ( n -- bool ) {: i:n :}
   i EDGE-TEXT$ {: s:str :}
   s PORTABLE? 0= if 0 0= exit then
   i EDGE-FROM BASE$ {: r:str :}
   r STR:LEN 0= if 0 0= 0= exit then
   r s UNDER {: q:str :}
   q STR:DATA NAMED-BUF PATH-CAP 1 + >span realpath {: n:n :}
   n 0 > 0= if 0 0= 0= exit then
   NAMED-BUF n >str  i EDGE-PATH$  STR:= ;

\ The bytes of edge i's path that name a directory holding path t too, where
\ its spelling holds the rest and names its file now; 0 where it does not.
: COMMON ( str n -- n ) {: t:str i:n :}
   i EDGE-PATH$ {: p:str :}
   p PORTABLE? if 0 exit then
   i NAMES? 0= if 0 exit then
   p  t DIR$  SHARED {: k:n :}
   i EDGE-TEXT$  p k PAST  ENDS? if k else 0 then ;

\ The edge of file from with the most bytes in common with path t, and those.
: NEAREST ( str n -- n n ) {: t:str from:n :}
   0 0  EDGE-N @ 0 ?do
      i EDGE-FROM from = if
         t i COMMON  2dup < if nip nip i swap else drop then
      then
   loop ;

\ Path t spelled as file from's require nearest it, or nothing.
: ALIKE ( str n -- str ) {: t:str from:n :}
   t PORTABLE? if s" " exit then
   t from NEAREST {: i:n k:n :}
   k 0= if s" " exit then
   i EDGE-TEXT$ {: s:str :}
   s STR:LEN  i EDGE-PATH$ k PAST STR:LEN  - {: h:n :}
   h 0= if t k PAST exit then
   s STR:DATA h 1 - >str  t k PAST  UNDER ;

\ The path by which file from requires file to: `..` n times, then the rest.
: SPELLED ( n n -- n str ) {: to:n from:n :}
   to PATH$ {: t:str :}
   t from ALIKE {: s:str :}
   s STR:LEN 0 > if 0 s exit then
   from BASE$ {: r:str :}
   r STR:LEN 0= if 0 t exit then
   t r FROM-ROOT {: p:str :}
   p PORTABLE? if 0 p exit then
   p r SHARED {: k:n :}
   r k UPS  p k PAST ;

: BLANK? ( str -- bool ) {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ 33 < if 0 0= unloop exit then
   loop
   0 0= 0= ;

: TICKS ( str -- )
   96 REJECTION:EMIT1  REJECTION:EMIT-STR  96 REJECTION:EMIT1 ;

: EMIT-UPS ( n -- )
   0 ?do s" ../" REJECTION:EMIT-STR loop ;

\ s as the text of an escaped string literal.
: EMIT-ESCAPED ( str -- ) {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@
      dup 34 = over 92 = or if 92 REJECTION:EMIT1 then
      dup 34 = if drop 113 then REJECTION:EMIT1
   loop ;

: REQUIRE-LINE ( n n -- )
   SPELLED {: ups:n s:str :}
   96 REJECTION:EMIT1
   s BLANK? if
      s\" s\\\q " REJECTION:EMIT-STR  ups EMIT-UPS  s EMIT-ESCAPED
      s\" \q required" REJECTION:EMIT-STR
   else
      s" require " REJECTION:EMIT-STR  ups EMIT-UPS  s REJECTION:EMIT-STR
   then
   96 REJECTION:EMIT1 ;

\ The token t calls a word of file id, which the current file does not
\ require.
: UNREQUIRED ( str n -- ) {: t:str id:n :}
   READING @ {: cur:n :}
   id NAME$ {: d:str :}
   cur NAME$ {: f:str :}
   s" E-UNREQUIRED" REJECTION:OPEN
   t TICKS  s"  is defined in " REJECTION:EMIT-STR  d TICKS
   s" , which " REJECTION:EMIT-STR  f TICKS
   s"  does not require" REJECTION:EMIT-STR
   t REJECTION:AT  s" defined in " REJECTION:EMIT-STR  d TICKS
   s" expected" REJECTION:NOTE
   id cur REQUIRE-LINE  s"  in " REJECTION:EMIT-STR  f TICKS
   s" found" REJECTION:NOTE
   s" no such require; one made by another file does not count" REJECTION:EMIT-STR
   REJECTION:HELP
   s" add " REJECTION:EMIT-STR  id cur REQUIRE-LINE
   s"  to " REJECTION:EMIT-STR  f TICKS
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Whether the current file may call a word of file id.
: CALLABLE? ( n -- bool ) {: id:n :}
   READING @ {: cur:n :}
   cur 0 <=  id 0=  or  id cur =  or if 0 0= exit then
   cur id EDGE? if 0 0= exit then
   cur id HOSTED? ;

public

\ A new file id. Its file is at canonical path p and its own requires resolve
\ under root; a portable p, with no `/` first, names an engine root's file, and
\ its root is empty.
: MINT ( str str -- n ) {: p:str root:str :}
   MINTED @ IMAGE-N @ max 1 + {: id:n :}
   p id PATH-ROWS PATH-ROWS-CAP TEXT!
   root id ROOT-ROWS ROOT-ROWS-CAP TEXT!
   id MINTED !
   id ;

\ The working directory is the u@ bytes at a, none until the loader asks for it.
: CWD! ( ^u8 ^n -- ) {: a:^ u:^ :}
   a CELL-VIEW CWD-BYTES !  u CWD-LEN ! ;

\ The engine root is the u@ bytes at a, or the working directory while u@ is 0.
: ENGINE! ( ^u8 ^n -- ) {: a:^ u:^ :}
   a CELL-VIEW ENGINE-BYTES !  u ENGINE-LEN ! ;

\ The policy's root, while one is open, is the u@ bytes at a.
: POLICY-ROOT! ( ^u8 ^n -- ) {: a:^ u:^ :}
   a CELL-VIEW POLICY-BYTES !  u POLICY-LEN ! ;

\ The reader: the id of the file whose text is read, negated while the text is
\ what a word of it evaluates; 0 for no file.
: CURRENT@ ( -- n ) READING @ ;

\ The reader is now n: the records made so far are the last one's file's.
: CURRENT! ( n -- ) STAMP READING ! ;

\ Runs q, which reads text a word evaluates, then the reader it found, on a
\ throw too.
: AS-TEXT ( [ -- ] -- ) {: q :}
   READING @ {: was:n :}
   FILE@ negate READING !
   q catch {: rc:n :}
   was READING !  rc 0 <> if rc throw then ;

\ Runs q with reader n current, then the one it found, on a throw too: text an
\ `include` in q reads is n's.
: WITH-FILE ( n [ -- ] -- ) {: id:n q :}
   READING @ {: was:n :}
   id CURRENT!
   q catch {: rc:n :}
   was CURRENT!  rc 0 <> if rc throw then ;

\ The requires the text of any file made so far.
: ASKS ( -- n ) ASKED @ ;

\ The current file's text requires, as it spells it, file id; text of no file
\ grants nothing.
: EDGE ( str n -- ) {: s:str to:n :}
   READING @ {: from:n :}
   from 0 <= if exit then
   ASKED @ 1 + ASKED !
   to 0=  from to = or if exit then
   from to EDGE-KEY {: key:n :}
   EDGE-N @ 1 + 2 * EDGE-CAP @ > if EDGE-CAP @ 2 * 1024 max EDGE-REHASH then
   EDGE-SET @ {: t:^ :}
   t  t EDGE-CAP @ key EDGE-PROBE cells +  {: at:^ :}
   at @ key = if exit then
   key at !
   EDGE-N @ {: n:n :}
   EDGE-KEYS EDGE-KEYS-CAP n 1 + GROW-CELLS
   key  EDGE-KEYS @ n cells + !
   s n EDGE-TEXTS EDGE-TEXTS-CAP TEXT!
   n 1 + EDGE-N ! ;

\ The id of the file dictionary record idx came from.
: RECORD-FILE ( n -- n )
   STAMP-SYNC FILE-OF ;

\ Whether the current file may call the record at dictionary index idx; a
\ negative idx is no record's, and is below the seal's count.
: ALLOWED? ( n -- bool ) {: idx:n :}
   idx  data-base POLICY-NDICT-CELL + @  < if 0 0= exit then
   idx RECORD-FILE CALLABLE? ;

\ Rejects the token t, bound to the record at dictionary index idx, unless
\ the current file may call it.
: CHECK ( str n -- ) {: t:str idx:n :}
   READING @ 0 <= if exit then
   idx ASKED @ COMPILE-READS:CALL+
   idx ALLOWED? if exit then
   t idx RECORD-FILE UNREQUIRED ;

\ Rejects the token t, bound to a declaration of file id n, unless the current
\ file may call it.
: CHECK-FILE ( str n -- ) {: t:str id:n :}
   id CALLABLE? if exit then
   t id UNREQUIRED ;

\ The image's records [first, end) are file id's: a build's end file states
\ each run of one file's records.
: SPAN ( n n n -- ) {: first:n end:n id:n :}
   SPAN-ROWS SPAN-ROWS-CAP SPAN-N @ 1 + 3 * GROW-CELLS
   SPAN-N @ 3 * cells SPAN-ROWS @ + {: s:^ :}
   first s !  end s CELL + !  id s 2 cells + !
   SPAN-N @ 1 + SPAN-N ! ;

\ A build's end file keeps the ids it minted, with their paths, and the spans
\ it stated where the image keeps DATA.
: PERSIST ( -- )
   MINTED @ 1 + 2 * {: rows:n :}
   align here CELL-VIEW {: t:^ :}
   rows cells PATH-TEXT-U @ + allot
   PATH-ROWS @ t rows ARENA-CELLS-COPY
   PATH-TEXT @ BYTE-VIEW
   t rows cells + BYTE-VIEW  PATH-TEXT-U @ >span  ARENA-COPY
   t IMAGE-PATHS !  MINTED @ IMAGE-N !
   align here CELL-VIEW {: s:^ :}
   SPAN-N @ 3 * {: cs:n :}
   cs cells allot
   SPAN-ROWS @ s cs ARENA-CELLS-COPY
   s IMAGE-SPANS ! ;

;package
