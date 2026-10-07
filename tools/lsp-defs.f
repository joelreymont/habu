\ lsp-defs.f - the definitions and uses the open documents' last completed
\ checks retained, decoded once per check and grouped by the file defining
\ them.
\
\ DEFS-KEEP takes the file, definition and use lines of a document's completed
\ check (CHECK:VERIFY-FILES$, CHECK:VERIFY-DEFS$ and CHECK:VERIFY-USES$: one
\ JSON object per line, as tools/check-verify-child.f states them) in place of
\ what that document's last check kept, and decodes each line once. A group is
\ one file a check read and the definitions it retained there, none if it
\ retained none: the file's canonical path, the URI its positions count at,
\ held as the JSON string that writes it, and its records in the order the
\ check retained them. A record is a definition's class, its word as the source writes it,
\ the package the checker recorded it under, empty for a global, whether it
\ recorded it public there, the statement token that declared it, its
\ declared effect, empty when it declared none, the bytes the token that
\ declared it starts and ends at, as its line states them, and that token's
\ range in LSP positions (LSP-TEXT),
\ which gives the line it is on: a missing start is 0, a
\ missing end the start, both are held to the text and the end
\ to no less than the start. The range counts through the bytes the check
\ read: the document's text for the file the check is of, the file on disk
\ for any other. The URI is the checked document's for its own file, the one
\ whose text the range counts through, and for any other the open document's
\ holding the file when the check completed, else the file URI of the path.
\ A file a check reads twice, as `include` after `undefine` does, gives each
\ of its definitions once.
\
\ A use is a use in the document that the check bound to a located
\ declaration, in the order the check published them: the bytes of the
\ document's text it starts and ends at, the check's group for the file the
\ declaration is in, and the bytes its declaring token starts and ends at
\ there. DEFS-USE-AT finds the use that holds a byte of a document's text and
\ USE-GROUP its group, which holds its declaration as the check read it: no
\ other check's group for the file, whose text may differ, answers for a use.
\ DEFS-EACH gives the groups that answer for their files, the ones workspace
\ symbols list. DEFS-OWN-GROUP gives the group of a document's own file while
\ its last check's positions are of its current text, from that check's
\ completion until its next check starts, so that a definition's token is
\ found at a byte of that text only from a check of it. DEFS-GROUP-OF gives,
\ for that same while, that check's group for a file by its path: the one a
\ candidate the check offered at a cursor is declared in, as the check read it.
\
\ The store holds the groups check after check, oldest first, each check's in
\ the order its definition lines first name their files, then the files it
\ read that hold none. A file several open documents' checks read is answered
\ from the check of the document holding it while the store keeps one, the
\ one check that read that document's text, else from the latest of them: a
\ later check's group for the same file, an empty one too, supersedes the
\ earlier ones. DEFS-DROP removes what a document's check kept, as its close
\ and its next check do, so a file another open document's check reached is
\ that check's again. DEFS-USES-DROP removes only its uses, as the start of
\ its next check does: a use is bytes of the text its check completed on,
\ while the definitions serve until a completed check replaces them.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the store belongs to the server's one task.

require lib/string.f
require lib/fs.f
require lib/span.f
require lib/uri.f
require lib/byte-buffer.f
require lib/adt/option.f
require lib/json-read.f
require lib/json-write.f
require tools/lsp-docs.f
require tools/lsp-text.f

package LSP-DEFS
using LSP-DOCS
using LSP-TEXT

public

0 constant CLASS-WORD                    \ a definition's class, which the
1 constant CLASS-CONSTANT                \ verifier states for the path that
2 constant CLASS-STORAGE                 \ declared it
3 constant CLASS-EXPORT

private

10 constant LF

\ A check's block: the slot of the document checked, its first group,
\ record, byte and use, and 1 while its positions are of the document's
\ current text.
0 constant B-SLOT
1 constant B-GROUP
2 constant B-REC
3 constant B-BYTE
4 constant B-USE
5 constant B-CURRENT
6 constant BLOCK-CELLS

\ A group: 1 while another group for its file answers in its place, its
\ path's offset and length in BYTES, its URI's JSON string's, its first and
\ last records, and 1 when its file is the one its check is of.
0 constant G-OVER
1 constant G-PATH
2 constant G-PATH-U
3 constant G-URI
4 constant G-URI-U
5 constant G-FIRST
6 constant G-LAST
7 constant G-OWN
8 constant GROUP-CELLS

\ A record: its group's next record, -1 after the last, its class, the line
\ and character its token starts at and ends at, its word's and package's
\ offsets and lengths in BYTES, the bytes its token starts and ends at, its
\ kind's and effect's offsets and lengths in BYTES, and 1 when it is public.
0 constant R-NEXT
1 constant R-CLASS
2 constant R-LINE
3 constant R-CHAR
4 constant R-END-LINE
5 constant R-END-CHAR
6 constant R-WORD
7 constant R-WORD-U
8 constant R-PKG
9 constant R-PKG-U
10 constant R-START
11 constant R-END
12 constant R-KIND
13 constant R-KIND-U
14 constant R-EFF
15 constant R-EFF-U
16 constant R-PUBLIC
17 constant REC-CELLS

\ A use: the bytes it starts and ends at, its check's group for its
\ declaration's file, and the bytes its declaring token starts and ends at.
0 constant U-START
1 constant U-END
2 constant U-GROUP
3 constant U-TARGET
4 constant U-TARGET-END
5 constant USE-CELLS

DYNAMIC-BUFFER BLOCKS n                  \ the blocks, oldest first,
variable BLOCK-N
DYNAMIC-BUFFER GROUPS n                  \ their groups,
variable GROUP-N
DYNAMIC-BUFFER RECS n                    \ their records,
variable REC-N
DYNAMIC-BUFFER USES n                    \ their uses,
variable USE-N
DYNAMIC-BUFFER BYTES u8                  \ and the bytes these name
variable BYTES-U
\ BYTES holds a byte past those it names, which each append (BYTES+,
\ STRING>BYTES) reserves with its own: its accessor refuses an index at its
\ capacity, and a string at the end of what it names, as an empty one is,
\ still needs the address of that index.

create JR-ST JR:STORAGE-BYTES allot      \ JR storage for decoding a line
DYNAMIC-BUFFER FILE-B u8                 \ the line's file, decoded,
variable FILE-U
variable CLASS-V                         \ its class,
variable WORD-AT                         \ its word's offset in BYTES,
variable WORD-U
variable PKG-AT                          \ its package's,
variable PKG-U
variable KIND-AT                         \ its kind's,
variable KIND-U
variable EFF-AT                          \ its effect's,
variable EFF-U
variable PUBLIC-V                        \ 1 when it is public,
variable START-V                         \ its token's offsets,
variable END-V
variable TARGET-V                        \ a use's declaring token's,
variable TARGET-END-V
variable LINE-AT                         \ and where its strings start in BYTES
variable CUR-G                           \ the group positions count for, -1 for none
FS-PATH-CAP 3 * 7 + SPAN-BUFFER: URI-SPAN  \ a file's URI: file:// and each byte escaped
create ESC-B BUF:HDR-BYTES allot         \ a URI's JSON string
TYPED-VARIABLE ESC-W JSON-WRITE:writer

: BLOCK@ ( n n -- n )  swap BLOCK-CELLS * + BLOCKS @ ;
: BLOCK! ( n n n -- )  swap BLOCK-CELLS * + BLOCKS ! ;
: GROUP@ ( n n -- n )  swap GROUP-CELLS * + GROUPS @ ;
: GROUP! ( n n n -- )  swap GROUP-CELLS * + GROUPS ! ;
: REC@ ( n n -- n )  swap REC-CELLS * + RECS @ ;
: REC! ( n n n -- )  swap REC-CELLS * + RECS ! ;
: USE@ ( n n -- n )  swap USE-CELLS * + USES @ ;
: USE! ( n n n -- )  swap USE-CELLS * + USES ! ;

\ The bytes at this offset of BYTES, this long.
: AT$ ( n n -- ptr u8 n )
   {: at:n u:n :}
   at BYTES u ;

: PATH$ ( n -- ptr u8 n )  dup G-PATH GROUP@ swap G-PATH-U GROUP@ AT$ ;
: FILE$ ( -- ptr u8 n )  0 FILE-B FILE-U @ ;

\ The bytes appended to BYTES: their offset there.
: BYTES+ ( ptr u8 n -- n )
   {: a:ptr u:n :}
   BYTES-U @ {: at:n :}
   at u + 1+ BYTES-RESERVE
   a at BYTES u BYTE-COPY
   u BYTES-U +!
   at ;

\ The string the reader is at, decoded onto the end of BYTES, which its raw
\ text's length bounds: its offset and length there.
: STRING>BYTES ( JR:reader -- JR:reader n n )
   JR:SPAN$ nip {: raw:n :}
   BYTES-U @ {: at:n :}
   at raw + 1+ BYTES-RESERVE
   at BYTES raw JR:STR {: u:n :}
   u BYTES-U +!
   at u ;

\ The string the reader is at, decoded into FILE-B.
: STRING>FILE ( JR:reader -- JR:reader )
   JR:SPAN$ nip {: raw:n :}
   raw 1 max FILE-B-RESERVE
   0 FILE-B raw JR:STR FILE-U ! ;

\ The class the string the reader is at names; a word for any class the
\ verifier names but this store does not tell apart.
: CLASS-OF ( JR:reader -- JR:reader n )
   s" constant" JR:STR-EQ? if CLASS-CONSTANT exit then
   s" storage" JR:STR-EQ? if CLASS-STORAGE exit then
   s" export" JR:STR-EQ? if CLASS-EXPORT exit then
   CLASS-WORD ;

\ The member whose key the reader is at, decoded into the line's cells; one
\ the store keeps nothing of, passed over.
: MEMBER ( JR:reader -- JR:reader )
   s" class" JR:STR-EQ? if JR:NEXT drop CLASS-OF CLASS-V ! exit then
   s" word" JR:STR-EQ? if JR:NEXT drop STRING>BYTES WORD-U ! WORD-AT ! exit then
   s" package" JR:STR-EQ? if JR:NEXT drop STRING>BYTES PKG-U ! PKG-AT ! exit then
   s" kind" JR:STR-EQ? if JR:NEXT drop STRING>BYTES KIND-U ! KIND-AT ! exit then
   s" effect" JR:STR-EQ? if JR:NEXT drop STRING>BYTES EFF-U ! EFF-AT ! exit then
   s" visibility" JR:STR-EQ? if
      JR:NEXT drop s" public" JR:STR-EQ? if 1 else 0 then PUBLIC-V ! exit
   then
   s" file" JR:STR-EQ? if JR:NEXT drop STRING>FILE exit then
   s" byte_start" JR:STR-EQ? if JR:NEXT drop JR:INT START-V ! exit then
   s" byte_end" JR:STR-EQ? if JR:NEXT drop JR:INT END-V ! exit then
   s" target_start" JR:STR-EQ? if JR:NEXT drop JR:INT TARGET-V ! exit then
   s" target_end" JR:STR-EQ? if JR:NEXT drop JR:INT TARGET-END-V ! exit then
   JR:NEXT drop JR:SKIP-VALUE ;

\ The line's members decoded, each once; a string it lacks is empty, at the
\ end of BYTES as the check's own strings are.
: DECODE ( ptr u8 n -- )
   {: a:ptr u:n :}
   BYTES-U @ LINE-AT !
   CLASS-WORD CLASS-V !
   BYTES-U @ WORD-AT !
   0 WORD-U !
   BYTES-U @ PKG-AT !
   0 PKG-U !
   BYTES-U @ KIND-AT !
   0 KIND-U !
   BYTES-U @ EFF-AT !
   0 EFF-U !
   0 PUBLIC-V !
   0 FILE-U !
   0 START-V !
   0 END-V !
   0 TARGET-V !
   0 TARGET-END-V !
   JR-ST JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT drop
   begin JR:NEXT JR:T-KEY = while MEMBER repeat
   JR:CLOSE ;

\ The group of the line's file among this check's, if it has one: the group
\ positions count for first, then the others.
: FOUND ( -- option<n> )
   CUR-G @ {: cur:n :}
   cur 0 >= if
      cur PATH$ FILE$ STR= if cur OPTION:SOME exit then
   then
   GROUP-N @ BLOCK-N @ 1- B-GROUP BLOCK@ ?do
      i PATH$ FILE$ STR= if i OPTION:SOME unloop exit then
   loop
   OPTION:NONE ;

\ The file URI of the file at this path.
: FILE-URI$ ( ptr u8 n -- ptr u8 n )
   URI-SPAN URI:PATH>FILE {: n:n :}
   URI-SPAN SPAN:$ drop n ;

\ The URI positions count at in the file at this path, another than the
\ checked document's own: the open document's that holds it, else the path's
\ file URI.
: DEP-URI$ ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu DOC-HOLDING MATCH option
      some OF DOC-URI$ ENDOF
      none OF p pu FILE-URI$ ENDOF
   ;MATCH ;

\ The JSON string that writes this URI.
: URI-JSON ( ptr u8 n -- ptr u8 n )
   {: u:ptr uu:n :}
   ESC-W ESC-B JSON-WRITE:OPEN-BUF
   u uu JSON-WRITE:STRING JSON-WRITE:$ ;

\ The slot of the document whose check the store is keeping: the newest
\ block's.
: KEPT-SLOT ( -- n )  BLOCK-N @ 1- B-SLOT BLOCK@ ;

\ A new group of this check's, for the line's file: its index. The group of
\ the document's own file is at the URI of that document, whose text its
\ positions count in (COUNT-IN).
: GROUP+ ( -- n )
   GROUP-N @ {: g:n :}
   g 1+ GROUP-CELLS * GROUPS-RESERVE
   FILE$ KEPT-SLOT DOC-CANON$ STR= {: own:bool :}
   0 g G-OVER GROUP!
   FILE$ BYTES+ g G-PATH GROUP!
   FILE-U @ g G-PATH-U GROUP!
   own if KEPT-SLOT DOC-URI$ else FILE$ DEP-URI$ then URI-JSON {: ua:ptr uu:n :}
   ua uu BYTES+ g G-URI GROUP!
   uu g G-URI-U GROUP!
   -1 g G-FIRST GROUP!
   -1 g G-LAST GROUP!
   own if 1 else 0 then g G-OWN GROUP!
   1 GROUP-N +!
   g ;

\ Positions count in the bytes the check read of the group's file from now
\ on: the text of the document the check is of, the file on disk for any
\ other, an open document's or not.
: COUNT-IN ( n -- )
   {: g:n :}
   g G-OWN GROUP@ 0<> if KEPT-SLOT DOC-TEXT$ TEXT! else g PATH$ FILE-TEXT! then
   g CUR-G ! ;

\ The group of the line's file, positions counting in its text.
: GROUP ( -- n )
   FOUND MATCH option
      some OF ENDOF
      none OF GROUP+ ENDOF
   ;MATCH
   dup CUR-G @ <> if dup COUNT-IN then ;

\ Whether record R is of the line's word, declared at the line's byte.
: SAME? ( n -- bool )
   {: r:n :}
   r R-START REC@ START-V @ <> if false exit then
   r R-WORD REC@ r R-WORD-U REC@ AT$ WORD-AT @ WORD-U @ AT$ STR= ;

\ Whether group G holds the line's record already: a file the check reads
\ again names each of its definitions again.
: HELD? ( n -- bool )
   G-FIRST GROUP@
   begin dup 0 >= while
      dup SAME? if drop true exit then
      R-NEXT REC@
   repeat
   drop false ;

\ The line's record, last of its group's; when the group holds it already,
\ the strings the line decoded are dropped instead.
: RECORD ( -- )
   GROUP {: g:n :}
   g HELD? if LINE-AT @ BYTES-U ! exit then
   REC-N @ {: r:n :}
   r 1+ REC-CELLS * RECS-RESERVE
   -1 r R-NEXT REC!
   CLASS-V @ r R-CLASS REC!
   START-V @ {: from:n :}
   from r R-START REC!
   END-V @ r R-END REC!
   from LINE-CHARACTER r R-CHAR REC! r R-LINE REC!
   END-V @ from max LINE-CHARACTER r R-END-CHAR REC! r R-END-LINE REC!
   WORD-AT @ r R-WORD REC!
   WORD-U @ r R-WORD-U REC!
   PKG-AT @ r R-PKG REC!
   PKG-U @ r R-PKG-U REC!
   KIND-AT @ r R-KIND REC!
   KIND-U @ r R-KIND-U REC!
   EFF-AT @ r R-EFF REC!
   EFF-U @ r R-EFF-U REC!
   PUBLIC-V @ r R-PUBLIC REC!
   g G-LAST GROUP@ {: last:n :}
   last 0 < if r g G-FIRST GROUP! else r last R-NEXT REC! then
   r g G-LAST GROUP!
   1 REC-N +! ;

\ The group of the line's file, made if this check has none.
: FILE-GROUP ( -- )
   FOUND MATCH option
      some OF drop ENDOF
      none OF GROUP+ drop ENDOF
   ;MATCH ;

\ The line's use, last of this check's.
: KEEP-USE ( -- )
   FOUND MATCH option
      some OF ENDOF
      none OF GROUP+ ENDOF
   ;MATCH {: g:n :}
   USE-N @ {: u:n :}
   u 1+ USE-CELLS * USES-RESERVE
   START-V @ u U-START USE!
   END-V @ u U-END USE!
   g u U-GROUP USE!
   TARGET-V @ u U-TARGET USE!
   TARGET-END-V @ u U-TARGET-END USE!
   1 USE-N +! ;

\ The line at offset O of the lines, decoded, given to XT; the offset after it.
: STEP ( ptr u8 n n [ -- ] -- n )
   {: a:ptr u:n o:n xt :}
   a o + u o - LF INDEX-OF MATCH option
      none OF u o - ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: len:n :}
   a o + len DECODE
   xt execute
   o len + 1+ ;

\ Each of the lines, decoded, given to XT.
: LINES ( ptr u8 n [ -- ] -- )
   {: a:ptr u:n xt :}
   0 begin dup u < while a u rot xt STEP repeat drop ;

\ Whether group H answers for its file in place of group G: the group of the
\ check of the document holding the file in place of a group of a check that
\ read the file from disk, else the later group, which is a later check's.
: OUTRANKS? ( n n -- bool )
   {: h:n g:n :}
   h G-OWN GROUP@ g G-OWN GROUP@ {: ho:n go:n :}
   ho go <> if ho go > exit then
   h g > ;

\ Whether another group for group G's file answers in its place.
: SUPERSEDED? ( n -- bool )
   {: g:n :}
   GROUP-N @ 0 ?do
      i g OUTRANKS? if
         i PATH$ g PATH$ STR= if unloop true exit then
      then
   loop
   false ;

: SUPERSEDE ( -- )
   GROUP-N @ 0 ?do
      i SUPERSEDED? if 1 else 0 then i G-OVER GROUP!
   loop ;

\ Where block K's FIELD ends: the next block's start, else TOTAL.
: BLOCK-END ( n n n -- n )
   {: k:n f:n total:n :}
   k 1+ BLOCK-N @ < if k 1+ f BLOCK@ else total then ;

\ COUNT cells moved down from index FROM to TO of the buffer XT indexes.
: CELLS-DOWN ( n n n [ n -- ptr n ] -- )
   {: from:n to:n count:n xt :}
   count 0 ?do
      from i + xt execute @ to i + xt execute !
   loop ;

\ Record R's offsets, DR records and DT bytes down.
: REC-DOWN ( n n n -- )
   {: r:n dr:n dt:n :}
   r R-NEXT REC@ {: next:n :}
   next 0 >= if next dr - r R-NEXT REC! then
   r R-WORD REC@ dt - r R-WORD REC!
   r R-PKG REC@ dt - r R-PKG REC!
   r R-KIND REC@ dt - r R-KIND REC!
   r R-EFF REC@ dt - r R-EFF REC! ;

\ Block K's uses removed; the later blocks' uses moved down, and their first
\ uses with them.
: CUT-USES ( n -- )
   {: k:n :}
   k B-USE BLOCK@ {: u0:n :}
   k B-USE USE-N @ BLOCK-END u0 - {: du:n :}
   u0 du + USE-CELLS * u0 USE-CELLS * USE-N @ u0 - du - USE-CELLS *
   [: USES ;] CELLS-DOWN
   du negate USE-N +!
   BLOCK-N @ k 1+ ?do i B-USE BLOCK@ du - i B-USE BLOCK! loop ;

\ Block K's groups, records, bytes and uses removed, and the block; what
\ follows them moved down and its offsets with it.
: REMOVE ( n -- )
   {: k:n :}
   k CUT-USES
   k B-GROUP BLOCK@ {: g0:n :}
   k B-REC BLOCK@ {: r0:n :}
   k B-BYTE BLOCK@ {: t0:n :}
   k B-USE BLOCK@ {: u0:n :}
   k B-GROUP GROUP-N @ BLOCK-END g0 - {: dg:n :}
   k B-REC REC-N @ BLOCK-END r0 - {: dr:n :}
   k B-BYTE BYTES-U @ BLOCK-END t0 - {: dt:n :}
   g0 dg + GROUP-CELLS * g0 GROUP-CELLS * GROUP-N @ g0 - dg - GROUP-CELLS *
   [: GROUPS ;] CELLS-DOWN
   r0 dr + REC-CELLS * r0 REC-CELLS * REC-N @ r0 - dr - REC-CELLS *
   [: RECS ;] CELLS-DOWN
   BYTES-U @ t0 - dt - {: tail:n :}
   tail 0 > if t0 dt + BYTES t0 BYTES tail BYTE-COPY then
   dg negate GROUP-N +!
   dr negate REC-N +!
   dt negate BYTES-U +!
   GROUP-N @ g0 ?do
      i G-PATH GROUP@ dt - i G-PATH GROUP!
      i G-URI GROUP@ dt - i G-URI GROUP!
      i G-FIRST GROUP@ dr - i G-FIRST GROUP!
      i G-LAST GROUP@ dr - i G-LAST GROUP!
   loop
   REC-N @ r0 ?do i dr dt REC-DOWN loop
   USE-N @ u0 ?do i U-GROUP USE@ dg - i U-GROUP USE! loop
   k 1+ BLOCK-CELLS * k BLOCK-CELLS * BLOCK-N @ k - 1- BLOCK-CELLS *
   [: BLOCKS ;] CELLS-DOWN
   -1 BLOCK-N +!
   BLOCK-N @ k ?do
      i B-GROUP BLOCK@ dg - i B-GROUP BLOCK!
      i B-REC BLOCK@ dr - i B-REC BLOCK!
      i B-BYTE BLOCK@ dt - i B-BYTE BLOCK!
   loop ;

\ The block of the last completed check of the document in this slot, if the
\ store keeps one.
: BLOCK-OF ( n -- option<n> )
   {: slot:n :}
   BLOCK-N @ 0 ?do
      i B-SLOT BLOCK@ slot = if i OPTION:SOME unloop exit then
   loop
   OPTION:NONE ;

\ The first use of block K whose bytes, from its start to before its end, hold
\ this byte, if one does.
: USE-IN ( n n -- option<n> )
   {: k:n at:n :}
   k B-USE USE-N @ BLOCK-END k B-USE BLOCK@ ?do
      i U-START USE@ at <= i U-END USE@ at > and
      if i OPTION:SOME unloop exit then
   loop
   OPTION:NONE ;

public

\ Readies the store, before any check.
: DEFS-PREPARE ( -- )
   ESC-B 1 BUF:N>BLEN BUF:INIT ;

\ Removes what the last check of the document in this slot kept.
: DEFS-DROP ( n -- )
   {: slot:n :}
   BLOCK-N @ 0 ?do
      i B-SLOT BLOCK@ slot = if i REMOVE SUPERSEDE unloop exit then
   loop ;

\ Removes the uses the last check of the document in this slot kept, and keeps
\ its definitions, whose positions are no longer of the document's text.
: DEFS-USES-DROP ( n -- )
   BLOCK-OF MATCH option
      some OF dup CUT-USES 0 swap B-CURRENT BLOCK! ENDOF
      none OF ENDOF
   ;MATCH ;

\ Keeps these file, definition and use lines of the completed check of the
\ document in this slot, in place of what its last check kept.
: DEFS-KEEP ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: fa:ptr fu:n a:ptr u:n ua:ptr uu:n slot:n :}
   slot DEFS-DROP
   BLOCK-N @ {: k:n :}
   k 1+ BLOCK-CELLS * BLOCKS-RESERVE
   slot k B-SLOT BLOCK!
   GROUP-N @ k B-GROUP BLOCK!
   REC-N @ k B-REC BLOCK!
   BYTES-U @ k B-BYTE BLOCK!
   USE-N @ k B-USE BLOCK!
   1 k B-CURRENT BLOCK!
   1 BLOCK-N +!
   -1 CUR-G !
   a u [: RECORD ;] LINES
   fa fu [: FILE-GROUP ;] LINES
   ua uu [: KEEP-USE ;] LINES
   SUPERSEDE ;

\ Whether another group for the same file answers in this group's place.
: GROUP-OVER? ( n -- bool )  G-OVER GROUP@ 0<> ;

\ Gives XT each group that answers for its file, in the store's order: each
\ one no other group for the same file answers in place of.
: DEFS-EACH ( [ n -- ] -- )
   {: xt :}
   GROUP-N @ 0 ?do
      i GROUP-OVER? 0= if i xt execute then
   loop ;

\ The path of the file the group is for.
: GROUP-PATH$ ( n -- ptr u8 n )  PATH$ ;

\ The JSON string of the URI the group's positions count at.
: GROUP-URI$ ( n -- ptr u8 n )  dup G-URI GROUP@ swap G-URI-U GROUP@ AT$ ;

\ The group's first record.
: GROUP-FIRST ( n -- n )  G-FIRST GROUP@ ;

\ The record after this one in its group, -1 after the last.
: REC-NEXT ( n -- n )  R-NEXT REC@ ;

: REC-CLASS ( n -- n )  R-CLASS REC@ ;
: REC-WORD$ ( n -- ptr u8 n )  dup R-WORD REC@ swap R-WORD-U REC@ AT$ ;
: REC-PACKAGE$ ( n -- ptr u8 n )  dup R-PKG REC@ swap R-PKG-U REC@ AT$ ;
: REC-KIND$ ( n -- ptr u8 n )  dup R-KIND REC@ swap R-KIND-U REC@ AT$ ;
: REC-EFFECT$ ( n -- ptr u8 n )  dup R-EFF REC@ swap R-EFF-U REC@ AT$ ;

\ Whether the checker recorded the record's word public in its package.
: REC-PUBLIC? ( n -- bool )  R-PUBLIC REC@ 0<> ;

\ The line and character the record's token starts at, then ends at.
: REC-RANGE ( n -- n n n n )
   {: r:n :}
   r R-LINE REC@ r R-CHAR REC@ r R-END-LINE REC@ r R-END-CHAR REC@ ;

\ The bytes the record's token starts and ends at, as its line states them.
: REC-BYTES ( n -- n n )
   {: r:n :}
   r R-START REC@ r R-END REC@ ;

\ The first use of the last completed check of the document in this slot whose
\ bytes hold this byte; none if the store keeps no check of it or no use
\ holds the byte.
: DEFS-USE-AT ( n n -- option<n> )
   {: slot:n at:n :}
   slot BLOCK-OF MATCH option
      some OF at USE-IN ENDOF
      none OF OPTION:NONE ENDOF
   ;MATCH ;

\ The check's group for the file the use's declaration is in.
: USE-GROUP ( n -- n )  U-GROUP USE@ ;

\ The bytes the use's declaring token starts and ends at, in that file.
: USE-TARGET ( n -- n n )
   {: u:n :}
   u U-TARGET USE@ u U-TARGET-END USE@ ;

\ The bytes of the document's text the use starts and ends at.
: USE-BYTES ( n -- n n )
   {: u:n :}
   u U-START USE@ u U-END USE@ ;

\ The first record of group G whose token starts and ends at these bytes, if
\ one does.
: GROUP-REC-AT ( n n n -- option<n> )
   {: g:n ts:n te:n :}
   g G-FIRST GROUP@
   begin dup 0 >= while
      dup REC-BYTES te = swap ts = and if OPTION:SOME exit then
      R-NEXT REC@
   repeat
   drop OPTION:NONE ;

private

\ The first record of group G whose token starts and ends at these bytes and
\ whose word is W, letters compared without case as Habu compares names, if
\ one is.
: GROUP-REC-WORD ( n n n ptr u8 n -- option<n> )
   {: g:n ts:n te:n w:ptr wu:n :}
   g G-FIRST GROUP@
   begin dup 0 >= while
      dup REC-BYTES te = swap ts = and if
         dup REC-WORD$ w wu STR=CI if OPTION:SOME exit then
      then
      R-NEXT REC@
   repeat
   drop OPTION:NONE ;

\ The record of group G whose token starts and ends at these bytes, if exactly
\ one does.
: GROUP-REC-SOLE ( n n n -- option<n> )
   {: g:n ts:n te:n :}
   0 -1 g G-FIRST GROUP@
   begin dup 0 >= while
      dup REC-BYTES te = swap ts = and if nip swap 1+ swap dup then
      R-NEXT REC@
   repeat
   drop swap 1 = if OPTION:SOME else drop OPTION:NONE then ;

\ When no record of group G at these bytes is of word W: the one of W's tail,
\ its bytes after the colon, when W is qualified as CHECKER-QUALIFIED? reads a
\ name, by one non-edge colon; else the one record there, if exactly one is.
\ SPLIT-NEXT's flag says only that its start was valid: the position it
\ returns lies past W's end when no colon follows that start.
: GROUP-REC-TAIL ( n n n ptr u8 n -- option<n> )
   {: g:n ts:n te:n w:ptr wu:n :}
   w wu [char] : 0 SPLIT-NEXT drop nip nip
   {: at:n :}
   w wu [char] : at SPLIT-NEXT drop nip nip
   {: at2:n :}
   at 1 >  at wu < and  at2 wu > and 0= if g ts te GROUP-REC-SOLE exit then
   g ts te w at + wu at - GROUP-REC-WORD MATCH option
      some OF OPTION:SOME ENDOF
      none OF g ts te GROUP-REC-SOLE ENDOF
   ;MATCH ;

public

\ The record of group G whose token starts and ends at these bytes and whose
\ word is W, letters compared without case as Habu compares names; else, for a
\ W qualified by one non-edge colon, the one whose word is W's tail; else the
\ one record whose token does, if exactly one does. Records can share a token,
\ as a DEFTYPE's two converters share its name's, so with several there and
\ none of them W or its tail, none answers.
: GROUP-REC-NAMED ( n n n ptr u8 n -- option<n> )
   {: g:n ts:n te:n w:ptr wu:n :}
   g ts te w wu GROUP-REC-WORD MATCH option
      some OF OPTION:SOME ENDOF
      none OF g ts te w wu GROUP-REC-TAIL ENDOF
   ;MATCH ;

\ The first record of group G whose token's bytes, from its start to before
\ its end, hold this byte, if one does.
: GROUP-REC-HOLDING ( n n -- option<n> )
   {: g:n at:n :}
   g G-FIRST GROUP@
   begin dup 0 >= while
      dup REC-BYTES at > swap at <= and if OPTION:SOME exit then
      R-NEXT REC@
   repeat
   drop OPTION:NONE ;

private

\ The group for the document's own file among block K's, while K's positions
\ are of the document's current text, if K has one.
: OWN-IN ( n -- option<n> )
   {: k:n :}
   k B-CURRENT BLOCK@ 0= if OPTION:NONE exit then
   k B-GROUP GROUP-N @ BLOCK-END k B-GROUP BLOCK@ ?do
      i G-OWN GROUP@ 0<> if i OPTION:SOME unloop exit then
   loop
   OPTION:NONE ;

\ The group for the file at this path among block K's, while K's positions
\ are of the document's current text, if K has one.
: PATH-IN ( n ptr u8 n -- option<n> )
   {: k:n p:ptr pu:n :}
   k B-CURRENT BLOCK@ 0= if OPTION:NONE exit then
   k B-GROUP GROUP-N @ BLOCK-END k B-GROUP BLOCK@ ?do
      i PATH$ p pu STR= if i OPTION:SOME unloop exit then
   loop
   OPTION:NONE ;

public

\ The group for the file of the document in this slot in its last completed
\ check, whose positions are of the document's text; none if the store keeps
\ no check of it, a check of it has started since, so that the group's
\ positions may not be of its text, or the check kept no group for it.
: DEFS-OWN-GROUP ( n -- option<n> )
   BLOCK-OF MATCH option
      some OF OWN-IN ENDOF
      none OF OPTION:NONE ENDOF
   ;MATCH ;

\ The group for the file at this path in the last completed check of the
\ document in this slot, while that check's positions are of the document's
\ text; none if the store keeps no check of it, a check of it has started
\ since, or the check kept no group for the file.
: DEFS-GROUP-OF ( n ptr u8 n -- option<n> )
   {: slot:n p:ptr pu:n :}
   slot BLOCK-OF MATCH option
      some OF p pu PATH-IN ENDOF
      none OF OPTION:NONE ENDOF
   ;MATCH ;

;using
;using
;package
