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
\ A file a check reads again, as `include` into a second package or after
\ `undefine` does, gives each of its definitions once, as the file spells
\ it. A record whose word a later reading declares at its token again,
\ letters compared without case as Habu compares names, stands for several
\ words, which REC-SEVERAL? answers, and so does that reading's own record
\ when the file changed its spelling between the readings: each reading
\ declares another word, in another wordlist or, after `undefine`, in the
\ same one.
\
\ A use is a use in the document that the check bound to a located
\ declaration, in the order the check published them: the bytes of the
\ document's text it starts and ends at, the check's group for the file the
\ declaration is in, and the bytes its declaring token starts and ends at
\ there. DEFS-USE-AT finds the use that holds a byte of a document's text and
\ USE-GROUP its group, which holds its declaration as the check read it: no
\ other check's group for the file, whose text may differ, answers for a use.
\ USE-WORD$ gives the word as a use writes it. DEFS-USE-RANGE gives the uses of
\ a document's last completed check in the order the check published them.
\ DEFS-KEEP also takes whether the check's verdict was verified, which
\ DEFS-VERIFIED? answers while the check's positions are of the document's
\ current text: a verified check reported every use in the document that it
\ bound to a located declaration.
\ DEFS-EACH gives the groups that answer for their files, the ones workspace
\ symbols list. DEFS-OWN-GROUP gives the group of a document's own file while
\ its last check's positions are of its current text, from that check's
\ completion until its next check starts, so that a definition's token is
\ found at a byte of that text only from a check of it. DEFS-GROUP-OF gives,
\ for that same while, that check's group for a file by its path: the one a
\ candidate the check offered at a cursor is declared in, as the check read it.
\
\ A group also keeps the digest its check's file lines state for its file,
\ SHA-256 as 64 lowercase hexadecimal digits: GROUP-SHA$ gives it while every
\ one of them states that one, and nothing once one states none, a malformed
\ one or another, whichever comes first, or while none has, so that bytes a
\ check read are identified only when it read one text of the file.
\ A decl is the declaration identity a definition line states, one per line,
\ so that a file read twice gives a decl per reading where it gives one
\ record: the check's group for the declaration's file, the bytes its token
\ starts and ends at as the line states them, the declaration visit the check
\ gave it, which tells apart only that check's declarations, the declaration's
\ lower-case tail, its package, empty for a global, and its visibility. A
\ line that lacks one of these or states a malformed one gives a decl whose
\ visit is -1, which identifies nothing. A use keeps the identity of the
\ declaration it is bound to as its line states it, the same way.
\ DEFS-DECL-RANGE gives the decls of a document's last completed check, which,
\ as its definitions do, serve until a completed check replaces them. The
\ digests and strings are the store's own: a caller copies what it needs
\ before the store keeps or drops another check.
\
\ DEFS-PROBE keeps a completed check of a file on disk that no open document
\ holds as the probe, under the slot PROBE, until DEFS-UNPROBE removes it: its
\ groups, digests, decls and uses, but no record, and none of its groups its
\ own file's or answering in another's place, so that it leaves every open
\ document's groups, decls and uses as they were. Nothing is kept or dropped
\ while a probe is kept.
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

-2 constant PROBE                        \ the probe's slot, no document's

private

10 constant LF

0 constant VIS-GLOBAL                    \ a declaration's visibility, which
1 constant VIS-PRIVATE                   \ the verifier states for it
2 constant VIS-PUBLIC

-1 constant UNSEEN                       \ a group's digest before a file line states one,
-2 constant UNUSABLE                     \ and once its file lines disagree
64 constant SHA-HEX-U                    \ a digest's hexadecimal digits

\ A check's block: the slot of the document checked, its first group,
\ record, byte, use and decl, 1 while its positions are of the document's
\ current text, and 1 when the check's verdict was verified.
0 constant B-SLOT
1 constant B-GROUP
2 constant B-REC
3 constant B-BYTE
4 constant B-USE
5 constant B-DECL
6 constant B-CURRENT
7 constant B-VERIFIED
8 constant BLOCK-CELLS

\ A group: 1 while another group for its file answers in its place, its
\ path's offset and length in BYTES, its URI's JSON string's, its first and
\ last records, 1 when its file is the one its check is of, and its digest's
\ offset in BYTES, else UNSEEN or UNUSABLE.
0 constant G-OVER
1 constant G-PATH
2 constant G-PATH-U
3 constant G-URI
4 constant G-URI-U
5 constant G-FIRST
6 constant G-LAST
7 constant G-OWN
8 constant G-SHA
9 constant GROUP-CELLS

\ A record: its group's next record, -1 after the last, its class, the line
\ and character its token starts at and ends at, its word's and package's
\ offsets and lengths in BYTES, the bytes its token starts and ends at, its
\ kind's and effect's offsets and lengths in BYTES, and 1 when it stands for
\ several words, then 1 when it is public.
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
16 constant R-SEVERAL
17 constant R-PUBLIC
18 constant REC-CELLS

\ A use: the bytes it starts and ends at, its check's group for its
\ declaration's file, the bytes its declaring token starts and ends at, and
\ its declaration's identity: its visit, -1 for none, its name's and
\ package's offsets and lengths in BYTES, and its visibility.
0 constant U-START
1 constant U-END
2 constant U-GROUP
3 constant U-TARGET
4 constant U-TARGET-END
5 constant U-VISIT
6 constant U-NAME
7 constant U-NAME-U
8 constant U-PKG
9 constant U-PKG-U
10 constant U-VIS
11 constant USE-CELLS

\ A decl: its check's group for its declaration's file, the bytes its token
\ starts and ends at, its visit, -1 for none, its name's and package's
\ offsets and lengths in BYTES, and its visibility.
0 constant D-GROUP
1 constant D-START
2 constant D-END
3 constant D-VISIT
4 constant D-NAME
5 constant D-NAME-U
6 constant D-PKG
7 constant D-PKG-U
8 constant D-VIS
9 constant DECL-CELLS

DYNAMIC-BUFFER BLOCKS n                  \ the blocks, oldest first,
variable BLOCK-N
DYNAMIC-BUFFER GROUPS n                  \ their groups,
variable GROUP-N
DYNAMIC-BUFFER RECS n                    \ their records,
variable REC-N
DYNAMIC-BUFFER USES n                    \ their uses,
variable USE-N
DYNAMIC-BUFFER DECLS n                   \ their decls,
variable DECL-N
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
variable PKG-AT                          \ its package's, 1 in PKG-OK when it states one,
variable PKG-U
variable PKG-OK
variable KIND-AT                         \ its kind's,
variable KIND-U
variable EFF-AT                          \ its effect's,
variable EFF-U
variable NAME-AT                         \ its declaration's name's,
variable NAME-U
variable SHA-AT                          \ its digest's,
variable SHA-U
variable VISIT-V                         \ its declaration's visit, 0 for none,
variable VIS-V                           \ its visibility, -1 for none,
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
: DECL@ ( n n -- n )  swap DECL-CELLS * + DECLS @ ;
: DECL! ( n n n -- )  swap DECL-CELLS * + DECLS ! ;

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

\ The visibility the string the reader is at names, -1 for any other.
: VIS-OF ( JR:reader -- JR:reader n )
   s" global" JR:STR-EQ? if VIS-GLOBAL exit then
   s" private" JR:STR-EQ? if VIS-PRIVATE exit then
   s" public" JR:STR-EQ? if VIS-PUBLIC exit then
   -1 ;

\ The member whose key the reader is at, decoded into the line's cells; one
\ the store keeps nothing of, or an identity member of another type than the
\ one its line states, passed over.
: MEMBER ( JR:reader -- JR:reader )
   s" class" JR:STR-EQ? if JR:NEXT drop CLASS-OF CLASS-V ! exit then
   s" word" JR:STR-EQ? if JR:NEXT drop STRING>BYTES WORD-U ! WORD-AT ! exit then
   s" package" JR:STR-EQ? if
      JR:NEXT JR:T-STR = if STRING>BYTES PKG-U ! PKG-AT ! 1 PKG-OK ! else JR:SKIP-VALUE then
      exit
   then
   s" kind" JR:STR-EQ? if JR:NEXT drop STRING>BYTES KIND-U ! KIND-AT ! exit then
   s" effect" JR:STR-EQ? if JR:NEXT drop STRING>BYTES EFF-U ! EFF-AT ! exit then
   s" decl_name" JR:STR-EQ? if
      JR:NEXT JR:T-STR = if STRING>BYTES NAME-U ! NAME-AT ! else JR:SKIP-VALUE then exit
   then
   s" sha256" JR:STR-EQ? if
      JR:NEXT JR:T-STR = if STRING>BYTES SHA-U ! SHA-AT ! else JR:SKIP-VALUE then exit
   then
   s" decl_visit" JR:STR-EQ? if
      JR:NEXT JR:T-INT = if JR:INT VISIT-V ! else JR:SKIP-VALUE then exit
   then
   s" visibility" JR:STR-EQ? if JR:NEXT drop VIS-OF VIS-V ! exit then
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
   0 PKG-OK !
   BYTES-U @ KIND-AT !
   0 KIND-U !
   BYTES-U @ EFF-AT !
   0 EFF-U !
   BYTES-U @ NAME-AT !
   0 NAME-U !
   BYTES-U @ SHA-AT !
   0 SHA-U !
   0 VISIT-V !
   -1 VIS-V !
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

public

\ The URI positions count at in the file at this path, another than the
\ checked document's own: the open document's that holds it, else the path's
\ file URI.
: DEP-URI$ ( ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n :}
   p pu DOC-HOLDING MATCH option
      some OF DOC-URI$ ENDOF
      none OF p pu FILE-URI$ ENDOF
   ;MATCH ;

private

\ The JSON string that writes this URI.
: URI-JSON ( ptr u8 n -- ptr u8 n )
   {: u:ptr uu:n :}
   ESC-W ESC-B JSON-WRITE:OPEN-BUF
   u uu JSON-WRITE:STRING JSON-WRITE:$ ;

\ The slot of the document whose check the store is keeping: the newest
\ block's.
: KEPT-SLOT ( -- n )  BLOCK-N @ 1- B-SLOT BLOCK@ ;

\ A new group of this check's, for the line's file, with no digest yet: its
\ index. The group of the document's own file is at the URI of that
\ document, whose text its positions count in (COUNT-IN); the probe's
\ check is of no document's.
: GROUP+ ( -- n )
   GROUP-N @ {: g:n :}
   g 1+ GROUP-CELLS * GROUPS-RESERVE
   KEPT-SLOT {: slot:n :}
   slot 0 >= if FILE$ slot DOC-CANON$ STR= else false then {: own:bool :}
   0 g G-OVER GROUP!
   UNSEEN g G-SHA GROUP!
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

\ The group of the line's file, made if this check has none.
: FILE-GROUP ( -- n )
   FOUND MATCH option
      some OF ENDOF
      none OF GROUP+ ENDOF
   ;MATCH ;

\ The group of the line's file, positions counting in its text.
: GROUP ( -- n )
   FILE-GROUP dup CUR-G @ <> if dup COUNT-IN then ;

\ The U bytes at offset AT of BYTES, which is no less than BYTES-U, moved
\ down to the end of what BYTES names: their offset there.
: BYTES-DOWN ( n n -- n )
   {: at:n u:n :}
   BYTES-U @ {: to:n :}
   u 0 > if at BYTES to BYTES u BYTE-COPY then
   u BYTES-U +!
   to ;

\ The line's strings dropped from BYTES but its declaration's name and
\ package, which move down to where they started, the earlier one first.
: IDS-DOWN ( -- )
   LINE-AT @ BYTES-U !
   NAME-AT @ PKG-AT @ < if
      NAME-AT @ NAME-U @ BYTES-DOWN NAME-AT !
      PKG-AT @ PKG-U @ BYTES-DOWN PKG-AT !
   else
      PKG-AT @ PKG-U @ BYTES-DOWN PKG-AT !
      NAME-AT @ NAME-U @ BYTES-DOWN NAME-AT !
   then ;

\ The visit of the declaration the line states, -1 unless it states a whole
\ identity: a visit, a name, a package, empty for a global, and a
\ visibility.
: LINE-VISIT ( -- n )
   VISIT-V @ 0 > NAME-U @ 0 > and PKG-OK @ 0<> and VIS-V @ 0 >= and
   if VISIT-V @ else -1 then ;

\ The line's decl, last of this check's, in group G.
: DECL+ ( n -- )
   {: g:n :}
   DECL-N @ {: d:n :}
   d 1+ DECL-CELLS * DECLS-RESERVE
   g d D-GROUP DECL!
   START-V @ d D-START DECL!
   END-V @ d D-END DECL!
   LINE-VISIT d D-VISIT DECL!
   NAME-AT @ d D-NAME DECL!
   NAME-U @ d D-NAME-U DECL!
   PKG-AT @ d D-PKG DECL!
   PKG-U @ d D-PKG-U DECL!
   VIS-V @ d D-VIS DECL!
   1 DECL-N +! ;

\ Whether record R is of the line's word, declared at the line's byte.
: SAME? ( n -- bool )
   {: r:n :}
   r R-START REC@ START-V @ <> if false exit then
   r R-WORD REC@ r R-WORD-U REC@ AT$ WORD-AT @ WORD-U @ AT$ STR= ;

\ The record of group G that holds the line's record already, if one does: a
\ file the check reads again names each of its definitions again.
: HOLDER ( n -- option<n> )
   G-FIRST GROUP@
   begin dup 0 >= while
      dup SAME? if OPTION:SOME exit then
      R-NEXT REC@
   repeat
   drop OPTION:NONE ;

\ Whether record R is of the line's word, letters compared without case as
\ Habu compares names, declared at the line's byte.
: ALIKE? ( n -- bool )
   {: r:n :}
   r R-START REC@ START-V @ <> if false exit then
   r R-WORD REC@ r R-WORD-U REC@ AT$ WORD-AT @ WORD-U @ AT$ STR=CI ;

\ Marks each record of group G whose word is the line's, letters compared
\ without case, declared at the line's byte, as standing for several words;
\ true when one is.
: SEVERAL! ( n -- bool )
   false swap G-FIRST GROUP@
   begin dup 0 >= while
      dup ALIKE? if 1 over R-SEVERAL REC! nip true swap then
      R-NEXT REC@
   repeat
   drop ;

\ The line's record, new, last of group G's, standing for several words when
\ SEVERAL is true.
: REC+ ( n bool -- )
   {: g:n several:bool :}
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
   several if 1 else 0 then r R-SEVERAL REC!
   VIS-V @ VIS-PUBLIC = if 1 else 0 then r R-PUBLIC REC!
   g G-LAST GROUP@ {: last:n :}
   last 0 < if r g G-FIRST GROUP! else r last R-NEXT REC! then
   r g G-LAST GROUP!
   1 REC-N +! ;

\ The line's record, last of its group's, unless the group holds it already,
\ when the strings the line decoded are dropped instead, but those of its
\ declaration's identity; its decl, either way. A line that declares a
\ record's word again at its token, letters compared without case as Habu
\ compares names, is of another reading of the file, which declared another
\ word there: that record, and the line's own when it is new, stands for
\ several from then on.
: RECORD ( -- )
   GROUP {: g:n :}
   g SEVERAL! {: several:bool :}
   g HOLDER MATCH option
      some OF drop IDS-DOWN ENDOF
      none OF g several REC+ ENDOF
   ;MATCH
   g DECL+ ;

\ The line's decl, and no record: the strings the line decoded are dropped
\ but those of its declaration's identity, before a new group's are added.
: PROBE-DECL ( -- )
   IDS-DOWN
   FILE-GROUP DECL+ ;

\ Whether the byte is a lowercase hexadecimal digit.
: HEX-DIGIT? ( n -- bool )
   {: c:n :}
   c [char] 0 >= c [char] 9 <= and  c [char] a >= c [char] f <= and  or ;

\ Whether these bytes are a digest as a file line states it.
: DIGEST? ( ptr u8 n -- bool )
   {: h:ptr hu:n :}
   hu SHA-HEX-U <> if false exit then
   hu 0 ?do
      h i + c@ HEX-DIGIT? 0= if unloop false exit then
   loop
   true ;

\ The digest of a group whose digest was D once the line reads its file, the
\ line's own digest dropped from BYTES unless kept: kept, the group's from
\ then on, when D is UNSEEN and the line's is a digest; D when D is the
\ line's; else UNUSABLE, which stays.
: DIGEST-AFTER ( n -- n )
   {: d:n :}
   SHA-AT @ SHA-U @ AT$ {: h:ptr hu:n :}
   h hu DIGEST? {: valid:bool :}
   d 0 >= valid and if d SHA-HEX-U AT$ h hu STR= else false then {: same:bool :}
   LINE-AT @ BYTES-U !
   d UNSEEN = valid and if SHA-AT @ SHA-U @ BYTES-DOWN exit then
   same if d else UNUSABLE then ;

\ The line's file read with the line's digest, its group made if this check
\ has none: the digest's fate is settled before a new group's strings follow
\ it.
: FILE-LINE ( -- )
   FOUND MATCH option
      some OF {: g:n :} g G-SHA GROUP@ DIGEST-AFTER g G-SHA GROUP! ENDOF
      none OF UNSEEN DIGEST-AFTER GROUP+ G-SHA GROUP! ENDOF
   ;MATCH ;

\ The line's use, last of this check's, with the identity of the
\ declaration it is bound to: the strings the line decoded are dropped but
\ that identity's, before a new group's are added.
: KEEP-USE ( -- )
   IDS-DOWN
   FILE-GROUP {: g:n :}
   USE-N @ {: u:n :}
   u 1+ USE-CELLS * USES-RESERVE
   START-V @ u U-START USE!
   END-V @ u U-END USE!
   g u U-GROUP USE!
   TARGET-V @ u U-TARGET USE!
   TARGET-END-V @ u U-TARGET-END USE!
   LINE-VISIT u U-VISIT USE!
   NAME-AT @ u U-NAME USE!
   NAME-U @ u U-NAME-U USE!
   PKG-AT @ u U-PKG USE!
   PKG-U @ u U-PKG-U USE!
   VIS-V @ u U-VIS USE!
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

\ Group G's offsets, DR records and DT bytes down, its digest's with them
\ when it has one.
: GROUP-DOWN ( n n n -- )
   {: g:n dr:n dt:n :}
   g G-PATH GROUP@ dt - g G-PATH GROUP!
   g G-URI GROUP@ dt - g G-URI GROUP!
   g G-FIRST GROUP@ dr - g G-FIRST GROUP!
   g G-LAST GROUP@ dr - g G-LAST GROUP!
   g G-SHA GROUP@ {: sha:n :}
   sha 0 >= if sha dt - g G-SHA GROUP! then ;

\ Decl D's group DG groups down, and its strings DT bytes.
: DECL-DOWN ( n n n -- )
   {: d:n dg:n dt:n :}
   d D-GROUP DECL@ dg - d D-GROUP DECL!
   d D-NAME DECL@ dt - d D-NAME DECL!
   d D-PKG DECL@ dt - d D-PKG DECL! ;

\ Use U's group DG groups down, and its strings DT bytes.
: USE-DOWN ( n n n -- )
   {: u:n dg:n dt:n :}
   u U-GROUP USE@ dg - u U-GROUP USE!
   u U-NAME USE@ dt - u U-NAME USE!
   u U-PKG USE@ dt - u U-PKG USE! ;

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

\ Block K's groups, records, decls, bytes and uses removed, and the block;
\ what follows them moved down and its offsets with it.
: REMOVE ( n -- )
   {: k:n :}
   k CUT-USES
   k B-GROUP BLOCK@ {: g0:n :}
   k B-REC BLOCK@ {: r0:n :}
   k B-BYTE BLOCK@ {: t0:n :}
   k B-USE BLOCK@ {: u0:n :}
   k B-DECL BLOCK@ {: d0:n :}
   k B-GROUP GROUP-N @ BLOCK-END g0 - {: dg:n :}
   k B-REC REC-N @ BLOCK-END r0 - {: dr:n :}
   k B-BYTE BYTES-U @ BLOCK-END t0 - {: dt:n :}
   k B-DECL DECL-N @ BLOCK-END d0 - {: dd:n :}
   g0 dg + GROUP-CELLS * g0 GROUP-CELLS * GROUP-N @ g0 - dg - GROUP-CELLS *
   [: GROUPS ;] CELLS-DOWN
   r0 dr + REC-CELLS * r0 REC-CELLS * REC-N @ r0 - dr - REC-CELLS *
   [: RECS ;] CELLS-DOWN
   d0 dd + DECL-CELLS * d0 DECL-CELLS * DECL-N @ d0 - dd - DECL-CELLS *
   [: DECLS ;] CELLS-DOWN
   BYTES-U @ t0 - dt - {: tail:n :}
   tail 0 > if t0 dt + BYTES t0 BYTES tail BYTE-COPY then
   dg negate GROUP-N +!
   dr negate REC-N +!
   dd negate DECL-N +!
   dt negate BYTES-U +!
   GROUP-N @ g0 ?do i dr dt GROUP-DOWN loop
   REC-N @ r0 ?do i dr dt REC-DOWN loop
   DECL-N @ d0 ?do i dg dt DECL-DOWN loop
   USE-N @ u0 ?do i dg dt USE-DOWN loop
   k 1+ BLOCK-CELLS * k BLOCK-CELLS * BLOCK-N @ k - 1- BLOCK-CELLS *
   [: BLOCKS ;] CELLS-DOWN
   -1 BLOCK-N +!
   BLOCK-N @ k ?do
      i B-GROUP BLOCK@ dg - i B-GROUP BLOCK!
      i B-REC BLOCK@ dr - i B-REC BLOCK!
      i B-DECL BLOCK@ dd - i B-DECL BLOCK!
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

\ A new block, the newest, for the completed check of the document in this
\ slot, whose verdict was verified or not.
: BLOCK+ ( n bool -- )
   {: slot:n verified:bool :}
   BLOCK-N @ {: k:n :}
   k 1+ BLOCK-CELLS * BLOCKS-RESERVE
   slot k B-SLOT BLOCK!
   GROUP-N @ k B-GROUP BLOCK!
   REC-N @ k B-REC BLOCK!
   BYTES-U @ k B-BYTE BLOCK!
   USE-N @ k B-USE BLOCK!
   DECL-N @ k B-DECL BLOCK!
   1 k B-CURRENT BLOCK!
   verified if 1 else 0 then k B-VERIFIED BLOCK!
   1 BLOCK-N +!
   -1 CUR-G ! ;

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
\ document in this slot, and whether its verdict was verified, in place of
\ what its last check kept.
: DEFS-KEEP ( ptr u8 n ptr u8 n ptr u8 n n bool -- )
   {: fa:ptr fu:n a:ptr u:n ua:ptr uu:n slot:n verified:bool :}
   slot DEFS-DROP
   slot verified BLOCK+
   a u [: RECORD ;] LINES
   fa fu [: FILE-LINE ;] LINES
   ua uu [: KEEP-USE ;] LINES
   SUPERSEDE ;

\ Removes the probe, if the store keeps one. No group answered in another's
\ place for it, so none answers in another's place once it is gone.
: DEFS-UNPROBE ( -- )
   PROBE BLOCK-OF MATCH option
      some OF REMOVE ENDOF
      none OF ENDOF
   ;MATCH ;

\ Keeps these file, definition and use lines of a completed check of a file
\ on disk that no open document holds, and whether its verdict was verified,
\ as the probe, in place of the one the store keeps: the newest block, under
\ the slot PROBE, with no record, whose groups answer in no other's place.
: DEFS-PROBE ( ptr u8 n ptr u8 n ptr u8 n bool -- )
   {: fa:ptr fu:n a:ptr u:n ua:ptr uu:n verified:bool :}
   DEFS-UNPROBE
   PROBE verified BLOCK+
   a u [: PROBE-DECL ;] LINES
   fa fu [: FILE-LINE ;] LINES
   ua uu [: KEEP-USE ;] LINES ;

\ Whether another group for the same file answers in this group's place.
: GROUP-OVER? ( n -- bool )  G-OVER GROUP@ 0<> ;

\ Whether the group's file is the one its check is of.
: GROUP-OWN? ( n -- bool )  G-OWN GROUP@ 0<> ;

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

\ Whether the record stands for several words: the check read its file again
\ and declared its word at its token again, letters compared without case.
: REC-SEVERAL? ( n -- bool )  R-SEVERAL REC@ 0<> ;

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

\ The word as use U of the text T writes it: the bytes the use spans, held to
\ the text.
: USE-WORD$ ( ptr u8 n n -- ptr u8 n )
   {: t:ptr tu:n u:n :}
   u USE-BYTES {: from:n to:n :}
   from 0 max tu min {: a:n :}
   to a max tu min {: b:n :}
   t a + b a - ;

\ The uses of the last completed check of the document in this slot, as the
\ limit and first index of a ?do loop over them, in the order the check
\ published them; none if the store keeps no check of it or a check of it
\ has started since.
: DEFS-USE-RANGE ( n -- n n )
   BLOCK-OF MATCH option
      some OF {: k:n :} k B-USE USE-N @ BLOCK-END k B-USE BLOCK@ ENDOF
      none OF 0 0 ENDOF
   ;MATCH ;

\ Whether the verdict of the last completed check of the document in this
\ slot was verified while its positions are of the document's text; false if
\ the store keeps no check of it or a check of it has started since.
: DEFS-VERIFIED? ( n -- bool )
   BLOCK-OF MATCH option
      some OF {: k:n :} k B-CURRENT BLOCK@ 0<> k B-VERIFIED BLOCK@ 0<> and ENDOF
      none OF false ENDOF
   ;MATCH ;

\ The digest of the bytes the group's check read of its file, while each of
\ the check's file lines for the file states that one; else empty. The bytes
\ are the store's until it keeps or drops another check.
: GROUP-SHA$ ( n -- ptr u8 n )
   G-SHA GROUP@ {: sha:n :}
   sha 0 < if s" " exit then
   sha SHA-HEX-U AT$ ;

\ The decls of the last completed check of the document in this slot, or of
\ the probe's for PROBE, as the limit and first index of a ?do loop over
\ them, in the order the check published its definition lines; none if the
\ store keeps no such check.
: DEFS-DECL-RANGE ( n -- n n )
   BLOCK-OF MATCH option
      some OF {: k:n :} k B-DECL DECL-N @ BLOCK-END k B-DECL BLOCK@ ENDOF
      none OF 0 0 ENDOF
   ;MATCH ;

\ The check's group for the file the decl's declaration is in.
: DECL-GROUP ( n -- n )  D-GROUP DECL@ ;

\ The bytes the decl's token starts and ends at, as its line states them.
: DECL-BYTES ( n -- n n )
   {: d:n :}
   d D-START DECL@ d D-END DECL@ ;

\ The declaration visit the check gave the decl, -1 when its line states no
\ whole identity; then its lower-case tail, its package, empty for a global,
\ and its visibility, the same number for the same one.
: DECL-VISIT ( n -- n )  D-VISIT DECL@ ;
: DECL-NAME$ ( n -- ptr u8 n )  dup D-NAME DECL@ swap D-NAME-U DECL@ AT$ ;
: DECL-PACKAGE$ ( n -- ptr u8 n )  dup D-PKG DECL@ swap D-PKG-U DECL@ AT$ ;
: DECL-VIS ( n -- n )  D-VIS DECL@ ;

\ The same of the declaration the use is bound to, as its line states them.
: USE-VISIT ( n -- n )  U-VISIT USE@ ;
: USE-NAME$ ( n -- ptr u8 n )  dup U-NAME USE@ swap U-NAME-U USE@ AT$ ;
: USE-PACKAGE$ ( n -- ptr u8 n )  dup U-PKG USE@ swap U-PKG-U USE@ AT$ ;
: USE-VIS ( n -- n )  U-VIS USE@ ;

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
