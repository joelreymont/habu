\ lsp-references.f - the language server's references: the declaration a
\ position selects in an open document whose check was verified, and every use
\ of it that the checks of the files whose loads reach its file bound, in
\ those files and in every file they read.
\
\ SELECT takes the open document D, a byte of its text and whether the request
\ includes the declaration. D's last completed check must have been verified
\ and be of D's text (LSP-DEFS:DEFS-VERIFIED?). The byte selects a declaration
\ as that check stated it: at a use (LSP-DEFS:DEFS-USE-AT), the declaration the
\ use is bound to; else at a definition token of D's own file, the one that
\ token declares, when it declares one word in one visit; else none, which
\ answers an empty list. Its identity is the file P it is in, its token's
\ bytes there, its lower-case tail, its package and its visibility, and the
\ visit the check gave it tells it apart from that check's other declarations
\ of the same identity. D's check must hold exactly one such declaration and
\ the digest of the bytes of P it read (LSP-DEFS:GROUP-SHA$). An included
\ declaration is placed in the text of P, the open document's that holds P,
\ else the file's, which must have that digest.
\
\ The subjects are the workspace files and open documents whose loads reach P,
\ and D, whose check read P though its loads on disk may no longer reach it,
\ as the walk gives them (tools/lsp-workspace.f): P first, then by path; and
\ P before them when the walk gives no P, as for a file outside every
\ workspace folder that no document holds. Each one is checked now for the
\ uses of the declaration's tail (LSP-CHECK:USES), its text the open
\ document's that holds it, else its file's: the check gives every use it
\ binds in the subject and every use of the tail it binds in each file it
\ reads. Every check reads each open
\ document's text in place of its file (tools/lsp-check.f), so a subject's
\ check reads the bytes of P that D's read, the open text when a document
\ holds P, unless P changed on disk between them. A check numbers only its own
\ visits, so a subject's check that read P must state P's digest the same and
\ declare the word at P's token, by its identity, in one visit at most, and
\ its uses bound to that token of the word's identity must carry that visit;
\ one that declares the word there in no visit and binds no use of it there
\ holds none, and contributes nothing. Each of those uses is placed in the
\ text of the file it is in, the open document's that holds the file, else
\ the file's, which must have the digest the check read of it, once however
\ many checks give it. Once every subject is gathered, SETTLE names the
\ subjects as below and orders the locations: the declaration, when
\ included, first, then the uses by file, P's first and the others in the
\ byte order of their paths, then by the bytes they start and end at.
\
\ The request is refused, REFUSED? true and BLOCKED$ naming the file relative
\ to the working directory and why, when D's check is not verified; the
\ position selects a use or a token without a whole identity, a token that
\ declares several words or one word in several visits, or a declaration D's
\ check does not hold once or holds no digest of P for; the declaration is
\ included and P's text no longer has the digest; or a workspace folder's walk
\ fails. Otherwise the answer comes. A file is covered only when a check whose
\ uses were placed read it, a verified check that read P and passed the tests
\ above: only that check's bindings are to this declaration, so a check that
\ read no P, or failed those tests, says nothing of the file's uses of it.
\ Each file the answer could not cover is named with why (WARNING),
\ contributing no location: a workspace file whose loads discovery cannot
\ find, but P and D, whose checks say what they read; a subject whose check
\ was not verified, by its check's verdict (LSP-CHECK:OUTCOME$), unless
\ another check covers its file; a file a use is in whose text no longer has
\ the digest the check read; a subject that cannot be read; one whose check
\ states no digest of P or another one, declares at P's token a word without
\ a whole identity or the word in several visits, or binds to that token a
\ use without a whole identity, or one of the word in another visit or while
\ it declares the word there in none.
\
\ PLACE selects as SELECT does and places the declaration, finding no
\ subjects: textDocument/prepareRename answers from it. TAIL-FROM gives where
\ the declaration's tail starts in a token: at the token's start when the
\ token spells the tail, letters compared without case as Habu compares
\ names; else, when the token is qualified as the checker reads a name, by
\ one colon neither its first byte nor its last, after that colon when the
\ bytes after it spell the tail; else nowhere. Each location keeps where its
\ tail starts, the bytes a rename edits. SPELT? holds while the use at the
\ byte that selected the declaration and every location placed spell the
\ tail, and UNSPELT$ says why, from the declaration, when one does not: a
\ DEFTYPE declares its two converters at its name's token, which spells
\ neither. EXPORT? holds when D's check records the declaration as an EXPORT
\ of another word, and EXPORT$ says so. RE-EXPORTED? holds when a location
\ placed is a token that also declares another word in its file, as an
\ EXPORT's operand does: a rename that edited the token would leave
\ that word's uses naming the old one. RE-EXPORT$ names the first such one.
\ LOCATIONS counts the locations, LOC-URI$ gives one's URI and LOC-TAIL its
\ range from where its tail starts. UNCOVERED$ says how many files are named,
\ after a phrase, then each one and why, as WARNING sends it.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the request being answered belongs to the
\ server's one task, and SELECT starts each one afresh.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/source.f
require lib/adt/option.f
require lib/sort.f
require lib/json-write.f
require lib/json-rpc.f
require src/core/sha256.f
require tools/lsp-docs.f
require tools/lsp-text.f
require tools/lsp-defs.f
require tools/lsp-check.f
require tools/lsp-workspace.f

package LSP-REFERENCES
using JSON-WRITE
using LSP-DOCS
using LSP-DEFS

private

2 constant WARNING-TYPE                  \ MessageType.Warning

\ A location: the file it is in, by its index among the files placed in,
\ the bytes it starts and ends at there, then the line and character it
\ starts at, those it ends at and those its tail starts at.
0 constant L-FILE
1 constant L-FROM
2 constant L-TO
3 constant L-LINE
4 constant L-CHAR
5 constant L-END-LINE
6 constant L-END-CHAR
7 constant L-TAIL-LINE
8 constant L-TAIL-CHAR
9 constant LOC-CELLS

\ A file placed in: its path's and its URI's offsets and lengths in POOL.
0 constant F-PATH
1 constant F-PATH-U
2 constant F-URI
3 constant F-URI-U
4 constant FILE-CELLS

\ A file named: its path's and its reason's offsets and lengths in POOL.
0 constant M-PATH
1 constant M-PATH-U
2 constant M-WHY
3 constant M-WHY-U
4 constant MISS-CELLS

\ A subject whose check was not verified: its index and its check's
\ verdict's offset and length in POOL.
0 constant X-SUB
1 constant X-WHY
2 constant X-WHY-U
3 constant PENDING-CELLS

DYNAMIC-BUFFER POOL u8                   \ the request's paths, URIs, reasons and digest,
variable POOL-U
DYNAMIC-BUFFER LOCS n                    \ the locations it answers,
variable LOC-N
DYNAMIC-BUFFER RANKS n                   \ their order in the answer,
DYNAMIC-BUFFER FILES n                   \ the files they are in,
variable FILE-N
DYNAMIC-BUFFER MISSES n                  \ the files it names,
variable MISS-N
DYNAMIC-BUFFER PENDING n                 \ the subjects SETTLE may name,
variable PENDING-N
DYNAMIC-BUFFER COVERED n                 \ whether a placing check read each subject,
DYNAMIC-BUFFER MSG u8                    \ and why it is refused, or its warning
variable MSG-U
TYPED-VARIABLE FOUND bool                \ whether a declaration is selected,
TYPED-VARIABLE REFUSED bool              \ whether the request is refused,
TYPED-VARIABLE SPELT bool                \ and whether its tokens spell the tail
TYPED-VARIABLE DECLARED bool             \ whether the declaration is placed
variable G                               \ the declaration: D's check's group for P,
DYNAMIC-BUFFER P-B u8                    \ P's path,
variable P-U
variable TS                              \ the bytes its token starts and ends at there,
variable TE
variable TOK-AT                          \ the token's text, once placed, in POOL,
variable TOK-U
variable NAME-AT                         \ its tail,
variable NAME-U
variable PKG-AT                          \ package
variable PKG-U
variable KEY-VIS                         \ and visibility,
variable VISIT                           \ the visit D's check gave it,
variable H-AT                            \ and the digest of the bytes of P it read
variable H-U
variable D-AT                            \ D's path in POOL,
variable D-U
variable AHEAD                           \ how many subjects come before the walk's,
variable CUR                             \ the subject being gathered, by the gather's index,
variable SUB-VISIT                       \ the visit its check declares the word in,
variable TEXT-IN                         \ the probe's group whose file positions count in,
TYPED-VARIABLE TEXT-OK bool              \ and whether its text has the digest read
variable RX-AT                           \ the first location that also declares
variable RX-U                            \ another word, said in POOL
DYNAMIC-BUFFER SRC u8                   \ a subject's text, read from disk,
variable SRC-U
SHA256-CTX-BYTES BUFFER: SHA-CTX         \ and a text's SHA-256 digest,
32 BUFFER: SHA-RAW
64 BUFFER: SHA-HEX                       \ as a file line states it
\ POOL, MSG and P-B hold a byte past those they name, which their writes
\ reserve: an accessor refuses an index at its capacity, and an empty string
\ still needs the address of index 0.

: LOC@ ( n n -- n )  swap LOC-CELLS * + LOCS @ ;
: LOC! ( n n n -- )  swap LOC-CELLS * + LOCS ! ;
: FILE@ ( n n -- n )  swap FILE-CELLS * + FILES @ ;
: FILE! ( n n n -- )  swap FILE-CELLS * + FILES ! ;
: PENDING@ ( n n -- n )  swap PENDING-CELLS * + PENDING @ ;
: PENDING! ( n n n -- )  swap PENDING-CELLS * + PENDING ! ;

\ The location K-th in the answer's order.
: RANKED ( n -- n )  RANKS @ ;
: MISS@ ( n n -- n )  swap MISS-CELLS * + MISSES @ ;
: MISS! ( n n n -- )  swap MISS-CELLS * + MISSES ! ;

: POOL$ ( n n -- ptr u8 n )
   {: at:n u:n :}
   at POOL u ;

: P$ ( -- ptr u8 n )  0 P-B P-U @ ;
: NAME$ ( -- ptr u8 n )  NAME-AT @ NAME-U @ POOL$ ;
: PKG$ ( -- ptr u8 n )  PKG-AT @ PKG-U @ POOL$ ;
: H$ ( -- ptr u8 n )  H-AT @ H-U @ POOL$ ;
: D$ ( -- ptr u8 n )  D-AT @ D-U @ POOL$ ;

\ The path of subject K: P's when the gather takes P before the walk's
\ subjects, else the walk's.
: SUBJECT-PATH$ ( n -- ptr u8 n )
   AHEAD @ - {: k:n :}
   k 0 < if P$ exit then
   k LSP-WORKSPACE:SUBJECT$ ;

: CUR$ ( -- ptr u8 n )  CUR @ SUBJECT-PATH$ ;

\ The index of the subject at this canonical path, -1 when none is.
: SUBJECT-AT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   AHEAD @ 0<> if a u P$ STR= if 0 exit then then
   a u LSP-WORKSPACE:SUBJECT-OF {: k:n :}
   k 0 < if -1 exit then
   k AHEAD @ + ;

\ The path of file F placed in.
: FILE$ ( n -- ptr u8 n )  dup F-PATH FILE@ swap F-PATH-U FILE@ POOL$ ;

\ The bytes appended to POOL: their offset there. They never lie in POOL,
\ which the append may move.
: POOL+ ( ptr u8 n -- n )
   {: a:ptr u:n :}
   POOL-U @ {: at:n :}
   at u + 1+ POOL-RESERVE
   a at POOL u BYTE-COPY
   u POOL-U +!
   at ;

: MSG+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   MSG-U @ {: at:n :}
   at u + 1+ MSG-RESERVE
   a at MSG u BYTE-COPY
   u MSG-U +! ;

: MSG$ ( -- ptr u8 n )  0 MSG MSG-U @ ;

\ A path as a message names it: relative to the working directory.
: SHOWN$ ( ptr u8 n -- ptr u8 n )  SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE ;

\ The digest of these bytes as a file line states it.
: DIGEST$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   SHA-CTX a u SHA-RAW SHA256-IN
   SHA-RAW SHA-HEX SHA256>HEX
   SHA-HEX 64 ;

: FRESH ( -- )
   0 POOL-U !
   1 POOL-RESERVE
   0 LOC-N !
   0 FILE-N !
   0 MISS-N !
   0 PENDING-N !
   0 AHEAD !
   0 RX-U !
   0 MSG-U !
   1 MSG-RESERVE
   false FOUND !
   false REFUSED !
   false DECLARED !
   true SPELT ! ;

\ The request refused for this fact about the file at this path.
: REFUSE ( ptr u8 n ptr u8 n -- )
   {: p:ptr pu:n w:ptr wu:n :}
   0 MSG-U !
   p pu SHOWN$ MSG+
   s" : " MSG+
   w wu MSG+
   true REFUSED !
   false FOUND ! ;

\ The file whose path is at this offset and length in POOL named, for the
\ reason at that offset and length there.
: NAMED! ( n n n n -- )
   {: at:n u:n why:n wu:n :}
   MISS-N @ {: k:n :}
   k 1+ MISS-CELLS * MISSES-RESERVE
   at k M-PATH MISS!
   u k M-PATH-U MISS!
   why k M-WHY MISS!
   wu k M-WHY-U MISS!
   k 1+ MISS-N ! ;

\ The file at this path named, for this reason. Neither string lies in POOL.
: MISS+ ( ptr u8 n ptr u8 n -- )
   {: p:ptr u:n w:ptr wu:n :}
   p u POOL+ {: at:n :}
   w wu POOL+ {: why:n :}
   at u why wu NAMED! ;

\ Whether the file at this path is named.
: NAMED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   MISS-N @ 0 ?do
      i M-PATH MISS@ i M-PATH-U MISS@ POOL$ a u STR= if true unloop exit then
   loop
   false ;

\ The bytes from byte FROM to byte TO of the text positions count in, held to
\ it as LSP-TEXT:LINE-CHARACTER holds an offset, and the byte they start at.
: SPAN-IN ( n n -- ptr u8 n n )
   {: from:n to:n :}
   LSP-TEXT:TEXT$ {: t:ptr tu:n :}
   from 0 max tu min {: a:n :}
   to a max tu min {: b:n :}
   t a + b a - a ;

public

\ Where the selected declaration's tail starts in the token from byte FROM to
\ byte TO of the text positions count in: at the token's start when it spells
\ the tail, letters compared without case as Habu compares names; else,
\ when the token is qualified (QUALIFIER-AT), after its qualifier when the
\ bytes after it spell the tail; else -1.
: TAIL-FROM ( n n -- n )
   SPAN-IN {: w:ptr wu:n a:n :}
   w wu NAME$ STR=CI if a exit then
   w wu QUALIFIER-AT {: at:n :}
   at 0 < if -1 exit then
   w at + wu at - NAME$ STR=CI if a at + else -1 then ;

private

\ The file at this canonical path, which does not lie in POOL, among the
\ files placed in, added when it is none of them: its index. Its URI is the
\ one positions count at in it (LSP-DEFS:DEP-URI$).
: FILE-AT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   FILE-N @ 0 ?do
      i FILE$ a u STR= if i unloop exit then
   loop
   FILE-N @ {: f:n :}
   f 1+ FILE-CELLS * FILES-RESERVE
   a u POOL+ f F-PATH FILE!
   u f F-PATH-U FILE!
   a u DEP-URI$ {: ua:ptr uu:n :}
   ua uu POOL+ f F-URI FILE!
   uu f F-URI-U FILE!
   f 1+ FILE-N !
   f ;

\ A location in file F from byte FROM to byte TO of the text positions count
\ in, last in the answer's order until SETTLE orders them, and where the
\ declaration's tail starts in it, its start when it spells none, which
\ clears SPELT.
: LOC+ ( n n n -- )
   {: f:n from:n to:n :}
   from to TAIL-FROM {: tail:n :}
   tail 0 < if false SPELT ! then
   LOC-N @ {: k:n :}
   k 1+ LOC-CELLS * LOCS-RESERVE
   k 1+ RANKS-RESERVE
   k k RANKS !
   f k L-FILE LOC!
   from k L-FROM LOC!
   to k L-TO LOC!
   from LSP-TEXT:LINE-CHARACTER k L-CHAR LOC! k L-LINE LOC!
   tail from max LSP-TEXT:LINE-CHARACTER k L-TAIL-CHAR LOC! k L-TAIL-LINE LOC!
   to from max LSP-TEXT:LINE-CHARACTER k L-END-CHAR LOC! k L-END-LINE LOC!
   k 1+ LOC-N ! ;

\ Whether a location in file F starts and ends at these bytes.
: LOCATED? ( n n n -- bool )
   {: f:n from:n to:n :}
   LOC-N @ 0 ?do
      i L-FILE LOC@ f = i L-FROM LOC@ from = and i L-TO LOC@ to = and
      if true unloop exit then
   loop
   false ;

\ ---- the declaration -----------------------------------------------------------

\ Whether this tail, package and visibility are the selected declaration's.
: KEY? ( ptr u8 n ptr u8 n n -- bool )
   {: w:ptr wu:n k:ptr ku:n v:n :}
   w wu NAME$ STR=  k ku PKG$ STR= and  v KEY-VIS @ = and ;

: DECL-KEY? ( n -- bool )
   {: d:n :}
   d DECL-NAME$ d DECL-PACKAGE$ d DECL-VIS KEY? ;

: USE-KEY? ( n -- bool )
   {: u:n :}
   u USE-NAME$ u USE-PACKAGE$ u USE-VIS KEY? ;

\ Whether these bytes are the selected declaration's token's.
: TOKEN? ( n n -- bool )
   {: from:n to:n :}
   from TS @ = to TE @ = and ;

\ Whether decl D is of group G, at the selected declaration's token.
: DECL-AT? ( n n -- bool )
   {: d:n g:n :}
   d DECL-GROUP g = if d DECL-BYTES TOKEN? else false then ;

\ Whether use U is bound to the token of group G at the selected declaration's.
: USE-AT? ( n n -- bool )
   {: u:n g:n :}
   u USE-GROUP g = if u USE-TARGET TOKEN? else false then ;

\ The declaration's file, by D's check's group G for it.
: P! ( n -- )
   {: g:n :}
   g G !
   g GROUP-PATH$ {: a:ptr u:n :}
   u 1+ P-B-RESERVE
   a 0 P-B u BYTE-COPY
   u P-U ! ;

: NAME! ( ptr u8 n ptr u8 n -- )
   {: w:ptr wu:n k:ptr ku:n :}
   w wu POOL+ NAME-AT !
   wu NAME-U !
   k ku POOL+ PKG-AT !
   ku PKG-U ! ;

\ The declaration use U is bound to, as its line states it.
: FROM-USE ( n -- )
   {: u:n :}
   u USE-GROUP P!
   u USE-TARGET TE ! TS !
   u USE-NAME$ u USE-PACKAGE$ NAME!
   u USE-VIS KEY-VIS !
   u USE-VISIT VISIT !
   true FOUND ! ;

\ The declaration decl D states.
: FROM-DECL ( n -- )
   {: d:n :}
   d DECL-GROUP P!
   d DECL-BYTES TE ! TS !
   d DECL-NAME$ d DECL-PACKAGE$ NAME!
   d DECL-VIS KEY-VIS !
   d DECL-VISIT VISIT !
   true FOUND ! ;

\ The declaration use U of D's check, in this slot, is bound to; refused
\ when the use's line states no whole identity. A use that does not spell
\ the declaration's tail in D's text clears SPELT.
: AT-USE ( n n -- )
   {: slot:n u:n :}
   u USE-VISIT 0 < if slot DOC-CANON$ s" the use has no declaration identity" REFUSE exit then
   u FROM-USE
   u USE-BYTES TAIL-FROM 0 < if false SPELT ! then ;

: HOLDS? ( n n n -- bool )
   {: from:n to:n at:n :}
   from at <= at to < and ;

\ The first decl of the check of the document in this slot, of group G, whose
\ token holds this byte, if one does.
: DECL-HOLDING ( n n n -- option<n> )
   {: slot:n g:n at:n :}
   slot DEFS-DECL-RANGE ?do
      i DECL-GROUP g = if
         i DECL-BYTES at HOLDS? if i OPTION:SOME unloop exit then
      then
   loop
   OPTION:NONE ;

\ Why the decls of D's check, in this slot, at the selected token in group G
\ select no one declaration, else empty.
: TOKEN-WHY ( n n -- ptr u8 n )
   {: slot:n g:n :}
   slot DEFS-DECL-RANGE ?do
      i g DECL-AT? if
         i DECL-VISIT 0 < if s" the token has no declaration identity" unloop exit then
         i DECL-KEY? 0= if s" the token declares several words" unloop exit then
         i DECL-VISIT VISIT @ <> if
            s" the token declares the word in several visits" unloop exit
         then
      then
   loop
   s" " ;

\ The declaration decl D of D's check, in this slot, states at its token,
\ when the token declares no other.
: AT-DECL ( n n -- )
   {: slot:n d:n :}
   d FROM-DECL
   slot G @ TOKEN-WHY {: w:ptr wu:n :}
   wu 0<> if P$ w wu REFUSE then ;

\ The declaration the definition token of D's own file, in group G, that
\ holds this byte states, if one does.
: AT-TOKEN-IN ( n n n -- )
   {: slot:n g:n at:n :}
   slot g at DECL-HOLDING MATCH option
      some OF slot swap AT-DECL ENDOF
      none OF ENDOF
   ;MATCH ;

: AT-TOKEN ( n n -- )
   {: slot:n at:n :}
   slot DEFS-OWN-GROUP MATCH option
      some OF slot swap at AT-TOKEN-IN ENDOF
      none OF ENDOF
   ;MATCH ;

\ How many decls of the check of the document in this slot are the selected
\ declaration: of its group, at its token, with its identity and visit.
: HELD ( n -- n )
   {: slot:n :}
   0
   slot DEFS-DECL-RANGE ?do
      i G @ DECL-AT? if
         i DECL-VISIT VISIT @ = if i DECL-KEY? if 1+ then then
      then
   loop ;

\ D's check, in this slot, holding the selected declaration once, and the
\ digest of the bytes of P it read, kept.
: ANCHORED ( n -- )
   {: slot:n :}
   slot HELD 1 <> if P$ s" the check does not hold the declaration once" REFUSE exit then
   G @ GROUP-SHA$ {: a:ptr u:n :}
   u 0= if P$ s" the check states no digest of the file" REFUSE exit then
   a u POOL+ H-AT !
   u H-U ! ;

\ The declaration this byte of the text of D, in this slot, selects.
: LOCATE ( n n -- )
   {: slot:n at:n :}
   slot DEFS-VERIFIED? 0= if slot DOC-CANON$ s" its check is not verified" REFUSE exit then
   slot at DEFS-USE-AT MATCH option
      some OF slot swap AT-USE ENDOF
      none OF slot at AT-TOKEN ENDOF
   ;MATCH
   FOUND @ if slot ANCHORED then ;

\ Positions count in the text of the file at this path: the open document's
\ that holds it, else the file's.
: TEXT-OF! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u DOC-HOLDING MATCH option
      some OF DOC-TEXT$ LSP-TEXT:TEXT! ENDOF
      none OF a u LSP-TEXT:FILE-TEXT! ENDOF
   ;MATCH ;

\ The declaration's location, first, and its token's text, while P's text has
\ the digest D's check read.
: DECLARATION ( -- )
   P$ TEXT-OF!
   LSP-TEXT:TEXT$ DIGEST$ H$ STR= 0= if P$ s" changed since it was checked" REFUSE exit then
   TS @ TE @ SPAN-IN drop {: a:ptr u:n :}
   a u POOL+ TOK-AT !
   u TOK-U !
   P$ FILE-AT TS @ TE @ LOC+
   true DECLARED ! ;

\ ---- the subjects ---------------------------------------------------------------

\ A file whose loads the walk could not find named, unless it is P or D,
\ whose checks say what they read.
: UNREAD+ ( ptr u8 n ptr u8 n -- )
   {: p:ptr pu:n w:ptr wu:n :}
   p pu P$ STR= p pu D$ STR= or if exit then
   p pu w wu MISS+ ;

\ The subjects of D, in this slot, and the files the walk could not read. P
\ is the first subject, as REACHING orders them, or one before them when the
\ walk gives none at its path, so that its own uses are read; no placing
\ check has read any yet.
: SUBJECTS! ( n -- )
   {: slot:n :}
   slot DOC-CANON$ {: a:ptr u:n :}
   a u POOL+ D-AT !
   u D-U !
   P$ D$ [: -1 swap DOC-EACH-BUT ;] LSP-WORKSPACE:REACHING
   LSP-WORKSPACE:REFUSED? if
      LSP-WORKSPACE:BLOCKED$ s" the workspace folder cannot be walked" REFUSE exit
   then
   LSP-WORKSPACE:UNREADABLE 0 ?do i LSP-WORKSPACE:UNREADABLE$ UNREAD+ loop
   0 LSP-WORKSPACE:SUBJECT$ P$ STR= if 0 else 1 then AHEAD !
   LSP-WORKSPACE:SUBJECTS AHEAD @ + {: n:n :}
   n COVERED-RESERVE
   n 0 ?do 0 i COVERED ! loop ;

\ ---- a subject's uses -----------------------------------------------------------

\ Why the check of the subject in this slot, whose group for P is G, cannot
\ place its uses of the declaration by their declaration, else empty, the
\ visit it declares the word in, in SUB-VISIT: -1 when none.
: DECL-WHY ( n n -- ptr u8 n )
   {: slot:n g:n :}
   -1 SUB-VISIT !
   slot DEFS-DECL-RANGE ?do
      i g DECL-AT? if
         i DECL-VISIT 0 < if s" no declaration identity" unloop exit then
         i DECL-KEY? if
            SUB-VISIT @ 0 < if i DECL-VISIT SUB-VISIT ! then
            i DECL-VISIT SUB-VISIT @ <> if s" several declaration visits" unloop exit then
         then
      then
   loop
   s" " ;

\ Why a use of that check bound to the declaration's token cannot be placed,
\ else empty.
: USE-WHY ( n n -- ptr u8 n )
   {: slot:n g:n :}
   slot DEFS-USE-RANGE ?do
      i g USE-AT? if
         i USE-VISIT 0 < if s" no use identity" unloop exit then
         i USE-KEY? if
            SUB-VISIT @ 0 < if s" no declaration visit" unloop exit then
            i USE-VISIT SUB-VISIT @ <> if s" a use of another visit" unloop exit then
         then
      then
   loop
   s" " ;

\ Why the check of the subject in this slot, whose group for P is G, cannot
\ contribute, else empty.
: WHY ( n n -- ptr u8 n )
   {: slot:n g:n :}
   g GROUP-SHA$ {: a:ptr u:n :}
   u 0= if s" no digest" exit then
   a u H$ STR= 0= if s" digest differs" exit then
   slot g DECL-WHY {: w:ptr wu:n :}
   wu 0<> if w wu exit then
   slot g USE-WHY ;

\ The first decl of the check of the subject in this slot, of group G, whose
\ token starts and ends at these bytes and declares another word than the
\ selected declaration, if one does.
: OTHER-DECL ( n n n n -- option<n> )
   {: slot:n g:n from:n to:n :}
   slot DEFS-DECL-RANGE ?do
      i DECL-GROUP g = if
         i DECL-BYTES to = swap from = and if
            i DECL-KEY? 0= if i OPTION:SOME unloop exit then
         then
      then
   loop
   OPTION:NONE ;

\ The location just placed, from use U of the probe, in the file of its
\ group IN, whose token also declares decl D, said in POOL: that file, the
\ line and text of the token, and the word it declares, whose uses a rename
\ would not edit.
: RE-EXPORT! ( n n n -- )
   {: in:n u:n d:n :}
   0 MSG-U !
   in GROUP-PATH$ SHOWN$ MSG+
   s" : the token " MSG+
   u USE-BYTES SPAN-IN drop MSG+
   s"  at line " MSG+
   SB-RESET LOC-N @ 1- L-LINE LOC@ 1+ FMT:SB-INT SB$ MSG+
   s"  also declares " MSG+
   d DECL-PACKAGE$ {: k:ptr ku:n :}
   ku 0<> if k ku MSG+ s" :" MSG+ then
   d DECL-NAME$ MSG+
   s" , whose uses a rename would not edit" MSG+
   MSG$ POOL+ RX-AT !
   MSG-U @ RX-U !
   0 MSG-U ! ;

\ Use U of the probe, just placed, kept for RE-EXPORT$ when its token also
\ declares another word in the probe's group IN, the file the use is in,
\ unless a location is kept already.
: RE-EXPORTS ( n n -- )
   {: in:n u:n :}
   RX-U @ 0<> if exit then
   PROBE in u USE-BYTES OTHER-DECL MATCH option
      some OF {: d:n :} in u d RE-EXPORT! ENDOF
      none OF ENDOF
   ;MATCH ;

\ Positions count in the text of the file of the probe's group IN, and
\ TEXT-OK holds whether that text has the digest the check read of the file;
\ the file is named, once, when it does not.
: TEXT-IN! ( n -- )
   {: in:n :}
   in TEXT-IN !
   in GROUP-PATH$ TEXT-OF!
   LSP-TEXT:TEXT$ DIGEST$ in GROUP-SHA$ STR= {: ok:bool :}
   ok TEXT-OK !
   ok if exit then
   in GROUP-PATH$ NAMED? if exit then
   in GROUP-PATH$ s" changed since it was checked" MISS+ ;

\ Use U of the probe, bound to the declaration, placed in the file it is in,
\ unless a location is there already or that file's text no longer has the
\ digest the check read; kept for RE-EXPORT$ when its token also declares
\ another word in that file.
: PLACE-USE ( n -- )
   {: u:n :}
   u USE-IN {: in:n :}
   in GROUP-PATH$ FILE-AT {: f:n :}
   u USE-BYTES {: from:n to:n :}
   f from to LOCATED? if exit then
   in TEXT-IN @ <> if in TEXT-IN! then
   TEXT-OK @ 0= if exit then
   f from to LOC+
   in u RE-EXPORTS ;

\ The uses of the declaration the probe, whose group for P is G, states,
\ each placed in the file it is in.
: PLACED ( n -- )
   {: g:n :}
   -1 TEXT-IN !
   PROBE DEFS-USE-RANGE ?do
      i g USE-AT? if i USE-KEY? if i PLACE-USE then then
   loop ;

\ The subject at this canonical path, if one is, marked read by a check whose
\ uses are placed.
: COVER! ( ptr u8 n -- )
   SUBJECT-AT {: k:n :}
   k 0 >= if 1 k COVERED ! then ;

\ Each subject whose file the probe read one text of marked read.
: COVERS ( -- )
   PROBE DEFS-GROUP-RANGE ?do
      i GROUP-SHA$ nip 0<> if i GROUP-PATH$ COVER! then
   loop ;

\ The uses the probe, whose group for P is G, binds to the declaration
\ placed, and the subjects it read marked; or the subject named.
: CONTRIBUTED ( n -- )
   {: g:n :}
   PROBE g WHY {: w:ptr wu:n :}
   wu 0<> if CUR$ w wu MISS+ exit then
   COVERS
   g PLACED ;

\ The subject being gathered held for SETTLE with its check's verdict, which
\ does not lie in POOL.
: PENDING+ ( ptr u8 n -- )
   {: w:ptr wu:n :}
   w wu POOL+ {: why:n :}
   PENDING-N @ {: k:n :}
   k 1+ PENDING-CELLS * PENDING-RESERVE
   CUR @ k X-SUB PENDING!
   why k X-WHY PENDING!
   wu k X-WHY-U PENDING!
   k 1+ PENDING-N ! ;

\ The subject's uses from the probe, the check of its text, or the subject
\ held for SETTLE, with its check's verdict, when that check was not
\ verified. A check that read no P has none.
: CONTRIBUTE ( -- )
   PROBE DEFS-VERIFIED? 0= if LSP-CHECK:OUTCOME$ PENDING+ exit then
   PROBE P$ DEFS-GROUP-OF MATCH option
      some OF CONTRIBUTED ENDOF
      none OF ENDOF
   ;MATCH ;

: SRC-ROOM ( n -- ptr u8 )
   1+ SRC-RESERVE 0 SRC ;

\ The file at this path read into SRC, its length in SRC-U.
: READ-SRC ( ptr u8 n -- ptr u8 n )
   2dup [: SRC-ROOM ;] SOURCE:READ-WHOLE-FILE SRC-U ! ;

\ The subject's uses from a check of this text of its file.
: FROM-TEXT ( ptr u8 n -- )
   CUR$ NAME$ [: CONTRIBUTE ;] LSP-CHECK:USES ;

\ The subject's uses from a check of its file on disk.
: FROM-DISK ( -- )
   CUR$ [: READ-SRC ;] catch {: code:n :} 2drop
   code 0<> if
      code LSP-TEXT:FS-RC? 0= if code throw then
      CUR$ s" cannot be read" MISS+ exit
   then
   0 SRC SRC-U @ FROM-TEXT ;

\ Pending subject K named by its check's verdict, unless a check whose uses
\ are placed read its file.
: SETTLED ( n -- )
   {: k:n :}
   k X-SUB PENDING@ {: s:n :}
   s COVERED @ 0<> if exit then
   s SUBJECT-PATH$ {: a:ptr u:n :}
   a u POOL+ u k X-WHY PENDING@ k X-WHY-U PENDING@ NAMED! ;

\ Whether location A comes before location B in the answer: by file, P's
\ first and the others in the byte order of their paths, then by the bytes
\ they start and end at.
: LOC< ( n n -- bool )
   {: a:n b:n :}
   a L-FILE LOC@ {: fa:n :}
   b L-FILE LOC@ {: fb:n :}
   fa fb <> if
      fa FILE$ P$ STR= if true exit then
      fb FILE$ P$ STR= if false exit then
      fa FILE$ fb FILE$ LSP-WORKSPACE:PATH< exit
   then
   a L-FROM LOC@ b L-FROM LOC@ <> if a L-FROM LOC@ b L-FROM LOC@ < exit then
   a L-TO LOC@ b L-TO LOC@ < ;

\ The declaration at this byte of the text of D, in this slot, selected, and
\ placed when TOO holds.
: LOCATED ( n n bool -- )
   {: slot:n at:n too:bool :}
   FRESH
   slot at LOCATE
   FOUND @ too and if DECLARATION then ;

\ The declaration once placed, the first location, said in MSG: its file, the
\ line and text of its token, and its tail.
: DECLARED-AT ( -- )
   0 MSG-U !
   P$ SHOWN$ MSG+
   s" : the token " MSG+
   TOK-AT @ TOK-U @ POOL$ MSG+
   s"  at line " MSG+
   SB-RESET 0 L-LINE LOC@ 1+ FMT:SB-INT SB$ MSG+
   s"  declares " MSG+
   NAME$ MSG+ ;

public

\ Selects the declaration at this byte of the text of the open document in
\ this slot, which positions count in, and places it; or refuses the request.
: PLACE ( n n -- )  true LOCATED ;

\ Selects the declaration at this byte of the text of the open document in
\ this slot, which positions count in, places it when the request includes
\ it, and finds the subjects; or refuses the request.
: SELECT ( n n bool -- )
   {: slot:n at:n too:bool :}
   slot at too LOCATED
   FOUND @ if slot SUBJECTS! then ;

\ Whether the request is refused, and why.
: REFUSED? ( -- bool )  REFUSED @ ;
: BLOCKED$ ( -- ptr u8 n )  MSG$ ;

\ Whether a declaration is selected, and the bytes its token starts and ends
\ at in P's text.
: FOUND? ( -- bool )  FOUND @ ;
: TOKEN ( -- n n )  TS @ TE @ ;

\ Whether the use that selected the declaration and every location placed
\ spell its tail.
: SPELT? ( -- bool )  SPELT @ ;

\ Why a token does not spell the tail, said of the declaration once placed,
\ the first location: its file, the line and text of its token, and its tail.
: UNSPELT$ ( -- ptr u8 n )
   DECLARED-AT
   s" , but a token naming it does not spell it" MSG+
   MSG$ ;

\ Whether D's check records the declaration as an EXPORT of another word,
\ and that said of it as UNSPELT$ says why.
: EXPORT? ( -- bool )
   G @ TS @ TE @ GROUP-REC-AT MATCH option
      some OF REC-CLASS CLASS-EXPORT = ENDOF
      none OF false ENDOF
   ;MATCH ;
: EXPORT$ ( -- ptr u8 n )
   DECLARED-AT
   s"  as an EXPORT of another word" MSG+
   MSG$ ;

\ Whether a location placed is a token that also declares another word in its
\ subject's file, and the first such one said: its file, the line and text
\ of the token, and the word.
: RE-EXPORTED? ( -- bool )  RX-U @ 0<> ;
: RE-EXPORT$ ( -- ptr u8 n )  RX-AT @ RX-U @ POOL$ ;

\ How many subjects the request gathers from: none unless it selected a
\ declaration, since only then did it walk.
: SUBJECT-N ( -- n )  FOUND @ if LSP-WORKSPACE:SUBJECTS AHEAD @ + else 0 then ;

\ Subject K's uses of the declaration, from a check of the text of the open
\ document that holds its file, else of the file on disk; or the subject
\ named.
: GATHER ( n -- )
   CUR !
   CUR$ NAMED? if exit then
   CUR$ DOC-HOLDING MATCH option
      some OF DOC-TEXT$ FROM-TEXT ENDOF
      none OF FROM-DISK ENDOF
   ;MATCH ;

\ Once every subject is gathered: each subject whose check was not verified
\ named by its verdict, unless a check whose uses are placed read its file,
\ and the locations in the answer's order, the declaration first when it is
\ placed.
: SETTLE ( -- )
   PENDING-N @ 0 ?do i SETTLED loop
   DECLARED @ if 1 else 0 then {: first:n :}
   LOC-N @ first - {: n:n :}
   n 2 < if exit then
   first RANKS n [: LOC< ;] SORT:SORT! ;

\ Whether a file is named.
: UNCOVERED? ( -- bool )  MISS-N @ 0 > ;

\ This phrase, then how many files are named and, a line each, every one's
\ path relative to the working directory and why.
: UNCOVERED$ ( ptr u8 n -- ptr u8 n )
   0 MSG-U !
   MSG+
   s" : " MSG+
   SB-RESET MISS-N @ FMT:SB-INT SB$ MSG+
   MISS-N @ 1 = if s"  file not covered:" else s"  files not covered:" then MSG+
   MISS-N @ 0 ?do
      s\" \n" MSG+
      i M-PATH MISS@ i M-PATH-U MISS@ POOL$ SHOWN$ MSG+
      s" : " MSG+
      i M-WHY MISS@ i M-WHY-U MISS@ POOL$ MSG+
   loop
   MSG$ ;

\ The window/showMessage warning naming the files, to the writer.
: WARNING ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   s" References may be incomplete" UNCOVERED$ {: m:ptr mu:n :}
   s" window/showMessage" JSON-RPC:NOTIFY
   OBJECT-START
   s" type" WARNING-TYPE FIELD-U COMMA
   s" message" m mu FIELD-S
   OBJECT-END ;

\ How many locations the request answers; location K's URI, the range from
\ where its tail starts to its end, the bytes a rename edits, and whether a
\ qualifier precedes that tail.
: LOCATIONS ( -- n )  LOC-N @ ;
: LOC-URI$ ( n -- ptr u8 n )
   RANKED L-FILE LOC@ {: f:n :}
   f F-URI FILE@ f F-URI-U FILE@ POOL$ ;
: LOC-TAIL ( n -- n n n n )
   RANKED {: k:n :}
   k L-TAIL-LINE LOC@ k L-TAIL-CHAR LOC@ k L-END-LINE LOC@ k L-END-CHAR LOC@ ;
: LOC-QUALIFIED? ( n -- bool )
   RANKED {: k:n :}
   k L-TAIL-LINE LOC@ k L-LINE LOC@ <>  k L-TAIL-CHAR LOC@ k L-CHAR LOC@ <>  or ;

private

: LOCATION ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: k:n :}
   k RANKED {: r:n :}
   OBJECT-START
   s" uri" k LOC-URI$ FIELD-S COMMA
   r L-LINE LOC@ r L-CHAR LOC@ r L-END-LINE LOC@ r L-END-CHAR LOC@ LSP-TEXT:RANGE-AT
   OBJECT-END ;

public

\ The result of textDocument/references, the Location list, to the writer.
: ANSWER ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   ARRAY-START
   LOC-N @ 0 ?do
      i 0 > if COMMA then
      i LOCATION
   loop
   ARRAY-END ;

;using
;using
;using
;package
