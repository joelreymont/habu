\ lsp-references.f - the language server's references: the declaration a
\ position selects in an open document whose check was verified, and every use
\ of it in the files whose loads reach its file, as their own checks bound
\ them.
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
\ as the walk gives them (tools/lsp-workspace.f): P first, then by path. Each
\ one's uses come from its own check: an open document's last completed one,
\ a file on disk checked now (LSP-CHECK:USES). A check numbers only its own
\ visits, so a subject's check that read P must state P's digest the same and
\ declare the word at P's token, by its identity, in one visit at most, and
\ its uses bound to that token of the word's identity must carry that visit;
\ one that declares the word there in no visit and binds no use of it there
\ holds none, and contributes nothing. The locations are the declaration,
\ when included, then each subject's uses in turn, in the subjects' order.
\
\ The request is refused, REFUSED? true and BLOCKED$ naming the file relative
\ to the working directory and why, when D's check is not verified; the
\ position selects a use or a token without a whole identity, a token that
\ declares several words or one word in several visits, or a declaration D's
\ check does not hold once or holds no digest of P for; the declaration is
\ included and P's text no longer has the digest; or a workspace folder's walk
\ fails. Otherwise the answer comes, and each file it could not cover is named
\ with why (WARNING), contributing no location: a workspace file whose loads
\ discovery cannot find, but P and D, whose checks say what they read; a
\ subject whose check was not verified, a file on disk by its check's verdict
\ (LSP-CHECK:OUTCOME$); one that cannot be read; one whose check states no
\ digest of P or another one, declares at P's token a word without a whole
\ identity or the word in several visits, or binds to that token a use
\ without a whole identity, or one of the word in another visit or while it
\ declares the word there in none.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the request being answered belongs to the
\ server's one task, and SELECT starts each one afresh.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/source.f
require lib/adt/option.f
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

\ A location: its URI's offset and length in POOL, then the line and
\ character it starts at and those it ends at.
0 constant L-URI
1 constant L-URI-U
2 constant L-LINE
3 constant L-CHAR
4 constant L-END-LINE
5 constant L-END-CHAR
6 constant LOC-CELLS

\ A file named: its path's and its reason's offsets and lengths in POOL.
0 constant M-PATH
1 constant M-PATH-U
2 constant M-WHY
3 constant M-WHY-U
4 constant MISS-CELLS

DYNAMIC-BUFFER POOL u8                   \ the request's paths, URIs, reasons and digest,
variable POOL-U
DYNAMIC-BUFFER LOCS n                    \ the locations it answers,
variable LOC-N
DYNAMIC-BUFFER MISSES n                  \ the files it names,
variable MISS-N
DYNAMIC-BUFFER MSG u8                    \ and why it is refused, or its warning
variable MSG-U
TYPED-VARIABLE FOUND bool                \ whether a declaration is selected,
TYPED-VARIABLE REFUSED bool              \ whether the request is refused
variable G                               \ the declaration: D's check's group for P,
variable P-AT                            \ P's path in POOL,
variable P-U
variable TS                              \ the bytes its token starts and ends at there,
variable TE
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
variable CUR                             \ the subject being gathered, by the walk's index,
variable URI-AT                          \ the URI its positions count at,
variable URI-U
variable SUB-VISIT                       \ and the visit its check declares the word in
DYNAMIC-BUFFER SRC u8                    \ a subject's text, read from disk,
variable SRC-U
SHA256-CTX-BYTES BUFFER: SHA-CTX         \ and a text's SHA-256 digest,
32 BUFFER: SHA-RAW
64 BUFFER: SHA-HEX                       \ as a file line states it
\ POOL and MSG hold a byte past those they name, which their appends reserve:
\ an accessor refuses an index at its capacity, and an empty string still
\ needs the address of index 0.

: LOC@ ( n n -- n )  swap LOC-CELLS * + LOCS @ ;
: LOC! ( n n n -- )  swap LOC-CELLS * + LOCS ! ;
: MISS@ ( n n -- n )  swap MISS-CELLS * + MISSES @ ;
: MISS! ( n n n -- )  swap MISS-CELLS * + MISSES ! ;

: POOL$ ( n n -- ptr u8 n )
   {: at:n u:n :}
   at POOL u ;

: P$ ( -- ptr u8 n )  P-AT @ P-U @ POOL$ ;
: NAME$ ( -- ptr u8 n )  NAME-AT @ NAME-U @ POOL$ ;
: PKG$ ( -- ptr u8 n )  PKG-AT @ PKG-U @ POOL$ ;
: H$ ( -- ptr u8 n )  H-AT @ H-U @ POOL$ ;
: D$ ( -- ptr u8 n )  D-AT @ D-U @ POOL$ ;
: CUR$ ( -- ptr u8 n )  CUR @ LSP-WORKSPACE:SUBJECT$ ;

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
   0 MISS-N !
   0 MSG-U !
   1 MSG-RESERVE
   false FOUND !
   false REFUSED ! ;

\ The request refused for this fact about the file at this path.
: REFUSE ( ptr u8 n ptr u8 n -- )
   {: p:ptr pu:n w:ptr wu:n :}
   0 MSG-U !
   p pu SHOWN$ MSG+
   s" : " MSG+
   w wu MSG+
   true REFUSED !
   false FOUND ! ;

\ The file at this path named, for this reason. Neither string lies in POOL.
: MISS+ ( ptr u8 n ptr u8 n -- )
   {: p:ptr u:n w:ptr wu:n :}
   p u POOL+ {: at:n :}
   w wu POOL+ {: why:n :}
   MISS-N @ {: k:n :}
   k 1+ MISS-CELLS * MISSES-RESERVE
   at k M-PATH MISS!
   u k M-PATH-U MISS!
   why k M-WHY MISS!
   wu k M-WHY-U MISS!
   k 1+ MISS-N ! ;

\ Whether the file at this path is named.
: NAMED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   MISS-N @ 0 ?do
      i M-PATH MISS@ i M-PATH-U MISS@ POOL$ a u STR= if true unloop exit then
   loop
   false ;

: URI! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u POOL+ URI-AT !
   u URI-U ! ;

\ A location at URI-AT from byte FROM to byte TO of the text positions count
\ in.
: LOC+ ( n n -- )
   {: from:n to:n :}
   LOC-N @ {: k:n :}
   k 1+ LOC-CELLS * LOCS-RESERVE
   URI-AT @ k L-URI LOC!
   URI-U @ k L-URI-U LOC!
   from LSP-TEXT:LINE-CHARACTER k L-CHAR LOC! k L-LINE LOC!
   to from max LSP-TEXT:LINE-CHARACTER k L-END-CHAR LOC! k L-END-LINE LOC!
   k 1+ LOC-N ! ;

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
   a u POOL+ P-AT !
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
\ when the use's line states no whole identity.
: AT-USE ( n n -- )
   {: slot:n u:n :}
   u USE-VISIT 0 < if slot DOC-CANON$ s" the use has no declaration identity" REFUSE exit then
   u FROM-USE ;

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

\ Positions count in P's text: the open document's that holds P, else the
\ file's.
: P-TEXT! ( -- )
   P$ DOC-HOLDING MATCH option
      some OF DOC-TEXT$ LSP-TEXT:TEXT! ENDOF
      none OF P$ LSP-TEXT:FILE-TEXT! ENDOF
   ;MATCH ;

\ The declaration's location, first, while P's text has the digest D's check
\ read.
: DECLARATION ( -- )
   P-TEXT!
   LSP-TEXT:TEXT$ DIGEST$ H$ STR= 0= if P$ s" changed since it was checked" REFUSE exit then
   P$ DEP-URI$ URI!
   TS @ TE @ LOC+ ;

\ ---- the subjects ---------------------------------------------------------------

\ Each open document's canonical path and text, given to Q.
: OPEN-DOCS ( [ ptr u8 n ptr u8 n -- ] -- )
   {: q :}
   DOC-SLOTS 0 ?do
      i DOC-LIVE? if i DOC-CANON$ i DOC-TEXT$ q execute then
   loop ;

\ A file whose loads the walk could not find named, unless it is P or D,
\ whose checks say what they read.
: UNREAD+ ( ptr u8 n ptr u8 n -- )
   {: p:ptr pu:n w:ptr wu:n :}
   p pu P$ STR= p pu D$ STR= or if exit then
   p pu w wu MISS+ ;

\ The subjects of D, in this slot, and the files the walk could not read.
: SUBJECTS! ( n -- )
   {: slot:n :}
   slot DOC-CANON$ {: a:ptr u:n :}
   a u POOL+ D-AT !
   u D-U !
   P$ D$ [: OPEN-DOCS ;] LSP-WORKSPACE:REACHING
   LSP-WORKSPACE:REFUSED? if
      LSP-WORKSPACE:BLOCKED$ s" the workspace folder cannot be walked" REFUSE exit
   then
   LSP-WORKSPACE:UNREADABLE 0 ?do i LSP-WORKSPACE:UNREADABLE$ UNREAD+ loop ;

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

\ The uses of the declaration the check of the subject in this slot, whose
\ group for P is G, states, in the text positions count in.
: PLACED ( n n -- )
   {: slot:n g:n :}
   slot DEFS-USE-RANGE ?do
      i g USE-AT? if i USE-KEY? if i USE-BYTES LOC+ then then
   loop ;

: CONTRIBUTED ( n n -- )
   {: slot:n g:n :}
   slot g WHY {: w:ptr wu:n :}
   wu 0<> if CUR$ w wu MISS+ exit then
   slot g PLACED ;

\ The subject's uses from the check of the document in this slot, or the
\ probe's, or the subject named for this reason when that check was not
\ verified. A check that read no P has none.
: CONTRIBUTE ( n ptr u8 n -- )
   {: slot:n w:ptr wu:n :}
   slot DEFS-VERIFIED? 0= if CUR$ w wu MISS+ exit then
   slot P$ DEFS-GROUP-OF MATCH option
      some OF slot swap CONTRIBUTED ENDOF
      none OF ENDOF
   ;MATCH ;

: SRC-ROOM ( n -- ptr u8 )
   1+ SRC-RESERVE 0 SRC ;

\ The file at this path read into SRC, its length in SRC-U.
: READ-SRC ( ptr u8 n -- ptr u8 n )
   2dup [: SRC-ROOM ;] SOURCE:READ-WHOLE-FILE SRC-U ! ;

\ The subject's uses from its check as the probe, positions counting in the
\ text read.
: ON-DISK ( -- )
   0 SRC SRC-U @ LSP-TEXT:TEXT!
   PROBE LSP-CHECK:OUTCOME$ CONTRIBUTE ;

\ The subject's uses from a check of its file on disk.
: FROM-DISK ( -- )
   CUR$ [: READ-SRC ;] catch {: code:n :} 2drop
   code 0<> if
      code LSP-TEXT:FS-RC? 0= if code throw then
      CUR$ s" cannot be read" MISS+ exit
   then
   0 SRC SRC-U @ CUR$ [: ON-DISK ;] LSP-CHECK:USES ;

public

\ Selects the declaration at this byte of the text of the open document in
\ this slot, which positions count in, places it when the request includes
\ it, and finds the subjects; or refuses the request.
: SELECT ( n n bool -- )
   {: slot:n at:n too:bool :}
   FRESH
   slot at LOCATE
   FOUND @ too and if DECLARATION then
   FOUND @ if slot SUBJECTS! then ;

\ Whether the request is refused, and why.
: REFUSED? ( -- bool )  REFUSED @ ;
: BLOCKED$ ( -- ptr u8 n )  MSG$ ;

\ How many subjects the request gathers from: none unless it selected a
\ declaration, since only then did it walk.
: SUBJECT-N ( -- n )  FOUND @ if LSP-WORKSPACE:SUBJECTS else 0 then ;

\ The slot of the open document that holds subject K, -1 when none does.
: SUBJECT-SLOT ( n -- n )
   LSP-WORKSPACE:SUBJECT$ DOC-HOLDING MATCH option
      some OF ENDOF
      none OF -1 ENDOF
   ;MATCH ;

\ Subject K's uses of the declaration, or the subject named.
: GATHER ( n -- )
   CUR !
   CUR$ NAMED? if exit then
   CUR$ DEP-URI$ URI!
   CUR$ DOC-HOLDING MATCH option
      some OF {: slot:n :} slot DOC-TEXT$ LSP-TEXT:TEXT! slot s" not verified" CONTRIBUTE ENDOF
      none OF FROM-DISK ENDOF
   ;MATCH ;

\ Whether a file is named.
: UNCOVERED? ( -- bool )  MISS-N @ 0 > ;

\ The window/showMessage warning naming the files, to the writer.
: WARNING ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   0 MSG-U !
   s" References may be incomplete: " MSG+
   SB-RESET MISS-N @ FMT:SB-INT SB$ MSG+
   MISS-N @ 1 = if s"  file not covered:" else s"  files not covered:" then MSG+
   MISS-N @ 0 ?do
      s\" \n" MSG+
      i M-PATH MISS@ i M-PATH-U MISS@ POOL$ SHOWN$ MSG+
      s" : " MSG+
      i M-WHY MISS@ i M-WHY-U MISS@ POOL$ MSG+
   loop
   s" window/showMessage" JSON-RPC:NOTIFY
   OBJECT-START
   s" type" WARNING-TYPE FIELD-U COMMA
   s" message" MSG$ FIELD-S
   OBJECT-END ;

private

: LOCATION ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: k:n :}
   OBJECT-START
   s" uri" k L-URI LOC@ k L-URI-U LOC@ POOL$ FIELD-S COMMA
   k L-LINE LOC@ k L-CHAR LOC@ k L-END-LINE LOC@ k L-END-CHAR LOC@ LSP-TEXT:RANGE-AT
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
