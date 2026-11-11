\ source-discovery.f - whole-file ordered source-composition discovery pass.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/memory.f
\ src/lib/fs.f src/lib/source.f tools/source-discovery.f
\ RUN replays every literal loader form, colon bodies included, in the core
\ loader's record-only mode (src/core/include.f event log): the closure is a
\ superset of the runtime one. Recorded loader-token spans are entry-file offsets.
\
\ A harvest collects all declarations and retirements the walk understands, even
\ before their row is known, and decides a row's safety from the complete set.
\ Every walk tracks the open package from the package records and the sites the
\ replay learned. Passes repeat until the rows settle: a changed candidate set
\ needs a complete scan under its new assumptions, and a fixed one can only lose
\ enabled rows; finite source occurrences make this settle without a pass count.
\ Re-enabling a row could hide evidence exposed by a disable.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/fs.f
require src/lib/source.f

package DISCOVER
using SOURCE
using SOURCE-ROOT

PATH-CAP constant SD-PATH-CAP

$5C constant SD-BACKSLASH
$28 constant SD-LPAREN
$29 constant SD-RPAREN
$0A constant SD-LF
$22 constant SD-DQUOTE
$20 constant SD-SP

1 constant SD-K-INCLUDED
2 constant SD-K-REQUIRED
3 constant SD-K-PROVIDED

1 constant SD-PEND-PATH
2 constant SD-PEND-OTHER

\ A loader's literal path has no cap here: the engine's resolver is its only one.
DYNAMIC-BUFFER SD-PATH u8
create SD-ROOT SD-PATH-CAP 1+ allot
create SD-ENTRY SD-PATH-CAP 1+ allot
create SD-LOADING SD-PATH-CAP allot
variable SD-ROOT-U
variable SD-ENTRY-U
variable SD-LOADING-U

TYPED-VARIABLE SD-STORAGE SPAN:span<u8>
PTR-VARIABLE SD-A
variable SD-U
variable SD-I
variable SD-PATH-U
variable SD-PATH-AT                     \ where the last string literal's payload starts
variable SD-PATH-SPAN                   \ and its length in the source
variable SD-PEND
variable SD-EMIT-LEN
variable SD-OPENER
variable SD-TOK-OFF
variable SD-TOK-LEN
variable SD-CALL-KIND
variable SD-DEF                          \ 1 from a definition's opener to its ;
variable SD-DEF-NAME
variable SD-DEF-STAGE
variable SD-DEF-OFF
variable SD-SHADOWED                     \ loader and family words the file defined
variable SD-RETIRED                      \ and those it retired
variable SD-HARVESTING                  \ only the harvest records declarations and rows
variable SD-FILE-ID
DYNAMIC-BUFFER SD-E-SPEC n

1 constant SD-R-DECL
2 constant SD-R-RETIRE
3 constant SD-R-PARSES
4 constant SD-R-THROUGH
5 constant SD-R-FAMILY
6 constant SD-R-PACKAGE
7 constant SD-R-END-PACKAGE

\ A source occurrence is kept once, even when the same canonical file is
\ reached under several roots or a harvest retries it after learning a row.
DYNAMIC-BUFFER SD-R-BYTES u8
variable SD-R-BYTES-U
DYNAMIC-BUFFER SD-F-AT n
DYNAMIC-BUFFER SD-F-U n
DYNAMIC-BUFFER SD-F-HEAD n
DYNAMIC-BUFFER SD-F-START n
DYNAMIC-BUFFER SD-F-SITE n               \ the file's newest site, -1 for none
variable SD-F-N
\ A record's offset is where its name starts, or for a nameless record where
\ its keyword does, and its span the source it names there: a refusal at the
\ record points at both.
DYNAMIC-BUFFER SD-R-OFF n
DYNAMIC-BUFFER SD-R-SPAN n
DYNAMIC-BUFFER SD-R-FILE n
DYNAMIC-BUFFER SD-R-NEXT n
DYNAMIC-BUFFER SD-R-KIND n
DYNAMIC-BUFFER SD-R-NAME n
DYNAMIC-BUFFER SD-R-NAME-U n
DYNAMIC-BUFFER SD-R-PKG n
DYNAMIC-BUFFER SD-R-PKG-U n
DYNAMIC-BUFFER SD-R-OUTER n
DYNAMIC-BUFFER SD-R-OUTER-U n
DYNAMIC-BUFFER SD-R-COUNT n
DYNAMIC-BUFFER SD-R-TERMS n
DYNAMIC-BUFFER SD-R-TERMS-U n
DYNAMIC-BUFFER SD-R-ACTIVE n
DYNAMIC-BUFFER SD-R-DISABLED n
DYNAMIC-BUFFER SD-R-BARE-OFF n             \ a row no bare spelling may use
DYNAMIC-BUFFER SD-R-CANON n
DYNAMIC-BUFFER SD-R-ROWS n
DYNAMIC-BUFFER SD-R-FROZEN n
variable SD-R-N
variable SD-R-ROWS-N
variable SD-R-VERSION
variable SD-R-F-N
variable SD-R-F-LOOKUP-N
variable SD-R-PASS
variable SD-R-PASS-STABLE
variable SD-R-OLD-HEAD
variable SD-R-START-N
variable SD-R-START-BYTES
\ The package row open where a walk or replay stands, -1 for none: SD-C-UNKNOWN
\ in a body-loaded file, after an unknown site, after a load made in either, and
\ after a body whose file may change it (CONTEXT-SPEC-END).
variable SD-C-PKG
\ The package row a part returns to. Where SD-C-PKG is unknown: -1 in a file a
\ body loads, where no package it opens can be a part; else SD-C-UNKNOWN.
variable SD-C-OUTER
-2 constant SD-C-UNKNOWN
-1 SD-C-PKG !
-1 SD-C-OUTER !
variable SD-C-N
DYNAMIC-BUFFER SD-C-ROW n
DYNAMIC-BUFFER SD-C-OWNER n
DYNAMIC-BUFFER SD-C-ORDER n
variable SD-C-ORDER-N
\ A site is a file's start (offset 0) or the point right after one of its loads
\ (the loader's offset + 1), with the package the replay knew open there, or
\ SD-C-UNKNOWN where two replays found different ones (SD-S-LEARN).
DYNAMIC-BUFFER SD-S-OFF n
DYNAMIC-BUFFER SD-S-PKG n
DYNAMIC-BUFFER SD-S-OUTER n
DYNAMIC-BUFFER SD-S-NEXT n
variable SD-S-N
variable SD-S-MARK                       \ sites from here on: this replay's

\ Local spellings are byte-exact and become visible after their group's closer.
\ Keep source offsets so discovery need not copy names or impose a locals cap.
DYNAMIC-BUFFER SD-LOCAL-OFF n
DYNAMIC-BUFFER SD-LOCAL-LEN n
DYNAMIC-BUFFER SD-SCOPE-N n
DYNAMIC-BUFFER SD-SCOPE-BASE n
variable SD-LOCALS
variable SD-LOCAL-BASE
variable SD-SCOPES

: SD-BUF ( -- ptr u8 )
   SD-A @ ;

\ The read grows this span as it fills it,
\ installing a larger one before the old is released: no scanner pointer
\ survives a read.
: SD-ROOM ( n -- ptr u8 ) {: need:n :}
   SD-STORAGE @ {: old :}
   need old SPAN:LEN > if
      need MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: fresh :}
      old SPAN:$ {: a:ptr u:n :}
      a fresh SPAN:$ drop u BYTE-COPY
      fresh SD-STORAGE !
      u 0 > if old MEM:FREE-SPAN then
   then
   SD-STORAGE @ SPAN:$ drop ;

: SD-BYTE ( n -- u8 )   SD-BUF + c@ ;
: SD-AT? ( n -- bool )  SD-U @ < ;
: SD-PATH$ ( -- ptr u8 n )  0 SD-PATH SD-PATH-U @ ;
: SD-TOK$ ( n n -- ptr u8 n ) {: off:n len:n :}  off SD-BUF + len ;

: SD-R-COPY ( ptr u8 n -- n ) {: a:ptr u:n :}
   SD-R-BYTES-U @ {: at:n :}
   at u + 1+ SD-R-BYTES-RESERVE
   a at SD-R-BYTES u BYTE-COPY
   at u + SD-R-BYTES-U !
   at ;

: SD-FILE-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   SD-F-N @ 0 ?do
      a u i SD-F-AT @ SD-R-BYTES i SD-F-U @ STR= if i unloop exit then
   loop
   -1 ;

: SD-FILE-OF ( -- n )
   SD-ENTRY SD-ENTRY-U @ SD-FILE-FIND dup 0 >= if exit then drop
   SD-F-N @ {: id:n :}
   id 1+ dup SD-F-AT-RESERVE dup SD-F-U-RESERVE dup SD-F-HEAD-RESERVE
   SD-F-SITE-RESERVE
   SD-ENTRY SD-ENTRY-U @ SD-R-COPY id SD-F-AT !
   SD-ENTRY-U @ id SD-F-U !
   -1 id SD-F-HEAD !
   -1 id SD-F-SITE !
   id 1+ SD-F-N !
   id ;

: SD-R-NAME$ ( n -- ptr u8 n ) {: r:n :}
   r SD-R-NAME @ SD-R-BYTES r SD-R-NAME-U @ ;

: SD-R-PKG$ ( n -- ptr u8 n ) {: r:n :}
   r SD-R-PKG @ SD-R-BYTES r SD-R-PKG-U @ ;

: SD-R-OUTER$ ( n -- ptr u8 n ) {: r:n :}
   r SD-R-OUTER @ SD-R-BYTES r SD-R-OUTER-U @ ;

: SD-C-NAME ( n -- n n ) {: row:n :}
   row 0 < if 0 0 exit then
   row SD-R-NAME @ row SD-R-NAME-U @ ;

\ The walked file's record at OFF, -1 for none or for a file no harvest read.
: SD-R-AT ( n -- n ) {: off:n :}
   SD-FILE-ID @ {: file:n :}
   file 0 < if -1 exit then
   file SD-F-HEAD @
   begin dup 0 >= while
      dup SD-R-OFF @ off = if exit then
      SD-R-NEXT @
   repeat ;

: SD-R-RECORD ( n n n n -- n ) {: off:n noff:n nu:n kind:n :}
   SD-HARVESTING @ 0= if -1 exit then
   off SD-R-AT 0 >= if -1 exit then
   SD-R-N @ {: r:n :}
   r 1+ dup SD-R-OFF-RESERVE dup SD-R-SPAN-RESERVE
   dup SD-R-FILE-RESERVE dup SD-R-NEXT-RESERVE
   dup SD-R-KIND-RESERVE
   dup SD-R-NAME-RESERVE dup SD-R-NAME-U-RESERVE
   dup SD-R-PKG-RESERVE dup SD-R-PKG-U-RESERVE
   dup SD-R-OUTER-RESERVE dup SD-R-OUTER-U-RESERVE
   dup SD-R-COUNT-RESERVE dup SD-R-TERMS-RESERVE
   dup SD-R-TERMS-U-RESERVE dup SD-R-ACTIVE-RESERVE
   dup SD-R-DISABLED-RESERVE dup SD-R-BARE-OFF-RESERVE
   SD-R-CANON-RESERVE
   SD-FILE-ID @ SD-F-HEAD @ r SD-R-NEXT !
   r SD-FILE-ID @ SD-F-HEAD !
   off r SD-R-OFF !
   nu r SD-R-SPAN !
   SD-FILE-ID @ r SD-R-FILE !
   kind r SD-R-KIND !
   noff nu SD-TOK$ SD-R-COPY r SD-R-NAME !
   nu r SD-R-NAME-U !
   \ Keyed by the package open here and its outer, as their `package` spells
   \ them: (A, B) in part A:B, (none, B) in top-level B. Both share rows' bytes.
   SD-C-PKG @ SD-C-NAME r SD-R-PKG-U ! r SD-R-PKG !
   SD-C-OUTER @ SD-C-NAME r SD-R-OUTER-U ! r SD-R-OUTER !
   0 r SD-R-COUNT !
   0 r SD-R-TERMS !
   0 r SD-R-TERMS-U !
   1 r SD-R-ACTIVE !
   0 r SD-R-DISABLED !
   0 r SD-R-BARE-OFF !
   0 r SD-R-CANON !
   r 1+ SD-R-N !
   r ;

\ Exactly one non-edge colon qualifies a name. The tail is the word defined
\ or called, irrespective of the spelling's package prefix.
: SD-QCOLON ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 2 < if -1 exit then
   -1
   u 1- 1 ?do
      a i + c@ $3A = if
         dup 0 >= if drop -1 unloop exit then
         drop i
      then
   loop ;

: SD-TAIL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u SD-QCOLON {: col:n :}
   col 0 < if a u exit then
   a col 1+ + u col 1+ - ;

: SD-R-KEY$ ( n -- ptr u8 n ptr u8 n ) {: r:n :}
   r SD-R-NAME$ {: a:ptr u:n :}
   a u SD-QCOLON {: col:n :}
   col 0 >= if a col a col 1+ + u col 1+ - exit then
   r SD-R-PKG$ a u ;

\ No package row, outside every package or where it is unknown, spells as empty:
\ a declaration made there is reached from every package.
: SD-C-PKG$ ( n -- ptr u8 n ) {: row:n :}
   row 0 < if 0 SD-R-BYTES 0 exit then
   row SD-R-NAME$ ;

: SD-C-KNOWN? ( -- bool )
   SD-C-PKG @ SD-C-UNKNOWN <> SD-C-OUTER @ SD-C-UNKNOWN <> and ;

\ A bare name reaches package (outer, name) open here and, inside part A:B,
\ (none, A) as well. Where either is unknown it may reach any package: refused.
: SD-C-OPEN? ( ptr u8 n ptr u8 n -- bool ) {: out:ptr outu:n pkg:ptr pkgu:n :}
   SD-C-KNOWN? 0= if E-DISC-SHADOW throw then
   pkg pkgu SD-C-PKG @ SD-C-PKG$ STR=CI
   out outu SD-C-OUTER @ SD-C-PKG$ STR=CI and if true exit then
   outu 0= pkg pkgu SD-C-OUTER @ SD-C-PKG$ STR=CI and ;

: SD-R-TOP$ ( n -- ptr u8 n ) {: r:n :}
   r SD-R-OUTER-U @ 0<> if r SD-R-OUTER$ exit then
   r SD-R-PKG$ ;

\ A qualified-name record `B:Q` made inside top-level package T, or a part of T,
\ names part T:B or top-level B: two packages.
: SD-R-IDS ( n -- n ) {: r:n :}
   r SD-R-NAME$ SD-QCOLON 0 < if 1 exit then
   r SD-R-TOP$ nip 0= if 1 exit then
   2 ;

\ The record's Kth package as its outer and name, each empty for none.
: SD-R-ID ( n n -- ptr u8 n ptr u8 n ) {: r:n k:n :}
   r SD-R-NAME$ {: a:ptr u:n :}
   a u SD-QCOLON {: col:n :}
   col 0 < if r SD-R-OUTER$ r SD-R-PKG$ exit then
   k 0= if r SD-R-TOP$ else 0 SD-R-BYTES 0 then
   a col ;

\ A bare use reaches the record's package; where the record may name either of
\ two packages and the use reaches one, discovery cannot know which: refused.
: SD-R-OPEN? ( n -- bool ) {: r:n :}
   r SD-R-IDS {: ids:n :}
   false
   ids 0 ?do r i SD-R-ID SD-C-OPEN? or loop
   dup ids 1 > and if E-DISC-SHADOW throw then ;

: SD-R-KEY-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n r:n :}
   r SD-R-KEY$ {: pkg:ptr pkgu:n tail:ptr tailu:n :}
   pkgu 0= if a u tail tailu STR=CI exit then
   a u SD-QCOLON 0 < if
      a u tail tailu STR=CI 0= if false exit then
      r SD-R-OPEN? exit
   then
   u pkgu tailu + 1+ <> if false exit then
   a pkgu pkg pkgu STR=CI 0= if false exit then
   a pkgu + c@ $3A <> if false exit then
   a pkgu 1+ + tailu tail tailu STR=CI ;

: SD-R-FAMILY-MATCH? ( n n -- bool ) {: ctx:n family:n :}
   ctx SD-C-ROW @ {: row:n :}
   row SD-R-KIND @ SD-R-RETIRE = row SD-R-NAME-U @ 0= and if true exit then
   row SD-R-NAME$ {: a:ptr u:n :}
   a u SD-QCOLON {: col:n :}
   col 0 >= if
      a col a col 1+ + u col 1+ -
   else
      ctx SD-C-OWNER @ SD-C-PKG$ a u
   then
   {: pkg:ptr pkgu:n tail:ptr tailu:n :}
   family SD-R-NAME$ tail tailu STR=CI 0= if false exit then
   pkgu 0= if true exit then
   SD-C-KNOWN? 0= if true exit then
   pkg pkgu SD-C-PKG @ SD-C-PKG$ STR=CI if true exit then
   \ Inside part A:B, A's words are reached too. Packages compare by spelling,
   \ so a top-level B matches inside A:B as well: a refusal, which is safe.
   pkg pkgu SD-C-OUTER @ SD-C-PKG$ STR=CI ;

\ A package opened inside another, even one left open by a prior file, is a part
\ (the engine refuses every other nested `package`), whose close returns to it.
\ A file cannot open a package while one open when it started loading is still
\ open.
: SD-C-PACKAGE-OPEN ( n -- ) {: row:n :}
   SD-C-PKG @ SD-C-UNKNOWN = if SD-C-OUTER @ SD-C-PKG ! then
   SD-C-PKG @ -1 <> if SD-C-PKG @ SD-C-OUTER ! then
   row SD-C-PKG ! ;

\ Where the package is unknown, `;package` closes one discovery cannot know.
: SD-C-PACKAGE-CLOSE ( -- )
   SD-C-PKG @ SD-C-UNKNOWN = if E-DISC-SHADOW throw then
   SD-C-OUTER @ SD-C-PKG !
   SD-C-PKG @ SD-C-UNKNOWN <> if -1 SD-C-OUTER ! then ;

: SD-S-FIND ( n n -- n ) {: file:n off:n :}
   file SD-F-SITE @
   begin dup 0 >= while
      dup SD-S-OFF @ off = if exit then
      SD-S-NEXT @
   repeat ;

: SD-S-ADD ( n n -- ) {: file:n off:n :}
   SD-C-KNOWN? 0= if exit then
   file off SD-S-FIND 0 >= if exit then
   SD-S-N @ {: s:n :}
   s 1+ dup SD-S-OFF-RESERVE dup SD-S-PKG-RESERVE dup SD-S-OUTER-RESERVE
   SD-S-NEXT-RESERVE
   off s SD-S-OFF !
   SD-C-PKG @ s SD-S-PKG !
   SD-C-OUTER @ s SD-S-OUTER !
   file SD-F-SITE @ s SD-S-NEXT !
   s file SD-F-SITE !
   s 1+ SD-S-N ! ;

: SD-S-SEED ( n n -- ) {: file:n off:n :}
   file off SD-S-FIND {: s:n :}
   s 0 < if exit then
   s SD-S-PKG @ SD-C-PKG !
   s SD-S-OUTER @ SD-C-OUTER ! ;

\ The replay corrects a site it learned; a site an earlier replay learned with
\ another package, or one a body's file may have changed (SD-C-OUTER unknown),
\ becomes unknown, once, so replays settle. Inside a body's file the walk reads
\ no site. A start site is never corrected: its records' keys answer for it,
\ its uses do not.
: SD-S-LEARN ( n n -- ) {: file:n off:n :}
   SD-C-PKG @ SD-C-UNKNOWN = SD-C-OUTER @ -1 = and if exit then
   file off SD-S-FIND {: s:n :}
   s 0 < if file off SD-S-ADD exit then
   off 0= if exit then
   s SD-S-PKG @ SD-C-UNKNOWN = if exit then
   SD-C-KNOWN? if
      s SD-S-PKG @ SD-C-PKG$ SD-C-PKG @ SD-C-PKG$ STR=CI
      s SD-S-OUTER @ SD-C-PKG$ SD-C-OUTER @ SD-C-PKG$ STR=CI and if exit then
   then
   s SD-S-MARK @ < SD-C-KNOWN? 0= or if
      SD-C-UNKNOWN s SD-S-PKG !
      SD-C-UNKNOWN s SD-S-OUTER !
   else
      SD-C-PKG @ s SD-S-PKG !
      SD-C-OUTER @ s SD-S-OUTER !
   then
   1 SD-R-VERSION +! ;

: SD-R-CONTEXT-FAMILY ( n -- ) {: family:n :}
   SD-C-N @ 0 ?do
      i family SD-R-FAMILY-MATCH? if
         i SD-C-ROW @ SD-R-CANON @ 0=
         i SD-C-OWNER @ -1 <> or if
            family SD-R-OFF @ SD-TOK-OFF !
            family SD-R-SPAN @ SD-TOK-LEN !
            i SD-C-ROW @ SD-R-KIND @ SD-R-DECL = if
               E-DISC-SHADOW throw
            then
            E-DISC-RETIRE throw
         then
      then
   loop ;

\ Each file chain is newest first. Fill its contiguous index backwards so
\ intervals can binary-search source offsets in their original scan order.
: SD-C-ORDER-BUILD ( -- )
   SD-F-N @ 1+ SD-F-START-RESERVE
   0 SD-C-ORDER-N !
   SD-F-N @ 0 ?do
      SD-C-ORDER-N @ i SD-F-START !
      i SD-F-HEAD @
      begin dup 0 >= while
         SD-C-ORDER-N @ 1+ SD-C-ORDER-N !
         SD-R-NEXT @
      repeat drop
   loop
   SD-C-ORDER-N @ SD-F-N @ SD-F-START !
   SD-C-ORDER-N @ SD-C-ORDER-RESERVE
   SD-F-N @ 0 ?do
      i SD-F-HEAD @ i 1+ SD-F-START @
      begin over 0 >= while
         1-
         2dup SD-C-ORDER !
         swap SD-R-NEXT @ swap
      repeat 2drop
   loop ;

: SD-C-LOWER ( n n n -- n ) {: first:n limit:n off:n :}
   first limit
   begin 2dup < while
      2dup + 2 / dup SD-C-ORDER @ SD-R-OFF @ off < if
         1+ rot drop swap
      else
         swap drop
      then
   repeat drop ;

: SD-R-CONTEXT-BEGIN ( -- )
   -1 SD-C-PKG !
   -1 SD-C-OUTER !
   0 SD-C-N !
   SD-S-N @ SD-S-MARK !
   SD-C-ORDER-BUILD ;

: SD-C-FAMILY-ROW? ( n -- bool ) {: row:n :}
   row SD-R-KIND @ SD-R-RETIRE = row SD-R-NAME-U @ 0= and if true exit then
   row SD-R-NAME$ SD-TAIL$ {: a:ptr u:n :}
   a u s" structure" STR=CI
   a u s" enum" STR=CI or
   a u s" sumtype" STR=CI or ;

: SD-C-ADD ( n -- ) {: row:n :}
   row SD-C-FAMILY-ROW? 0= if exit then
   SD-C-N @ 0 ?do
      i SD-C-ROW @ row =
      i SD-C-OWNER @ SD-C-PKG @ = and if unloop exit then
   loop
   SD-C-N @ 1+ dup SD-C-ROW-RESERVE SD-C-OWNER-RESERVE
   row SD-C-N @ SD-C-ROW !
   SD-C-PKG @ SD-C-N @ SD-C-OWNER !
   SD-C-N @ 1+ SD-C-N ! ;

\ A key decides a lookup only for a row's tail: a declaration or retirement of
\ it (SD-R-USABLE?), or the row itself, qualified or not (SD-R-IDS).
: SD-R-KEYED? ( n -- bool ) {: row:n :}
   row SD-R-KIND @ {: kind:n :}
   kind SD-R-PARSES = kind SD-R-THROUGH = or if true exit then
   row SD-R-NAME-U @ 0= if false exit then
   row SD-R-NAME$ SD-TAIL$ {: tail:ptr tailu:n :}
   SD-R-ROWS-N @ 0 ?do
      i SD-R-ROWS @ SD-R-NAME$ SD-TAIL$ tail tailu STR=CI if
         true unloop exit
      then
   loop
   false ;

\ A record keyed by another package than the one open here is harvested again
\ under the sites this replay learned; else its key cannot be known and it is
\ refused: one file loaded in two packages, a file whose start a later round's
\ loads move to another package (sites learn once), a file a body loads before
\ its own package.
: SD-R-CONTEXT-KEY ( n -- ) {: row:n :}
   SD-C-PKG @ SD-C-UNKNOWN = {: unknown:bool :}
   unknown 0= if
      row SD-R-PKG$ SD-C-PKG @ SD-C-PKG$ STR=CI
      row SD-R-OUTER$ SD-C-OUTER @ SD-C-PKG$ STR=CI and if exit then
   then
   row SD-R-KEYED? 0= if exit then
   unknown 0= SD-S-N @ SD-S-MARK @ > and if 1 SD-R-VERSION +! exit then
   row SD-R-OFF @ SD-TOK-OFF !
   row SD-R-SPAN @ SD-TOK-LEN !
   row SD-R-KIND @ SD-R-RETIRE = if E-DISC-RETIRE throw then
   E-DISC-SHADOW throw ;

\ Only rows between two successive loader sites in this file run before the
\ next child. Replaying an include visits these same rows again in a new place.
: SD-R-CONTEXT-BETWEEN ( n n ptr u8 n -- )
   {: start:n stop:n pa:ptr pu:n :}
   pa pu SD-FILE-FIND {: file:n :}
   file 0 < if exit then
   file start SD-S-LEARN
   file SD-F-START @ file 1+ SD-F-START @ start SD-C-LOWER
   file 1+ SD-F-START @ swap ?do
      i SD-C-ORDER @ {: row:n :}
      row SD-R-OFF @ stop >= if unloop exit then
      row SD-R-KIND @ {: kind:n :}
      kind SD-R-PACKAGE = if
         row SD-C-PACKAGE-OPEN
      else kind SD-R-END-PACKAGE = if
         SD-C-PACKAGE-CLOSE
      else
         kind SD-R-FAMILY = if
            row SD-R-CONTEXT-FAMILY
         else
            kind SD-R-DECL = kind SD-R-RETIRE = or if row SD-C-ADD then
            kind SD-R-FAMILY < if row SD-R-CONTEXT-KEY then
         then
      then then
   loop ;

: SD-R-BEFORE? ( n n -- bool ) {: row:n next:n :}
   row SD-R-FILE @ next SD-R-FILE @ <> if
      row SD-R-FILE @ next SD-R-FILE @ < exit
   then
   row SD-R-OFF @ next SD-R-OFF @ < ;

: SD-R-ROWS+ ( n -- ) {: row:n :}
   SD-R-ROWS-N @ {: at:n :}
   at 1+ SD-R-ROWS-RESERVE
   row at SD-R-ROWS !
   at 1+ SD-R-ROWS-N ! ;

: SD-R-ROWS-REBUILD ( -- )
   0 SD-R-ROWS-N !
   SD-R-N @ 0 ?do
      i SD-R-ACTIVE @ 0<> i SD-R-KIND @ SD-R-PARSES >= and
      i SD-R-KIND @ SD-R-FAMILY < and if
         i SD-R-ROWS+
      then
   loop ;

: SD-R-SAME? ( n n -- bool ) {: old:n new:n :}
   old SD-R-FILE @ new SD-R-FILE @ <> if false exit then
   old SD-R-OFF @ new SD-R-OFF @ <> if false exit then
   old SD-R-KIND @ new SD-R-KIND @ <> if false exit then
   old SD-R-COUNT @ new SD-R-COUNT @ <> if false exit then
   old SD-R-NAME$ new SD-R-NAME$ STR= 0= if false exit then
   old SD-R-PKG$ new SD-R-PKG$ STR= 0= if false exit then
   old SD-R-OUTER$ new SD-R-OUTER$ STR= 0= if false exit then
   old SD-R-TERMS @ SD-R-BYTES old SD-R-TERMS-U @
   new SD-R-TERMS @ SD-R-BYTES new SD-R-TERMS-U @ STR= ;

: SD-R-FROZEN-OF ( n -- n ) {: row:n :}
   SD-R-F-N @ 0 ?do
      i SD-R-FROZEN @ dup row SD-R-SAME? if unloop exit then drop
   loop
   -1 ;

: SD-R-FREEZE ( -- )
   SD-R-ROWS-N @ SD-R-FROZEN-RESERVE
   SD-R-ROWS-N @ 0 ?do i SD-R-ROWS @ i SD-R-FROZEN ! loop
   SD-R-ROWS-N @ dup SD-R-F-N ! SD-R-F-LOOKUP-N !
   \ A vanished row stays frozen, without a lookup slot, once PASS-END disabled
   \ it: a row that reappears inherits its DISABLED there instead of cycling.
   SD-R-N @ 0 ?do
      i SD-R-ACTIVE @ 0= i SD-R-DISABLED @ 0<> and if
         i SD-R-FROZEN-OF 0 < if
            SD-R-F-N @ {: at:n :}
            at 1+ SD-R-FROZEN-RESERVE
            i at SD-R-FROZEN !
            at 1+ SD-R-F-N !
         then
      then
   loop ;

: SD-R-CHAIN-SAME? ( n n -- bool )
   begin
      over 0< over 0< or if = exit then
      2dup SD-R-SAME? 0= if 2drop false exit then
      swap SD-R-NEXT @ swap SD-R-NEXT @
   again ;

: SD-R-BEGIN ( -- )
   SD-R-N @ SD-R-START-N !
   SD-R-BYTES-U @ SD-R-START-BYTES !
   SD-FILE-ID @ SD-F-HEAD @ dup SD-R-OLD-HEAD !
   begin dup 0 >= while
      0 over SD-R-ACTIVE !
      SD-R-NEXT @
   repeat drop
   -1 SD-FILE-ID @ SD-F-HEAD ! ;

: SD-R-END ( -- )
   SD-R-OLD-HEAD @ SD-FILE-ID @ SD-F-HEAD @ SD-R-CHAIN-SAME? if
      SD-R-OLD-HEAD @ SD-FILE-ID @ SD-F-HEAD !
      SD-R-OLD-HEAD @
      begin dup 0 >= while
         1 over SD-R-ACTIVE !
         SD-R-NEXT @
      repeat drop
      SD-R-START-N @ SD-R-N !
      SD-R-START-BYTES @ SD-R-BYTES-U !
   else
      1 SD-R-VERSION +!
   then
   SD-R-PASS @ 0= if SD-R-ROWS-REBUILD then ;

\ R, of ROW's tail, counts for a qualified spelling under ROW's key spelled:
\ part A:B and top-level B count under one key B.
: SD-R-AFFECTS? ( n n -- bool ) {: row:n r:n :}
   row SD-R-KEY$ {: pkg:ptr pkgu:n tail:ptr tailu:n :}
   pkgu 0= if true exit then
   r SD-R-NAME$ {: name:ptr nameu:n :}
   name nameu SD-QCOLON {: col:n :}
   col 0 >= if name col pkg pkgu STR=CI exit then
   r SD-R-PKG$ pkg pkgu STR=CI ;

\ Two packages share a bare scope when either is global, they are one package,
\ or one is the other's outer; two parts of one package share none.
: SD-SHARE? ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: ao:ptr aou:n an:ptr anu:n bo:ptr bou:n bn:ptr bnu:n :}
   anu 0= bnu 0= or if true exit then
   an anu bn bnu STR=CI ao aou bo bou STR=CI and if true exit then
   aou 0= bo bou an anu STR=CI and if true exit then
   bou 0= ao aou bn bnu STR=CI and ;

: SD-R-SHARES-ID? ( ptr u8 n ptr u8 n n -- bool ) {: o:ptr ou:n p:ptr pu:n r:n :}
   r SD-R-IDS 0 ?do
      o ou p pu r i SD-R-ID SD-SHARE? if true unloop exit then
   loop
   false ;

\ R, of ROW's tail, counts for a bare spelling when their packages share a scope.
: SD-R-SHARES? ( n n -- bool ) {: row:n r:n :}
   row SD-R-IDS 0 ?do
      row i SD-R-ID r SD-R-SHARES-ID? if true unloop exit then
   loop
   false ;

\ Whether ROW's tail admits a qualified and a bare spelling (SD-R-USE-OK?): one
\ declaration and no retirement of it count for each. A retirement weighs two.
: SD-R-USABLE? ( n -- bool bool ) {: row:n :}
   row SD-R-NAME$ SD-TAIL$ {: tail:ptr tailu:n :}
   0 0
   SD-R-N @ 0 ?do
      i SD-R-KIND @ {: kind:n :}
      kind SD-R-DECL = if 1 else kind SD-R-RETIRE = if 2 else 0 then then
      {: w:n :}
      w 0<> i SD-R-ACTIVE @ 0<> and if
         i SD-R-NAME-U @ 0= if
            w + swap w + swap
         else i SD-R-NAME$ SD-TAIL$ tail tailu STR=CI if
            row i SD-R-AFFECTS? if swap w + swap then
            row i SD-R-SHARES? if w + then
         then then
      then
   loop
   {: qual:n bare:n :}
   qual 1 = bare 1 = ;

\ ROW lies earlier in this file than OFF, after a declaration of its tail in its
\ package: a row before that binds another word.
: SD-R-SEEN? ( n n -- bool ) {: row:n off:n :}
   row SD-R-FILE @ SD-FILE-ID @ <> row SD-R-OFF @ off >= or if false exit then
   row SD-R-NAME$ SD-TAIL$ {: tail:ptr tailu:n :}
   row SD-R-NEXT @
   begin dup 0 >= while
      {: r:n :}
      r SD-R-KIND @ SD-R-DECL = r SD-R-NAME$ SD-TAIL$ tail tailu STR=CI and
      row r SD-R-AFFECTS? and if true exit then
      r SD-R-NEXT @
   repeat drop false ;

\ A bare spelling uses a package's enabled, not bare-off ROW only where
\ SD-R-SEEN?; elsewhere the replay refuses and a harvest reads on to record it.
: SD-R-USE-OK? ( n n n -- bool ) {: off:n len:n row:n :}
   row SD-R-DISABLED @ 0= {: on:bool :}
   off len SD-TOK$ SD-QCOLON 0 >= if on exit then
   row SD-R-KEY$ 2drop nip 0= if
      on 0= if false exit then
      row SD-R-BARE-OFF @ 0= if true exit then
   else
      on row SD-R-BARE-OFF @ 0= and row off SD-R-SEEN? and if true exit then
   then
   SD-HARVESTING @ if false exit then
   E-DISC-SHADOW throw ;

: SD-R-FROZEN-FIND ( n n -- n ) {: off:n len:n :}
   off len SD-TOK$ {: a:ptr u:n :}
   -1
   SD-R-F-LOOKUP-N @ 0 ?do
      i SD-R-FROZEN @ {: row:n :}
      a u row SD-R-KEY-MATCH? if
         dup 0 < if drop row else
            row over SD-R-BEFORE? if drop row then
         then
      then
   loop
   dup 0 < if exit then
   {: found:n :}
   off len found SD-R-USE-OK? if found exit then
   -1 ;

: SD-R-FIND ( n n -- n ) {: off:n len:n :}
   SD-HARVESTING @ SD-R-PASS @ and 0<> if off len SD-R-FROZEN-FIND exit then
   off len SD-TOK$ {: a:ptr u:n :}
   -1
   SD-R-ROWS-N @ 0 ?do
      i SD-R-ROWS @ {: row:n :}
      row SD-R-ACTIVE @ 0<> a u row SD-R-KEY-MATCH? and if
         dup 0 < if drop row else
            row over SD-R-BEFORE? if drop row then
         then
      then
   loop
   dup 0 < if exit then
   {: found:n :}
   off len found SD-R-USE-OK? if found exit then
   -1 ;

: SD-R-ACTIVE-MATCH? ( n -- bool ) {: old:n :}
   SD-R-N @ 0 ?do
      i SD-R-ACTIVE @ 0<> i SD-R-KIND @ SD-R-PARSES >= and
      i SD-R-KIND @ SD-R-FAMILY < and if
         old i SD-R-SAME? if true unloop exit then
      then
   loop
   false ;

: SD-R-PASS-END ( -- )
   1 SD-R-PASS-STABLE !
   SD-R-N @ 0 ?do
      i SD-R-ACTIVE @ 0<> i SD-R-KIND @ SD-R-PARSES >= and
      i SD-R-KIND @ SD-R-FAMILY < and if
         i SD-R-FROZEN-OF dup 0 < if
            drop 0 SD-R-PASS-STABLE !
         else
            dup SD-R-DISABLED @ i SD-R-DISABLED !
            SD-R-BARE-OFF @ i SD-R-BARE-OFF !
         then
      then
   loop
   SD-R-F-N @ 0 ?do
      i SD-R-FROZEN @ dup SD-R-ACTIVE-MATCH? 0= if
         dup SD-R-DISABLED @ 0= if
            1 over SD-R-DISABLED !
            0 SD-R-PASS-STABLE !
            1 SD-R-VERSION +!
         then
      then
      drop
   loop
   SD-R-PASS-STABLE @ if
      SD-R-N @ 0 ?do
         i SD-R-ACTIVE @ 0<> i SD-R-KIND @ SD-R-PARSES >= and
         i SD-R-KIND @ SD-R-FAMILY < and if
            i SD-R-USABLE? {: qual:bool bare:bool :}
            i SD-R-DISABLED @ 0= qual 0= and if
               1 i SD-R-DISABLED !
               0 SD-R-PASS-STABLE !
               1 SD-R-VERSION +!
            then
            i SD-R-BARE-OFF @ 0= bare 0= and if
               1 i SD-R-BARE-OFF !
               0 SD-R-PASS-STABLE !
               1 SD-R-VERSION +!
            then
         then
      loop
   then
   0 SD-R-PASS !
   SD-R-ROWS-REBUILD ;

: SD-R-SPACE-END ( n n -- n ) {: end:n :}
   begin dup end < while
      dup SD-R-BYTES c@ 32 > if exit then
      1+
   repeat ;

: SD-R-TOKEN-END ( n n -- n ) {: end:n :}
   begin dup end < while
      dup SD-R-BYTES c@ 32 > 0= if exit then
      1+
   repeat ;

: SD-R-TERM-MATCH? ( n n n n -- bool ) {: off:n len:n start:n stop:n :}
   off len SD-TOK$ start SD-R-BYTES stop start - STR= ;

: SD-R-TERM? ( n n n -- bool ) {: off:n len:n row:n :}
   row SD-R-TERMS @
   row SD-R-TERMS @ row SD-R-TERMS-U @ + {: end:n :}
   begin dup end < while
      end SD-R-SPACE-END
      dup end >= if drop false exit then
      dup end SD-R-TOKEN-END
      2dup off len 2swap SD-R-TERM-MATCH? if 2drop true exit then
      nip
   repeat
   drop false ;

: SD-SKIP-WS ( -- )
   begin SD-I @ SD-AT? while
      SD-I @ SD-BYTE 32 > if exit then
      SD-I @ 1+ SD-I !
   repeat ;

: SD-NEXT ( -- n n )
   SD-SKIP-WS
   SD-I @ {: start:n :}
   start SD-AT? 0= if start 0 exit then
   begin SD-I @ SD-AT? while
      SD-I @ SD-BYTE 33 < if start SD-I @ start - exit then
      SD-I @ 1+ SD-I !
   repeat
   start SD-I @ start - ;

\ The next token, kept as the token read last when there is one: a refusal
\ names the form discovery read last, and the source's end names none.
: SD-RAW ( -- n n )
   SD-NEXT {: off:n len:n :}
   len 0<> if
      off SD-TOK-OFF !
      len SD-TOK-LEN !
   then
   off len ;

: SD-R-CONSUME ( n -- ) {: row:n :}
   row SD-R-COUNT @ 0 ?do SD-RAW nip 0= if unloop exit then loop
   row SD-R-KIND @ SD-R-THROUGH <> if exit then
   begin
      SD-RAW {: off:n len:n :}
      len 0= if exit then
      off len row SD-R-TERM?
   until ;

: SD-R-USE? ( n n -- bool ) {: off:n len:n :}
   SD-DEF @ 0<> SD-SCOPES @ 0<> or if false exit then
   off len SD-R-FIND {: row:n :}
   row 0 < if false exit then
   row SD-R-CONSUME
   true ;

variable SD-TERM-START
variable SD-TERM-END

\ The through list is read as raw tokens. Only a complete nonempty list can
\ describe operands safely; malformed declarations remain ordinary source.
: SD-R-LIST ( -- bool )
   -1 SD-TERM-START !
   SD-RAW SD-TOK$ s" (" STR= 0= if false exit then
   begin
      SD-RAW {: off:n len:n :}
      len 0= if false exit then
      off len SD-TOK$ s" )" STR= if SD-TERM-START @ 0 >= exit then
      SD-TERM-START @ 0 < if off SD-TERM-START ! then
      off len + SD-TERM-END !
   again ;

: SD-R-ROW ( n -- ) {: kind:n :}
   SD-RAW {: noff:n nu:n :}
   SD-RAW {: coff:n cu:n :}
   kind SD-R-THROUGH = if SD-R-LIST 0= if exit then then
   nu 0= cu 0= or if exit then
   coff cu SD-TOK$ num-parse {: count:n flt:bool ok:bool :}
   ok 0= flt or count 0 < or if exit then
   noff noff nu kind SD-R-RECORD {: row:n :}
   row 0 < if exit then
   count row SD-R-COUNT !
   kind SD-R-THROUGH = if
      SD-TERM-END @ SD-TERM-START @ - {: u:n :}
      SD-TERM-START @ u SD-TOK$ SD-R-COPY row SD-R-TERMS !
      u row SD-R-TERMS-U !
   then
   SD-R-PASS @ 0= if row SD-R-ROWS+ then ;

: SD-R-DECL-NAME ( n n -- ) {: off:n len:n :}
   len 0= if exit then
   off off len SD-R-DECL SD-R-RECORD drop ;

: SD-R-RETIRE-NAME ( n n -- ) {: off:n len:n :}
   len 0= if exit then
   off off len SD-R-RETIRE SD-R-RECORD drop ;

: SD-R-RETIRE-STACK ( n n -- ) {: off:n pend:n :}
   pend SD-PEND-PATH <> if off off 0 SD-R-RETIRE SD-R-RECORD drop exit then
   SD-PATH-AT @ dup 0 SD-R-RETIRE SD-R-RECORD {: r:n :}
   r 0 < if exit then
   SD-PATH$ SD-R-COPY r SD-R-NAME !
   SD-PATH-U @ r SD-R-NAME-U !
   SD-PATH-SPAN @ r SD-R-SPAN ! ;

: SD-SKIP-TO ( n -- ) {: stop:n :}
   begin SD-I @ SD-AT? while
      SD-I @ SD-BYTE stop = if SD-I @ 1+ SD-I ! exit then
      SD-I @ 1+ SD-I !
   repeat ;

: SD-SKIP-LINE ( -- )   SD-LF SD-SKIP-TO ;
: SD-SKIP-PAREN ( -- )  SD-RPAREN SD-SKIP-TO ;

: SD-STR-LEAD ( n -- n )
   dup $73 = over $53 = or if drop SD-PEND-PATH exit then
   dup $2E = over $63 = or swap $43 = or if SD-PEND-OTHER exit then
   0 ;

: SD-OPENER-KIND ( n n -- n ) {: off:n len:n :}
   len 2 = if
      off 1 + SD-BYTE SD-DQUOTE = 0= if 0 exit then
      off SD-BYTE SD-STR-LEAD exit
   then
   len 3 = if
      off 1 + SD-BYTE SD-BACKSLASH = 0= if 0 exit then
      off 2 + SD-BYTE SD-DQUOTE = 0= if 0 exit then
      off SD-BYTE SD-STR-LEAD exit
   then
   0 ;

\ One byte more than the path, so an empty one still has a byte 0.
: SD-PATH-FILL ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1+ SD-PATH-RESERVE
   a 0 SD-PATH u BYTE-COPY
   u SD-PATH-U ! ;

\ Room for an escaped literal's decode: as many bytes as its payload, since a
\ decode is never longer than its spelling.
DYNAMIC-BUFFER SD-DEC u8

\ A literal holding a bad escape is a bad string literal: the engine refuses it
\ where the load stops. At top level the walk ends there, follows nothing and
\ refuses nothing; the check refuses it at its opener. In a definition the check
\ rejects the body and reads past it, and so does the walk, with no path
\ pending: a loader or retirement taking it is a call in a body.
: SD-REFUSED-LITERAL ( -- )
   SD-DEF @ 0= if SD-U @ SD-I ! exit then
   0 SD-PEND ! ;

\ An escaped literal's path is what the engine's literal makes of its payload,
\ the payload decoded by the engine's escape table, unless an escape is bad.
: SD-DECODE-PATH ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1+ SD-DEC-RESERVE
   0 SD-DEC {: dst:ptr :}
   a u dst ESC-DECODE {: k:n ok:bool :}
   ok 0= if SD-REFUSED-LITERAL exit then
   dst k SD-PATH-FILL ;

\ The literal whose opener SD-I stands after: SD-I steps past its closing quote.
: SD-SCAN-STRING ( bool -- ) {: escaped:bool :}
   SD-I @ SD-AT? if SD-I @ SD-BYTE SD-SP = if SD-I @ 1+ SD-I ! then then
   SD-I @ {: start:n :}
   start SD-PATH-AT !
   begin SD-I @ SD-AT? while
      SD-I @ SD-BYTE SD-DQUOTE = if
         SD-I @ start - SD-PATH-SPAN !
         start SD-BUF +  SD-PATH-SPAN @
         SD-I @ 1+ SD-I !
         escaped if SD-DECODE-PATH else SD-PATH-FILL then
         exit
      then
      SD-I @ SD-BYTE SD-BACKSLASH = escaped and if SD-I @ 1+ SD-I ! then
      SD-I @ SD-AT? if SD-I @ 1+ SD-I ! then
   repeat
   E-DISC-UNTERM throw ;

: SD-LOADER-KIND ( n n -- n )
   SD-TOK$ {: a:ptr u:n :}
   a u s" included" STR=CI if SD-K-INCLUDED exit then
   a u s" required" STR=CI if SD-K-REQUIRED exit then
   a u s" provided" STR=CI if SD-K-PROVIDED exit then
   0 ;

: SD-NAME-BIT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" include" STR=CI if $01 exit then
   a u s" included" STR=CI if $02 exit then
   a u s" require" STR=CI if $04 exit then
   a u s" required" STR=CI if $08 exit then
   a u s" provided" STR=CI if $10 exit then
   a u s" structure" STR=CI if $20 exit then
   a u s" enum" STR=CI if $40 exit then
   a u s" sumtype" STR=CI if $80 exit then
   0 ;

$FF constant SD-NAMES                    \ loaders and family openers

\ A definition or retirement of a loader or family name refuses nothing, but
\ a later use with that spelling is ambiguous (SD-CHECK-USE).
: SD-SHADOW ( ptr u8 n -- )
   SD-TAIL$ SD-NAME-BIT SD-SHADOWED @ or SD-SHADOWED ! ;

: SD-RETIRE ( ptr u8 n -- )
   SD-TAIL$ SD-NAME-BIT SD-RETIRED @ or SD-RETIRED ! ;

\ UNDEFINE-IF-DEFINED at top level retires the word the path literal right
\ before it names, and when no path literal does, a word the walk cannot read,
\ any loader word among them. In a body it retires nothing until the word runs,
\ which no check runs.
: SD-RETIRE-NAMED ( n -- ) {: pend:n :}
   pend SD-PEND-PATH = if SD-PATH$ SD-RETIRE exit then
   SD-RETIRED @ SD-NAMES or SD-RETIRED ! ;

: SD-CHECK-USE ( n n -- ) {: off:n len:n :}
   off len SD-TOK$ SD-NAME-BIT {: bit:n :}
   bit SD-SHADOWED @ and 0<> if E-DISC-SHADOW throw then
   bit SD-RETIRED @ and 0<> if E-DISC-RETIRE throw then ;

: SD-CALL-ACT ( -- )
   SD-CALL-KIND @ {: kind:n :}
   kind SD-K-INCLUDED = if SD-PATH$ included exit then
   kind SD-K-REQUIRED = if SD-PATH$ required exit then
   SD-PATH$ provided ;

\ False, and nothing recorded, for a path the loader's resolver refuses
\ (E-PATH-NUL, a NUL in the path; E-PATH-RANGE).
: SD-LOAD? ( n n n -- bool )
   {: off:n len:n kind:n :}
   off len DISC-TOK!
   kind SD-CALL-KIND !
   EVENT-COUNT {: before:n :}
   [: SD-CALL-ACT ;] catch {: rc:n :}
   EVENT-COUNT before > if
      EVENT-COUNT SD-E-SPEC-RESERVE
      SD-DEF @ before SD-E-SPEC !
   then
   rc 0= if
      SD-FILE-ID @ 0 >= if
         SD-HARVESTING @ 0= if SD-FILE-ID @ off 1+ SD-S-ADD then
         SD-C-KNOWN? if SD-FILE-ID @ off 1+ SD-S-SEED else SD-C-UNKNOWN SD-C-PKG ! then
      then
      true exit
   then
   rc E-PATH-RANGE =  rc E-PATH-NUL =  or 0= if rc throw then
   false ;

\ A load at top level, which runs while the file loads, or an `include` or
\ `require` in a body, which the engine refuses there but whose file the quiet
\ composition reads: a path it cannot resolve is a form discovery cannot
\ follow, refused at the loader word that names it.
: SD-CALL-LOADER ( n n n -- )
   {: off:n len:n kind:n :}
   off len kind SD-LOAD? if exit then
   off SD-TOK-OFF !
   len SD-TOK-LEN !
   E-DISC-CAPACITY throw ;

\ A loader word in a body is a call the word makes when it runs, which no check
\ runs: right after a path literal the resolver takes it is recorded, the
\ superset an ordinary composition loads. After anything else, or after a path
\ the resolver refuses, which the call loads nothing from, it is nothing to
\ follow or refuse.
\ At top level the load runs while the file loads, so a path the walk cannot
\ follow is refused: a dynamic one (E-DISC-DYNAMIC) and a C\" or .\" literal
\ (E-DISC-OPENER). A form the walk reads, any at top level and one right after
\ a path literal in a body, is refused first when the file has replaced its
\ loader word.
: SD-DISPATCH-LOADER ( n n n n -- ) {: off:n len:n kind:n pend:n :}
   SD-DEF @ 0<> if
      pend SD-PEND-PATH = 0= if exit then
      off len SD-CHECK-USE
      off len kind SD-LOAD? drop
      exit
   then
   off len SD-CHECK-USE
   pend SD-PEND-OTHER = if E-DISC-OPENER throw then
   pend SD-PEND-PATH = 0= if E-DISC-DYNAMIC throw then
   off len kind SD-CALL-LOADER ;

\ `include` and `require` take the next token as their path and refuse one
\ over PATH-CAP bytes before they resolve it; the resolver decides the rest. The
\ walk reads either wherever it stands, refused first when the file has
\ replaced it.
: SD-LOADER-IMM ( n n n -- ) {: toff:n tlen:n kind:n :}
   toff tlen SD-CHECK-USE
   SD-RAW {: poff:n plen:n :}
   plen 0= if E-DISC-DYNAMIC throw then
   plen PATH-CAP > if E-DISC-CAPACITY throw then
   poff plen SD-TOK$ SD-PATH-FILL
   toff tlen kind SD-CALL-LOADER ;


: SD-LOCALS-RESET ( -- )
   0 SD-LOCALS !
   0 SD-LOCAL-BASE !
   0 SD-SCOPES ! ;

: SD-DEF-OPEN ( -- )
   SD-LOCALS-RESET
   0 SD-DEF-NAME !
   0 SD-DEF-STAGE !
   1 SD-DEF ! ;


: SD-LOCALS-RELEASE ( -- )
   SD-LOCAL-OFF-RELEASE SD-LOCAL-LEN-RELEASE
   SD-SCOPE-N-RELEASE SD-SCOPE-BASE-RELEASE
   SD-LOCALS-RESET ;


: SD-LOCAL? ( n n -- bool ) {: off:n len:n :}
   SD-LOCALS @ SD-LOCAL-BASE @ ?do
      off len SD-TOK$
      i SD-LOCAL-OFF @ i SD-LOCAL-LEN @ SD-TOK$ STR= if true unloop exit then
   loop
   false ;


: SD-LOCAL-NAME-LEN ( n n -- n ) {: off:n len:n :}
   len 0 ?do
      off i + SD-BYTE $3A = if i unloop exit then
   loop
   len ;


: SD-LOCAL+ ( n n -- ) {: off:n len:n :}
   SD-LOCALS @ 1+ dup SD-LOCAL-OFF-RESERVE SD-LOCAL-LEN-RESERVE
   off SD-LOCALS @ SD-LOCAL-OFF !
   off len SD-LOCAL-NAME-LEN SD-LOCALS @ SD-LOCAL-LEN !
   SD-LOCALS @ 1+ SD-LOCALS ! ;


: SD-LOCAL-GROUP ( -- )
   begin
      SD-RAW {: off:n len:n :}
      len 0= if E-DISC-UNTERM throw then
      off len SD-TOK$ s" :}" STR= if exit then
      off len SD-LOCAL+
   again ;


: SD-SCOPE-OPEN ( -- )
   SD-SCOPES @ 1+ dup SD-SCOPE-N-RESERVE SD-SCOPE-BASE-RESERVE
   SD-LOCALS @ SD-SCOPES @ SD-SCOPE-N !
   SD-LOCAL-BASE @ SD-SCOPES @ SD-SCOPE-BASE !
   SD-SCOPES @ 1+ SD-SCOPES ! ;


: SD-SCOPE-RESTORE ( -- )
   SD-SCOPES @ 0= if exit then
   SD-SCOPES @ 1- SD-SCOPE-N @ SD-LOCALS !
   SD-SCOPES @ 1- SD-SCOPE-BASE @ SD-LOCAL-BASE ! ;


: SD-SCOPE-CLOSE ( -- )
   SD-SCOPE-RESTORE
   SD-SCOPES @ 0 > if SD-SCOPES @ 1- SD-SCOPES ! then ;


: SD-SCOPE-STEP ( n n -- ) {: off:n len:n :}
   off len SD-TOK$ s" ;" STR= if SD-LOCALS-RESET 0 SD-DEF ! exit then
   off len SD-TOK$ s" else" STR=CI if SD-SCOPE-RESTORE exit then
   off len SD-TOK$ BLOCK-OPENER? if SD-SCOPE-OPEN exit then
   off len SD-TOK$ BLOCK-CLOSER? if SD-SCOPE-CLOSE then ;


\ These type definers consume a declaration through its matching closer. Its
\ family name, field names and variant names are raw operands, not calls or
\ definitions. A member name can itself spell a closer or a loader word.
: SD-FAMILY-KIND ( n n -- n )
   SD-TOK$ {: a:ptr u:n :}
   a u s" structure" STR=CI if 1 exit then
   a u s" enum" STR=CI if 2 exit then
   a u s" sumtype" STR=CI if 3 exit then
   0 ;

\ Habu's three global family words forward directly to their parser runners.
\ Such a definition retains the grammar of the family opener. A different
\ body is an ordinary shadow and must still be refused at the later use.
: SD-FORWARD-WORD? ( n n n -- bool ) {: off:n len:n kind:n :}
   kind 1 = if off len SD-TOK$ s" STRUCTURE-DECL:SD-RUN" STR=CI exit then
   kind 2 = if off len SD-TOK$ s" ENUM-DECL:ED-RUN" STR=CI exit then
   off len SD-TOK$ s" TDECL-SUMTYPE" STR=CI ;

: SD-DEF-TRACK ( n n -- ) {: off:n len:n :}
   SD-C-PKG @ 0 >= if exit then
   off len SD-FAMILY-KIND dup 0= if drop exit then
   SD-DEF-NAME !
   1 SD-DEF-STAGE !
   off SD-DEF-OFF ! ;

: SD-R-CANON-MARK ( -- )
   SD-HARVESTING @ 0= if exit then
   SD-R-N @ 0 ?do
      i SD-R-FILE @ SD-FILE-ID @ =
      i SD-R-OFF @ SD-DEF-OFF @ = and if
         1 i SD-R-CANON !
      then
   loop ;

: SD-DEF-FORWARD-STEP ( n n -- ) {: off:n len:n :}
   SD-DEF-STAGE @ 0= if exit then
   SD-DEF-STAGE @ 1 = if
      off len SD-DEF-NAME @ SD-FORWARD-WORD? if
         2 SD-DEF-STAGE !
      else
         0 SD-DEF-STAGE !
      then
      exit
   then
   off len SD-TOK$ s" ;" STR= if SD-R-CANON-MARK then
   0 SD-DEF-STAGE ! ;

: SD-FAMILY-END? ( n n n -- bool ) {: off:n len:n kind:n :}
   kind 1 = if off len SD-TOK$ s" ;structure" STR=CI exit then
   kind 2 = if off len SD-TOK$ s" ;enum" STR=CI exit then
   off len SD-TOK$ s" ;sumtype" STR=CI ;

: SD-CHECK-FAMILY ( n n -- ) {: off:n len:n :}
   off len SD-CHECK-USE
   off off len SD-R-FAMILY SD-R-RECORD drop ;

: SD-FAMILY-BODY ( n -- ) {: kind:n :}
   SD-RAW 2drop                        \ family name
   begin
      SD-RAW {: off:n len:n :}
      len 0= if exit then
      off len SD-TOK$ s" field" STR=CI
      off len SD-TOK$ s" variant" STR=CI or if
         SD-RAW 2drop                   \ member name
      else
         off len kind SD-FAMILY-END? if exit then
      then
   again ;


\ In a body only `[char]` and `[']` parse; at top level `char` and `'` do too.
: SD-PARSER? ( n n -- bool ) {: off:n len:n :}
   SD-DEF @ 0<> SD-SCOPES @ 0<> or if
      off len SD-TOK$ SOURCE-SYNTAX:BODY-PARSING-KEYWORD? exit
   then
   off len SD-TOK$ PARSING-KEYWORD? ;

\ A comment is skipped first, as the loader skips it, and like blanks it keeps a
\ pending literal path: `s" x.f" ( why ) required` loads x.f. Every other token
\ ends it. A local is looked up before the parsing keywords: in a body a local
\ named `char` is that local and takes no operand. A definition's opener and,
\ outside a definition, an engine definer, a package keyword and `undefine`
\ take the next token as the name they give, take or retire, a loader word's
\ too, and load nothing; a name missing at the source's end is theirs to
\ refuse. A name given or retired is a loader word's spelling replaced.
: SD-STEP ( n n -- )
   {: off:n len:n :}
   len 1 = off SD-BYTE SD-BACKSLASH = and if SD-SKIP-LINE exit then
   len 1 = off SD-BYTE SD-LPAREN = and if SD-SKIP-PAREN exit then
   SD-DEF @ 0<> if off len SD-DEF-FORWARD-STEP then
   SD-PEND @ {: pend:n :}
   0 SD-PEND !
   off len SD-TOK$ s" [:" STR= if
      SD-SCOPE-OPEN SD-LOCALS @ SD-LOCAL-BASE ! exit
   then
   off len SD-TOK$ s" ;]" STR= if SD-SCOPE-CLOSE exit then
   off len SD-LOCAL? if exit then
   off len SD-PARSER? if SD-RAW 2drop exit then
   \ PRIM: declares an effect for the named word; that name is source data.
   SD-DEF @ 0= off len SD-TOK$ s" PRIM:" STR=CI and if
      SD-RAW 2drop exit
   then
   SD-DEF @ 0= off len SD-TOK$ DEFINER-KEYWORD? and if
      SD-RAW {: noff:n nu:n :}
      noff nu SD-R-DECL-NAME
      noff nu SD-TOK$ SD-SHADOW exit
   then
   SD-DEF @ 0= off len SD-TOK$ s" package" STR=CI and if
      SD-RAW {: poff:n pu:n :}
      poff poff pu SD-R-PACKAGE SD-R-RECORD drop
      poff SD-R-AT {: r:n :}
      r 0 >= if r SD-C-PACKAGE-OPEN then
      exit
   then
   SD-DEF @ 0= off len SD-TOK$ s" ;package" STR=CI and if
      off off 0 SD-R-END-PACKAGE SD-R-RECORD drop
      off SD-R-AT 0 >= if SD-C-PACKAGE-CLOSE then
      exit
   then
   SD-DEF @ 0= off len SD-TOK$ PACKAGE-KEYWORD? and if
      SD-RAW {: noff:n nu:n :}
      off len SD-TOK$ s" export" STR=CI if noff nu SD-R-DECL-NAME then
      exit
   then
   SD-DEF @ 0= SD-SCOPES @ 0= and if
      off len SD-FAMILY-KIND dup 0<> if
         off len SD-CHECK-FAMILY SD-FAMILY-BODY exit
      then drop
   then
   off len SD-TOK$ s" {:" STR= if off SD-OPENER ! SD-LOCAL-GROUP exit then
   off len SD-OPENER-KIND {: opener:n :}
   opener 0= 0= if off SD-OPENER ! opener SD-PEND ! len 3 = SD-SCAN-STRING exit then
   off len SD-LOADER-KIND {: lkind:n :}
   lkind 0= 0= if off len lkind pend SD-DISPATCH-LOADER exit then
   off len SD-TOK$ s" :" STR= if
      SD-DEF-OPEN SD-RAW {: noff:n nu:n :}
      noff nu SD-R-DECL-NAME noff nu SD-DEF-TRACK
      noff nu SD-TOK$ SD-SHADOW exit
   then
   off len SD-TOK$ SOURCE-SYNTAX:BODY-OPENER? if
      SD-DEF-OPEN SD-RAW {: noff:n nu:n :}
      noff nu SD-R-DECL-NAME noff nu SD-TOK$ SD-SHADOW exit
   then
   SD-DEF @ 0= off len SD-TOK$ s" undefine" STR=CI and if
      SD-RAW {: noff:n nu:n :}
      noff nu SD-R-RETIRE-NAME noff nu SD-TOK$ SD-RETIRE exit
   then
   SD-DEF @ 0= off len SD-TOK$ s" UNDEFINE-IF-DEFINED" STR=CI and if
      off pend SD-R-RETIRE-STACK pend SD-RETIRE-NAMED exit
   then
   SD-DEF @ 0= SD-SCOPES @ 0= and off len SD-TOK$ s" parses:" STR=CI and if
      SD-R-PARSES SD-R-ROW exit
   then
   SD-DEF @ 0= SD-SCOPES @ 0= and off len SD-TOK$ s" parses-through:" STR=CI and if
      SD-R-THROUGH SD-R-ROW exit
   then
   off len SD-TOK$ s" include" STR=CI if off len SD-K-INCLUDED SD-LOADER-IMM exit then
   off len SD-TOK$ s" require" STR=CI if off len SD-K-REQUIRED SD-LOADER-IMM exit then
   off len SD-R-USE? if exit then
   off len SD-SCOPE-STEP ;

: SD-WALK ( -- )
   0 SD-I !
   0 SD-PEND !
   0 SD-TOK-OFF !
   0 SD-TOK-LEN !
   0 SD-DEF !
   0 SD-SHADOWED !
   0 SD-RETIRED !
   SD-LOCALS-RESET
   begin
      SD-RAW {: off:n len:n :}
      len 0= if exit then
      off len SD-STEP
   again ;

\ The entry is read to its end however it grows while it is read: its size,
\ whose refusal (E-FS-STAT) stays a missing or irregular entry's, is only the
\ first room.
: SD-READ-ENTRY ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu  pa pu FS:FILE-SIZE  [: SD-ROOM ;] READ-WHOLE-SAMPLED SD-U !
   SD-STORAGE @ SPAN:$ drop SD-A ! ;

: SD-KIND-NAME ( n -- ptr u8 n ) {: kind:n :}
   kind EV-INCLUDED = if s" included" exit then
   kind EV-REQUIRED = if s" required" exit then
   s" provided" ;

: SD-EMIT-EVENT ( n ptr u8 len ptr len -- ) {: ix:n dst:ptr cap:len lenp:ptr :}
   ix EVENT-KIND@ SD-KIND-NAME >LEN dst cap lenp SOURCE-APPEND-BYTES
   SD-SP dst cap lenp SOURCE-APPEND-C
   ix EVENT-STATE@ $30 + dst cap lenp SOURCE-APPEND-C
   SD-SP dst cap lenp SOURCE-APPEND-C
   ix EVENT-PATH@ >LEN dst cap lenp SOURCE-APPEND-QPATH
   SD-LF dst cap lenp SOURCE-APPEND-C ;

: SD-WALK-IN ( -- )
   SD-ROOT SD-ROOT-U @ [: SD-WALK ;] WITH ;

: SELECT-ENTRY ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n root:ptr rootu:n :}
   pu SD-PATH-CAP > rootu SD-PATH-CAP > or if E-DISC-CAPACITY throw then
   pa SD-ENTRY pu BYTE-COPY pu SD-ENTRY-U !
   root SD-ROOT rootu BYTE-COPY rootu SD-ROOT-U ! ;

public

\ A zero code is no refusal; any other names the file on fd 2 and is rethrown
\ unchanged, because the code alone (E-FS-STAT or E-FS-OPEN for a missing file)
\ does not say which closure member it was. RUN-IN's reader refuses through it,
\ so the line names no tool. The write result is dropped because the next step
\ raises the refusal itself.
: READ-THROW ( ptr u8 n n -- ) {: pa:ptr pu:n rc:n :}
   rc 0= if exit then
   2 s" cannot read " write drop
   2 pa pu write drop
   2 S\" \n" write drop
   rc throw ;

private

: READ-SELECTED ( -- )
   [: SD-ENTRY SD-ENTRY-U @ SD-READ-ENTRY ;] catch {: rc:n :}
   SD-ENTRY SD-ENTRY-U @ rc READ-THROW ;

\ The replay moves the package again from the file's start, segment by segment,
\ so a walk leaves it as it found it.
: RUN-SELECTED ( -- )
   SD-C-PKG @ SD-C-OUTER @ {: pkg:n outer:n :}
   SD-HARVESTING @ if
      SD-FILE-OF SD-FILE-ID ! SD-R-BEGIN
      -1 SD-C-PKG ! -1 SD-C-OUTER ! SD-FILE-ID @ 0 SD-S-SEED
   else
      SD-ENTRY SD-ENTRY-U @ SD-FILE-FIND SD-FILE-ID !
   then
   REQUIRE-SNAPSHOT
   SD-LOADING-U @ 0<> if SD-LOADING SD-LOADING-U @ REQUIRE-STORE 2drop then
   EVENTS-RESET EVENT-ON DISCOVERY-ON
   [: SD-WALK-IN ;] catch {: rc:n :}
   pkg SD-C-PKG ! outer SD-C-OUTER !
   DISCOVERY-OFF EVENT-OFF
   SD-HARVESTING @ if SD-R-END then
   REQUIRE-RESTORE
   SD-LOCALS-RELEASE
   SD-PATH-RELEASE
   SD-DEC-RELEASE
   rc 0= 0= if rc throw then ;

public

: ROWS-RESET ( -- )
   -1 SD-C-PKG !
   -1 SD-C-OUTER !
   0 SD-R-N !
   0 SD-F-N !
   0 SD-R-ROWS-N !
   0 SD-R-F-N !
   0 SD-R-F-LOOKUP-N !
   0 SD-R-PASS !
   0 SD-R-PASS-STABLE !
   0 SD-R-BYTES-U !
   0 SD-R-VERSION !
   0 SD-S-N !
   SD-R-BYTES-RELEASE
   SD-F-AT-RELEASE SD-F-U-RELEASE SD-F-HEAD-RELEASE SD-F-START-RELEASE
   SD-F-SITE-RELEASE
   SD-S-OFF-RELEASE SD-S-PKG-RELEASE SD-S-OUTER-RELEASE SD-S-NEXT-RELEASE
   SD-R-OFF-RELEASE SD-R-SPAN-RELEASE SD-R-FILE-RELEASE SD-R-NEXT-RELEASE
   SD-R-KIND-RELEASE SD-R-NAME-RELEASE SD-R-NAME-U-RELEASE
   SD-R-PKG-RELEASE SD-R-PKG-U-RELEASE SD-R-OUTER-RELEASE SD-R-OUTER-U-RELEASE
   SD-R-COUNT-RELEASE SD-R-TERMS-RELEASE SD-R-TERMS-U-RELEASE
   SD-R-ACTIVE-RELEASE SD-R-DISABLED-RELEASE SD-R-BARE-OFF-RELEASE
   SD-R-CANON-RELEASE
   SD-C-ROW-RELEASE SD-C-OWNER-RELEASE SD-C-ORDER-RELEASE
   SD-E-SPEC-RELEASE
   SD-R-ROWS-RELEASE SD-R-FROZEN-RELEASE ;

: ROWS-VERSION ( -- n ) SD-R-VERSION @ ;
: EVENT-SPEC@ ( n -- bool ) SD-E-SPEC @ 0<> ;

: CONTEXT-BEGIN ( -- ) SD-R-CONTEXT-BEGIN ;
: CONTEXT-BETWEEN ( n n ptr u8 n -- ) SD-R-CONTEXT-BETWEEN ;

\ A file a body loads starts where the body runs, in a package the replay
\ cannot know. Unless it closes each package it opens, the body may change the
\ package anywhere after its definition, so from there on it is unknown.
: CONTEXT-SPEC-BEGIN ( -- n n )
   SD-C-PKG @ SD-C-OUTER @
   SD-C-UNKNOWN SD-C-PKG !
   -1 SD-C-OUTER ! ;

: CONTEXT-SPEC-END ( n n -- )
   SD-C-PKG @ 0 < SD-C-OUTER @ -1 = and 0= if 2drop SD-C-UNKNOWN SD-C-UNKNOWN then
   SD-C-OUTER ! SD-C-PKG ! ;

\ A pass reads only rows frozen before any file in it was scanned. Its new
\ evidence cannot make a row valid or invalid halfway through the pass.
: ROWS-PASS-BEGIN ( -- ) SD-R-FREEZE 1 SD-R-PASS ! ;
: ROWS-PASS-END ( -- ) SD-R-PASS-END ;
: ROWS-PASS-STABLE? ( -- bool ) SD-R-PASS-STABLE @ 0<> ;

\ The caller's discovery run keeps its events up to a source refusal. They
\ give consumers a closure to harvest before their final expansion.
: HARVEST ( [ -- ] -- ) {: q :}
   EVENTS-RESET
   1 SD-HARVESTING !
   q catch {: rc:n :}
   0 SD-HARVESTING !
   rc 0= if exit then
   rc E-DISC-FIRST <= rc E-DISC-LAST >= and if exit then
   rc E-FS-FIRST <= rc E-FS-LAST >= and if EVENTS-RESET exit then
   rc throw ;

\ Discovery takes the canonical PATH as a file the loader has begun, as
\ `bin/hb --load PATH` registers PATH before it loads PATH's closure: a require
\ of PATH resolves to it whether a file is there or not, never to another file
\ a fallback root finds. It lasts until the next LOADING!; an empty PATH sets
\ none.
: LOADING! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u SD-PATH-CAP > if E-DISC-CAPACITY throw then
   a SD-LOADING u BYTE-COPY
   u SD-LOADING-U ! ;

: RUN-IN ( ptr u8 n ptr u8 n -- )
   SELECT-ENTRY
   READ-SELECTED
   RUN-SELECTED ;

\ RUN-IN in two steps, for a caller that reports a member it cannot read
\ itself: READ-IN reads PATH under ROOT and throws the read's code with no
\ prose, and RUN-READ walks what it read.
: READ-IN ( ptr u8 n ptr u8 n -- )
   SELECT-ENTRY
   SD-ENTRY SD-ENTRY-U @ SD-READ-ENTRY ;

: RUN-READ ( -- )
   RUN-SELECTED ;

: RUN-BYTES ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: pa:ptr pu:n root:ptr rootu:n bytes:ptr size:n :}
   pa pu root rootu SELECT-ENTRY
   bytes SD-A ! size SD-U !
   RUN-SELECTED ;

: RUN ( ptr u8 n -- )
   ENTRY-RESOLVE drop RESOLVED-ROOT$ RUN-IN ;

\ Where the string or locals group a walk ended at with E-DISC-UNTERM opens:
\ the byte of its opener in the file that ended the walk.
: OPENER-AT ( -- n )
   SD-OPENER @ ;

\ The bytes the last run read, and where the token it read last starts in them
\ and its length. A run that refused a loader form read that form last: a
\ loader word with no literal path, or one whose spelling the file replaced.
: BYTES$ ( -- ptr u8 n )
   SD-BUF SD-U @ ;

: LAST-TOKEN ( -- n n )
   SD-TOK-OFF @ SD-TOK-LEN @ ;

: EMIT ( ptr u8 n -- n ) {: dst:ptr cap:n :}
   0 >LEN SD-EMIT-LEN !
   cap >LEN {: clen:len :}
   0 begin dup EVENT-COUNT < while
      dup dst clen SD-EMIT-LEN SD-EMIT-EVENT
      1+
   repeat drop
   SD-EMIT-LEN @ LEN>N ;

;using
;using
;package
