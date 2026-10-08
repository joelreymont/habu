\ source-discovery.f - whole-file ordered source-composition discovery pass.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f
\ lib/source.f tools/source-discovery.f
\
\ DISCOVER:RUN lexes an entry file's ENTIRE token stream - colon bodies
\ included - and replays every literal source-composition form against a fresh
\ require/provided registry, driving the instrumented core loader words
\ (included/required/provided) in record-only discovery mode so the ordered
\ event log (src/core/include.f) captures include multiplicity and
\ require/provided canonical registry state without loading or compiling
\ anything. A guarded loader inside a colon body taking a literal path the loader
\ resolves is recorded unconditionally, so the event closure over-approximates
\ (superset of the runtime closure) and can never under-approximate a
\ statically-visible loader call. A string loader word in a body after anything
\ else, a path the engine's resolver refuses among them, is a call the word
\ makes when it runs, which no check runs, and the walk reads past it. The name
\ a definer takes is data, a loader word's too: defining, undefining or
\ retiring one refuses nothing. The walk holds no wordlists to tell which word
\ that spelling names after it, so a later loader form with it that the walk
\ would follow, at top level or in a body, rejects fail-closed at the word, as
\ a dynamic (non-literal) loader path and an unsupported string opener (C\" /
\ .\") before a loader word at top level do. An escaped literal (S\")
\ names the path its escapes decode to. A bad escape refuses nothing: at top
\ level it ends the walk where the load stops, and in a definition, which the
\ check rejects and reads past, the walk reads past it and follows no loader
\ taking it. Recorded loader-token spans are entry-file byte offsets.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/span.f
require lib/fs.f
require lib/source.f

package DISCOVER
using SOURCE                             \ the shared source-string emitters
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

\ The literal path a loader word takes, however long: the engine's resolver is
\ the only cap on it.
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
variable SD-PEND
variable SD-EMIT-LEN
variable SD-OPENER                       \ where the last string or group opener starts
variable SD-TOK-OFF                      \ the token read last: where it starts
variable SD-TOK-LEN                      \ and its length
variable SD-CALL-KIND                    \ the loader SD-CALL-LOADER calls
variable SD-DEF                          \ 1 from a definition's opener to its ;
variable SD-SHADOWED                     \ the loader words (SD-LOADER-BIT) the file defined
variable SD-RETIRED                      \ and those it retired

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

\ Room for n bytes of the source being read, reusing the largest allocation.
\ The read fills it as it grows (lib/source.f READ-WHOLE-SAMPLED), so a larger
\ span takes the bytes the old one holds and is installed before the old one is
\ released. No scanner pointer survives a read.
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

\ The u bytes at a become the path, with a byte more so that an empty one still
\ has a byte 0.
: SD-PATH-FILL ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1+ SD-PATH-RESERVE
   a 0 SD-PATH u BYTE-COPY
   u SD-PATH-U ! ;

\ Room for an escaped literal's decode: as many bytes as its payload, since a
\ decode is never longer than its spelling.
DYNAMIC-BUFFER SD-DEC u8

\ A literal holding a bad escape is one the engine refuses, a bad string
\ literal, where the load stops. At top level the walk ends at it too: it
\ follows nothing at or after the literal and refuses nothing, leaving the
\ literal to the check, which refuses it at its opener. In a definition it is
\ one more error of a body the check rejects and reads past, so the walk reads
\ past it as well, with no path pending: a loader or retirement taking it is a
\ call in a body, nothing to follow or refuse.
: SD-REFUSED-LITERAL ( -- )
   SD-DEF @ 0= if SD-U @ SD-I ! exit then
   0 SD-PEND ! ;

\ An escaped literal's path is what the engine's literal makes of its payload,
\ the payload decoded by the engine's escape table (src/core/checker.f
\ ESC-DECODE), unless an escape is bad.
: SD-DECODE-PATH ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1+ SD-DEC-RESERVE
   0 SD-DEC {: dst:ptr :}
   a u dst ESC-DECODE {: k:n ok:bool :}
   ok 0= if SD-REFUSED-LITERAL exit then
   dst k SD-PATH-FILL ;

\ The literal whose opener SD-I stands after: SD-I steps past its closing quote,
\ and its path is its payload, decoded when the opener is an escaped one, where
\ a backslash and the byte after it never close the literal.
: SD-SCAN-STRING ( bool -- ) {: escaped:bool :}
   SD-I @ SD-AT? if SD-I @ SD-BYTE SD-SP = if SD-I @ 1+ SD-I ! then then
   SD-I @ {: start:n :}
   begin SD-I @ SD-AT? while
      SD-I @ SD-BYTE SD-DQUOTE = if
         start SD-BUF +  SD-I @ start -
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

\ A loader word's bit in SD-SHADOWED and SD-RETIRED, 0 for any other token.
: SD-LOADER-BIT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" include" STR=CI if $01 exit then
   a u s" included" STR=CI if $02 exit then
   a u s" require" STR=CI if $04 exit then
   a u s" required" STR=CI if $08 exit then
   a u s" provided" STR=CI if $10 exit then
   0 ;

$1F constant SD-LOADERS                  \ every loader word's bit

\ A definition of a loader word's name, or a retirement of it, refuses nothing,
\ but past it the walk cannot tell which word the spelling names, so a later
\ loader form with it is refused (SD-CHECK-USE).
: SD-SHADOW ( ptr u8 n -- )   SD-LOADER-BIT SD-SHADOWED @ or SD-SHADOWED ! ;

: SD-RETIRE ( ptr u8 n -- )   SD-LOADER-BIT SD-RETIRED @ or SD-RETIRED ! ;

\ UNDEFINE-IF-DEFINED at top level retires the word the path literal right
\ before it names, and when no path literal does, a word the walk cannot read,
\ any loader word among them. In a body it retires nothing until the word runs,
\ which no check runs.
: SD-RETIRE-NAMED ( n -- ) {: pend:n :}
   pend SD-PEND-PATH = if SD-PATH$ SD-RETIRE exit then
   SD-RETIRED @ SD-LOADERS or SD-RETIRED ! ;

\ A loader form the walk reads, whose loader word stands at OFF LEN, is refused
\ at the word when the file has defined that word's name (E-DISC-SHADOW), and
\ else when it has retired it (E-DISC-RETIRE).
: SD-CHECK-USE ( n n -- ) {: off:n len:n :}
   off len SD-TOK$ SD-LOADER-BIT {: bit:n :}
   bit SD-SHADOWED @ and 0<> if E-DISC-SHADOW throw then
   bit SD-RETIRED @ and 0<> if E-DISC-RETIRE throw then ;

: SD-CALL-ACT ( -- )
   SD-CALL-KIND @ {: kind:n :}
   kind SD-K-INCLUDED = if SD-PATH$ included exit then
   kind SD-K-REQUIRED = if SD-PATH$ required exit then
   SD-PATH$ provided ;

\ The loader resolves the literal against its roots before it records the load:
\ false, and nothing recorded, for a path the resolver refuses (E-PATH-RANGE:
\ empty, holding a NUL, absolute and 2050 bytes or more, relative and 2050 bytes
\ or more joined as root, `/` and path to a root searched before one holds the
\ file, or over PATH-CAP bytes once normalized; src/core/include.f SOURCE-ROOT
\ CHECK, SEARCH-ROOTS, JOIN! and NORMALIZE).
: SD-LOAD? ( n n n -- bool )
   {: off:n len:n kind:n :}
   off len DISC-TOK!
   kind SD-CALL-KIND !
   [: SD-CALL-ACT ;] catch {: rc:n :}
   rc 0= if true exit then
   rc E-PATH-RANGE <> if rc throw then
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
\ superset an ordinary composition loads (src/habu/verify-source.f, a loader in
\ a definition). After anything else, or after a path the resolver refuses,
\ which the call loads nothing from, it is nothing to follow or refuse.
\ At top level the load runs while the file loads, so a path the walk cannot
\ follow is refused. A form the walk reads, any at top level and one right after
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
\ over INCLUDE-PATH-CAP bytes, which is PATH-CAP, before they resolve it
\ (src/core/include.f INCLUDE-CHECK-PATH); the resolver decides the rest. The
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

\ A definition, from its `:`, `kernel:` or `TRUSTED:` to its `;`, opens with no
\ locals.
: SD-DEF-OPEN ( -- )
   SD-LOCALS-RESET
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
   SD-PEND @ {: pend:n :}
   0 SD-PEND !
   off len SD-TOK$ s" [:" STR= if
      SD-SCOPE-OPEN SD-LOCALS @ SD-LOCAL-BASE ! exit
   then
   off len SD-TOK$ s" ;]" STR= if SD-SCOPE-CLOSE exit then
   off len SD-LOCAL? if exit then
   off len SD-TOK$ PARSING-KEYWORD? if SD-RAW 2drop exit then
   SD-DEF @ 0= off len SD-TOK$ DEFINER-KEYWORD? and if SD-RAW SD-TOK$ SD-SHADOW exit then
   SD-DEF @ 0= off len SD-TOK$ PACKAGE-KEYWORD? and if SD-RAW 2drop exit then
   off len SD-TOK$ s" {:" STR= if off SD-OPENER ! SD-LOCAL-GROUP exit then
   off len SD-OPENER-KIND {: opener:n :}
   opener 0= 0= if off SD-OPENER ! opener SD-PEND ! len 3 = SD-SCAN-STRING exit then
   off len SD-LOADER-KIND {: lkind:n :}
   lkind 0= 0= if off len lkind pend SD-DISPATCH-LOADER exit then
   off len SD-TOK$ s" :" STR= if SD-DEF-OPEN SD-RAW SD-TOK$ SD-SHADOW exit then
   off len SD-TOK$ s" kernel:" STR=CI if SD-DEF-OPEN SD-RAW SD-TOK$ SD-SHADOW exit then
   off len SD-TOK$ s" TRUSTED:" STR=CI if SD-DEF-OPEN SD-RAW SD-TOK$ SD-SHADOW exit then
   SD-DEF @ 0= off len SD-TOK$ s" undefine" STR=CI and if SD-RAW SD-TOK$ SD-RETIRE exit then
   SD-DEF @ 0= off len SD-TOK$ s" UNDEFINE-IF-DEFINED" STR=CI and if pend SD-RETIRE-NAMED exit then
   off len SD-TOK$ s" include" STR=CI if off len SD-K-INCLUDED SD-LOADER-IMM exit then
   off len SD-TOK$ s" require" STR=CI if off len SD-K-REQUIRED SD-LOADER-IMM exit then
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
   pa pu  pa pu FILE-SIZE  [: SD-ROOM ;] READ-WHOLE-SAMPLED SD-U !
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

\ `throw` for a closure member's read. A zero code is no refusal; any other
\ names the file on fd 2 and is rethrown unchanged, because the code alone
\ (E-FS-STAT or E-FS-OPEN for a missing file) does not say which member it was.
\ RUN-IN's reader and the source view's (tools/native-source-view.f
\ READ-COLLECT) both refuse through it, so the line names no tool. The write
\ result is dropped because the next step raises the refusal itself, as
\ src/core/checker.f's compile rejects do.
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

: RUN-SELECTED ( -- )
   REQUIRE-SNAPSHOT
   SD-LOADING-U @ 0<> if SD-LOADING SD-LOADING-U @ REQUIRE-STORE 2drop then
   EVENTS-RESET EVENT-ON DISCOVERY-ON
   [: SD-WALK-IN ;] catch {: rc:n :}
   DISCOVERY-OFF EVENT-OFF
   REQUIRE-RESTORE
   SD-LOCALS-RELEASE
   SD-PATH-RELEASE
   SD-DEC-RELEASE
   rc 0= 0= if rc throw then ;

public

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
