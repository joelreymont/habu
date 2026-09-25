\ include.f - checked source include words.
\
\ `include` is source composition. Package reopening owns shared namespace;
\ this file only gives source files a checked way to load dependencies.

$400 constant INCLUDE-PATH-CAP
$100000 constant INCLUDE-BUF-CAP  \ checker.f crossed the old 512 KiB slot
$200 constant REQUIRE-MAX  \ composed maki+stdlib require closure crossed 256 (2026-07-20)
$1 constant INCLUDE-PROBE-CAP
$4A constant INCLUDE-IO-RC
$46 constant INCLUDE-EVAL-RC
$37D8 constant INCLUDE-EVALERR-CELL
\ Where this file publishes the innermost open source file's path for the engine's
\ own refusal tail (src/habu/habu2.f LCOMPILEDIE, dot habu-name-the-file-70acbf10).
\ Fixed offsets from data-base, spelled here the way INCLUDE-EVALERR-CELL is and
\ reserved in src/habu/layout.f as SRCLOC:PATH-CELL / SRCLOC:PATHLEN-CELL.
$2800 constant INCLUDE-SRCLOC-PATH-CELL
$2808 constant INCLUDE-SRCLOC-PATHLEN-CELL
create INCLUDE-PATH INCLUDE-PATH-CAP 1 + allot
create INCLUDE-PROBE INCLUDE-PROBE-CAP allot
create REQUIRE-LENS REQUIRE-MAX cells allot

package REQUIRE-REG
private

REQUIRE-MAX INCLUDE-PATH-CAP * constant POOL-CAP
create OFFSETS REQUIRE-MAX cells allot
PERSISTED-PTR-VARIABLE FROZEN
PTR-VARIABLE MAPPED
variable PREFIX-N

;package

variable INCLUDE-DEPTH
variable INCLUDE-FD
variable INCLUDE-U
variable INCLUDE-RD
PTR-VARIABLE INCLUDE-PATH-A
variable INCLUDE-PATH-U
variable INCLUDE-PATH-I
variable REQUIRE-N
variable REQUIRE-BOOT-N
variable REQUIRE-BOOT-V
variable REQUIRE-BASE
variable REQUIRE-SAVE-N
variable REQUIRE-SAVE-BASE

-1 INCLUDE-FD !

: INCLUDE-FALSE ( -- bool )
   0 0= 0= ;

: INCLUDE-TRUE ( -- bool )
   0 0= ;

\ Rows recorded while the boot registry is open describe what the ENGINE
\ carries; every other row describes what a process went on to load. Only the
\ boot prefix may open it, and it says so with a token of its own (habu2.f
\ EMIT-REQUIRE-BOOT-OPEN-TOKEN) between the loader's own text and the first
\ row. Loading this file is NOT the signal: a build re-loads it into a booted
\ engine, and an engine that opened the registry there would record every
\ later require portably and stop recognising its own canonical rows.
: REQUIRE-BOOT-OPEN ( -- )
   INCLUDE-TRUE REQUIRE-BOOT-V ! ;

: REQUIRE-BOOT-OPEN? ( -- bool )
   REQUIRE-BOOT-V @ 0= 0= ;

\ How many rows are the ENGINE's. While the registry is open every row recorded
\ so far is one, because only the prefix records there; once it closes the
\ frozen count says so for the rest of the process. The open case is not a
\ nicety: the prefix requires engine files WHILE it is recording them, and a
\ portable row answers no other question, so a bound of REQUIRE-BOOT-N alone
\ made the prefix reload its own checker (measured: `duplicate definition:
\ RAW-OFF`, src/core/checker.f loaded twice, exit 78).
: REQUIRE-BOOT-LIMIT ( -- n )
   REQUIRE-BOOT-OPEN? if REQUIRE-N @ exit then
   REQUIRE-BOOT-N @ ;

\ Canonical source paths and a dynamically scoped owner root. This bootstrap
\ layer uses only core bytes, mappings and the bounded realpath OS primitive.
package SOURCE-ROOT
private

7134 constant PATH-RC  \ PATHZ range refusal; util.f precedes checker registration.

INCLUDE-PATH-CAP 1+ constant PATH-BYTES
PATH-BYTES 2 * constant WORK-BYTES
create CWD-BUF PATH-BYTES allot
create CANON-BUF PATH-BYTES allot
create NORMAL-BUF WORK-BYTES allot
create CANDIDATE-BUF PATH-BYTES allot
create JOIN-BUF WORK-BYTES allot
create WORK-BUF WORK-BYTES allot
create ZBUF WORK-BYTES allot
create OWNER-BUF PATH-BYTES allot
create REQUEST-BUF WORK-BYTES allot
variable REQUEST-U
variable CWD-U
variable CANON-U
variable NORMAL-U
variable CANDIDATE-U
variable JOIN-U
variable OWNER-U
PTR-VARIABLE CURRENT-A
variable CURRENT-U
variable SCOPES


: CURRENT-PTR ( -- ptr u8 ) CURRENT-A @ ;


: CURRENT! ( ptr u8 n -- )
   CURRENT-U ! CURRENT-A ! ;


: CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= u WORK-BYTES >= or if PATH-RC throw then
   u 0 ?do a i + c@ 0= if PATH-RC throw then loop ;


: COPY-Z ( ptr u8 n ptr u8 -- ) {: a:ptr u:n dst:ptr :}
   a u CHECK
   a dst u BYTE-COPY 0 dst u + c! ;


: TRY-CANON ( ptr u8 n -- bool )
   ZBUF COPY-Z
   ZBUF CANON-BUF PATH-BYTES realpath {: n:n :}
   n -2 = if PATH-RC throw then
   n 0 < if 0 CANON-U ! INCLUDE-FALSE exit then
   n CANON-U ! INCLUDE-TRUE ;


: CANON$ ( -- ptr u8 n ) CANON-BUF CANON-U @ ;


: CWD-INIT ( -- )
   CWD-U @ 0= 0= if exit then
   s" ." TRY-CANON 0= if INCLUDE-IO-RC throw then
   CANON-BUF CWD-BUF CANON-U @ 1+ BYTE-COPY
   CANON-U @ CWD-U ! ;

public

: CWD$ ( -- ptr u8 n ) CWD-INIT CWD-BUF CWD-U @ ;


: CURRENT$ ( -- ptr u8 n )
   CURRENT-U @ 0 > if CURRENT-PTR CURRENT-U @ exit then
   CWD$ ;

private

: JOIN! ( ptr u8 n ptr u8 n -- ) {: root:ptr rootu:n a:ptr u:n :}
   rootu u + 1+ WORK-BYTES >= if PATH-RC throw then
   root WORK-BUF rootu BYTE-COPY
   47 WORK-BUF rootu + c!
   a WORK-BUF rootu 1+ + u BYTE-COPY
   rootu u + 1+ JOIN-U !
   WORK-BUF JOIN-BUF JOIN-U @ BYTE-COPY ;


: ABSOLUTE! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECK
   a c@ 47 = if
      a JOIN-BUF u BYTE-COPY u JOIN-U ! exit
   then
   CWD$ a u JOIN! ;


: PARENT-U ( ptr u8 n -- n ) {: a:ptr u:n :}
   u
   begin dup 1 > while
      1- dup a + c@ 47 = if exit then
   repeat ;


: NORMAL-ROOM ( n n -- ) {: limit:n :}
   NORMAL-U @ + limit > if PATH-RC throw then ;


: NORMAL-SEG ( ptr u8 n n -- ) {: a:ptr u:n limit:n :}
   u 0= if exit then
   a u s" ." CORE-STR= if exit then
   a u s" .." CORE-STR= if
      NORMAL-BUF NORMAL-U @ PARENT-U NORMAL-U ! exit
   then
   NORMAL-U @ 1 > if
      1 limit NORMAL-ROOM
      47 NORMAL-BUF NORMAL-U @ + c!
      1 NORMAL-U +!
   then
   u limit NORMAL-ROOM
   a NORMAL-BUF NORMAL-U @ + u BYTE-COPY
   u NORMAL-U +! ;


: SEG-END ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   start
   begin dup u < while
      dup a + c@ 47 = if exit then
      1+
   repeat ;


: NORMALIZE-LIMIT ( ptr u8 n n -- ) {: a:ptr u:n limit:n :}
   47 NORMAL-BUF c! 1 NORMAL-U !
   0 begin dup u < while
      dup a u rot SEG-END {: end:n :}
      a over + end rot - limit NORMAL-SEG
      end 1+
   repeat drop
   0 NORMAL-BUF NORMAL-U @ + c! ;


: NORMALIZE ( ptr u8 n -- ) INCLUDE-PATH-CAP NORMALIZE-LIMIT ;


: EXISTING-PARENT ( -- n )
   JOIN-U @ begin
      JOIN-BUF swap PARENT-U
      dup JOIN-BUF swap TRY-CANON if exit then
      dup 1 <= if drop INCLUDE-IO-RC throw then
   again ;


: MISSING-NORMALIZE ( -- )
   EXISTING-PARENT {: cut:n :}
   CANON-U @ JOIN-U @ cut - + {: total:n :}
   total WORK-BYTES >= if PATH-RC throw then
   CANON-BUF WORK-BUF CANON-U @ BYTE-COPY
   JOIN-BUF cut + WORK-BUF CANON-U @ + JOIN-U @ cut - BYTE-COPY
   WORK-BUF total NORMALIZE ;

public

\ The pathname is canonical even for a missing leaf: resolve its existing
\ prefix physically, then normalize only the absent suffix for provided facts.
\ The flag says whether the complete pathname existed. Result storage lasts
\ until the next CANONICAL call.
: CANONICAL ( ptr u8 n -- ptr u8 n bool )
   ABSOLUTE!
   JOIN-BUF JOIN-U @ TRY-CANON if
      CANON-BUF NORMAL-BUF CANON-U @ 1+ BYTE-COPY
      CANON-U @ NORMAL-U !
      NORMAL-BUF NORMAL-U @ INCLUDE-TRUE exit
   then
   MISSING-NORMALIZE
   NORMAL-BUF NORMAL-U @ INCLUDE-FALSE ;


: DIRNAME ( ptr u8 n -- ptr u8 n )
   over swap PARENT-U ;


: JOIN ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u CHECK
   a c@ 47 = if 2drop a u exit then
   a u JOIN!
   JOIN-BUF JOIN-U @ ;


: BELOW? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n root:ptr rootu:n :}
   u rootu < if INCLUDE-FALSE exit then
   a rootu root rootu CORE-STR= 0= if INCLUDE-FALSE exit then
   u rootu = if INCLUDE-TRUE exit then
   rootu 1 = root c@ 47 = and if INCLUDE-TRUE exit then
   a rootu + c@ 47 = ;


: RELATIVE ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u:n root:ptr rootu:n :}
   a u root rootu BELOW? 0= if a u exit then
   u rootu = if s" ." exit then
   rootu 1 = if a 1+ u 1- exit then
   a rootu 1+ + u rootu 1+ - ;


: RESOLVED-ROOT$ ( -- ptr u8 n ) OWNER-BUF OWNER-U @ ;

private

: OWNER! ( ptr u8 n -- ) {: a:ptr u:n :}
   u INCLUDE-PATH-CAP > if PATH-RC throw then
   a OWNER-BUF u BYTE-COPY u OWNER-U ! ;


: ROOT-CANON ( ptr u8 n -- )
   ABSOLUTE!
   JOIN-U @ 2 + WORK-BYTES >= if PATH-RC throw then
   47 JOIN-BUF JOIN-U @ + c!
   46 JOIN-BUF JOIN-U @ 1+ + c!
   JOIN-BUF JOIN-U @ 2 + TRY-CANON 0= if INCLUDE-IO-RC throw then ;

public

\ Each scope owns a mapping sized to its root string. There is no additional
\ root-count/depth limit, and a throw restores the caller before releasing it.
: WITH ( ptr u8 n [ -- ] -- ) {: q :}
   ROOT-CANON
   CANON-U @ {: u:n :}
   u 1+ map-anon 0= 0= if drop INCLUDE-IO-RC throw then {: fresh:ptr :}
   CANON-BUF fresh u 1+ BYTE-COPY
   CURRENT$ {: old:ptr oldu:n :}
   fresh u CURRENT!
   1 SCOPES +!
   q catch {: rc:n :}
   -1 SCOPES +!
   old oldu CURRENT!
   fresh u 1+ munmap {: release:n :}
   rc 0= 0= if rc throw then
   release 0 < if INCLUDE-IO-RC throw then ;

private

: CLEAR-BYTES ( ptr u8 n -- )
   0 ?do 0 over i + c! loop drop ;

public

: RESET ( -- )
   SCOPES @ 0= 0= if INCLUDE-IO-RC throw then
   CWD-BUF PATH-BYTES CLEAR-BYTES
   CANON-BUF PATH-BYTES CLEAR-BYTES
   NORMAL-BUF WORK-BYTES CLEAR-BYTES
   CANDIDATE-BUF PATH-BYTES CLEAR-BYTES
   OWNER-BUF PATH-BYTES CLEAR-BYTES
   JOIN-BUF WORK-BYTES CLEAR-BYTES
   WORK-BUF WORK-BYTES CLEAR-BYTES
   ZBUF WORK-BYTES CLEAR-BYTES
   REQUEST-BUF WORK-BYTES CLEAR-BYTES
   0 REQUEST-U !
   0 CWD-U ! 0 CANON-U ! 0 NORMAL-U ! 0 CANDIDATE-U !
   0 JOIN-U ! 0 OWNER-U !
   NULL$ CURRENT! ;

;package

using SOURCE-ROOT

: INCLUDE-DIE ( ptr u8 n -- )
   INCLUDE-IO-RC die ;

: INCLUDE-EVAL-DIE ( ptr u8 n -- )
   INCLUDE-EVAL-RC die ;

: INCLUDE-CLOSE ( -- )
   INCLUDE-FD @ dup 0 >= if
      close
   else
      drop
   then
   -1 INCLUDE-FD ! ;

: INCLUDE-IO-DIE ( ptr u8 n -- )
   INCLUDE-CLOSE
   INCLUDE-DIE ;

: INCLUDE-PATH-A@ ( -- ptr u8 )
   INCLUDE-PATH-A @ ;

: INCLUDE-PATH-A! ( ptr u8 -- )
   INCLUDE-PATH-A ! ;

: INCLUDE-CHECK-PATH ( ptr u8 n -- ptr u8 n )
   dup 0 <= if s" include: missing path" INCLUDE-DIE then
   dup INCLUDE-PATH-CAP > if s" include: path too long" INCLUDE-DIE then ;

: REQUIRE-LEN@ ( n -- n )
   cells REQUIRE-LENS + @ ;

: REQUIRE-LEN! ( n n -- ) {: u:n idx:n :}
   u REQUIRE-LENS idx cells + ! ;

package REQUIRE-REG
private

: OFFSET@ ( n -- n )
   cells OFFSETS + @ ;

: OFFSET! ( n n -- ) {: off:n idx:n :}
   off OFFSETS idx cells + ! ;

: USED ( -- n )
   REQUIRE-N @ 0= if 0 exit then
   REQUIRE-N @ 1- dup OFFSET@ swap REQUIRE-LEN@ + ;

: CLAMP ( -- )
   PREFIX-N @ REQUIRE-N @ > if REQUIRE-N @ PREFIX-N ! then ;

\ A mapping, once allocated, mirrors all live bytes. Frozen rows still return
\ their DATA addresses, so appending never changes an existing live row borrow.
: ENSURE-MAPPED ( n -- ) {: used:n :}
   MAPPED @ NULL-PTR <> if exit then
   POOL-CAP map-anon 0= 0= if drop INCLUDE-IO-RC throw then
   {: fresh:ptr :}
   used 0 > if FROZEN @ fresh used BYTE-COPY then
   fresh MAPPED ! ;

: ROUND-CELL ( n -- n ) {: bytes:n :}
   bytes CELL mod {: rem:n :}
   rem 0= if bytes exit then
   bytes CELL rem - + ;

public

: SLOT ( n -- ptr u8 ) {: idx:n :}
   idx PREFIX-N @ < if FROZEN @ else MAPPED @ then
   idx OFFSET@ + ;

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   REQUIRE-N @ REQUIRE-MAX >= if
      s" require: too many files" INCLUDE-DIE
   then
   u 0 <= u INCLUDE-PATH-CAP > or if
      s" require: invalid path length" INCLUDE-DIE
   then
   USED {: used:n :}
   u POOL-CAP used - > if
      s" require: path pool full" INCLUDE-DIE
   then
   REQUIRE-N @ {: idx:n :}
   PREFIX-N @ idx min {: prefix:n :}
   used ENSURE-MAPPED
   a MAPPED @ used + u BYTE-COPY
   used idx OFFSET!
   u idx REQUIRE-LEN!
   prefix PREFIX-N !
   idx 1+ REQUIRE-N ! ;

: REWIND ( n -- )
   REQUIRE-N !
   CLAMP ;

\ Capture has one DATA allocation only when a new suffix survives. Its
\ leading and trailing padding are zero, and a failed unmap rolls back exactly
\ that one allot while the old owner is still published.
: PERSIST ( -- )
   MAPPED @ NULL-PTR = if
      CLAMP
      REQUIRE-N @ 0= if NULL-PTR FROZEN ! then
      exit
   then
   REQUIRE-N @ PREFIX-N @ <= if
      MAPPED @ POOL-CAP munmap 0< if INCLUDE-IO-RC throw then
      NULL-PTR MAPPED !
      CLAMP
      REQUIRE-N @ 0= if NULL-PTR FROZEN ! then
      exit
   then
   USED {: used:n :}
   here {: start:ptr :}
   start data-base - negate CELL 1- and {: pad:n :}
   pad used ROUND-CELL + {: total:n :}
   total allot
   total 0 ?do 0 start i + c! loop
   MAPPED @ start pad + used BYTE-COPY
   MAPPED @ POOL-CAP munmap 0< if
      total negate allot
      INCLUDE-IO-RC throw
   then
   start pad + FROZEN !
   NULL-PTR MAPPED !
   REQUIRE-N @ PREFIX-N ! ;

;package

: REQUIRE-SLOT ( n -- ptr u8 )
   REQUIRE-REG:SLOT ;

: REQUIRE-BYTE= ( ptr u8 n n -- bool ) {: a:ptr idx:n i:n :}
   a i ZBYTE@ idx REQUIRE-SLOT i ZBYTE@ = ;

: REQUIRE-PATH= ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   idx REQUIRE-LEN@ u <> if INCLUDE-FALSE exit then
   0 begin dup u < while
      dup a idx rot REQUIRE-BYTE= 0= if drop INCLUDE-FALSE exit then
      1+
   repeat drop INCLUDE-TRUE ;

: REQUIRE-KNOWN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   REQUIRE-BASE @ begin dup REQUIRE-N @ < while
      dup a u rot REQUIRE-PATH= if drop INCLUDE-TRUE exit then
      1+
   repeat drop INCLUDE-FALSE ;

: REQUIRE-CHECK-ROOM ( -- )
   REQUIRE-N @ REQUIRE-MAX >= if s" require: too many files" INCLUDE-DIE then ;

\ A boot row is recorded PORTABLY, in the one spelling BOOT-KNOWN? asks for: the
\ path relative to CWD where it lies below CWD, and the canonical path where it
\ does not, which is exactly what `CWD$ RELATIVE` answers. Store and query share
\ that root deliberately. Against the load's OWN owner root a nested require -
\ one issued by a boot file rather than by the manifest - would shorten to a bare
\ basename that no query ever spells, and a basename names a different file in
\ every directory.
\
\ Recording the canonical absolute spelling instead baked the build directory
\ into the engine binary, so two builds of one revision from two directories
\ differed (dot habu-bake-prefix-src-1047b604). An application row keeps its
\ canonical identity, so identical names in distinct roots still coexist. The two
\ spellings cannot collide: a canonical name always begins with `/` and a
\ portable one never does.
: REQUIRE-STORE ( ptr u8 n -- )
   REQUIRE-CHECK-ROOM
   REQUIRE-BOOT-OPEN? if CWD$ RELATIVE then {: a:ptr u:n :}
   a u REQUIRE-REG:APPEND ;

package SOURCE-ROOT
private


: REQUEST! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECK
   a REQUEST-BUF u BYTE-COPY u REQUEST-U ! ;

: REQUEST$ ( -- ptr u8 n ) REQUEST-BUF REQUEST-U @ ;

\ Portable names describe only frozen engine facts, and a boot row IS its
\ portable name, so this reads the registry itself. Ordinary application facts
\ retain their canonical identity so identical names in distinct roots coexist.
: BOOT-KNOWN? ( ptr u8 n n -- bool ) {: a:ptr u:n first:n :}
   first begin dup REQUIRE-BOOT-LIMIT < while
      dup a u rot REQUIRE-PATH= if drop INCLUDE-TRUE exit then
      1+
   repeat drop INCLUDE-FALSE ;

: CANDIDATE$ ( -- ptr u8 n ) CANDIDATE-BUF CANDIDATE-U @ ;

\ A directory symlink may carry an invocation-root engine spelling outside
\ CWD. Check only that spelling's portable row, then require its normalized path to
\ resolve to the same physical candidate: symlink/.. can name another file.
: BOOT-CANDIDATE ( ptr u8 n n -- ptr u8 n bool ) {: first:n :}
   first REQUIRE-BOOT-LIMIT >= if INCLUDE-FALSE exit then
   2dup CWD$ RELATIVE first BOOT-KNOWN? if INCLUDE-TRUE exit then
   dup CANDIDATE-U ! CANDIDATE-BUF swap BYTE-COPY
   REQUEST$ ABSOLUTE!
   \ The invocation spelling may be longer than its canonical symlink target.
   JOIN-BUF JOIN-U @ WORK-BYTES 1- NORMALIZE-LIMIT
   NORMAL-BUF NORMAL-U @ CWD$ BELOW? if
      NORMAL-BUF NORMAL-U @ CWD$ RELATIVE first BOOT-KNOWN? if
         NORMAL-BUF NORMAL-U @ CANONICAL drop
         CANDIDATE$ CORE-STR= CANDIDATE$ rot exit
      then
   then
   CANDIDATE$ INCLUDE-FALSE ;

: CANDIDATE ( ptr u8 n bool -- ptr u8 n bool bool ) {: fallback:bool :}
   2dup OWNER!
   REQUEST$ JOIN CANONICAL {: exists:bool :}
   2dup REQUIRE-KNOWN? {: known:bool :}
   fallback known 0= and if
      REQUIRE-BASE @ BOOT-CANDIDATE
   else known then
   dup exists or ;

: ABS-OWNER ( ptr u8 n -- )
   2dup CURRENT$ BELOW? if 2drop CURRENT$ OWNER! exit then
   2dup CWD$ BELOW? if 2drop CWD$ OWNER! exit then
   DIRNAME OWNER! ;

public

: RESOLVE ( ptr u8 n -- ptr u8 n bool )
   REQUEST!
   REQUEST-BUF c@ $2F = if
      REQUEST$ CANONICAL drop
      2dup ABS-OWNER
      2dup REQUIRE-KNOWN? {: known:bool :}
      known 0= if
         REQUIRE-BASE @ BOOT-CANDIDATE
      else known then
      exit
   then
   CURRENT$ CWD$ CORE-STR= if
      CWD$ INCLUDE-TRUE CANDIDATE drop exit
   then
   CURRENT$ INCLUDE-FALSE CANDIDATE if exit then drop 2drop
   CWD$ INCLUDE-TRUE CANDIDATE if exit then drop 2drop
   CURRENT$ INCLUDE-FALSE CANDIDATE drop ;

\ Command-line entries are relative to the invocation directory; dependencies
\ beneath them inherit the entry directory as their primary root.
: ENTRY-RESOLVE ( ptr u8 n -- ptr u8 n bool )
   REQUEST!
   CWD$ INCLUDE-TRUE CANDIDATE
   {: path:ptr pathu:n known:bool exists:bool :}
   exists if
      path pathu DIRNAME OWNER!
   else
      \ A missing command-line path may sit below a dangling directory
      \ symlink. Its canonical parent is not a usable WITH root; keep the
      \ invocation root so INCLUDE-OPEN can report the path it could not open.
      CWD$ OWNER!
   then
   path pathu known ;

\ Every boot row is portable now: each prefix that records one opens the
\ registry first, with REQUIRE-BOOT-OPEN as a source token (src/habu/habu2.f
\ EMIT-REQUIRE-BOOT-OPEN-TOKEN, bootstrap/cg/forth.fs's mirror of it) or
\ literally (src/habu/native-runtime.f). So one question answers for every
\ engine kind: BOOT-CANDIDATE, which asks the CWD-relative spelling the rows
\ are stored in. A canonical scan alongside it would answer for no engine and
\ hide a prefix that had stopped opening the registry.
: ENGINE-KNOWN? ( ptr u8 n -- bool )
   REQUEST!
   REQUEST$ CANONICAL drop
   0 BOOT-CANDIDATE nip nip ;

;package

\ One scratch line for diagnostics that have to name a path. Sized so the
\ longest accepted path plus the longest prefix below always fits, and the
\ append refuses rather than truncates, so a message is whole or absent.
$20 constant INCLUDE-DIAG-PREFIX-CAP
INCLUDE-PATH-CAP INCLUDE-DIAG-PREFIX-CAP + constant INCLUDE-DIAG-CAP
create INCLUDE-DIAG INCLUDE-DIAG-CAP allot
create INCLUDE-LF 1 allot
variable INCLUDE-DIAG-U

$0A INCLUDE-LF 0 ZBYTE!

: INCLUDE-DIAG-RESET ( -- )
   0 INCLUDE-DIAG-U ! ;

: INCLUDE-DIAG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   INCLUDE-DIAG-U @ u + INCLUDE-DIAG-CAP > if exit then
   a INCLUDE-DIAG INCLUDE-DIAG-U @ + u BYTE-COPY
   INCLUDE-DIAG-U @ u + INCLUDE-DIAG-U ! ;

: INCLUDE-DIAG$ ( -- ptr u8 n )
   INCLUDE-DIAG INCLUDE-DIAG-U @ ;

: INCLUDE-PATH-COPY ( -- )
   0 INCLUDE-PATH-I !
   begin INCLUDE-PATH-I @ INCLUDE-PATH-U @ < while
      INCLUDE-PATH-A@ INCLUDE-PATH-I @ ZBYTE@ INCLUDE-PATH INCLUDE-PATH-I @ ZBYTE!
      INCLUDE-PATH-I @ 1 + INCLUDE-PATH-I !
   repeat ;

: INCLUDE-PATH0 ( ptr u8 n -- ptr u8 )
   INCLUDE-CHECK-PATH INCLUDE-PATH-U ! INCLUDE-PATH-A!
   INCLUDE-PATH-COPY
   0 INCLUDE-PATH INCLUDE-PATH-U @ ZBYTE!
   INCLUDE-PATH ;

\ Each active load owns its source bytes until evaluation returns. A linked
\ mapping keeps parent buffers stable without imposing a nesting limit.
variable SCRIPT-NAMED-PEND

: SCRIPT-NAMED-PEND! ( bool -- )
   SCRIPT-NAMED-PEND ! ;


\ A failed open is almost always a typo or a moved file, and the one thing the
\ reader needs is WHICH path. The message used to drop it even though
\ INCLUDE-PATH holds the resolved path at exactly that moment, so a bad
\ `require` said only "include: open failed". Now that `bin/hb --load` routes
\ its argv files through `required` too (dot habu-make-load-consult-85c88fb3),
\ this is the diagnostic a mistyped command line gets, and the raw argv reader
\ it replaced always named the path.
: INCLUDE-OPEN-DIE ( -- )
   INCLUDE-DIAG-RESET
   s" include: cannot open " INCLUDE-DIAG+
   INCLUDE-PATH INCLUDE-PATH-U @ INCLUDE-DIAG+
   INCLUDE-LF 1 INCLUDE-DIAG+
   INCLUDE-DIAG$ INCLUDE-DIE ;

: INCLUDE-OPEN ( ptr u8 n -- )
   INCLUDE-PATH0 open-rd INCLUDE-FD !
   INCLUDE-FD @ 0 < if INCLUDE-OPEN-DIE then ;

: INCLUDE-PROBE-OVERFLOW ( -- bool )
   INCLUDE-FD @ INCLUDE-PROBE INCLUDE-PROBE-CAP read INCLUDE-RD !
   INCLUDE-RD @ 0 < if s" include: read failed" INCLUDE-IO-DIE then
   INCLUDE-RD @ 0 > if s" include: file too large" INCLUDE-IO-DIE then
   INCLUDE-TRUE ;

: INCLUDE-EVALERR? ( -- bool )
   data-base INCLUDE-EVALERR-CELL + @ 0 = 0= ;

TRUSTED: INCLUDE-EVALUATE ( ptr u8 n -- )
   evaluate ;

\ ---- Ordered source-composition event log (TFAM 5, item 5) --------------
\ The loader words append one event per source-composition act so a restricted
\ discovery pass can reconstruct include multiplicity and require/provided
\ canonical registry state in order. Recording is gated by EVENT-ON? so a
\ normal boot/gate records nothing (no overhead, no overflow). During discovery
\ the walker sets DISCOVERY and supplies the loader-token byte span in
\ DISC-TOK-A/DISC-TOK-U; a real load reads the live token span from the
\ interpreter TKA/TKL cells instead.

$100 constant EVENT-MAX
8 constant EVENT-FIELDS
$8000 constant EVENT-POOL-CAP
$4D constant INCLUDE-EVENT-RC

0 constant EV-INCLUDED
1 constant EV-REQUIRED
2 constant EV-PROVIDED
0 constant EV-STATE-FRESH
1 constant EV-STATE-KNOWN

create EVENT-RECS EVENT-MAX EVENT-FIELDS * cells allot
create EVENT-POOL EVENT-POOL-CAP allot
variable EVENT-N
variable EVENT-POOL-N
variable EVENT-ON-V
variable EVENT-DISC-V
variable DISC-TOK-A
variable DISC-TOK-U

: EVENT-ON? ( -- bool )   EVENT-ON-V @ 0= 0= ;
: DISCOVERY? ( -- bool )  EVENT-DISC-V @ 0= 0= ;
: EVENT-ON ( -- )         1 EVENT-ON-V ! ;
: EVENT-OFF ( -- )        0 EVENT-ON-V ! ;
: DISCOVERY-ON ( -- )     1 EVENT-DISC-V ! ;
: DISCOVERY-OFF ( -- )    0 EVENT-DISC-V ! ;
: DISC-TOK! ( n n -- )    DISC-TOK-U ! DISC-TOK-A ! ;
: EVENTS-RESET ( -- )     0 EVENT-N !  0 EVENT-POOL-N ! ;

: LOADER-TOK-A ( -- n )   data-base TKA-CELL + @ ;
: LOADER-TOK-U ( -- n )   data-base TKL-CELL + @ ;
: LOADER-TOKEN-SPAN ( -- n n )  LOADER-TOK-A LOADER-TOK-U ;

: EVENT-SPAN ( -- n n )
   DISCOVERY? if DISC-TOK-A @ DISC-TOK-U @ exit then
   LOADER-TOKEN-SPAN ;

: EVENT-SLOT ( n n -- ptr n )
   swap EVENT-FIELDS * + cells EVENT-RECS + ;

: EVENT-FIELD@ ( n n -- n )  EVENT-SLOT @ ;
: EVENT-FIELD! ( n n n -- )  EVENT-SLOT ! ;

: EVENT-POOL-AT ( n -- ptr u8 )  EVENT-POOL + ;

: EVENT-RECS-ROOM ( -- )
   EVENT-N @ EVENT-MAX >= if s" events: too many events" INCLUDE-EVENT-RC die then ;

: EVENT-POOL-ROOM ( n -- )
   EVENT-POOL-N @ + EVENT-POOL-CAP > if s" events: pool overflow" INCLUDE-EVENT-RC die then ;

: EVENT-COPY-PATH ( ptr u8 n -- n n ) {: a:ptr u:n :}
   u EVENT-POOL-ROOM
   EVENT-POOL-N @ {: off:n :}
   a off EVENT-POOL-AT u BYTE-COPY
   off u + EVENT-POOL-N !
   off u ;

package SOURCE-EVENT
public

: ROOT@ ( n -- ptr u8 n ) {: ix:n :}
   ix 6 EVENT-FIELD@ EVENT-POOL-AT ix 7 EVENT-FIELD@ ;

private

: COPY-ROOT ( -- n n )
   RESOLVED-ROOT$ {: a:ptr u:n :}
   0 begin dup EVENT-N @ < while
      dup ROOT@ a u CORE-STR= if
         dup 6 EVENT-FIELD@ swap 7 EVENT-FIELD@ exit
      then
      1+
   repeat drop
   a u EVENT-COPY-PATH ;

public

: STORE-ROOT ( n -- ) {: ix:n :}
   COPY-ROOT {: off:n len:n :}
   off ix 6 EVENT-FIELD!
   len ix 7 EVENT-FIELD! ;

;package

: EVENT-RECORD ( ptr u8 n n n -- ) {: kd:n st:n :}
   EVENT-ON? 0= if 2drop exit then
   EVENT-RECS-ROOM
   EVENT-N @ {: ix:n :}
   EVENT-SPAN {: toka:n toku:n :}
   EVENT-COPY-PATH {: off:n len:n :}
   kd    ix 0 EVENT-FIELD!
   off   ix 1 EVENT-FIELD!
   len   ix 2 EVENT-FIELD!
   toka  ix 3 EVENT-FIELD!
   toku  ix 4 EVENT-FIELD!
   st    ix 5 EVENT-FIELD!
   ix SOURCE-EVENT:STORE-ROOT
   ix 1 + EVENT-N ! ;

: REQUIRE-STATE ( bool -- n )
   if EV-STATE-KNOWN exit then EV-STATE-FRESH ;

: EVENT-COUNT ( -- n )       EVENT-N @ ;
: EVENT-KIND@ ( n -- n )     0 EVENT-FIELD@ ;
: EVENT-STATE@ ( n -- n )    5 EVENT-FIELD@ ;
: EVENT-PATH@ ( n -- ptr u8 n ) {: ix:n :}
   ix 1 EVENT-FIELD@ EVENT-POOL-AT
   ix 2 EVENT-FIELD@ ;
: EVENT-TOK@ ( n -- n n ) {: ix:n :}
   ix 3 EVENT-FIELD@ ix 4 EVENT-FIELD@ ;

package SOURCE-ROOT
private

\ A frame carries its own path as well as its own bytes. INCLUDE-PATH cannot
\ answer "which file is open": a nested include overwrites it before the inner
\ file is even read, so the outer path is gone by the time an inner refusal or a
\ POP needs it. The frame is the only per-load storage there is, so the resolved
\ path is copied into it and the header grows by one length cell plus the path.
0 constant FR-PREV                          \ parent frame (a declared pointer field)
CELL constant FR-NAMED                       \ SCRIPT-NAMED-PEND byte
2 cells constant FR-PATHLEN                  \ this load's path length
3 cells constant FR-PATH                     \ this load's path bytes
FR-PATH INCLUDE-PATH-CAP + 1 cells + constant HEADER-BYTES   \ cell-rounded, so SOURCE stays aligned
HEADER-BYTES INCLUDE-BUF-CAP + constant MAP-BYTES
PTR-VARIABLE TOP

: TOP@ ( -- ptr u8 ) TOP @ ;
: TOP! ( ptr u8 -- ) TOP ! ;

: CHECK-ACTIVE ( -- )
   INCLUDE-DEPTH @ 0 <= if s" include: depth underflow" INCLUDE-DIE then ;

\ The engine's refusal tail prints ` at <path>:<line>` from these two cells and
\ nothing else decides when they are right: they are republished on every PUSH
\ and every POP, so they name the file the interpreter is inside and read 0 when
\ it is inside none (tty REPL, `-e`, the boot prefix).
: PUBLISH-LOCATION ( -- )
   INCLUDE-DEPTH @ 0 <= if
      0 data-base INCLUDE-SRCLOC-PATH-CELL + !
      0 data-base INCLUDE-SRCLOC-PATHLEN-CELL + !
      exit
   then
   TOP@ {: frame:ptr :}
   frame FR-PATH + NULL-PTR - data-base INCLUDE-SRCLOC-PATH-CELL + !
   frame FR-PATHLEN + CELL-VIEW @ data-base INCLUDE-SRCLOC-PATHLEN-CELL + ! ;

: PUSH ( -- )
   MAP-BYTES map-anon 0= 0= if drop INCLUDE-IO-RC throw then {: frame:ptr :}
   TOP@ frame FR-PREV ptr-field !
   SCRIPT-NAMED-PEND @ frame FR-NAMED + c!
   INCLUDE-PATH-U @ frame FR-PATHLEN + CELL-VIEW !
   INCLUDE-PATH frame FR-PATH + INCLUDE-PATH-U @ BYTE-COPY
   frame TOP!
   INCLUDE-FALSE SCRIPT-NAMED-PEND!
   1 INCLUDE-DEPTH +!
   PUBLISH-LOCATION ;

: POP ( -- n )
   CHECK-ACTIVE
   TOP@ {: frame:ptr :}
   frame FR-PREV ptr-field @ TOP!
   -1 INCLUDE-DEPTH +!
   PUBLISH-LOCATION
   frame MAP-BYTES munmap ;

: SOURCE ( -- ptr u8 ) CHECK-ACTIVE TOP@ HEADER-BYTES + ;

: INCLUDE-READ-DONE? ( -- bool )
   INCLUDE-U @ INCLUDE-BUF-CAP >= if INCLUDE-PROBE-OVERFLOW exit then
   INCLUDE-FD @ SOURCE INCLUDE-U @ + INCLUDE-BUF-CAP INCLUDE-U @ - read INCLUDE-RD !
   INCLUDE-RD @ 0 < if s" include: read failed" INCLUDE-IO-DIE then
   INCLUDE-RD @ 0 = if INCLUDE-TRUE exit then
   INCLUDE-U @ INCLUDE-RD @ + INCLUDE-U !
   INCLUDE-FALSE ;

: INCLUDE-READ-ALL ( ptr u8 n -- ptr u8 n )
   INCLUDE-OPEN
   0 INCLUDE-U !
   begin INCLUDE-READ-DONE? 0= while repeat
   INCLUDE-CLOSE
   SOURCE INCLUDE-U @ ;

: LOAD-CURRENT ( -- )
   PUSH
   [: INCLUDE-PATH INCLUDE-PATH-U @ INCLUDE-READ-ALL INCLUDE-EVALUATE ;] catch {: rc:n :}
   INCLUDE-CLOSE
   POP {: release:n :}
   rc 0= 0= if rc throw then
   release 0 < if INCLUDE-IO-RC throw then
   INCLUDE-EVALERR? if s" include: evaluation failed" INCLUDE-EVAL-DIE then ;

public

: NAMED? ( -- bool )
   INCLUDE-DEPTH @ 0= if INCLUDE-FALSE exit then
   TOP@ FR-NAMED + c@ 0= 0= ;

: LOAD ( ptr u8 n -- )
   INCLUDE-PATH0 drop
   RESOLVED-ROOT$ [: LOAD-CURRENT ;] WITH ;

;package

: INCLUDE-LOAD ( ptr u8 n -- ) LOAD ;

: included ( ptr u8 n -- )
   RESOLVE drop
   2dup EV-INCLUDED EV-STATE-FRESH EVENT-RECORD
   DISCOVERY? if 2drop exit then
   INCLUDE-LOAD ;

\ One body for both spellings. A path the registry already holds is skipped, so
\ the pending flag has to be cleared on every exit or it would leak into the
\ next unrelated load.
: REQUIRE-BODY ( ptr u8 n bool -- ) {: known:bool :}
   2dup EV-REQUIRED known REQUIRE-STATE EVENT-RECORD
   known if 2drop INCLUDE-FALSE SCRIPT-NAMED-PEND! exit then
   2dup REQUIRE-STORE
   DISCOVERY? if 2drop INCLUDE-FALSE SCRIPT-NAMED-PEND! exit then
   INCLUDE-LOAD ;

: required ( ptr u8 n -- )
   INCLUDE-FALSE SCRIPT-NAMED-PEND!
   RESOLVE REQUIRE-BODY ;

\ The `--load` argv row. Same load, and it records that the command line is
\ what asked for it.
: script-required ( ptr u8 n -- )
   INCLUDE-TRUE SCRIPT-NAMED-PEND!
   ENTRY-RESOLVE REQUIRE-BODY ;

\ Is the file being loaded right now one the command line named?
: SCRIPT-NAMED-LOAD? ( -- bool )
   SOURCE-ROOT:NAMED? ;

: provided ( ptr u8 n -- )
   RESOLVE {: known:bool :}
   2dup EV-PROVIDED known REQUIRE-STATE EVENT-RECORD
   known if 2drop exit then
   REQUIRE-STORE ;

: include ( -- )
   parse-name INCLUDE-CHECK-PATH included ;
immediate

: require ( -- )
   parse-name INCLUDE-CHECK-PATH required ;
immediate

\ ---- Fresh discovery registry (TFAM 5, item 5) --------------------------
\ A restricted discovery pass must see a fresh require/provided registry so a
\ tool's own preloaded paths cannot dedup-hide a later user require/provided.
\ Raising REQUIRE-BASE to the current count makes REQUIRE-KNOWN? ignore the
\ tool's entries while discovery records into slots above the base; RESTORE
\ drops the discovery entries and reinstates the tool's registry unchanged, so
\ warm-snapshot serialization of the registry stays intact.

: REQUIRE-SNAPSHOT ( -- )
   REQUIRE-N @ REQUIRE-SAVE-N !
   REQUIRE-BASE @ REQUIRE-SAVE-BASE !
   REQUIRE-N @ REQUIRE-BASE ! ;

: REQUIRE-RESTORE ( -- )
   REQUIRE-SAVE-BASE @ REQUIRE-BASE !
   REQUIRE-SAVE-N @ REQUIRE-REG:REWIND ;

\ ---- the registry's own truncation seam -------------------------------------
\ THE ONE WAY TO MAKE THIS REGISTRY SMALLER, and it exists because storing the
\ count is not a truncation. Four other cells are cursors INTO the rows, and a
\ bare `REQUIRE-N !` leaves every one of them naming rows that are gone:
\ REQUIRE-BOOT-N would go on answering ENGINE-PROVIDES? for a file the caller
\ just dropped, REQUIRE-BASE would hide surviving rows from REQUIRE-KNOWN?
\ (dedup silently off), and the two SAVE cells would restore a discovery pass
\ back to the vanished rows. So each is brought down with the count, and none of
\ them can end up above it. REQUIRE-BOOT-V is deliberately absent from that list:
\ it says whether the boot registry is still open, not where a row is, and while
\ it is set REQUIRE-BOOT-LIMIT reads REQUIRE-N, which this word already moves.
\
\ ITS CALLER IS THE BUILD'S CORE-PREFIX REWIND (src/habu/prefix-rewind.f), which
\ returns a compiling host to the end of its own boot prefix: the rows the boot
\ recorded after that point describe files whose definitions the rewind removes,
\ and a snapshot taken afterwards would otherwise persist an image that claims
\ to provide what it no longer carries (measured: the stdlib).
\
\ The two words are a package block because the rest of this file is legacy
\ global surface and the ownership gate refuses a new global here (measured:
\ "`REQUIRE-TRUNCATE` defines a changed module word outside a package"). The
\ tails are NOT spelled like the cells they move - a tail that folded onto
\ `REQUIRE-N` inside this block would be that cell.
package REQUIRE-REG

public

: COUNT ( -- n )
   REQUIRE-N @ ;

: TRUNCATE ( n -- ) {: keep:n :}
   keep 0 < keep REQUIRE-N @ > or if
      s" require: truncate outside the registry" INCLUDE-DIE
   then
   keep REWIND
   REQUIRE-BOOT-N @ keep > if keep REQUIRE-BOOT-N ! then
   REQUIRE-BASE @ keep > if keep REQUIRE-BASE ! then
   REQUIRE-SAVE-N @ keep > if keep REQUIRE-SAVE-N ! then
   REQUIRE-SAVE-BASE @ keep > if keep REQUIRE-SAVE-BASE ! then ;

;package

\ The loader scratch a fresh pass may reset at any time: the open file, the read
\ counters, the path buffer, the discovery base and the event log. None of it
\ describes a load in flight, so resetting it underneath one is safe.
\
\ Resetting the path buffer is its BYTES and its copy cursor, not just its
\ length. Either leftover describes the last pathname resolved, and a capture
\ taken afterwards bakes it into the engine: the bytes as the absolute path of
\ the directory the build ran in, and the cursor as that path's length - one
\ byte, and the only one left between two builds of one revision from two
\ directories (dot habu-bake-prefix-src-1047b604).
: INCLUDE-RESET-SCRATCH ( -- )
   INCLUDE-CLOSE
   0 INCLUDE-U !
   0 INCLUDE-RD !
   INCLUDE-PATH-CAP 1 + 0 ?do 0 INCLUDE-PATH i ZBYTE! loop
   0 INCLUDE-PATH-I !
   NULL$ INCLUDE-PATH-U ! INCLUDE-PATH-A!
   0 REQUIRE-BASE !
   EVENT-OFF
   DISCOVERY-OFF
   EVENTS-RESET ;

\ Every source mapping is released when its load returns. Snapshot preparation
\ requires no load in flight and clears only process-local resolver scratch.
: INCLUDE-SNAPSHOT-PREPARE ( -- )
   INCLUDE-DEPTH @ 0= 0= if
      s" include: snapshot prepare under an open load" INCLUDE-DIE
   then
   INCLUDE-RESET-SCRATCH
   RESET
   REQUIRE-REG:PERSIST ;

\ ---- what the ENGINE provides, as opposed to what this process has loaded ---
\
\ The boot prefix marks its own files `provided` before any user token runs, so
\ the registry opens with exactly the engine's surface in it. Freezing the count
\ at the end of the prefix is what lets a later question separate "the engine
\ carries this" from "something in this process required it", which a plain
\ REQUIRE-KNOWN? cannot: by the time a tool asks, its own dependencies are in
\ the registry too. tools/bundle-lib-core.f needs exactly this separation - it
\ must not bundle a copy of a file the engine already loaded, and must not skip
\ one the engine does not have.
: REQUIRE-BOOT-FREEZE ( -- )
   REQUIRE-N @ REQUIRE-BOOT-N !
   INCLUDE-FALSE REQUIRE-BOOT-V ! ;

: ENGINE-PROVIDES? ( ptr u8 n -- bool )
   ENGINE-KNOWN? ;

\ A bundle (tools/bundle-lib.f) carries the modules this engine does NOT have
\ and states the ones it assumes. Stating the assumption is the bundle's half;
\ checking it is the engine's, so a bundle built against a richer engine and run
\ on a barer one refuses here, by name, instead of dying later on a missing word
\ or - worse - loading a second copy of a module the engine already carries.
: ?ENGINE-PROVIDES ( ptr u8 n -- ) {: a:ptr u:n :}
   a u ENGINE-PROVIDES? if exit then
   INCLUDE-DIAG-RESET
   s" bundle: this engine does not provide " INCLUDE-DIAG+
   a u INCLUDE-DIAG+
   INCLUDE-LF 1 INCLUDE-DIAG+
   INCLUDE-DIAG$ INCLUDE-DIE ;

\ constructor generation (sumtype.f, loaded earlier in the boot prefix) crosses
\ evaluate only through this audited INCLUDE-EVALUATE boundary; engines without
\ include.f (stage builders) leave TYPE-DECL's armed flag 0 and generation stays
\ fail-closed. Bind the defer (wrapped so the quotation compiles), then arm.
\ The binder is a package private rather than an 85th global in this file: it is
\ a one-shot installer with no caller, and the `is` target has to be written
\ qualified because `is` is a parsing word and parsing words resolve outside
\ using-imports (measured: bare `is TDECL-EVAL-XT` under `using TYPE-DECL`
\ answers `hb: is: no deferred word named TDECL-EVAL-XT`, rc 70).
using TYPE-DECL

package INCLUDE-EVAL-BIND
private
: INSTALL ( -- ) [: INCLUDE-EVALUATE ;] is TYPE-DECL:TDECL-EVAL-XT ;
INSTALL
;package

-1 TDECL-EVAL-ARMED !
;using

;using
