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
INCLUDE-PATH-CAP 1 + constant REQUIRE-SLOT-BYTES

create INCLUDE-PATH INCLUDE-PATH-CAP 1 + allot
create INCLUDE-PROBE INCLUDE-PROBE-CAP allot
create REQUIRE-PATHS REQUIRE-MAX REQUIRE-SLOT-BYTES * allot
create REQUIRE-LENS REQUIRE-MAX cells allot

variable INCLUDE-DEPTH
variable INCLUDE-FD
variable INCLUDE-U
variable INCLUDE-RD
variable INCLUDE-PATH-A
variable INCLUDE-PATH-U
variable INCLUDE-PATH-I
variable REQUIRE-N
variable REQUIRE-BOOT-N
variable REQUIRE-BASE
variable REQUIRE-SAVE-N
variable REQUIRE-SAVE-BASE

-1 INCLUDE-FD !

: INCLUDE-FALSE ( -- bool )
   0 0= 0= ;

: INCLUDE-TRUE ( -- bool )
   0 0= ;

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
variable CURRENT-A
variable CURRENT-U
variable SCOPES


: CURRENT-PTR ( -- ptr u8 ) CURRENT-A 0 ptr-field @ ;


: CURRENT! ( ptr u8 n -- )
   CURRENT-U ! CURRENT-A 0 ptr-field ! ;


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

private

create ALIAS-PATHS REQUIRE-MAX REQUIRE-SLOT-BYTES * allot
create ALIAS-LENS REQUIRE-MAX cells allot

: ALIAS-SLOT ( n -- ptr u8 ) REQUIRE-SLOT-BYTES * ALIAS-PATHS + ;

: ALIAS= ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   a u idx ALIAS-SLOT idx cells ALIAS-LENS + @ CORE-STR= ;

public

: REMEMBER ( ptr u8 n n -- ) {: idx:n :}
   RESOLVED-ROOT$ RELATIVE {: a:ptr u:n :}
   a idx ALIAS-SLOT u BYTE-COPY
   u idx cells ALIAS-LENS + ! ;

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

: INCLUDE-PATH-A-FIELD ( -- ptr ptr u8 )
   INCLUDE-PATH-A 0 ptr-field ;

: INCLUDE-PATH-A@ ( -- ptr u8 )
   INCLUDE-PATH-A-FIELD @ ;

: INCLUDE-PATH-A! ( ptr u8 -- )
   INCLUDE-PATH-A-FIELD ! ;

: INCLUDE-CHECK-PATH ( ptr u8 n -- ptr u8 n )
   dup 0 <= if s" include: missing path" INCLUDE-DIE then
   dup INCLUDE-PATH-CAP > if s" include: path too long" INCLUDE-DIE then ;

: REQUIRE-SLOT ( n -- ptr u8 )
   REQUIRE-SLOT-BYTES * REQUIRE-PATHS + ;

: REQUIRE-LEN@ ( n -- n )
   cells REQUIRE-LENS + @ ;

: REQUIRE-LEN! ( n n -- ) {: u:n idx:n :}
   u REQUIRE-LENS idx cells + ! ;

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

: REQUIRE-STORE ( ptr u8 n -- ) {: a:ptr u:n :}
   REQUIRE-CHECK-ROOM
   REQUIRE-N @ {: idx:n :}
   a idx REQUIRE-SLOT u BYTE-COPY
   u idx REQUIRE-LEN!
   a u idx REMEMBER
   idx 1 + REQUIRE-N ! ;

package SOURCE-ROOT
private


: REQUEST! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECK
   a REQUEST-BUF u BYTE-COPY u REQUEST-U ! ;

: REQUEST$ ( -- ptr u8 n ) REQUEST-BUF REQUEST-U @ ;

\ Portable names describe only frozen engine facts. Ordinary application facts
\ retain their canonical identity so identical names in distinct roots coexist.
: BOOT-KNOWN? ( ptr u8 n n -- bool ) {: a:ptr u:n first:n :}
   first begin dup REQUIRE-BOOT-N @ < while
      dup a u rot ALIAS= if drop INCLUDE-TRUE exit then
      1+
   repeat drop INCLUDE-FALSE ;

: CANDIDATE$ ( -- ptr u8 n ) CANDIDATE-BUF CANDIDATE-U @ ;

\ A directory symlink may carry an invocation-root engine spelling outside
\ CWD. Check only that spelling's alias, then require its normalized path to
\ resolve to the same physical candidate: symlink/.. can name another file.
: BOOT-CANDIDATE ( ptr u8 n n -- ptr u8 n bool ) {: first:n :}
   first REQUIRE-BOOT-N @ >= if INCLUDE-FALSE exit then
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
   CWD$ INCLUDE-TRUE CANDIDATE drop
   {: known:bool :}
   2dup DIRNAME OWNER!
   known ;

: ENGINE-KNOWN? ( ptr u8 n -- bool )
   REQUEST!
   REQUEST$ CANONICAL drop {: a:ptr u:n :}
   0 begin dup REQUIRE-BOOT-N @ < while
      dup a u rot REQUIRE-PATH= if drop INCLUDE-TRUE exit then
      1+
   repeat drop
   a u 0 BOOT-CANDIDATE nip nip ;

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

2 cells constant HEADER-BYTES
HEADER-BYTES INCLUDE-BUF-CAP + constant MAP-BYTES
variable TOP

: TOP@ ( -- ptr u8 ) TOP 0 ptr-field @ ;
: TOP! ( ptr u8 -- ) TOP 0 ptr-field ! ;

: CHECK-ACTIVE ( -- )
   INCLUDE-DEPTH @ 0 <= if s" include: depth underflow" INCLUDE-DIE then ;


: PUSH ( -- )
   MAP-BYTES map-anon 0= 0= if drop INCLUDE-IO-RC throw then {: frame:ptr :}
   TOP@ frame 0 ptr-field !
   SCRIPT-NAMED-PEND @ frame CELL + c!
   frame TOP!
   INCLUDE-FALSE SCRIPT-NAMED-PEND!
   1 INCLUDE-DEPTH +! ;

: POP ( -- n )
   CHECK-ACTIVE
   TOP@ {: frame:ptr :}
   frame 0 ptr-field @ TOP!
   -1 INCLUDE-DEPTH +!
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
   TOP@ CELL + c@ 0= 0= ;

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
   REQUIRE-SAVE-N @ REQUIRE-N ! ;

\ ---- the registry's own truncation seam -------------------------------------
\ THE ONE WAY TO MAKE THIS REGISTRY SMALLER, and it exists because storing the
\ count is not a truncation. Four other cells are cursors INTO the rows, and a
\ bare `REQUIRE-N !` leaves every one of them naming rows that are gone:
\ REQUIRE-BOOT-N would go on answering ENGINE-PROVIDES? for a file the caller
\ just dropped, REQUIRE-BASE would hide surviving rows from REQUIRE-KNOWN?
\ (dedup silently off), and the two SAVE cells would restore a discovery pass
\ back to the vanished rows. So each is brought down with the count, and none of
\ them can end up above it.
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
   keep REQUIRE-N !
   REQUIRE-BOOT-N @ keep > if keep REQUIRE-BOOT-N ! then
   REQUIRE-BASE @ keep > if keep REQUIRE-BASE ! then
   REQUIRE-SAVE-N @ keep > if keep REQUIRE-SAVE-N ! then
   REQUIRE-SAVE-BASE @ keep > if keep REQUIRE-SAVE-BASE ! then ;

;package

\ The loader scratch a fresh pass may reset at any time: the open file, the read
\ counters, the path buffer, the discovery base and the event log. None of it
\ describes a load in flight, so resetting it underneath one is safe.
: INCLUDE-RESET-SCRATCH ( -- )
   INCLUDE-CLOSE
   0 INCLUDE-U !
   0 INCLUDE-RD !
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
   RESET ;

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
   REQUIRE-N @ REQUIRE-BOOT-N ! ;

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
