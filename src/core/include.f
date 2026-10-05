\ include.f - checked source include words.
\
\ `include` is source composition. Package reopening owns shared namespace;
\ this file only gives source files a checked way to load dependencies.

PATH-CAP constant INCLUDE-PATH-CAP
$200 constant REQUIRE-MAX  \ composed maki+stdlib require closure crossed 256 (2026-07-20)
\ A loader refusal's exit status, always after a line naming the refusal: an
\ uncaught throw of a code below 256 ends the process with no word at all.
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

\ An invocation that owns source bytes supplies both filesystem answers through
\ this loader-local pair. A newly source-loaded target gets its own pair, so its
\ require registry and source frames remain independent of the retained host.
package SOURCE-INPUT
private

defer CANON-XT ( ptr u8 n -- ptr u8 n bool )
defer READ-XT ( ptr u8 n ptr u8 n -- ptr u8 n )

public

: CANON ( ptr u8 n -- ptr u8 n bool ) CANON-XT ;
: READ ( ptr u8 n ptr u8 n -- ptr u8 n ) READ-XT ;

: USE ( [ ptr u8 n -- ptr u8 n bool ] [ ptr u8 n ptr u8 n -- ptr u8 n ] -- )
   {: canon read :}
   canon is CANON-XT
   read is READ-XT ;

;package

\ A source load is also the publication boundary for an explicitly selected
\ package unit. The continuation keeps the ordinary frame and scope behavior;
\ callers may bind a unit importer after the complete loader has been loaded.
package SOURCE-UNIT
private

defer LOAD-XT ( ptr u8 n ptr u8 n ptr u8 [ -- ] -- )

: ORDINARY ( ptr u8 n ptr u8 n ptr u8 [ -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr q :}
   q execute ;

public

: LOAD ( ptr u8 n ptr u8 n ptr u8 [ -- ] -- ) LOAD-XT ;
: USE ( [ ptr u8 n ptr u8 n ptr u8 [ -- ] -- ] -- ) is LOAD-XT ;
: RESET ( -- ) [: ORDINARY ;] is LOAD-XT ;

;package

SOURCE-UNIT:RESET

\ Canonical source paths and a dynamically scoped owner root. This bootstrap
\ layer uses only core bytes, mappings and the bounded realpath OS primitive.
package SOURCE-ROOT
private

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
create OWNER-SAVE PATH-BYTES allot
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
create ENGINE-BUF PATH-BYTES allot
variable ENGINE-U
variable ENGINE-READ
\ The stack PUSHPATH fills: a mapped copy of each saved current path and its
\ length, 0 for the working directory.
8 constant PATH-DEPTH
PATH-DEPTH PTR-U8-TABLE SLOT-A
create SLOT-U PATH-DEPTH cells allot
variable SLOT-N

: CURRENT-PTR ( -- ptr u8 ) CURRENT-A @ ;


: CURRENT! ( ptr u8 n -- )
   CURRENT-U ! CURRENT-A ! ;

\ The current path as set, null for the working directory, which a scope saves
\ and restores: CURRENT$ would hand back CWD-BUF, which no top-level path may
\ hold (RESET clears it, and CD and POPPATH release the path they replace).
: CURRENT@ ( -- ptr u8 n )
   CURRENT-PTR CURRENT-U @ ;


: CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= u WORK-BYTES >= or if E-PATH-RANGE throw then
   u 0 ?do a i + c@ 0= if E-PATH-RANGE throw then loop ;


: COPY-Z ( ptr u8 n ptr u8 -- ) {: a:ptr u:n dst:ptr :}
   a u CHECK
   a dst u BYTE-COPY 0 dst u + c! ;


\ realpath of a path into CANON-BUF: its length, -2 when that does not fit,
\ below 0 when the path does not resolve.
: CANON-N ( ptr u8 n -- n )
   ZBUF COPY-Z
   ZBUF CANON-BUF PATH-BYTES realpath ;

public

: CANON-OS ( ptr u8 n -- ptr u8 n bool )
   CANON-N {: n:n :}
   n -2 = if E-PATH-RANGE throw then
   n 0 < if CANON-BUF 0 INCLUDE-FALSE exit then
   CANON-BUF n INCLUDE-TRUE ;

private

: TRY-CANON ( ptr u8 n -- bool )
   SOURCE-INPUT:CANON {: a:ptr u:n found:bool :}
   u PATH-BYTES >= if E-PATH-RANGE throw then
   a CANON-BUF <> if a CANON-BUF u BYTE-COPY then
   u CANON-U !
   found ;


: CANON$ ( -- ptr u8 n ) CANON-BUF CANON-U @ ;


\ The working directory is the root of every command-line entry, a root of every
\ relative require and the engine root's first candidate, so it must fit
\ PATH-CAP as every root does; every path below a longer one is longer still,
\ so the engine refuses it by name rather than resolve without it. realpath
\ does not say why it fails, so the refusal names what a removed, unsearchable
\ or long directory all are not; macOS's refuses a result of 1023 bytes or
\ more, and glibc answers a longer path whole, which the canon answer refuses
\ with E-PATH-RANGE.
: CWD-DIE ( -- )
   s" source root: the working directory does not resolve to a searchable directory within PATH-CAP bytes"
   INCLUDE-IO-RC die ;


: CWD-READ ( -- )
   s" ." TRY-CANON 0= if CWD-DIE then
   CANON-BUF CWD-BUF CANON-U @ 1+ BYTE-COPY
   CANON-U @ CWD-U ! ;


: CWD-INIT ( -- )
   CWD-U @ 0= 0= if exit then
   [: CWD-READ ;] catch {: rc:n :}
   rc E-PATH-RANGE = if CWD-DIE then
   rc 0= 0= if rc throw then ;

public

: CWD$ ( -- ptr u8 n ) CWD-INIT CWD-BUF CWD-U @ ;


: CURRENT$ ( -- ptr u8 n )
   CURRENT-U @ 0 > if CURRENT-PTR CURRENT-U @ exit then
   CWD$ ;

private

\ Both callers bound u with CHECK. The root length is the caller's, so it is
\ compared with the room the name and its separator leave: in a sum, a length
\ near the maximum cell wraps back into range.
: JOIN! ( ptr u8 n ptr u8 n -- ) {: root:ptr rootu:n a:ptr u:n :}
   rootu 0 < if E-PATH-RANGE throw then
   rootu WORK-BYTES 1- u - >= if E-PATH-RANGE throw then
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
   NORMAL-U @ + limit > if E-PATH-RANGE throw then ;


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

\ A source-root refusal: its reason, then the path it names, on one line. The
\ reason is written first, so a path of any length a caller holds is named whole.
: PATH-DIE ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n path:ptr pathu:n :}
   2 a u write drop
   path pathu INCLUDE-IO-RC die ;

\ The file system root always resolves through realpath; only a supplied canon
\ (SOURCE-INPUT:USE) can answer that no directory above a path does.
: EXISTING-PARENT ( -- n )
   JOIN-U @ begin
      JOIN-BUF swap PARENT-U
      dup JOIN-BUF swap TRY-CANON if exit then
      dup 1 <= if
         s" source root: no directory above the path resolves: " JOIN-BUF JOIN-U @ PATH-DIE
      then
   again ;


: MISSING-NORMALIZE ( -- )
   EXISTING-PARENT {: cut:n :}
   CANON-U @ JOIN-U @ cut - + {: total:n :}
   total WORK-BYTES >= if E-PATH-RANGE throw then
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


\ A name with no slash lies in the current directory, so its directory is `.`;
\ PARENT-U alone would keep its first byte.
: DIRNAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u 0 SEG-END u = if s" ." exit then
   a a u PARENT-U ;


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
   u INCLUDE-PATH-CAP > if E-PATH-RANGE throw then
   a OWNER-BUF u BYTE-COPY u OWNER-U ! ;


\ Whether the n canonical bytes in CANON-BUF name a directory this process can
\ search, which is all a root needs: the files below a directory resolve without
\ read permission on it. macOS realpath answers a file's own path for
\ `<file>/.`, so the kernel's lookup of `<path>/.` decides, and it passes only
\ through a searchable directory. It opens nothing, so a FIFO cannot make it
\ wait. Without errno a file and an unsearchable directory fail alike, and the
\ refusal names what both are not.
: ENGINE-DIR? ( n -- bool ) {: n:n :}
   CANON-BUF ZBUF n BYTE-COPY
   47 ZBUF n + c!
   46 ZBUF n 1+ + c!
   0 ZBUF n 2 + + c!
   ZBUF 0 access 0= ;

\ A root is a searchable directory, which ENGINE-DIR? decides on every platform
\ and the canon of a path does not. Neither says why a path fails, so the
\ refusal names the absolute spelling tried and what a missing, unsearchable or
\ long directory and a file all are not.
: ROOT-DIE ( -- )
   s" source root: does not resolve to a searchable directory within PATH-CAP bytes: "
   JOIN-BUF JOIN-U @ PATH-DIE ;

: ROOT-CANON ( ptr u8 n -- )
   ABSOLUTE!
   JOIN-BUF JOIN-U @ TRY-CANON 0= if ROOT-DIE then
   CANON-U @ ENGINE-DIR? 0= if ROOT-DIE then ;

\ A fresh mapping holding n path bytes and the NUL after them.
: MAP-COPY ( ptr u8 n -- ptr u8 )
   {: a:ptr u:n :}
   u 1+ map-anon 0= 0= if drop s" source root: cannot map a path copy" INCLUDE-IO-RC die then
   {: fresh:ptr :}
   a fresh u 1+ BYTE-COPY
   fresh ;

: ROOT-ENTER ( ptr u8 n -- ptr u8 n ptr u8 n )
   ROOT-CANON
   CANON-U @ {: u:n :}
   CANON-BUF u MAP-COPY {: fresh:ptr :}
   CURRENT@ {: old:ptr oldu:n :}
   fresh u CURRENT!
   1 SCOPES +!
   fresh u old oldu ;

: ROOT-LEAVE ( ptr u8 n ptr u8 n n -- )
   {: fresh:ptr u:n old:ptr oldu:n rc:n :}
   -1 SCOPES +!
   old oldu CURRENT!
   fresh u 1+ munmap 0 < if
      s" source root: cannot release a root mapping" INCLUDE-IO-RC die
   then
   rc 0= 0= if rc throw then ;

public

\ Each scope owns a mapping sized to its root string. There is no additional
\ root-count/depth limit, and a throw restores the caller before releasing it.
\ A root that does not resolve to a searchable directory is refused by name.
: WITH ( ptr u8 n [ -- ] -- ) {: q :}
   ROOT-ENTER {: fresh:ptr u:n old:ptr oldu:n :}
   q catch {: rc:n :}
   fresh u old oldu rc ROOT-LEAVE ;

\ A builder's target source is discovered from the invocation root. Restore
\ its caller's source root after compilation, including on a checker refusal.
: WITH-CWD ( [ -- ] -- ) {: q :}
   CURRENT@ {: old:ptr oldu:n :}
   CWD$ CURRENT!
   1 SCOPES +!
   q catch {: rc:n :}
   -1 SCOPES +!
   old oldu CURRENT!
   rc 0= 0= if rc throw then ;

private

: CLEAR-BYTES ( ptr u8 n -- )
   0 ?do 0 over i + c! loop drop ;

\ The path words act on the top level only: each open scope, a load's among
\ them, holds the current path it replaced and restores it on exit.
: TOP-ONLY ( ptr u8 n -- )
   SCOPES @ 0= if 2drop exit then
   INCLUDE-IO-RC die ;

\ At the top level a current path is CD's mapping, or POPPATH's.
: RELEASE-CURRENT ( -- )
   CURRENT-U @ 0 > if
      CURRENT-PTR CURRENT-U @ 1+ munmap 0 < if
         s" source root: cannot release the current path" INCLUDE-IO-RC die
      then
   then
   NULL$ CURRENT! ;

: SLOT-FIELD ( n -- ptr ptr u8 )
   cells SLOT-A + 0 ptr-field ;

: SLOT-U! ( n n -- )
   cells SLOT-U + ! ;

\ The interpreter's input cursor and end (src/habu/layout.f).
INP-CELL RESERVED-PTR-U8-CELL INPUT-AT
INE-CELL RESERVED-PTR-U8-CELL INPUT-END

\ Whether the rest of the input line holds no name. The tokenizer reads a
\ newline as a blank and a piped stdin session is one buffer, so a bare CD
\ would otherwise take the next line's first word as its directory.
: LINE-BLANK? ( -- bool )
   INPUT-AT @ INPUT-END @ {: e:ptr :}
   begin dup e < while
      dup c@ 10 = if drop INCLUDE-TRUE exit then
      dup c@ 32 > if drop INCLUDE-FALSE exit then
      1 +
   repeat
   drop INCLUDE-TRUE ;

\ CD's directory: the current path, which a relative require searches first,
\ becomes it, relative to the current path when relative. It never changes the
\ process's directory or the engine root. A refusal names the absolute
\ spelling tried.
: CD-PATH ( ptr u8 n -- )
   {: a:ptr u:n :}
   u INCLUDE-PATH-CAP > if s" CD: path is too long" INCLUDE-IO-RC die then
   CURRENT$ a u JOIN {: p:ptr pu:n :}
   pu INCLUDE-PATH-CAP > if s" CD: path is too long" INCLUDE-IO-RC die then
   p pu CANON-N {: n:n :}
   n -2 = if s" CD: path is too long" INCLUDE-IO-RC die then
   n 0 < if s" CD: does not exist: " p pu PATH-DIE then
   n ENGINE-DIR? 0= if s" CD: is not a searchable directory: " p pu PATH-DIE then
   CANON-BUF n MAP-COPY {: fresh:ptr :}
   RELEASE-CURRENT
   fresh n CURRENT! ;

public

\ SwiftForth's path words, at the top level of a stdin session or of a program
\ file run as `bin/hb file.f`; inside a loaded file (`--load`, `require`) they
\ are refused, and a file scopes a root with WITH. `CD <dir>` on its own line
\ moves the current path (CD-PATH), and a bare CD prints it.
: CD ( -- )
   s" CD: only at top level" TOP-ONLY
   LINE-BLANK? if CURRENT$ type cr exit then
   parse-name CD-PATH ;

\ PUSHPATH saves the current path; a null slot is the working directory.
: PUSHPATH ( -- )
   s" PUSHPATH: only at top level" TOP-ONLY
   SLOT-N @ PATH-DEPTH >= if
      s" PUSHPATH: directory stack is full" INCLUDE-IO-RC die
   then
   SLOT-N @ {: ix:n :}
   CURRENT-U @ {: u:n :}
   u 0 > if CURRENT-PTR u MAP-COPY ix SLOT-FIELD ! then
   u ix SLOT-U!
   ix 1+ SLOT-N ! ;

\ POPPATH restores the path PUSHPATH saved last.
: POPPATH ( -- )
   s" POPPATH: only at top level" TOP-ONLY
   SLOT-N @ 0= if s" POPPATH: directory stack is empty" INCLUDE-IO-RC die then
   SLOT-N @ 1- {: ix:n :}
   RELEASE-CURRENT
   ix cells SLOT-U + @ {: u:n :}
   u 0 > if ix SLOT-FIELD @ u CURRENT! then
   NULL$ drop ix SLOT-FIELD !
   0 ix SLOT-U!
   ix SLOT-N ! ;

\ A captured image starts at the working directory, so it refuses a saved
\ path as it refuses an open scope, and releases CD's.
: RESET ( -- )
   SCOPES @ 0= 0= if
      s" source root: an image cannot be saved inside a root scope" INCLUDE-IO-RC die
   then
   SLOT-N @ 0= 0= if
      s" PUSHPATH: an image cannot be saved with a path pushed" INCLUDE-IO-RC die
   then
   RELEASE-CURRENT
   CWD-BUF PATH-BYTES CLEAR-BYTES
   CANON-BUF PATH-BYTES CLEAR-BYTES
   NORMAL-BUF WORK-BYTES CLEAR-BYTES
   CANDIDATE-BUF PATH-BYTES CLEAR-BYTES
   OWNER-BUF PATH-BYTES CLEAR-BYTES
   OWNER-SAVE PATH-BYTES CLEAR-BYTES
   JOIN-BUF WORK-BYTES CLEAR-BYTES
   WORK-BUF WORK-BYTES CLEAR-BYTES
   ZBUF WORK-BYTES CLEAR-BYTES
   REQUEST-BUF WORK-BYTES CLEAR-BYTES
   ENGINE-BUF PATH-BYTES CLEAR-BYTES
   0 REQUEST-U !
   0 CWD-U ! 0 CANON-U ! 0 NORMAL-U ! 0 CANDIDATE-U !
   0 JOIN-U ! 0 OWNER-U !
   0 ENGINE-U ! 0 ENGINE-READ ! ;

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
   POOL-CAP map-anon 0= 0= if drop s" require: cannot map the path pool" INCLUDE-DIE then
   {: fresh:ptr :}
   used 0 > if FROZEN @ fresh used BYTE-COPY then
   fresh MAPPED ! ;

: ROUND-CELL ( n -- n ) {: bytes:n :}
   bytes CELL mod {: rem:n :}
   rem 0= if bytes exit then
   bytes CELL rem - + ;

: POOL-RELEASE ( -- )
   MAPPED @ POOL-CAP munmap 0< if s" require: cannot release the path pool" INCLUDE-DIE then
   NULL-PTR MAPPED ! ;

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
\ leading and trailing padding are zero.
: PERSIST ( -- )
   MAPPED @ NULL-PTR = if
      CLAMP
      REQUIRE-N @ 0= if NULL-PTR FROZEN ! then
      exit
   then
   REQUIRE-N @ PREFIX-N @ <= if
      POOL-RELEASE
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
   POOL-RELEASE
   start pad + FROZEN !
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

package SOURCE-ROOT
private

\ The engine's own source root: the working directory when it carries the
\ engine's first row; else the executable's tree when that does (`%`, two
\ levels above the executable, <tree>/bin/hb, as SwiftForth places it); else
\ the working directory. The working directory comes first because it is
\ searched before the root and only the root's physical copy of a row is
\ frozen: with `%` first, a copied tree there would load as application source.
\ A build's fresh registry holds no row, so nothing is a tree and the rows it
\ records are relative to the working directory whatever host builds them.
\ The root is derived on the first question that needs it, from the
\ executable's path and realpath rather than through SOURCE-INPUT: it
\ is a fact of this process, not a source file a view answers. RESET forgets
\ it with the bytes, so a captured image derives the root of the executable
\ that restores it. Deriving touches only JOIN, WORK, Z and CANON scratch, so a
\ candidate held in NORMAL-BUF or CANDIDATE-BUF survives.

\ The running engine's path: on macOS the one execve received, in the
\ executable_path= apple string past envp's terminator; on Linux
\ /proc/self/exe. Both name the engine, not the script, for a `#!` program
\ file. lib/engine-id.f asks proc_pidpath through FUNCTION:, which this layer
\ cannot use.
: ENVP-END ( -- n )
   0 begin dup ENVP 0= 0= while 1+ repeat 1+ ;

\ Apple strings can be empty, so the scan ends at the null pointer.
: APPLE-EXE ( -- ptr u8 n )
   ENVP-BASE 0= if NULL$ exit then
   ENVP-END begin dup ENVP 0= 0= while
      dup ENVP s" executable_path" ENV=? if
         ENVP s" executable_path=" nip ZPTR+ dup ZLEN exit
      then
      1+
   repeat drop NULL$ ;

\ `%` in CANON-BUF, as a length: the canonical executable cut twice to its
\ parent. 0 when there is no path, it does not fit or realpath refuses it;
\ never a throw: the length is checked before CANON-N, and neither path
\ holds a NUL.
: EXE$ ( -- n )
   HB-TARGET-MACOS? if APPLE-EXE else s" /proc/self/exe" then {: a:ptr u:n :}
   u 0= u INCLUDE-PATH-CAP > or if 0 exit then
   a u CANON-N {: n:n :}
   n 0 <= if 0 exit then
   CANON-BUF CANON-BUF n PARENT-U PARENT-U ;

\ Whether a directory carries the engine's first row: one access, no read.
: TREE? ( ptr u8 n -- bool )
   0 REQUIRE-SLOT 0 REQUIRE-LEN@ JOIN ZBUF COPY-Z
   ZBUF 0 access 0= ;

\ The root's length in CANON-BUF, 0 for the working directory.
: ENGINE-FIND ( -- n )
   REQUIRE-BOOT-LIMIT 0= if 0 exit then
   CWD$ TREE? if 0 exit then
   EXE$ {: n:n :}
   n 0= if 0 exit then
   CANON-BUF n TREE? 0= if 0 exit then
   n ;

: ENGINE-INIT ( -- )
   ENGINE-READ @ 0= 0= if exit then
   ENGINE-FIND {: n:n :}
   CANON-BUF ENGINE-BUF n BYTE-COPY
   n ENGINE-U !
   1 ENGINE-READ ! ;

public

\ The engine's source root, canonical.
: ENGINE$ ( -- ptr u8 n )
   ENGINE-INIT
   ENGINE-U @ 0= if CWD$ exit then
   ENGINE-BUF ENGINE-U @ ;

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

\ A request spelled under a root, normalized but not resolved, in NORMAL-BUF.
\ The root's spelling may be longer than its canonical symlink target.
: UNDER ( ptr u8 n ptr u8 n -- ptr u8 n )
   JOIN WORK-BYTES 1- NORMALIZE-LIMIT
   NORMAL-BUF NORMAL-U @ ;

\ Whether a selected candidate is the engine root's own copy of a frozen row,
\ from row `first` on. Below the root its physical path spells the row. A
\ directory symlink inside the root can carry the root's spelling of a row
\ outside it, so the request spelled under the root is asked too, and counts
\ only when it resolves to the same physical candidate: symlink/.. can name
\ another file. Spelling the request reuses NORMAL-BUF, which holds the
\ candidate, so the candidate is copied first.
: BOOT-CANDIDATE ( ptr u8 n ptr u8 n ptr u8 n n -- ptr u8 n bool )
   {: a:ptr u:n root:ptr rootu:n req:ptr requ:n first:n :}
   first REQUIRE-BOOT-LIMIT >= if a u INCLUDE-FALSE exit then
   a u root rootu RELATIVE first BOOT-KNOWN? if a u INCLUDE-TRUE exit then
   u CANDIDATE-U ! a CANDIDATE-BUF u BYTE-COPY
   root rootu req requ UNDER root rootu BELOW? if
      NORMAL-BUF NORMAL-U @ root rootu RELATIVE first BOOT-KNOWN? if
         NORMAL-BUF NORMAL-U @ CANONICAL drop
         CANDIDATE$ CORE-STR= CANDIDATE$ rot exit
      then
   then
   CANDIDATE$ INCLUDE-FALSE ;

\ One root's candidate for REQUEST$: its canonical path, whether it is known,
\ and whether a search stops there (known, or on disk). Under any root the
\ candidate can be the engine root's physical copy of a frozen row, counted
\ from row `first`: the root's files reached through its parent directory or a
\ symlinked working directory are still the engine's, and BOOT-CANDIDATE's
\ physical test keeps another directory's file with an engine file's name its
\ own.
: CANDIDATE ( ptr u8 n n -- ptr u8 n bool bool )
   {: first:n :}
   2dup OWNER!
   REQUEST$ JOIN CANONICAL {: exists:bool :}
   2dup REQUIRE-KNOWN? {: known:bool :}
   known 0= if
      ENGINE$ REQUEST$ first BOOT-CANDIDATE
   else known then
   dup exists or ;

\ A relative request's roots in order, each once: its owner, the working
\ directory, then the engine root. The first root whose candidate stops the
\ search answers; when none does, the owner's answer names the missing file.
\ The working directory stays ahead of the engine root: nested checking copies
\ subject text into a temporary child loader and relies on finding files there.
: SEARCH-ROOTS ( n -- ptr u8 n bool )
   {: first:n :}
   CURRENT$ first CANDIDATE if exit then drop 2drop
   CWD$ CURRENT$ CORE-STR= 0= if
      CWD$ first CANDIDATE if exit then drop 2drop
   then
   ENGINE$ CURRENT$ CORE-STR= ENGINE$ CWD$ CORE-STR= or 0= if
      ENGINE$ first CANDIDATE if exit then drop 2drop
   then
   CURRENT$ first CANDIDATE drop ;

\ The root a load of an absolute path opens: a root it lies below, else its
\ directory. A missing file's directory need not be a root at all (missing, a
\ file, unsearchable), so its load keeps the current root and INCLUDE-OPEN names
\ the file, as ENTRY-RESOLVE does for an entry.
: ABS-OWNER ( ptr u8 n bool -- )
   {: a:ptr u:n exists:bool :}
   a u CURRENT$ BELOW? if CURRENT$ OWNER! exit then
   a u CWD$ BELOW? if CWD$ OWNER! exit then
   exists 0= if CURRENT$ OWNER! exit then
   a u DIRNAME OWNER! ;

\ The file a require of REQUEST$ selects, with frozen rows counted from row
\ `first`.
: SELECT ( n -- ptr u8 n bool )
   {: first:n :}
   REQUEST-BUF c@ $2F = if
      REQUEST$ CANONICAL {: exists:bool :}
      2dup exists ABS-OWNER
      2dup REQUIRE-KNOWN? {: known:bool :}
      known 0= if
         ENGINE$ REQUEST$ first BOOT-CANDIDATE
      else known then
      exit
   then
   first SEARCH-ROOTS ;

public

: RESOLVE ( ptr u8 n -- ptr u8 n bool )
   REQUEST! REQUIRE-BASE @ SELECT ;

\ Command-line entries are relative to the invocation directory only;
\ dependencies beneath them inherit the entry directory as their primary root.
\ Like an absolute path, an entry is the engine's own file only as the physical
\ copy at the engine root.
: ENTRY-RESOLVE ( ptr u8 n -- ptr u8 n bool )
   REQUEST!
   CWD$ REQUIRE-BASE @ CANDIDATE
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
\ engine kind: BOOT-CANDIDATE, which asks the engine-root spelling the rows are
\ stored in of the source a require would select.
\ A canonical scan alongside it would answer for no engine and hide a prefix
\ that had stopped opening the registry. The selection counts every frozen row
\ whatever REQUIRE-BASE is, so a frozen row is the engine's even where its file
\ is missing, and this process's own rows only steer where it stops. The
\ question leaves the caller's RESOLVED-ROOT$ as the caller's own RESOLVE left
\ it.
: ENGINE-KNOWN? ( ptr u8 n -- bool )
   OWNER-BUF OWNER-SAVE OWNER-U @ BYTE-COPY
   OWNER-U @ {: owner:n :}
   REQUEST! 0 SELECT drop
   ENGINE$ REQUEST$ 0 BOOT-CANDIDATE nip nip
   OWNER-SAVE OWNER-BUF owner BYTE-COPY
   owner OWNER-U ! ;

\ The row a boot file is recorded as: the spelling BOOT-CANDIDATE asks for. That
\ is its path below the engine root, or, for a file a directory symlink inside
\ the root carries from outside it, the request spelled under the root when that
\ resolves to this very file. Any other file is refused by name: its row could
\ only be its canonical path, and an engine carrying one takes that file for a
\ stranger under every other root. Below the row, the file again from
\ CANDIDATE-BUF, which the check leaves intact for the caller's load.
: BOOT-ROW ( ptr u8 n -- ptr u8 n ptr u8 n ) {: a:ptr u:n :}
   u CANDIDATE-U ! a CANDIDATE-BUF u BYTE-COPY
   ENGINE$ {: root:ptr rootu:n :}
   CANDIDATE$ root rootu BELOW? if
      CANDIDATE$ CANDIDATE$ root rootu RELATIVE exit
   then
   root rootu REQUEST$ UNDER root rootu BELOW? if
      NORMAL-BUF NORMAL-U @ CANONICAL drop CANDIDATE$ CORE-STR=
   else INCLUDE-FALSE then
   0= if s" source root: a boot file is outside the engine root: " CANDIDATE$ PATH-DIE then
   CANDIDATE$ root rootu REQUEST$ UNDER root rootu RELATIVE ;

;package

\ A boot row is recorded PORTABLY, in the one spelling BOOT-KNOWN? asks for,
\ relative to the engine root (SOURCE-ROOT:BOOT-ROW). Store and query share that
\ root deliberately. A build records the tree it compiles: its fresh registry
\ holds no row, so its root is the working directory. Against the load's OWN
\ owner root a nested
\ require - one issued by a boot file rather than by the manifest - would shorten
\ to a bare basename that no query ever spells, and a basename names a different
\ file in every directory.
\
\ Recording the canonical absolute spelling instead baked the build directory
\ into the engine binary, so two builds of one revision from two directories
\ differed (dot habu-bake-prefix-src-1047b604). An application row keeps its
\ canonical identity, so identical names in distinct roots still coexist. The two
\ spellings cannot collide: a canonical name always begins with `/` and a
\ portable one never does. The path comes back in storage the store did not
\ reuse, for the caller's load.
: REQUIRE-STORE ( ptr u8 n -- ptr u8 n )
   REQUIRE-CHECK-ROOM
   REQUIRE-BOOT-OPEN? if BOOT-ROW else 2dup then
   REQUIRE-REG:APPEND ;

\ One scratch line for diagnostics that have to name a path. Sized so the
\ longest accepted path plus the longest prefix below always fits, and the
\ append refuses rather than truncates, so a message is whole or absent.
$20 constant INCLUDE-DIAG-PREFIX-CAP
INCLUDE-PATH-CAP INCLUDE-DIAG-PREFIX-CAP + constant INCLUDE-DIAG-CAP
create INCLUDE-DIAG INCLUDE-DIAG-CAP allot
variable INCLUDE-DIAG-U

: INCLUDE-DIAG-RESET ( -- )
   0 INCLUDE-DIAG-U ! ;

\ A piece that does not fit is dropped. It is compared with the room left, so a
\ length near the maximum cell cannot wrap the sum back under the capacity, and
\ a negative one is dropped too.
: INCLUDE-DIAG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if exit then
   u INCLUDE-DIAG-CAP INCLUDE-DIAG-U @ - > if exit then
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
   INCLUDE-DIAG$ INCLUDE-DIE ;

: INCLUDE-OPEN ( ptr u8 n -- )
   INCLUDE-PATH0 open-rd INCLUDE-FD !
   INCLUDE-FD @ 0 < if INCLUDE-OPEN-DIE then ;

: INCLUDE-EVALERR? ( -- bool )
   data-base INCLUDE-EVALERR-CELL + @ 0 = 0= ;

\ The loader's evaluation boundary. Every loaded file (require, include,
\ `--load`) and every generated core declaration crosses it through the two
\ bindings at the end of this file, so each runs as a closed program: its floor
\ is the loader's depth and whatever it leaves is refused E-EVAL-RESIDUE. A file
\ that leaves cells is fixed at the file, never here.
: INCLUDE-EVALUATE ( ptr u8 n -- )
   evaluate-closed ;

\ ---- Ordered source-composition event log (TFAM 5, item 5) --------------
\ The loader words append one event per source-composition act so a restricted
\ discovery pass can reconstruct include multiplicity and require/provided
\ canonical registry state in order. Recording is gated by EVENT-ON? so a
\ normal boot/gate records nothing (no overhead, no overflow). During discovery
\ the walker sets DISCOVERY and supplies the loader-token byte span in
\ DISC-TOK-A/DISC-TOK-U; a real load reads the live token span from the
\ interpreter TKA/TKL cells instead.

8 constant EVENT-FIELDS
EVENT-FIELDS cells constant EVENT-BYTES          \ one record
$1000 constant EVENT-ROOM-MIN                    \ a first mapping's bytes
$4D constant INCLUDE-EVENT-RC

0 constant EV-INCLUDED
1 constant EV-REQUIRED
2 constant EV-PROVIDED
0 constant EV-STATE-FRESH
1 constant EV-STATE-KNOWN

\ The records, and the pool that holds each event's path and, unless an
\ earlier event stored the same one, the root it resolved under, are mappings
\ that grow as events arrive, so a discovery records every act however many
\ there are. Each CAP counts the bytes its mapping holds, 0 while unmapped.
PTR-VARIABLE EVENT-RECS
PTR-VARIABLE EVENT-POOL
variable EVENT-RECS-CAP
variable EVENT-POOL-CAP
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
: EVENT-RECS@ ( -- ptr u8 ) EVENT-RECS @ ;
: EVENT-POOL@ ( -- ptr u8 ) EVENT-POOL @ ;

\ Only memory refuses, by name, as in the include frames: a failed mapping or
\ release ends the process with INCLUDE-IO-RC.
: EVENT-UNMAP ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   a u munmap 0 < if s" events: cannot release an event table" INCLUDE-DIE then ;

\ A fresh mapping of CAP bytes holding the first USED bytes of OLD, which maps
\ OLDCAP bytes and is released first; the caller then publishes the answer and
\ CAP, so a refusal ends the process while OLD is still the one published.
: EVENT-REMAP ( ptr u8 n n n -- ptr u8 )
   {: old:ptr oldcap:n used:n cap:n :}
   cap map-anon 0= 0= if drop s" events: cannot map an event table" INCLUDE-DIE then
   {: fresh:ptr :}
   old fresh used BYTE-COPY
   old oldcap EVENT-UNMAP
   fresh ;

\ Reset releases both mappings, so snapshot preparation leaves a captured image
\ no path of the machine that recorded them and no address of a mapping. Each
\ descriptor is cleared once its own mapping is released, the records' with the
\ count of events they held.
: EVENTS-RESET ( -- )
   EVENT-RECS@ EVENT-RECS-CAP @ EVENT-UNMAP
   NULL-PTR EVENT-RECS !  0 EVENT-RECS-CAP !  0 EVENT-N !
   EVENT-POOL@ EVENT-POOL-CAP @ EVENT-UNMAP
   NULL-PTR EVENT-POOL !  0 EVENT-POOL-CAP !  0 EVENT-POOL-N ! ;

: LOADER-TOK-A ( -- n )   data-base TKA-CELL + @ ;
: LOADER-TOK-U ( -- n )   data-base TKL-CELL + @ ;
: LOADER-TOKEN-SPAN ( -- n n )  LOADER-TOK-A LOADER-TOK-U ;

: EVENT-SPAN ( -- n n )
   DISCOVERY? if DISC-TOK-A @ DISC-TOK-U @ exit then
   LOADER-TOKEN-SPAN ;

: EVENT-SLOT ( n n -- ptr n )
   swap EVENT-FIELDS * + cells EVENT-RECS@ + CELL-VIEW ;

: EVENT-FIELD@ ( n n -- n )  EVENT-SLOT @ ;
: EVENT-FIELD! ( n n n -- )  EVENT-SLOT ! ;

: EVENT-POOL-AT ( n -- ptr u8 )  EVENT-POOL@ + ;

\ A mapping that must hold NEED bytes doubles, or takes NEED if that is more.
: EVENT-GROWN ( n n -- n ) {: need:n cap:n :}
   cap 2 * need max EVENT-ROOM-MIN max ;

: EVENT-RECS-ROOM ( -- )
   EVENT-N @ 1 + EVENT-BYTES * {: need:n :}
   need EVENT-RECS-CAP @ <= if exit then
   need EVENT-RECS-CAP @ EVENT-GROWN {: cap:n :}
   EVENT-RECS@ EVENT-RECS-CAP @ EVENT-N @ EVENT-BYTES * cap EVENT-REMAP
   EVENT-RECS !
   cap EVENT-RECS-CAP ! ;

\ EVENT-COPY-PATH is an engine word any source can call, so the length is
\ refused when it is negative or so long that its sum with the fill wraps.
: EVENT-POOL-ROOM ( n -- ) {: u:n :}
   EVENT-POOL-N @ u + {: need:n :}
   u 0 < need 0 < or if
      s" events: pool overflow" INCLUDE-EVENT-RC die
   then
   need EVENT-POOL-CAP @ <= if exit then
   need EVENT-POOL-CAP @ EVENT-GROWN {: cap:n :}
   EVENT-POOL@ EVENT-POOL-CAP @ EVENT-POOL-N @ cap EVENT-REMAP
   EVENT-POOL !
   cap EVENT-POOL-CAP ! ;

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
\ The file's bytes follow the header in the same mapping, which is sized to the
\ file (GROW): FR-ROOM counts the source bytes it can hold, FR-SIZE the file's.
0 constant FR-PREV                          \ parent frame (a declared pointer field)
CELL constant FR-NAMED                       \ SCRIPT-NAMED-PEND byte
2 cells constant FR-PATHLEN                  \ this load's path length
3 cells constant FR-ROOM                     \ source bytes the mapping holds
4 cells constant FR-SIZE                     \ this load's source length
5 cells constant FR-PATH                     \ this load's path bytes
FR-PATH INCLUDE-PATH-CAP + 1 cells + constant HEADER-BYTES   \ cell-rounded, so SOURCE stays aligned
$1000 constant ROOM-MIN                      \ a new frame's room, before its file is read
PTR-VARIABLE TOP

: TOP@ ( -- ptr u8 ) TOP @ ;
: TOP! ( ptr u8 -- ) TOP ! ;

: CHECK-ACTIVE ( -- )
   INCLUDE-DEPTH @ 0 <= if s" include: depth underflow" INCLUDE-DIE then ;

\ The engine's refusal tail prints ` at <path>:<line>` from these two cells and
\ nothing else decides when they are right: they are republished on every PUSH
\ and every POP, so they name the file the interpreter is inside and read 0 when
\ it is inside none (the REPL or another stdin session, the boot prefix).
: PUBLISH-LOCATION ( -- )
   INCLUDE-DEPTH @ 0 <= if
      0 data-base INCLUDE-SRCLOC-PATH-CELL + !
      0 data-base INCLUDE-SRCLOC-PATHLEN-CELL + !
      exit
   then
   TOP@ {: frame:ptr :}
   frame FR-PATH + NULL-PTR - data-base INCLUDE-SRCLOC-PATH-CELL + !
   frame FR-PATHLEN + CELL-VIEW @ data-base INCLUDE-SRCLOC-PATHLEN-CELL + ! ;

\ The bytes FRAME maps: its header and its room.
: MAPPED ( ptr u8 -- n ) FR-ROOM + CELL-VIEW @ HEADER-BYTES + ;

: ROOM ( -- n ) TOP@ FR-ROOM + CELL-VIEW @ ;

: PUSH ( -- )
   HEADER-BYTES ROOM-MIN + map-anon 0= 0= if drop s" include: cannot map a source frame" INCLUDE-DIE then
   {: frame:ptr :}
   TOP@ frame FR-PREV ptr-field !
   SCRIPT-NAMED-PEND @ frame FR-NAMED + c!
   INCLUDE-PATH-U @ frame FR-PATHLEN + CELL-VIEW !
   ROOM-MIN frame FR-ROOM + CELL-VIEW !
   INCLUDE-PATH frame FR-PATH + INCLUDE-PATH-U @ BYTE-COPY
   frame TOP!
   INCLUDE-FALSE SCRIPT-NAMED-PEND!
   1 INCLUDE-DEPTH +!
   PUBLISH-LOCATION ;

: POP ( -- )
   CHECK-ACTIVE
   TOP@ {: frame:ptr :}
   frame FR-PREV ptr-field @ TOP!
   -1 INCLUDE-DEPTH +!
   PUBLISH-LOCATION
   frame dup MAPPED munmap 0 < if s" include: cannot release a source frame" INCLUDE-DIE then ;

: SOURCE ( -- ptr u8 ) CHECK-ACTIVE TOP@ HEADER-BYTES + ;

\ The open frame learns its file's size by filling, so it grows as lib/pg.f's
\ call arena does: it doubles its room, or takes WANT bytes if that is more. The
\ header and the first KEEP source bytes move to a fresh mapping; once the old
\ one is released, the fresh one becomes TOP and the published location, the
\ only two that point into a frame while it fills. Only memory refuses, by
\ name, as in PUSH and POP: a refusal ends the process while the old frame is
\ still the one published.
: GROW ( n n -- )
   {: want:n keep:n :}
   TOP@ {: frame:ptr :}
   ROOM 2 * want max {: room:n :}
   HEADER-BYTES room + map-anon 0= 0= if drop s" include: cannot map a source frame" INCLUDE-DIE then
   {: fresh:ptr :}
   frame fresh HEADER-BYTES keep + BYTE-COPY
   room fresh FR-ROOM + CELL-VIEW !
   frame dup MAPPED munmap 0 < if s" include: cannot release a source frame" INCLUDE-DIE then
   fresh TOP!
   PUBLISH-LOCATION ;

: INCLUDE-READ-DONE? ( -- bool )
   INCLUDE-U @ ROOM = if INCLUDE-U @ 1+ INCLUDE-U @ GROW then
   INCLUDE-FD @ SOURCE INCLUDE-U @ + ROOM INCLUDE-U @ - read INCLUDE-RD !
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

public

: READ-OS ( ptr u8 n ptr u8 n -- ptr u8 n )
   2drop INCLUDE-READ-ALL ;

\ A source whose first two bytes are `#!` names its interpreter on that line,
\ and the engine reads the line as a comment: the two bytes become `\ `, so
\ every line and column stays the file's own. The program stream an engine
\ runs gets the same rewrite (src/habu/habu2.f EMIT-SHEBANG-COMMENT).
: SHEBANG-COMMENT ( ptr u8 n -- ) {: a:ptr u:n :}
   u 2 < if exit then
   a c@ $23 <> if exit then
   a 1 + c@ $21 <> if exit then
   $5c a c!
   $20 a 1 + c! ;

\ The loop a loaded file's bytes go through, inside the closed boundary:
\ INCLUDE-EVAL-BIND below binds INCLUDE-EVALUATE, and test/outer-loop-on.f binds
\ the loop written in Habu, src/habu/interpret.f OUTER:INTERPRET, under
\ evaluate-closed for the files loaded after it. The Habu loop checks the
\ design seal at each token, so sealed files use the same binding.
defer INCLUDE-INTERPRET ( ptr u8 n -- )

private

\ Holds a load's U source bytes at A in its frame and records their length
\ there. The OS reader (READ-OS) has filled the frame already, learning the size
\ as it read; another reader's bytes come with their length, and the frame grows
\ to fit them before they are copied in.
: HOLD ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   a SOURCE <> if
      u ROOM > if u 0 GROW then
      a SOURCE u BYTE-COPY
   then
   u TOP@ FR-SIZE + CELL-VIEW !
   SOURCE u ;

: READ-SOURCE ( -- ptr u8 n )
   INCLUDE-PATH INCLUDE-PATH-U @ RESOLVED-ROOT$ SOURCE-INPUT:READ HOLD ;

: LOAD-BYTES ( -- )
   SOURCE TOP@ FR-SIZE + CELL-VIEW @
   INCLUDE-INTERPRET ;

\ The file is read before the unit importer runs: a frame moves while it grows,
\ and the source address the importer is given must be the one the interpreter
\ reads (tools/native-source-view.f LOAD-START keeps it).
: LOAD-UNIT ( -- )
   READ-SOURCE SHEBANG-COMMENT
   TOP@ {: frame:ptr :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @
   CURRENT$ SOURCE [: LOAD-BYTES ;] SOURCE-UNIT:LOAD ;

: LOAD-CURRENT ( -- )
   PUSH
   [: LOAD-UNIT ;] catch {: rc:n :}
   INCLUDE-CLOSE
   POP
   rc 0= 0= if rc throw then
   INCLUDE-EVALERR? if s" include: evaluation failed" INCLUDE-EVAL-DIE then ;

\ A verifier can traverse the loader's resolved file frames without invoking
\ the interpreter. The callback receives bytes owned by the frame and its
\ canonical path; both remain valid throughout the callback, including nested
\ loads. This keeps the ordinary loader's root and cleanup rules.
: READ-FOR ( [ ptr u8 n ptr u8 n -- ] -- [ ptr u8 n ptr u8 n -- ] )
   {: q :}
   READ-SOURCE TOP@ {: frame:ptr :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @ q execute
   q ;

: COPY-FOR ( ptr u8 n [ ptr u8 n ptr u8 n -- ] -- ptr u8 n [ ptr u8 n ptr u8 n -- ] )
   {: a:ptr u:n q :}
   a u HOLD TOP@ {: frame:ptr :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @ q execute
   a u q ;

: WITH-BYTES-CURRENT ( [ ptr u8 n ptr u8 n -- ] -- [ ptr u8 n ptr u8 n -- ] ) {: q :}
   PUSH
   q
   [: READ-FOR ;] catch {: rc:n :}
   drop
   INCLUDE-CLOSE
   POP
   rc 0= 0= if rc throw then
   q ;

: WITH-SUPPLIED-CURRENT ( ptr u8 n [ ptr u8 n ptr u8 n -- ] -- ptr u8 n [ ptr u8 n ptr u8 n -- ] )
   {: a:ptr u:n q :}
   PUSH a u q
   [: COPY-FOR ;] catch {: rc:n :}
   2drop drop
   INCLUDE-CLOSE
   POP
   rc 0= 0= if rc throw then
   a u q ;

public

: NAMED? ( -- bool )
   INCLUDE-DEPTH @ 0= if INCLUDE-FALSE exit then
   TOP@ FR-NAMED + c@ 0= 0= ;

: LOAD ( ptr u8 n -- )
   INCLUDE-PATH0 drop
   RESOLVED-ROOT$ [: LOAD-CURRENT ;] WITH ;

\ PATH has already been resolved by the loader's resolver. Like LOAD, this
\ opens a source frame and makes its root current until the callback returns.
: WITH-BYTES ( ptr u8 n [ ptr u8 n ptr u8 n -- ] -- )
   {: path:ptr pathu:n q :}
   path pathu INCLUDE-PATH0 drop
   RESOLVED-ROOT$ ROOT-ENTER {: fresh:ptr u:n old:ptr oldu:n :}
   q [: WITH-BYTES-CURRENT ;] catch {: rc:n :}
   drop
   fresh u old oldu rc ROOT-LEAVE ;

\ A caller that owns PATH's source bytes can open the same loader frame without
\ reading a different disk copy. This is used for an included buffer subject.
: WITH-SUPPLIED ( ptr u8 n ptr u8 n [ ptr u8 n ptr u8 n -- ] -- )
   {: path:ptr pathu:n src:ptr srcu:n q :}
   path pathu INCLUDE-PATH0 drop
   RESOLVED-ROOT$ ROOT-ENTER {: fresh:ptr u:n old:ptr oldu:n :}
   src srcu q [: WITH-SUPPLIED-CURRENT ;] catch {: rc:n :}
   2drop drop
   fresh u old oldu rc ROOT-LEAVE ;

;package

package SOURCE-INPUT
public

: RESET ( -- )
   [: SOURCE-ROOT:CANON-OS ;] is CANON-XT
   [: SOURCE-ROOT:READ-OS ;] is READ-XT ;

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
   REQUIRE-STORE
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
   REQUIRE-STORE 2drop ;

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
   INCLUDE-DIAG$ INCLUDE-DIE ;

\ constructor generation (sumtype.f, loaded earlier in the boot prefix) crosses
\ evaluate only through the closed INCLUDE-EVALUATE boundary; engines without
\ include.f (stage builders) leave TYPE-DECL's armed flag 0 and generation stays
\ fail-closed. Bind the defer (wrapped so the quotation compiles), then arm.
\ The loaded-bytes seam SOURCE-ROOT:INCLUDE-INTERPRET gets the same boundary.
\ Each binder is a package private rather than an 85th global in this file: it is
\ a one-shot installer with no caller, and the `is` target has to be written
\ qualified because `is` is a parsing word and parsing words resolve outside
\ using-imports (measured: bare `is TDECL-EVAL-XT` under `using TYPE-DECL`
\ answers `hb: is: no deferred word named TDECL-EVAL-XT`, rc 70).
using TYPE-DECL

package INCLUDE-EVAL-BIND
private
: INSTALL ( -- ) [: INCLUDE-EVALUATE ;] is TYPE-DECL:TDECL-EVAL-XT ;
INSTALL
: INTERPRET-INSTALL ( -- ) [: INCLUDE-EVALUATE ;] is SOURCE-ROOT:INCLUDE-INTERPRET ;
INTERPRET-INSTALL
;package

-1 TDECL-EVAL-ARMED !
;using

;using
