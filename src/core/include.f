\ include.f: checked source loading. SOURCE-ROOT resolves paths against the owner
\ root, REQUIRE-REG records loaded files (engine rows frozen at boot), the event
\ log feeds a discovery pass, and the frames hold each open file. `include` only
\ composes source; package reopening owns shared namespace.

PATH-CAP constant INCLUDE-PATH-CAP
$46 constant INCLUDE-EVAL-RC
-2110 constant INCLUDE-RANGE-RC
$37D8 constant INCLUDE-EVALERR-CELL
\ The innermost open source file's path for the engine's refusal tail
\ (src/habu/habu2.f LCOMPILEDIE): fixed data-base offsets, reserved in
\ src/habu/layout.f as SRCLOC:PATH-CELL / SRCLOC:PATHLEN-CELL.
$2800 constant INCLUDE-SRCLOC-PATH-CELL
$2808 constant INCLUDE-SRCLOC-PATHLEN-CELL
$2D68 constant INCLUDE-SRCLOC-OWNED-CELL
$2D70 constant INCLUDE-SRCLOC-OWNEDLEN-CELL
\ The design seal's watermark (src/lib/vocabulary.f), spelled here the same way and
\ reserved in src/habu/layout.f as POLICY-NDICT-CELL: 0 while nothing is sealed.
$4898 constant INCLUDE-POLICY-NDICT-CELL
$4CA0 constant INCLUDE-POLICY-REQUIRE-CELL
create INCLUDE-PATH INCLUDE-PATH-CAP 1 + allot

package REQUIRE-REG
private

\ FROZEN holds a DATA block of (offset,length) pairs followed by path bytes.
\ Process rows use two movable columns, mapped and grown together at ROW-CAP
\ cells: ROWS holds row i's path pointer at cell i and ROW-LENS its length at
\ cell i. The paths live in chunks that never move.
PERSISTED-PTR-VARIABLE FROZEN
PERSISTED-PTR-U8-TABLE-VARIABLE ROWS
PERSISTED-PTR-VARIABLE ROW-LENS
variable ROW-CAP
PERSISTED-PTR-U8-TABLE-VARIABLE CHUNK
variable PREFIX-N
variable PREFIX-EPOCH
variable PREFIX-CUT

;package

variable INCLUDE-FD
variable INCLUDE-U
variable INCLUDE-RD
variable INCLUDE-PATH-U
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
\ carries; every other row, what a process loaded. Only the boot prefix opens
\ it, with a token of its own (habu2.f EMIT-REQUIRE-BOOT-OPEN-TOKEN) between the
\ loader's text and the first row. Loading this file is not the signal: a build
\ re-loads it into a booted engine, which would then record every later require
\ portably and stop recognising its own canonical rows.
: REQUIRE-BOOT-OPEN ( -- )
   INCLUDE-TRUE REQUIRE-BOOT-V ! ;

: REQUIRE-BOOT-OPEN? ( -- bool )
   REQUIRE-BOOT-V @ 0= 0= ;

\ While the registry is open every row so far is the engine's, because only the
\ prefix records there; once closed, the frozen count says. The prefix requires
\ engine files while recording them, so bounding by REQUIRE-BOOT-N alone made it
\ reload its own checker (`duplicate definition: RAW-OFF`, exit 78).
: REQUIRE-BOOT-LIMIT ( -- n )
   REQUIRE-BOOT-OPEN? if REQUIRE-N @ exit then
   REQUIRE-BOOT-N @ ;

\ The loader's first callback cells are typed storage, whose generated
\ accessors need TYPE-DECL's evaluator before REQUIRE-REG:MARK exists. Their
\ source is closed here; INCLUDE-EVALUATE replaces this binding with the
\ require rollback boundary once the registry is ready.
using TYPE-DECL
package INCLUDE-EVAL-BIND
private
: BOOT ( -- ) [: evaluate-closed ;] is TYPE-DECL:TDECL-EVAL-XT ;
BOOT
;package
-1 TDECL-EVAL-ARMED !
;using

\ An invocation that owns source bytes supplies both filesystem answers through
\ this loader-local pair. A newly source-loaded target gets its own pair, so its
\ require registry and source frames remain independent of the retained host.
package SOURCE-INPUT
private

TYPED-VARIABLE CANON-XT [ ptr u8 n -- ptr u8 n bool ]
TYPED-VARIABLE READ-XT [ ptr u8 n ptr u8 n -- ptr u8 n ]
defer OBSERVE-XT ( ptr u8 n ptr u8 n ptr u8 n -- )
defer CLEAR-XT ( -- )

: NO-OBSERVE ( ptr u8 n ptr u8 n ptr u8 n -- )
   2drop 2drop 2drop ;
: NO-CLEAR ( -- ) ;

public

: CANON ( ptr u8 n -- ptr u8 n bool ) CANON-XT @ execute ;
: READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n root:ptr rootu:n :}
   path pathu root rootu READ-XT @ execute {: bytes:ptr size:n :}
   path pathu root rootu bytes size OBSERVE-XT
   bytes size ;

: USE ( [ ptr u8 n -- ptr u8 n bool ] [ ptr u8 n ptr u8 n -- ptr u8 n ] -- )
   {: canon read :}
   canon CANON-XT !
   read READ-XT ! ;

: WITH ( [ ptr u8 n -- ptr u8 n bool ] [ ptr u8 n ptr u8 n -- ptr u8 n ] [ -- ] -- )
   {: canon read action :}
   CANON-XT @ READ-XT @ {: old-canon old-read :}
   canon read USE
   action catch {: rc:n :}
   old-canon old-read USE
   rc 0 <> if rc throw then ;

: USE-OBSERVER ( [ ptr u8 n ptr u8 n ptr u8 n -- ] [ -- ] -- )
   {: observe clear :}
   observe is OBSERVE-XT
   clear is CLEAR-XT ;

: CLEAR-OBSERVED ( -- ) CLEAR-XT ;

: OBSERVER-DEFAULT ( -- )
   [: NO-OBSERVE ;] [: NO-CLEAR ;] USE-OBSERVER ;

;package

SOURCE-INPUT:OBSERVER-DEFAULT

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
create POLICY-ROOT-BUF PATH-BYTES allot
variable POLICY-ROOT-U
create POLICY-LF 10 c,
create REQUEST-BUF WORK-BYTES allot
variable REQUEST-U
variable CWD-U
variable CANON-U
variable NORMAL-U
variable CANDIDATE-U
variable JOIN-U
variable OWNER-U
\ The loader's record: top frame, current root and length, open scopes, ROOT!'s
\ count. Readers reach it by SRCLOC:SOURCE-CELL, the running hb's in a build.
0 constant REC-TOP
CELL constant REC-ROOT
2 cells constant REC-ROOTLEN
3 cells constant REC-SCOPES
4 cells constant REC-CHANGES
5 cells constant REC-BYTES
create LOADER-REC REC-BYTES SCRATCH-ALLOT   \ a saved program starts with it empty
SRCLOC:SOURCE-CELL RESERVED-PTR-U8-CELL LOADER-CELL
LOADER-REC LOADER-CELL ptr-cell!            \ a build's store goes to its image (habu2.f ROUTE-HEADER)
CAST: REC-CELLS ( ptr u8 -- ptr ptr u8 )
: LOADER ( -- ptr u8 ) LOADER-CELL @ ;
: TOP ( -- ptr ptr u8 ) LOADER REC-CELLS REC-TOP CELL / ptr-field ;
: CURRENT-A ( -- ptr ptr u8 ) LOADER REC-CELLS REC-ROOT CELL / ptr-field ;
: CURRENT-U ( -- ptr n ) LOADER REC-ROOTLEN + CELL-VIEW ;
: SCOPES ( -- ptr n ) LOADER REC-SCOPES + CELL-VIEW ;
: CHANGES ( -- ptr n ) LOADER REC-CHANGES + CELL-VIEW ;
create ENGINE-BUF PATH-BYTES allot
variable ENGINE-U
variable ENGINE-READ
\ The stack PUSHPATH fills: a mapped copy of each saved current path and its
\ length, 0 for the top-level root.
8 constant PATH-DEPTH
PATH-DEPTH PTR-U8-TABLE SLOT-A
create SLOT-U PATH-DEPTH cells allot
variable SLOT-N

: CURRENT-PTR ( -- ptr u8 ) CURRENT-A @ ;


: CURRENT! ( ptr u8 n -- )
   CURRENT-U ! CURRENT-A ! ;

\ The current path as set, null for the top-level root, which a scope saves
\ and restores: CURRENT$ would hand back CWD-BUF, which no top-level path may
\ hold (CD and POPPATH release the path they replace).
: CURRENT@ ( -- ptr u8 n )
   CURRENT-PTR CURRENT-U @ ;


: CHECK ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 <= u WORK-BYTES >= or if E-PATH-RANGE throw then ;


\ realpath of a path into CANON-BUF: its length, -2 when that does not fit,
\ below 0 when the path does not resolve.
: CANON-N ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u CHECK
   a u ZBUF WORK-BYTES PATHZ
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


private

: PARENT-U ( ptr u8 n -- n ) {: a:ptr u:n :}
   u
   begin dup 1 > while
      1- dup a + c@ 47 = if exit then
   repeat ;


\ The program file `hb prog.f` runs: argv[1], which the launcher leaves in
\ PROGRAM-FILE-CELL only on that route (src/habu/habu2.f SRC-FPLAIN,
\ src/habu/main.f RUN). Its canonical path, against the working directory no
\ Habu word changes, is derived on first need, as ENGINE$ is. Its directory is
\ the top-level root (CURRENT$) and PROGRAM? counts it as loaded: RUN-PROGRAM
\ gives it a frame but no registry row names it, since a row appended on first
\ use would vanish under the rollback of a failed require or a discovery pass.
\ With the cell clear nothing is derived. Deriving writes only PROGRAM-BUF, so
\ the caller's scratch survives.
PROGRAM-FILE-CELL RESERVED-PTR-U8-CELL PROGRAM-ARG
create PROGRAM-BUF PATH-BYTES allot
variable PROGRAM-U

: PROGRAM-INIT ( -- )
   PROGRAM-U @ 0= 0= if exit then
   PROGRAM-ARG @ {: a:ptr :}
   a 0= if exit then
   a PROGRAM-BUF PATH-BYTES realpath {: n:n :}
   n 0 <= if
      s" source root: the program file does not resolve within PATH-CAP bytes"
      INCLUDE-IO-RC die
   then
   n PROGRAM-U ! ;

\ Whether a canonical path is the program file. A fresh discovery registry
\ (REQUIRE-SNAPSHOT) hides it, as it hides every row recorded before it.
: PROGRAM? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   REQUIRE-BASE @ 0= 0= if INCLUDE-FALSE exit then
   PROGRAM-INIT
   PROGRAM-U @ 0= if INCLUDE-FALSE exit then
   a u PROGRAM-BUF PROGRAM-U @ CORE-STR= ;

\ The working directory, the engine root, the program file and the resolver's
\ work belong to the process that derived them, so their cells are scratch: a
\ saved program, a cross-built one included, starts with none and derives its
\ own instead of resolving against its builder's directory.
: SCRATCH-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + scratch-cell-mark 1 cells +loop ;

CWD-BUF PATH-BYTES SCRATCH-BYTES        CWD-U scratch-cell-mark
CANON-BUF PATH-BYTES SCRATCH-BYTES      CANON-U scratch-cell-mark
NORMAL-BUF WORK-BYTES SCRATCH-BYTES     NORMAL-U scratch-cell-mark
CANDIDATE-BUF PATH-BYTES SCRATCH-BYTES  CANDIDATE-U scratch-cell-mark
JOIN-BUF WORK-BYTES SCRATCH-BYTES       JOIN-U scratch-cell-mark
WORK-BUF WORK-BYTES SCRATCH-BYTES
ZBUF WORK-BYTES SCRATCH-BYTES
OWNER-BUF PATH-BYTES SCRATCH-BYTES      OWNER-U scratch-cell-mark
OWNER-SAVE PATH-BYTES SCRATCH-BYTES
REQUEST-BUF WORK-BYTES SCRATCH-BYTES    REQUEST-U scratch-cell-mark
ENGINE-BUF PATH-BYTES SCRATCH-BYTES     ENGINE-U scratch-cell-mark
ENGINE-READ scratch-cell-mark
PROGRAM-BUF PATH-BYTES SCRATCH-BYTES    PROGRAM-U scratch-cell-mark

public

: CURRENT$ ( -- ptr u8 n )
   CURRENT-U @ 0 > if CURRENT-PTR CURRENT-U @ exit then
   PROGRAM-INIT
   PROGRAM-U @ 0= if CWD$ exit then
   PROGRAM-BUF PROGRAM-BUF PROGRAM-U @ PARENT-U ;

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


: NORMALIZE-LIMIT ( ptr u8 n n -- )
   {: a:ptr u:n limit:n :}
   47 NORMAL-BUF c! 1 NORMAL-U !
   0 begin dup u < while
      dup a u rot SEG-END {: end:n :}
      a over + end rot - limit NORMAL-SEG
      end 1+
   repeat drop ;


: NORMALIZE ( ptr u8 n -- ) INCLUDE-PATH-CAP NORMALIZE-LIMIT ;

\ A source-root refusal: its reason, then the path it names, on one line. The
\ reason is written first, so a path of any length a caller holds is named whole.
: PATH-DIE ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n path:ptr pathu:n :}
   2 a u write drop
   path pathu INCLUDE-IO-RC die ;

\ Whether the process's loader record is this copy's own. A build compiles a
\ second copy of this file into the program it builds, and there LOADER answers
\ hb's record: the copy's loading words would run its own registry, buffers and
\ frames over hb's. The build's loader does every load, at a file's top level,
\ so in a build the copy's loads refuse (BUILD-REFUSE): LOAD-RESULT, and so
\ every load of a file (included, required, LOAD, script-required);
\ RUN-PROGRAM; WITH-BYTES; WITH-SUPPLIED; provided, before it stores a row.
\ READ-OS is refused by its frame's state instead (READ-REFUSE), in a build as
\ elsewhere.
: OWN-LOADER? ( -- bool ) LOADER LOADER-REC = ;

public

\ The build refusal for the path a loading word was handed, unless this copy's
\ record is the process's. The resolver does not refuse: RESOLVE loads nothing,
\ and in a build it searches the process's current root, so a word may ask
\ where a file lies (test/aot-shifted-bytes.f RESOLVES-BUILD; test/file-objects.f
\ WORD-LOAD-BUILD). Public so provided, which this file defines outside
\ SOURCE-ROOT, refuses here too: its row would lie outside the build's space,
\ which only the linker would catch.
: BUILD-REFUSE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   OWN-LOADER? if exit then
   s" include: a word loaded or provided a file during a build; do it at the file's top level: "
   path pathu PATH-DIE ;

private

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
      CANON-BUF NORMAL-BUF CANON-U @ BYTE-COPY
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
: ENGINE-DIR? ( n -- bool )
   {: n:n :}
   CANON-BUF WORK-BUF n BYTE-COPY
   47 WORK-BUF n + c!
   46 WORK-BUF n 1+ + c!
   WORK-BUF n 2 + ZBUF WORK-BYTES PATHZ
   ZBUF 0 access 0= ;

\ ENGINE-DIR? decides a root on every platform; the canon of a path does not.
\ Neither says why a path fails, so the refusal names the absolute spelling
\ tried and what a missing, unsearchable or long directory and a file all are not.
: ROOT-DIE ( -- )
   s" source root: does not resolve to a searchable directory within PATH-CAP bytes: "
   JOIN-BUF JOIN-U @ PATH-DIE ;

: ROOT-CANON ( ptr u8 n -- )
   ABSOLUTE!
   JOIN-BUF JOIN-U @ TRY-CANON 0= if ROOT-DIE then
   CANON-U @ ENGINE-DIR? 0= if ROOT-DIE then ;

public

: CONFINE ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   data-base INCLUDE-POLICY-REQUIRE-CELL + @ 0= if a u exit then
   a u CANONICAL drop {: path:ptr pathu:n :}
   path pathu POLICY-ROOT-BUF POLICY-ROOT-U @ BELOW? if path pathu exit then
   2 s" hb: policy: source outside root: " write drop
   2 path pathu write drop
   2 POLICY-LF 1 write drop
   107 throw ;

private

\ A fresh mapping holding n path bytes and the NUL after them.
: MAP-COPY ( ptr u8 n -- ptr u8 )
   {: a:ptr u:n :}
   u 1+ map-anon 0= 0= if drop s" source root: cannot map a path copy" INCLUDE-IO-RC die then
   {: fresh:ptr :}
   a fresh u 1+ BYTE-COPY
   fresh ;

: ROOT-ENTER ( ptr u8 n -- ptr u8 n )
   ROOT-CANON
   CANON-U @ {: u:n :}
   CANON-BUF u MAP-COPY {: fresh:ptr :}
   CURRENT@
   fresh u CURRENT!
   1 SCOPES +! ;

\ The root current at a scope's exit is the scope's own mapping, or the one a
\ ROOT! in the scope replaced it with; either is released once the caller's
\ root is back.
: ROOT-LEAVE ( ptr u8 n n -- )
   {: old:ptr oldu:n rc:n :}
   -1 SCOPES +!
   CURRENT@ {: cur:ptr curu:n :}
   old oldu CURRENT!
   cur curu 1+ munmap 0 < if
      s" source root: cannot release a root mapping" INCLUDE-IO-RC die
   then
   rc 0= 0= if rc throw then ;

public

\ Each scope maps its root string at entry and releases the root current at its
\ exit. There is no additional root-count/depth limit, and a throw restores the
\ caller before releasing it. A root that does not resolve to a searchable
\ directory is refused by name.
: WITH ( ptr u8 n [ -- ] -- ) {: q :}
   ROOT-ENTER {: old:ptr oldu:n :}
   q catch {: rc:n :}
   old oldu rc ROOT-LEAVE ;

private

\ CD, PUSHPATH and POPPATH act on the top level only: each open scope, a load's
\ among them, holds the current path it replaced and restores it on exit.
: TOP-ONLY ( ptr u8 n -- )
   SCOPES @ 0= if 2drop exit then
   INCLUDE-IO-RC die ;

\ A current path is ROOT!'s mapping or POPPATH's, inside a scope also the scope's.
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

public

\ The current root, which a relative require searches first, becomes the
\ directory, relative to the current root when relative. Legal at the top
\ level, inside a load and inside a WITH body: a file sets its root with ROOT!,
\ which its load's end undoes, or scopes one with WITH. It never changes the
\ process's directory or the engine root. A refusal names the absolute
\ spelling tried.
: ROOT! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u INCLUDE-PATH-CAP > if s" source root: path is too long" INCLUDE-IO-RC die then
   CURRENT$ a u JOIN {: p:ptr pu:n :}
   pu INCLUDE-PATH-CAP > if s" source root: path is too long" INCLUDE-IO-RC die then
   p pu CANON-N {: n:n :}
   n -2 = if s" source root: path is too long" INCLUDE-IO-RC die then
   n 0 < if s" source root: does not resolve to a directory: " p pu PATH-DIE then
   n ENGINE-DIR? 0= if s" source root: is not a searchable directory: " p pu PATH-DIE then
   CANON-BUF n MAP-COPY {: fresh:ptr :}
   RELEASE-CURRENT
   fresh n CURRENT!
   1 CHANGES +! ;

\ How many times ROOT! has set a root (src/habu/linker.f ROOT-CK).
: CHANGE-COUNT ( -- n ) CHANGES @ ;

\ SwiftForth's path words, at the top level of a stdin session or of a program
\ file run as `bin/hb file.f`; inside a load (`--load`, `require`) or a WITH
\ body they are refused, and a file sets its root with ROOT! or scopes one with
\ WITH. `CD <dir>` on its own line is `ROOT!` of that directory, and a bare CD
\ prints the current root.
: CD ( -- )
   s" CD: refused inside a load or a root scope; scope a root with SOURCE-ROOT:WITH" TOP-ONLY
   LINE-BLANK? if CURRENT$ type cr exit then
   parse-name ROOT! ;

\ PUSHPATH saves the current path; a null slot is the top-level root.
: PUSHPATH ( -- )
   s" PUSHPATH: refused inside a load or a root scope; scope a root with SOURCE-ROOT:WITH" TOP-ONLY
   SLOT-N @ PATH-DEPTH >= if
      s" PUSHPATH: directory stack is full" INCLUDE-IO-RC die
   then
   SLOT-N @ {: ix:n :}
   CURRENT-U @ {: u:n :}
   u 0 > if CURRENT-PTR u MAP-COPY ix SLOT-FIELD ! then
   u ix SLOT-U!
   ix 1+ SLOT-N ! ;

: POPPATH ( -- )
   s" POPPATH: refused inside a load or a root scope; scope a root with SOURCE-ROOT:WITH" TOP-ONLY
   SLOT-N @ 0= if s" POPPATH: directory stack is empty" INCLUDE-IO-RC die then
   SLOT-N @ 1- {: ix:n :}
   RELEASE-CURRENT
   ix cells SLOT-U + @ {: u:n :}
   u 0 > if ix SLOT-FIELD @ u CURRENT! then
   NULL$ drop ix SLOT-FIELD !
   0 ix SLOT-U!
   ix SLOT-N ! ;

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

: INCLUDE-CHECK-PATH ( ptr u8 n -- ptr u8 n )
   dup 0 <= if s" include: missing path" INCLUDE-DIE then
   dup INCLUDE-PATH-CAP > if s" include: path too long" INCLUDE-DIE then ;

package REQUIRE-REG
private

$7FFFFFFFFFFFFFFF constant MAX-BYTES
$7FFFFFFFFFFFFFFF 2 cells / constant ROW-LIMIT
3 cells constant CHUNK-HEAD
MAX-BYTES CHUNK-HEAD - constant CHUNK-LIMIT

\ A mapped chunk's room and used counts are the two cells after its prev
\ pointer, and its path bytes follow them at CHUNK-HEAD.
CAST: CHUNK-COUNTS ( ptr ptr u8 -- ptr n )

: SIZE-DIE ( -- )
   s" require: inventory size overflow" INCLUDE-DIE ;

: FROZEN-REC ( n -- ptr n )
   2 * cells FROZEN @ swap + ;

: FROZEN-LEN ( n -- n )
   FROZEN-REC CELL + @ ;

: FROZEN-SLOT ( n -- ptr u8 ) {: idx:n :}
   FROZEN @ idx FROZEN-REC @ + ;

: ROW-PTR ( n -- ptr ptr u8 )
   ROWS @ swap ptr-field ;

: ROW-PTR@ ( n -- ptr u8 )
   ROW-PTR @ ;

: ROW-LEN-ADDR ( n -- ptr n )
   ROW-LENS @ swap cells + ;

: ROW-LEN@ ( n -- n )
   ROW-LEN-ADDR @ ;

\ FROZEN's DATA block opens with one (offset,length) pair per row.
: FROZEN-BYTES ( n -- n )
   2 * cells ;

: ROW-NEW-CAP ( n -- n ) {: need:n :}
   need $40 max
   ROW-CAP @ ROW-LIMIT 2 / <= if ROW-CAP @ 2 * max then ;

: COL-MAP ( n -- ptr a )
   cells map-anon 0 <> if
      drop s" require: cannot map the row table" INCLUDE-DIE
   then ;

: COLS-UNMAP ( ptr ptr u8 ptr n n -- ) {: rows:ptr lens:ptr cap:n :}
   rows cap cells munmap {: rowsrc:n :}
   lens cap cells munmap {: lensrc:n :}
   rowsrc 0 < lensrc 0 < or if
      s" require: cannot release the row table" INCLUDE-DIE
   then ;

\ Both columns move to fresh mappings of CAP cells; each live cell is copied
\ as its own type before the old pair is released.
: ROW-GROW ( n -- ) {: need:n :}
   need ROW-LIMIT > if SIZE-DIE then
   need ROW-NEW-CAP {: cap:n :}
   ROWS @ ROW-LENS @ ROW-CAP @ {: old:ptr oldlens:ptr oldcap:n :}
   cap COL-MAP ROWS !
   cap COL-MAP ROW-LENS !
   cap ROW-CAP !
   oldcap 0 > if
      oldcap 0 ?do
         old i ptr-field @ ROWS @ i ptr-field !
         oldlens i cells + @ ROW-LENS @ i cells + !
      loop
      old oldlens oldcap COLS-UNMAP
   then ;

: ROW-ENSURE ( n -- ) {: need:n :}
   need ROW-LIMIT > if SIZE-DIE then
   need ROW-CAP @ > if need ROW-GROW then ;

: ROWS-RELEASE ( -- )
   ROW-CAP @ 0= if exit then
   ROWS @ ROW-LENS @ ROW-CAP @ COLS-UNMAP
   NULL-PTR ROWS !
   NULL-PTR ROW-LENS !
   0 ROW-CAP ! ;

: CHUNK-PREV@ ( ptr ptr u8 -- ptr ptr u8 )
   0 ptr-field @ ;

: CHUNK-ROOM@ ( ptr ptr u8 -- n )
   CHUNK-COUNTS CELL + @ ;

: CHUNK-USED@ ( ptr ptr u8 -- n )
   CHUNK-COUNTS 2 cells + @ ;

: CHUNK-USED! ( n ptr ptr u8 -- )
   CHUNK-COUNTS 2 cells + ! ;

: CHUNK-DATA ( ptr ptr u8 -- ptr u8 )
   CHUNK-COUNTS byte-view CHUNK-HEAD + ;

: CHUNK-BYTES ( ptr ptr u8 -- n )
   CHUNK-ROOM@ CHUNK-HEAD + ;

: CHUNK-NEW-ROOM ( n -- n ) {: u:n :}
   CHUNK @ NULL-PTR = if u $1000 max exit then
   CHUNK @ CHUNK-ROOM@ {: old:n :}
   old CHUNK-LIMIT 2 / <= if old 2 * u max else u then ;

: CHUNK-NEW ( n -- )
   CHUNK-NEW-ROOM {: room:n :}
   room CHUNK-LIMIT > if SIZE-DIE then
   CHUNK @ {: prev:ptr :}
   room CHUNK-HEAD + map-anon 0 <> if
      drop s" require: cannot map a path chunk" INCLUDE-DIE
   then
   {: fresh:ptr :}
   fresh CHUNK !
   prev CHUNK @ 0 ptr-field !
   room CHUNK @ CHUNK-COUNTS CELL + !
   0 CHUNK @ CHUNK-USED! ;

: CHUNK-SPACE ( n -- ptr u8 ) {: u:n :}
   CHUNK @ NULL-PTR = if
      u CHUNK-NEW
   else
      u CHUNK @ CHUNK-ROOM@ CHUNK @ CHUNK-USED@ - > if u CHUNK-NEW then
   then
   CHUNK @ {: chunk:ptr :}
   chunk CHUNK-USED@ {: used:n :}
   used u + chunk CHUNK-USED!
   chunk CHUNK-DATA used + ;

: CHUNKS-RELEASE ( -- )
   CHUNK @ begin dup NULL-PTR <> while
      dup CHUNK-PREV@ swap dup CHUNK-BYTES munmap 0 < if
         s" require: cannot release a path chunk" INCLUDE-DIE
      then
   repeat drop
   NULL-PTR CHUNK ! ;

: CHUNK-CONTAINS? ( ptr ptr u8 ptr u8 -- bool ) {: chunk:ptr path:ptr :}
   path chunk CHUNK-DATA - {: off:n :}
   off 0 >= off chunk CHUNK-ROOM@ < and ;

: CHUNK-TRIM-TO ( ptr ptr u8 ptr u8 n -- ) {: chunk:ptr path:ptr u:n :}
   path chunk CHUNK-DATA - u + chunk CHUNK-USED!
   chunk CHUNK ! ;

: CHUNKS-TRIM ( ptr u8 n -- ) {: path:ptr u:n :}
   CHUNK @ begin dup NULL-PTR <> while
      dup path CHUNK-CONTAINS? if path u CHUNK-TRIM-TO exit then
      dup CHUNK-PREV@ swap dup CHUNK-BYTES munmap 0 < if
         s" require: cannot release a path chunk" INCLUDE-DIE
      then
   repeat drop
   s" require: path chunk missing" INCLUDE-DIE ;

: ROUND-CELL ( n -- n ) {: bytes:n :}
   bytes CELL mod {: rem:n :}
   rem 0= if bytes exit then
   bytes CELL rem - + ;

: CLAMP ( -- )
   PREFIX-N @ REQUIRE-N @ > if REQUIRE-N @ PREFIX-N ! then ;

: LIVE-BYTES ( -- n )
   0 REQUIRE-N @ 0 ?do
      i PREFIX-N @ < if i FROZEN-LEN else i ROW-LEN@ then {: u:n :}
      {: used:n :}
      u MAX-BYTES used - > if SIZE-DIE then
      used u +
   loop ;

: PACK-BYTES ( n -- n ) {: used:n :}
   REQUIRE-N @ ROW-LIMIT > if SIZE-DIE then
   REQUIRE-N @ FROZEN-BYTES {: recs:n :}
   used MAX-BYTES recs - > if SIZE-DIE then
   recs used + ;

: PAD-BYTES ( ptr u8 -- n )
   data-base - negate CELL 1- and ;

: TOTAL-BYTES ( n n -- n ) {: bytes:n pad:n :}
   bytes MAX-BYTES CELL 1- - pad - > if SIZE-DIE then
   pad bytes + ROUND-CELL ;

public

: SLOT ( n -- ptr u8 ) {: idx:n :}
   idx PREFIX-N @ < if idx FROZEN-SLOT else idx ROW-PTR@ then ;

: LEN ( n -- n ) {: idx:n :}
   idx PREFIX-N @ < if idx FROZEN-LEN else idx ROW-LEN@ then ;

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= u INCLUDE-PATH-CAP > or if
      s" require: invalid path length" INCLUDE-DIE
   then
   REQUIRE-N @ {: idx:n :}
   idx ROW-LIMIT >= if SIZE-DIE then
   idx 1+ ROW-ENSURE
   u CHUNK-SPACE {: path:ptr :}
   a path u BYTE-COPY
   path idx ROW-PTR !
   u idx ROW-LEN-ADDR !
   idx 1+ REQUIRE-N ! ;

: REWIND ( n -- ) {: keep:n :}
   keep PREFIX-N @ <= if
      CHUNKS-RELEASE
   else
      keep 1- dup ROW-PTR@ swap ROW-LEN@ CHUNKS-TRIM
   then
   keep REQUIRE-N !
   CLAMP ;

\ Evaluation rollback removes its own rows, including nested requires. The
\ core-prefix rewind uses this same seam before capture. Four other cells are
\ cursors into rows; a bare REQUIRE-N ! would leave them past the new end.
: TRUNCATE ( n -- )
   {: keep:n :}
   keep 0 < keep REQUIRE-N @ > or if
      s" require: truncate outside the registry" INCLUDE-DIE
   then
   keep REWIND
   REQUIRE-BOOT-N @ keep > if keep REQUIRE-BOOT-N ! then
   REQUIRE-BASE @ keep > if keep REQUIRE-BASE ! then
   REQUIRE-SAVE-N @ keep > if keep REQUIRE-SAVE-N ! then
   REQUIRE-SAVE-BASE @ keep > if keep REQUIRE-SAVE-BASE ! then ;

\ A core-prefix rewind can retire the registry beneath an evaluation mark.
\ Its epoch distinguishes that deliberate cut from a malformed rollback mark;
\ the cut is the surviving prefix to which the interrupted evaluation returns.
: MARK ( -- n n ) REQUIRE-N @ PREFIX-EPOCH @ ;

: ROLLBACK ( n n -- )
   {: mark:n epoch:n :}
   epoch PREFIX-EPOCH @ <> if
      mark PREFIX-CUT @ min
   else
      mark
   then
   TRUNCATE ;

: PREFIX-REWIND ( n -- )
   TRUNCATE
   REQUIRE-N @ PREFIX-CUT !
   1 PREFIX-EPOCH +! ;

\ A failed require may have loaded a child in a reader or unit callback before
\ entering evaluation. Those child definitions survive that failure; remove
\ only the parent's provisional row and keep later rows in order.
: REMOVE ( n -- )
   {: idx:n :}
   idx PREFIX-N @ < idx REQUIRE-N @ >= or if
      s" require: remove outside live rows" INCLUDE-DIE
   then
   idx REQUIRE-N @ 1- = if
      idx REWIND
   else
      REQUIRE-N @ idx 1+ ?do
         i ROW-PTR@ i 1- ROW-PTR !
         i ROW-LEN@ i 1- ROW-LEN-ADDR !
      loop
      -1 REQUIRE-N +!
   then
   REQUIRE-BOOT-N @ idx > if -1 REQUIRE-BOOT-N +! then
   REQUIRE-BASE @ idx > if -1 REQUIRE-BASE +! then
   REQUIRE-SAVE-N @ idx > if -1 REQUIRE-SAVE-N +! then
   REQUIRE-SAVE-BASE @ idx > if -1 REQUIRE-SAVE-BASE +! then ;

\ The caller's provisional row may already have died in a core-prefix rewind.
\ A later row at the same index belongs to the new prefix epoch.
: REMOVE-PROVISIONAL ( n n -- )
   {: row:n epoch:n :}
   epoch PREFIX-EPOCH @ <> row PREFIX-CUT @ >= and if exit then
   row REMOVE ;

\ Capture packs the live rows once; no process pointer enters the DATA block.
\ An unchanged or discarded suffix keeps the prior block and DATA extent.
: PERSIST ( -- )
   REQUIRE-N @ PREFIX-N @ <= if
      CHUNKS-RELEASE
      ROWS-RELEASE
      CLAMP
      REQUIRE-N @ 0= if NULL-PTR FROZEN ! then
      exit
   then
   LIVE-BYTES PACK-BYTES {: bytes:n :}
   here {: start:ptr :}
   start PAD-BYTES {: pad:n :}
   bytes pad TOTAL-BYTES {: total:n :}
   total allot
   start pad + {: fresh:ptr :}
   REQUIRE-N @ FROZEN-BYTES
   REQUIRE-N @ 0 ?do
      {: cursor:n :}
      i LEN {: u:n :}
      i SLOT fresh cursor + u BYTE-COPY
      cursor fresh i 2 * cells + CELL-VIEW !
      u fresh i 2 * 1+ cells + CELL-VIEW !
      cursor u +
   loop drop
   CHUNKS-RELEASE
   ROWS-RELEASE
   fresh FROZEN !
   REQUIRE-N @ PREFIX-N ! ;

\ The cells this registry writes, each handed to keep with its byte extent: its
\ own eight, the row count, and the four cursors TRUNCATE and REMOVE keep inside
\ the rows. The rows and chunks are mapped, never DATA, and PERSIST's block is
\ allotted fresh, so no other span is written.
: CELL-SPANS ( [ ptr u8 n -- ] -- ) {: keep :}
   FROZEN BYTE-VIEW CELL keep execute
   ROWS BYTE-VIEW CELL keep execute
   ROW-LENS BYTE-VIEW CELL keep execute
   ROW-CAP BYTE-VIEW CELL keep execute
   CHUNK BYTE-VIEW CELL keep execute
   PREFIX-N BYTE-VIEW CELL keep execute
   PREFIX-EPOCH BYTE-VIEW CELL keep execute
   PREFIX-CUT BYTE-VIEW CELL keep execute
   REQUIRE-N BYTE-VIEW CELL keep execute
   REQUIRE-BOOT-N BYTE-VIEW CELL keep execute
   REQUIRE-BASE BYTE-VIEW CELL keep execute
   REQUIRE-SAVE-N BYTE-VIEW CELL keep execute
   REQUIRE-SAVE-BASE BYTE-VIEW CELL keep execute ;

;package

: REQUIRE-LEN@ ( n -- n )
   REQUIRE-REG:LEN ;

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

package SOURCE-ROOT
private

: LOADED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u PROGRAM? if INCLUDE-TRUE exit then
   a u REQUIRE-KNOWN? ;

\ The engine's own source root: the working directory when it carries the
\ engine's first row; else the executable's tree when that does (`%`, two levels
\ above <tree>/bin/hb, as SwiftForth places it); else the working directory.
\ The working directory comes first because a stdin session's requires search it
\ before the root and only the root's physical copy of a row is frozen: with `%`
\ first, a copied tree there would load as application source. A build's fresh
\ registry holds no row, so its rows are relative to the working directory.
\ Derived on first need from the executable's path and realpath, not through
\ SOURCE-INPUT: it is a fact of this process. Deriving touches only JOIN, WORK,
\ Z and CANON scratch, so NORMAL-BUF and CANDIDATE-BUF survive.

\ The running engine's path: on macOS the one execve received, in the
\ executable_path= apple string past envp's terminator; on Linux
\ /proc/self/exe. Both name the engine, not the script, for a `#!` program
\ file. src/lib/engine-id.f asks proc_pidpath through FUNCTION:, which this layer
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
: EXE-ROOT ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= u INCLUDE-PATH-CAP > or if 0 exit then
   a u CANON-N {: n:n :}
   n 0 <= if 0 exit then
   CANON-BUF CANON-BUF n PARENT-U PARENT-U ;

\ Whether a directory carries the engine's first row: one access, no read.
: TREE? ( ptr u8 n -- bool )
   0 REQUIRE-SLOT 0 REQUIRE-LEN@ JOIN ZBUF WORK-BYTES PATHZ
   ZBUF 0 access 0= ;

\ The root's length in CANON-BUF, 0 for the working directory. The same
\ CWD-first rule applies when a tool selects another executable as its child.
: ENGINE-FIND-FOR ( ptr u8 n -- n )
   REQUIRE-BOOT-LIMIT 0= if 2drop 0 exit then
   CWD$ TREE? if 2drop 0 exit then
   EXE-ROOT {: n:n :}
   n 0= if 0 exit then
   CANON-BUF n TREE? 0= if 0 exit then
   n ;

: ENGINE-FIND ( -- n )
   HB-TARGET-MACOS? if APPLE-EXE else s" /proc/self/exe" then
   ENGINE-FIND-FOR ;

: ENGINE-INIT ( -- )
   ENGINE-READ @ 0= 0= if exit then
   ENGINE-FIND {: n:n :}
   CANON-BUF ENGINE-BUF n BYTE-COPY
   n ENGINE-U !
   1 ENGINE-READ ! ;

public

: ENGINE$ ( -- ptr u8 n )
   ENGINE-INIT
   ENGINE-U @ 0= if CWD$ exit then
   ENGINE-BUF ENGINE-U @ ;

\ A selected child executable uses this same fallback rule after it starts.
\ CANON-BUF is scratch; callers copy the answer before resolving another path.
: ENGINE-FOR$ ( ptr u8 n -- ptr u8 n )
   ENGINE-FIND-FOR dup 0= if drop CWD$ exit then
   CANON-BUF swap ;

\ Override the fallback root; the caller restores the old one after its load
\ (tools/check-verify-child.f).
: ENGINE! ( ptr u8 n -- )
   ROOT-CANON
   CANON-BUF ENGINE-BUF CANON-U @ BYTE-COPY
   CANON-U @ ENGINE-U !
   1 ENGINE-READ ! ;

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
   2dup LOADED? {: known:bool :}
   known 0= if
      ENGINE$ REQUEST$ first BOOT-CANDIDATE
   else known then
   dup exists or ;

\ A relative request's roots in order, each once: its owner, the engine root,
\ then the working directory. The first root whose candidate stops the search
\ answers; when none does, the owner's answer names the missing file. So a
\ library the owner lacks is the engine's whatever the working directory holds,
\ and the working directory answers only what neither holds: an application's
\ entry below its tree, run from the tree, requires the tree's files by their
\ tree-relative names.
: SEARCH-ROOTS ( n -- ptr u8 n bool )
   {: first:n :}
   CURRENT$ first CANDIDATE if exit then drop 2drop
   ENGINE$ CURRENT$ CORE-STR= 0= if
      ENGINE$ first CANDIDATE if exit then drop 2drop
   then
   CWD$ CURRENT$ CORE-STR= CWD$ ENGINE$ CORE-STR= or 0= if
      CWD$ first CANDIDATE if exit then drop 2drop
   then
   CURRENT$ first CANDIDATE drop ;

\ The root a load of an absolute path opens: the first root it lies below, in
\ SEARCH-ROOTS order, else its directory. A missing file's directory need not
\ be a root at all (missing, a file, unsearchable), so its load keeps the
\ current root and INCLUDE-OPEN names the file, as ENTRY-RESOLVE does for an
\ entry.
: ABS-OWNER ( ptr u8 n bool -- )
   {: a:ptr u:n exists:bool :}
   a u CURRENT$ BELOW? if CURRENT$ OWNER! exit then
   a u ENGINE$ BELOW? if ENGINE$ OWNER! exit then
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
      2dup LOADED? {: known:bool :}
      known 0= if
         ENGINE$ REQUEST$ first BOOT-CANDIDATE
      else known then
      exit
   then
   first SEARCH-ROOTS ;

public

: RESOLVE ( ptr u8 n -- ptr u8 n bool )
   REQUEST! REQUIRE-BASE @ SELECT
   >r CONFINE r> ;

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
   path pathu CONFINE known ;

\ Every boot row is portable: each prefix that records one opens the registry
\ first, with REQUIRE-BOOT-OPEN as a source token (src/habu/habu2.f
\ EMIT-REQUIRE-BOOT-OPEN-TOKEN) or literally (tools/hb-build-core.f END-WRITE),
\ so BOOT-CANDIDATE answers for every engine kind. A canonical scan beside it
\ would answer for no engine and hide a prefix that does not open the registry. The selection counts every frozen row whatever
\ REQUIRE-BASE is, so a frozen row is the engine's even where its file is
\ missing. RESOLVED-ROOT$ stays as the caller's own RESOLVE left it.
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

\ A boot row is recorded portably, in the one spelling BOOT-KNOWN? asks for,
\ relative to the engine root (SOURCE-ROOT:BOOT-ROW); a build's fresh registry
\ makes that root the working directory. Against the load's own owner root a
\ require issued by a boot file would shorten to a bare basename no query spells,
\ naming a different file in every directory. A canonical absolute spelling
\ would bake the build directory into the engine, so two builds of one revision
\ from two directories would differ. An application row keeps its canonical
\ identity; the two cannot collide, as a canonical name begins with `/` and a
\ portable one never does. The path comes back in storage the store did not
\ reuse, for the caller's load.
: REQUIRE-STORE ( ptr u8 n -- ptr u8 n )
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

: INCLUDE-PATH0 ( ptr u8 n -- ptr u8 )
   {: a:ptr u:n :}
   a u INCLUDE-CHECK-PATH 2drop
   a u INCLUDE-PATH INCLUDE-PATH-CAP 1+ PATHZ
   u INCLUDE-PATH-U !
   INCLUDE-PATH ;

\ Each active load owns its source bytes until evaluation returns. A linked
\ mapping keeps parent buffers stable without imposing a nesting limit.
variable SCRIPT-NAMED-PEND

: SCRIPT-NAMED-PEND! ( bool -- )
   SCRIPT-NAMED-PEND ! ;


\ A failed open names the resolved path INCLUDE-PATH holds: it is the diagnostic
\ a mistyped `require` or `bin/hb --load` argument gets.
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

\ Generated source (TYPE-DECL:TDECL-EVAL-XT) runs as a closed program, refused
\ E-EVAL-RESIDUE for what it leaves; files run in LOAD-UNIT (RESIDUE-CHECK).
: INCLUDE-EVALUATE ( ptr u8 n -- )
   REQUIRE-REG:MARK {: mark:n epoch:n :}
   [: 2dup evaluate-closed ;] catch {: rc:n :}
   2drop
   rc 0 <> if mark epoch REQUIRE-REG:ROLLBACK rc throw then ;

\ Once REQUIRE-REG is ready, generated declarations also roll back its rows
\ on failure. Stage builders without include.f leave the evaluator unarmed.
\ The `is` target is qualified because parsing words do not resolve through
\ using-imports (bare `is TDECL-EVAL-XT` was measured to fail with rc 70).
using TYPE-DECL

package INCLUDE-EVAL-BIND
private
: INSTALL ( -- ) [: INCLUDE-EVALUATE ;] is TYPE-DECL:TDECL-EVAL-XT ;
INSTALL
;package
;using

\ A source load is also the publication boundary for an explicitly selected
\ package unit. The continuation keeps the ordinary frame and scope behavior;
\ callers may bind a unit importer after the complete loader has been loaded.
package SOURCE-UNIT
private

TYPED-VARIABLE LOAD-XT [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]

: ORDINARY ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   0 sourceu q execute ;

public

variable GEN
: LOAD ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   1 GEN +! LOAD-XT @ execute ;
\ A runner calls the file's quotation as `0 sourceu q execute` with nothing of
\ its own beneath it; the quotation throws for a refused file.
: USE ( [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ] -- ) LOAD-XT ! ;
: CURRENT ( -- [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ] ) LOAD-XT @ ;
: RESET ( -- ) [: ORDINARY ;] LOAD-XT ! ;

\ Run one source quotation with the engine's unit hook armed: the guard sees
\ every token the interpreter dispatches and may refuse it or, at the parsed
\ package name, skip the body. The engine refuses a busy hook or a quotation
\ outside live code with 70 before the source runs.
: GUARDED ( [ ptr u8 n n n -- n ] [ -- ] -- n ) unit-compile-run ;

;package

SOURCE-UNIT:RESET

\ Source-composition event log: one event per loader act, the request as spelled
\ and the path it resolved to, so a restricted discovery pass can rebuild include
\ multiplicity and require/provided registry state in order, and a build's
\ linker can carry the acts in a file's object. Recorded only while EVENT-ON?.
\ During discovery the walker runs DISCOVERY-ON and passes the loader-token span
\ to DISC-TOK!; a real load reads TKA-CELL and TKL-CELL. A build's linker reads
\ the depth to tell a request made with cells on the stack.

11 constant EVENT-FIELDS
EVENT-FIELDS cells constant EVENT-BYTES          \ one record
$1000 constant EVENT-ROOM-MIN                    \ a first mapping's bytes
$4D constant INCLUDE-EVENT-RC

0 constant EV-INCLUDED
1 constant EV-REQUIRED
2 constant EV-PROVIDED
0 constant EV-STATE-FRESH
1 constant EV-STATE-KNOWN

\ The records, and the pool that holds each event's paths and, unless an
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

: EVENT-RECORD ( ptr u8 n ptr u8 n n n -- )
   {: a:ptr u:n path:ptr pathu:n kd:n st:n :}
   EVENT-ON? 0= if exit then
   EVENT-RECS-ROOM
   EVENT-N @ {: ix:n :}
   EVENT-SPAN {: toka:n toku:n :}
   path pathu EVENT-COPY-PATH {: off:n len:n :}
   a u EVENT-COPY-PATH {: soff:n slen:n :}
   kd    ix 0 EVENT-FIELD!
   off   ix 1 EVENT-FIELD!
   len   ix 2 EVENT-FIELD!
   toka  ix 3 EVENT-FIELD!
   toku  ix 4 EVENT-FIELD!
   st    ix 5 EVENT-FIELD!
   soff  ix 8 EVENT-FIELD!
   slen  ix 9 EVENT-FIELD!
   depth ix 10 EVENT-FIELD!     \ the callers hold their arguments in locals: the requester's depth
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
: EVENT-SPELLED@ ( n -- ptr u8 n ) {: ix:n :}
   ix 8 EVENT-FIELD@ EVENT-POOL-AT
   ix 9 EVENT-FIELD@ ;
: EVENT-TOK@ ( n -- n n ) {: ix:n :}
   ix 3 EVENT-FIELD@ ix 4 EVENT-FIELD@ ;
: EVENT-DEPTH@ ( n -- n )    10 EVENT-FIELD@ ;

package SOURCE-ROOT
private

\ A frame carries its own resolved path: a nested include overwrites
\ INCLUDE-PATH before the inner file is read, so only the frame names the outer
\ file to an inner refusal or a POP. The file's bytes follow the header in the
\ same mapping, sized to the file (GROW): FR-ROOM counts the source bytes it can
\ hold, FR-SIZE the file's.
0 constant FR-PREV
CELL constant FR-NAMED
2 cells constant FR-PATHLEN
3 cells constant FR-ROOM
4 cells constant FR-SIZE
5 cells constant FR-ROOTLEN
6 cells constant FR-OWNER
7 cells constant FR-EXEC-COPY
8 cells constant FR-EVAL-OK
9 cells constant FR-EVAL-PTR
10 cells constant FR-EVAL-SIZE
11 cells constant FR-EVAL-FIRST
12 cells constant FR-EVAL-END
\ How many root programs (RUN-ROOT) are being read while this frame is the top.
13 cells constant FR-ROOT-RUNS
\ The file's stamp (ACTIVE-STAMP): change time seconds, nanoseconds and size.
14 cells constant FR-CTIME-SEC
15 cells constant FR-CTIME-NS
16 cells constant FR-STAMP-SIZE
17 cells constant FR-PATH
FR-PATH INCLUDE-PATH-CAP + 1 cells + constant FR-ROOT
FR-ROOT INCLUDE-PATH-CAP + 1 cells + constant HEADER-BYTES
$1000 constant ROOM-MIN
\ A frame's FR-PREV, FR-OWNER, FR-EXEC-COPY and FR-EVAL-PTR cells hold
\ addresses; its other header cells hold counts, flags and stamps, and its bytes
\ follow them.
CAST: FRAME-CELLS ( ptr u8 -- ptr ptr u8 )
: FRAME-PREV ( ptr u8 -- ptr ptr u8 ) FRAME-CELLS FR-PREV CELL / ptr-field ;
: FRAME-OWNER ( ptr u8 -- ptr ptr u8 ) FRAME-CELLS FR-OWNER CELL / ptr-field ;
: FRAME-EXEC-COPY ( ptr u8 -- ptr ptr u8 )
   FRAME-CELLS FR-EXEC-COPY CELL / ptr-field ;
: FRAME-EVAL-PTR ( ptr u8 -- ptr ptr u8 )
   FRAME-CELLS FR-EVAL-PTR CELL / ptr-field ;
PTR-VARIABLE RANGE-OWNER
variable LAST-LOAD-EVAL-OK

: TOP@ ( -- ptr u8 ) TOP @ ;
: TOP! ( ptr u8 -- ) TOP ! ;

: CHECK-ACTIVE ( -- )
   TOP@ 0= if s" include: depth underflow" INCLUDE-DIE then ;

: FRAME-EXEC-SOURCE ( ptr u8 -- ptr u8 )
   {: frame:ptr :}
   frame FRAME-EXEC-COPY @ dup 0= if
      drop frame HEADER-BYTES +
   then ;

\ The engine's refusal tail prints ` at <path>:<line>` from these two cells and
\ nothing else decides when they are right: PUBLISH-LOCATION republishes them
\ when the top frame, its size, execution copy or root-run count changes, so
\ they name the file being read, or 0 for none (the REPL, a stdin session, the
\ boot prefix, a root program run over an open file: FR-ROOT-RUNS).
: PUBLISH-NONE ( -- )
   0 data-base INCLUDE-SRCLOC-PATH-CELL + !
   0 data-base INCLUDE-SRCLOC-PATHLEN-CELL + !
   0 data-base INCLUDE-SRCLOC-OWNED-CELL + !
   0 data-base INCLUDE-SRCLOC-OWNEDLEN-CELL + ! ;

: PUBLISH-LOCATION ( -- )
   TOP@ 0= if PUBLISH-NONE exit then
   TOP@ {: frame:ptr :}
   frame FR-ROOT-RUNS + CELL-VIEW @ 0 > if PUBLISH-NONE exit then
   frame FR-PATH + NULL-PTR - data-base INCLUDE-SRCLOC-PATH-CELL + !
   frame FR-PATHLEN + CELL-VIEW @ data-base INCLUDE-SRCLOC-PATHLEN-CELL + !
   frame FR-SIZE + CELL-VIEW @ dup 0 < if
      drop 0 data-base INCLUDE-SRCLOC-OWNED-CELL + !
      0 data-base INCLUDE-SRCLOC-OWNEDLEN-CELL + !
      exit
   then
   data-base INCLUDE-SRCLOC-OWNEDLEN-CELL + !
   frame FRAME-EXEC-SOURCE NULL-PTR -
      data-base INCLUDE-SRCLOC-OWNED-CELL + ! ;

: MAPPED ( ptr u8 -- n ) FR-ROOM + CELL-VIEW @ HEADER-BYTES + ;

: ROOM ( -- n ) TOP@ FR-ROOM + CELL-VIEW @ ;

: PUSH ( -- )
   HEADER-BYTES ROOM-MIN + map-anon 0= 0= if drop s" include: cannot map a source frame" INCLUDE-DIE then
   {: frame:ptr :}
   TOP@ frame FRAME-PREV !
   RANGE-OWNER @ frame FRAME-OWNER !
   SCRIPT-NAMED-PEND @ frame FR-NAMED + c!
   INCLUDE-PATH-U @ frame FR-PATHLEN + CELL-VIEW !
   ROOM-MIN frame FR-ROOM + CELL-VIEW !
   -1 frame FR-SIZE + CELL-VIEW !
   NULL-PTR frame FRAME-EXEC-COPY !
   0 frame FR-EVAL-OK + CELL-VIEW !
   0 frame FR-ROOT-RUNS + CELL-VIEW !
   0 frame FR-CTIME-SEC + CELL-VIEW !
   0 frame FR-CTIME-NS + CELL-VIEW !
   -1 frame FR-STAMP-SIZE + CELL-VIEW !
   INCLUDE-PATH frame FR-PATH + INCLUDE-PATH-U @ BYTE-COPY
   CURRENT$ {: root:ptr rootu:n :}
   rootu frame FR-ROOTLEN + CELL-VIEW !
   root frame FR-ROOT + rootu BYTE-COPY
   frame TOP!
   INCLUDE-FALSE SCRIPT-NAMED-PEND!
   PUBLISH-LOCATION ;

: POP ( -- )
   CHECK-ACTIVE
   TOP@ {: frame:ptr :}
   frame FRAME-OWNER @ RANGE-OWNER !
   frame FRAME-EXEC-COPY @ {: copy:ptr :}
   frame FR-SIZE + CELL-VIEW @ 1 max {: copyu:n :}
   frame FRAME-PREV @ TOP!
   PUBLISH-LOCATION
   copy 0= if 0 else copy copyu munmap then {: copyrc:n :}
   frame dup MAPPED munmap 0 < copyrc 0 < or if
      s" include: cannot release a source frame" INCLUDE-DIE then ;

: SOURCE ( -- ptr u8 ) CHECK-ACTIVE TOP@ HEADER-BYTES + ;

\ The open frame learns its file's size by filling, so it grows as src/lib/pg.f's
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

\ A stat64 buffer of this file's own: src/lib/fs.f, which owns the per-task
\ one, loads after it.
create STAT-BUF FS-ABI:STAT-BYTES allot

: STAT-CELL@ ( n -- n ) STAT-BUF + CELL-VIEW @ ;

: STAT-REGULAR? ( -- bool )
   STAT-BUF FS-ABI:STAT-MODE-OFF + {: m:ptr :}
   m c@  m 1 + c@ 256 * or  FS-ABI:S-IFMT and  FS-ABI:S-IFREG = ;

\ The open frame's stamp, from a stat of the path taken before the file is
\ opened, so a change during the read leaves a stamp no later load matches. A
\ path that is not a regular file keeps 0 0 -1: its bytes have no stamp.
: STAMP! ( ptr u8 -- )
   STAT-BUF stat64 0 < if exit then
   STAT-REGULAR? 0= if exit then
   TOP@ {: frame:ptr :}
   FS-ABI:STAT-CTIME-SEC-OFF STAT-CELL@ frame FR-CTIME-SEC + CELL-VIEW !
   FS-ABI:STAT-CTIME-NS-OFF STAT-CELL@ frame FR-CTIME-NS + CELL-VIEW !
   FS-ABI:STAT-SIZE-OFF STAT-CELL@ frame FR-STAMP-SIZE + CELL-VIEW ! ;

\ Refuses PATH unless the top frame awaits its bytes (FR-SIZE -1 from PUSH until
\ HOLD). A word's read finds its own file's frame held, or no frame; in a build,
\ hb's held frame.
: READ-REFUSE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TOP@ 0= 0= if TOP@ FR-SIZE + CELL-VIEW @ -1 = if exit then then
   s" include: READ-OS reads only the file a load is opening; a word read: "
   path pathu PATH-DIE ;

: INCLUDE-READ-ALL ( ptr u8 n -- ptr u8 n )
   2dup INCLUDE-PATH0 STAMP!
   INCLUDE-OPEN
   0 INCLUDE-U !
   begin INCLUDE-READ-DONE? 0= while repeat
   INCLUDE-CLOSE
   SOURCE INCLUDE-U @ ;

public

: ACTIVE-SOURCE ( -- ptr u8 n ptr u8 n ptr u8 n )
   CHECK-ACTIVE
   TOP@ {: frame:ptr :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @
   frame FR-ROOT + frame FR-ROOTLEN + CELL-VIEW @
   SOURCE frame FR-SIZE + CELL-VIEW @ ;

\ The active file's change time seconds, nanoseconds and size as STAMP! took
\ them; 0 0 -1 for bytes READ-OS did not read from a regular file, as
\ WITH-SUPPLIED's are.
: ACTIVE-STAMP ( -- n n n )
   CHECK-ACTIVE
   TOP@ {: frame:ptr :}
   frame FR-CTIME-SEC + CELL-VIEW @
   frame FR-CTIME-NS + CELL-VIEW @
   frame FR-STAMP-SIZE + CELL-VIEW @ ;

\ The loader's reader, public so a reader installed with SOURCE-INPUT:USE may
\ call it from READ-SOURCE; it fills the top frame only while that awaits its bytes.
: READ-OS ( ptr u8 n ptr u8 n -- ptr u8 n )
   2drop 2dup READ-REFUSE INCLUDE-READ-ALL ;

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

\ The loop a loaded file's bytes go through, inside LOAD-UNIT's source-unit-run:
\ the owner binds its native evaluator, and test/outer-loop-on.f binds the loop
\ written in Habu through its framed entry, src/habu/interpret.f
\ OUTER:EVAL-FRAMED-RANGE, for the files loaded after it. A file loaded under
\ the design seal does not go through it (INTERPRET-SAVED-RANGE).
private

defer INCLUDE-INTERPRET ( ptr u8 n n n -- )

\ The design seal is read where the engine's loop reads a token, and the loop
\ written in Habu reads it nowhere, so a file loaded once the seal is set is read
\ by the engine's loop whatever INCLUDE-INTERPRET holds. `0 <>`, not `0<>`: the
\ prefix loads src/lib/prelude.f, which defines `0<>`, after this file.
: POLICY-SEALED? ( -- bool )
   data-base INCLUDE-POLICY-NDICT-CELL + @ 0 <> ;

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
   PUBLISH-LOCATION
   SOURCE u ;

: READ-SOURCE ( -- ptr u8 n )
   INCLUDE-PATH INCLUDE-PATH-U @ RESOLVED-ROOT$ SOURCE-INPUT:READ HOLD ;

\ The original bytes remain in the frame while a shebang is rewritten in a
\ private execution copy. Range positions continue to name the full source.
: EXEC-SOURCE ( -- ptr u8 )
   TOP@ FRAME-EXEC-SOURCE ;

\ A loaded program starts on an empty data stack. RUN-RANGE keeps the range in
\ its source frame across catch; copied arguments beneath evaluate-range would
\ let an underflow consume loader cells and turn it into E-EVAL-RESIDUE instead.
\ The sealed design reads every file through the engine's loop.
: INTERPRET-SAVED-RANGE ( -- )
   TOP@ {: frame:ptr :}
   frame FRAME-EVAL-PTR @
   frame FR-EVAL-SIZE + CELL-VIEW @
   frame FR-EVAL-FIRST + CELL-VIEW @
   frame FR-EVAL-END + CELL-VIEW @
   POLICY-SEALED? if evaluate-range else INCLUDE-INTERPRET then ;

: CHECK-RANGE ( n n n -- ) {: size:n first:n end:n :}
   first 0 < first end > or end size > or if INCLUDE-RANGE-RC throw then ;

PTR-VARIABLE EVAL-A
variable EVAL-U
variable EVAL-FIRST
variable EVAL-END

: EVAL-STAGED ( -- )
   EVAL-A @ EVAL-U @ EVAL-FIRST @ EVAL-END @ evaluate-range ;
: INTERPRET-INSTALL ( -- ) [: evaluate-range ;] is INCLUDE-INTERPRET ;
INTERPRET-INSTALL

public

: USE-INTERPRETER ( [ ptr u8 n n n -- ] -- ) is INCLUDE-INTERPRET ;
: RUN-CLOSED ( [ -- ] -- ) source-unit-run ;

private

: KEEP-CLOSED ( [ -- ] -- [ -- ] ) {: q :} q RUN-CLOSED q ;

: CLOSE-RANGE ( ptr u8 n n n [ -- ] -- )
   {: source:ptr size:n first:n end:n q :}
   size first end CHECK-RANGE
   EVAL-A @ EVAL-U @ EVAL-FIRST @ EVAL-END @
      {: old:ptr oldu:n oldfirst:n oldend:n :}
   source EVAL-A ! size EVAL-U ! first EVAL-FIRST ! end EVAL-END !
   REQUIRE-REG:MARK {: mark:n epoch:n :}
   q [: KEEP-CLOSED ;] catch {: rc:n :} drop
   old EVAL-A ! oldu EVAL-U ! oldfirst EVAL-FIRST ! oldend EVAL-END !
   rc 0 <> if mark epoch REQUIRE-REG:ROLLBACK rc throw then ;

public

\ Direct renderers own a closed program. Loader fragments run open (RUN-RANGE)
\ inside LOAD-UNIT's whole-program boundary, so values can cross fragments.
: RUN-EVAL-RANGE ( ptr u8 n n n -- ) [: EVAL-STAGED ;] CLOSE-RANGE ;

\ The process's root stream (stdin, or the `--load` or `--build` stream
\ src/habu/main.f builds) runs open, as the engine's own loop runs it: it may
\ leave cells on the stack and the process still exits 0. The files it loads
\ keep their own closed units (LOAD-UNIT); the program file of `hb prog.f` runs
\ in a frame of its own (RUN-PROGRAM). The stream's text is no file's, so a refusal in it
\ names none: run while a file is open (a harness calling src/habu/main.f), it
\ counts itself on that file's frame, which then publishes no location until
\ the stream returns or throws.
: ROOT-RUNS+ ( n -- ) {: k:n :}
   TOP@ 0= if exit then
   TOP@ {: frame:ptr :}
   frame FR-ROOT-RUNS + CELL-VIEW @ k + frame FR-ROOT-RUNS + CELL-VIEW !
   PUBLISH-LOCATION ;

: RUN-ROOT ( ptr u8 n -- ) {: a:ptr u:n :}
   1 ROOT-RUNS+
   a u 0 u [: INCLUDE-INTERPRET ;] [: -1 ROOT-RUNS+ ;] finally ;

private

: LOAD-BYTES ( -- )
   READ-SOURCE {: a:ptr u:n :}
   u 2 >= if
      a c@ $23 = a 1 + c@ $21 = and if
         u 1 max map-anon 0= 0= if drop INCLUDE-IO-RC throw then
         {: copy:ptr :}
         a copy u BYTE-COPY
         copy u SHEBANG-COMMENT
         copy TOP@ FRAME-EXEC-COPY !
         PUBLISH-LOCATION
      then
   then ;

\ Evaluates one fragment of the current file through the bound loop. A failed
\ fragment rolls back the require rows it added whichever loop read it; the
\ residue check (CALL-UNIT) stays outside, on the unit's stack.
: RUN-RANGE ( n n -- ) {: first:n end:n :}
   RANGE-OWNER @ 0= if INCLUDE-RANGE-RC throw then
   CHECK-ACTIVE
   TOP@ RANGE-OWNER @ <> if INCLUDE-RANGE-RC throw then
   TOP@ {: frame:ptr :}
   frame FR-SIZE + CELL-VIEW @ {: size:n :}
   size first end CHECK-RANGE
   EXEC-SOURCE frame FRAME-EVAL-PTR !
   size frame FR-EVAL-SIZE + CELL-VIEW !
   first frame FR-EVAL-FIRST + CELL-VIEW !
   end frame FR-EVAL-END + CELL-VIEW !
   REQUIRE-REG:MARK {: mark:n epoch:n :}
   [: INTERPRET-SAVED-RANGE ;] catch {: rc:n :}
   rc 0 <> if mark epoch REQUIRE-REG:ROLLBACK rc throw then
   -1 frame FR-EVAL-OK + CELL-VIEW ! ;

create RESIDUE-DIGIT 1 allot

: RESIDUE-DEC ( n -- )
   dup 10 >= if dup 10 / recurse then
   10 mod 48 + RESIDUE-DIGIT c!  2 RESIDUE-DIGIT 1 write drop ;

\ A nested file's refusal throws out of its loader's run before the loader
\ reaches its own check, so only the innermost file is named.
: RESIDUE-REFUSE ( n -- )
   2 s" E-EVAL-RESIDUE: stack depth " write drop
   RESIDUE-DEC
   2 s"  at " write drop
   TOP@ {: frame:ptr :}
   2 frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @ write drop
   2 POLICY-LF 1 write drop
   STACK-ABI:E-EVAL-RESIDUE throw ;

\ Run at the end of the unit, where depth is what the file left
\ (SOURCE-UNIT:USE).
: RESIDUE-CHECK ( -- )
   depth {: n:n :}
   n 0 > if n RESIDUE-REFUSE then ;

CAST: CLOSE-ACTION ( n -- [ -- ] )

\ The active checker owner closes the created body pending at a load boundary,
\ so the body's `,` grid stops there (src/core/checker.f CHECKER-BODY-CLOSE).
\ After a native build's handover that owner is the target's checker, which
\ holds the pending record. No owner, or one that predates the slot, has no
\ pending body.
: BODY-CLOSE ( -- )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ {: owner:ptr :}
   owner NULL-PTR = if exit then
   owner CELL - CELL-VIEW @ CHECKER-OWNER-ABI:BODY-CLOSE-OFF CELL + <
      if exit then
   owner CHECKER-OWNER-ABI:BODY-CLOSE-OFF + CELL-VIEW @ {: close:n :}
   close 0= if exit then
   close CLOSE-ACTION execute ;

\ A pending created body closes and the residue check runs inside the file's
\ unit, so a runner's call returns only for an accepted file.
: CALL-UNIT ( -- )
   TOP@ {: frame:ptr :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @
   frame FR-ROOT + frame FR-ROOTLEN + CELL-VIEW @
   EXEC-SOURCE frame FR-SIZE + CELL-VIEW @
   [: RUN-RANGE BODY-CLOSE RESIDUE-CHECK ;] SOURCE-UNIT:LOAD ;

\ A loaded file's DATA starts and ends on a cell, so its bytes are a function
\ of its own source and a stored object installs whatever earlier files
\ allotted. The start align precedes the cache's SEGMENT-OPEN for the file,
\ the end align the parent's SEGMENT-OPEN after the load. A throw skips the
\ end align, which does not matter: a load that threw refuses the parent's
\ object, so no stored segment continues the parent's data from there.
\ A created body the loader's file left pending closes before the loaded file
\ runs, so its `,` grid belongs to the loader's file.
: LOAD-UNIT ( -- )
   BODY-CLOSE
   LOAD-BYTES
   TOP@ RANGE-OWNER !
   align
   [: CALL-UNIT ;] source-file-run
   align ;

: LOAD-CURRENT ( -- )
   PUSH
   [: LOAD-UNIT ;] catch {: rc:n :}
   rc 0 <> if TOP@ FR-EVAL-OK + CELL-VIEW @ LAST-LOAD-EVAL-OK ! then
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

\ The row decision belongs to the whole load, including path resolution and
\ reader callbacks before PUSH. Its result is consumed before returning.
: LOAD-RAW ( ptr u8 n -- )
   CONFINE
   INCLUDE-PATH0 drop
   RESOLVED-ROOT$ [: LOAD-CURRENT ;] WITH ;

public

\ The program file `hb prog.f` runs (src/habu/main.f RUN), as argv names it, is
\ a loaded file without a registry row (PROGRAM? counts it as loaded): a frame
\ holds its canonical path, root and bytes, `#!` made a comment, so
\ ACTIVE-SOURCE reads it and a refusal names it. It runs open, as RUN-ROOT runs
\ its stream: the stack it leaves is kept, with no unit and no residue check.
\ No WITH scope opens, so CD stays legal in its text (TOP-ONLY) and the frame's
\ root is CURRENT$, at the top level the program's directory (PROGRAM-INIT).
\ A path that does not resolve is refused by name before the frame opens.
: RUN-PROGRAM ( ptr u8 n -- )
   2dup BUILD-REFUSE
   CANONICAL {: path:ptr pathu:n exists:bool :}
   path pathu INCLUDE-PATH0 drop
   exists 0= if INCLUDE-OPEN-DIE then
   PUSH
   [: LOAD-BYTES TOP@ RANGE-OWNER ! 0 TOP@ FR-SIZE + CELL-VIEW @ RUN-RANGE ;]
   catch {: rc:n :}
   INCLUDE-CLOSE
   POP
   rc 0= 0= if rc throw then ;

\ A completed source range leaves definitions live even if a unit callback
\ later throws. A require keeps its row in that case.
: LOAD-RESULT ( ptr u8 n -- bool n )
   2dup BUILD-REFUSE
   0 LAST-LOAD-EVAL-OK !
   [: 2dup LOAD-RAW ;] catch {: rc:n :}
   2drop
   LAST-LOAD-EVAL-OK @ 0 <> {: committed:bool :}
   0 LAST-LOAD-EVAL-OK !
   committed rc ;

: NAMED? ( -- bool )
   TOP@ 0= if INCLUDE-FALSE exit then
   TOP@ FR-NAMED + c@ 0= 0= ;

: LOAD ( ptr u8 n -- )
   LOAD-RESULT {: committed:bool rc:n :}
   rc 0 <> if rc throw then ;

\ PATH has already been resolved by the loader's resolver. Like LOAD, this
\ opens a source frame and makes its root current until the callback returns.
: WITH-BYTES ( ptr u8 n [ ptr u8 n ptr u8 n -- ] -- )
   {: path:ptr pathu:n q :}
   path pathu BUILD-REFUSE
   path pathu CONFINE INCLUDE-PATH0 drop
   RESOLVED-ROOT$ ROOT-ENTER {: old:ptr oldu:n :}
   q [: WITH-BYTES-CURRENT ;] catch {: rc:n :}
   drop
   old oldu rc ROOT-LEAVE ;

\ A caller that owns PATH's source bytes can open the same loader frame without
\ reading a different disk copy. This is used for an included buffer subject.
: WITH-SUPPLIED ( ptr u8 n ptr u8 n [ ptr u8 n ptr u8 n -- ] -- )
   {: path:ptr pathu:n src:ptr srcu:n q :}
   path pathu BUILD-REFUSE
   path pathu CONFINE INCLUDE-PATH0 drop
   RESOLVED-ROOT$ ROOT-ENTER {: old:ptr oldu:n :}
   src srcu q [: WITH-SUPPLIED-CURRENT ;] catch {: rc:n :}
   2drop drop
   old oldu rc ROOT-LEAVE ;

;package

: INCLUDE-EVALUATE-RANGE ( ptr u8 n n n -- )
   SOURCE-ROOT:RUN-EVAL-RANGE ;

package SOURCE-INPUT
public

: RESET ( -- )
   [: SOURCE-ROOT:CANON-OS ;] CANON-XT !
   [: SOURCE-ROOT:READ-OS ;] READ-XT ! ;

;package

: INCLUDE-LOAD ( ptr u8 n -- bool n ) LOAD-RESULT ;

: included ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u RESOLVE drop {: path:ptr pathu:n :}
   a u path pathu EV-INCLUDED EV-STATE-FRESH EVENT-RECORD
   DISCOVERY? if exit then
   path pathu INCLUDE-LOAD {: committed:bool rc:n :}
   rc 0 <> if rc throw then ;

create SOURCE-ORIGINAL-INCLUDED-ENTRY 0 ,
' included SOURCE-ORIGINAL-INCLUDED-ENTRY xt!

\ One body for both spellings, given the path as spelled and as resolved. A path
\ the registry already holds is skipped, so the pending flag has to be cleared on
\ every exit or it would leak into the next unrelated load.
: REQUIRE-BODY ( ptr u8 n ptr u8 n bool -- )
   {: a:ptr u:n path:ptr pathu:n known:bool :}
   a u path pathu EV-REQUIRED known REQUIRE-STATE EVENT-RECORD
   known if INCLUDE-FALSE SCRIPT-NAMED-PEND! exit then
   REQUIRE-REG:MARK {: row:n epoch:n :}
   path pathu REQUIRE-STORE
   DISCOVERY? if 2drop INCLUDE-FALSE SCRIPT-NAMED-PEND! exit then
   INCLUDE-LOAD {: committed:bool rc:n :}
   rc 0 <> if
      committed 0= if row epoch REQUIRE-REG:REMOVE-PROVISIONAL then
      rc throw
   then ;

: required ( ptr u8 n -- )
   {: a:ptr u:n :}
   INCLUDE-FALSE SCRIPT-NAMED-PEND!
   a u  a u RESOLVE  REQUIRE-BODY ;

create SOURCE-ORIGINAL-REQUIRED-ENTRY 0 ,
' required SOURCE-ORIGINAL-REQUIRED-ENTRY xt!

\ The `--load` argv row. Same load, and it records that the command line is
\ what asked for it.
: script-required ( ptr u8 n -- )
   {: a:ptr u:n :}
   INCLUDE-TRUE SCRIPT-NAMED-PEND!
   a u  a u ENTRY-RESOLVE  REQUIRE-BODY ;

: SCRIPT-NAMED-LOAD? ( -- bool )
   SOURCE-ROOT:NAMED? ;

: provided ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u RESOLVE {: path:ptr pathu:n known:bool :}
   path pathu SOURCE-ROOT:BUILD-REFUSE
   a u path pathu EV-PROVIDED known REQUIRE-STATE EVENT-RECORD
   known if exit then
   path pathu REQUIRE-STORE 2drop ;

: include ( -- )
   parse-name INCLUDE-CHECK-PATH included ;
immediate

create SOURCE-ORIGINAL-INCLUDE-ENTRY 0 ,
' include SOURCE-ORIGINAL-INCLUDE-ENTRY xt!

: require ( -- )
   parse-name INCLUDE-CHECK-PATH required ;
immediate

create SOURCE-ORIGINAL-REQUIRE-ENTRY 0 ,
' require SOURCE-ORIGINAL-REQUIRE-ENTRY xt!

package SOURCE-UNIT
public
0 constant LOAD-INCLUDE
1 constant LOAD-REQUIRE
0 constant NONE
1 constant PARSED-PATH
2 constant STACK-PATH

: LOADER-KIND ( n n n -- n ) {: op:n rec:n entry:n :}
   op 5 <> if NONE exit then
   entry SOURCE-ORIGINAL-INCLUDE-ENTRY @ =
   entry SOURCE-ORIGINAL-REQUIRE-ENTRY @ = or if PARSED-PATH exit then
   entry SOURCE-ORIGINAL-INCLUDED-ENTRY @ =
   entry SOURCE-ORIGINAL-REQUIRED-ENTRY @ = or if STACK-PATH exit then
   NONE ;

: LOAD-PATH ( n ptr u8 n -- ) {: mode:n path:ptr size:n :}
   SOURCE-ROOT:ACTIVE-SOURCE 2drop 2drop 2drop
   mode LOAD-INCLUDE = if path size included exit then
   mode LOAD-REQUIRE = if path size required exit then
   INCLUDE-EVAL-RC throw ;
;package

package SOURCE-ROOT
private

: POLICY-SEALED ( -- )
   2 s" hb: policy: sealed" write drop
   2 POLICY-LF 1 write drop
   107 throw ;

: POLICY-ROOT-SET ( [ -- ] -- )
   data-base INCLUDE-POLICY-REQUIRE-CELL + xt!
   CANON-BUF POLICY-ROOT-BUF CANON-U @ BYTE-COPY
   CANON-U @ POLICY-ROOT-U ! ;

: POLICY-ROOT-RELEASE ( ptr u8 n -- )
   1+ munmap 0 < if s" policy: cannot release a root copy" INCLUDE-IO-RC die then ;

public

\ Configure confinement before checking or sealing. The policy library passes
\ the ordinary require xt it captured when loaded.
: POLICY-ROOT ( ptr u8 n [ -- ] -- )
   {: path:ptr u:n require-xt :}
   data-base INCLUDE-POLICY-NDICT-CELL + @ 0 <> if POLICY-SEALED then
   path u ROOT-CANON
   require-xt POLICY-ROOT-SET ;

\ Scope a root and require owner for reusable source checks. A callback that
\ seals leaves the new policy root and refuses the attempted return to the old.
: POLICY-WITH-ROOT ( ptr u8 n [ -- ] [ -- ] -- )
   {: path:ptr u:n q require-xt :}
   data-base INCLUDE-POLICY-NDICT-CELL + @ 0 <> if POLICY-SEALED then
   path u ROOT-ENTER {: prior:ptr prioru:n :}
   data-base INCLUDE-POLICY-REQUIRE-CELL + @ 0= 0= {: had:bool :}
   POLICY-ROOT-U @ {: old-u:n :}
   POLICY-ROOT-BUF old-u MAP-COPY {: old:ptr :}
   require-xt POLICY-ROOT-SET
   q catch {: rc:n :}
   data-base INCLUDE-POLICY-NDICT-CELL + @ 0 <> if
      prior prioru 0 ROOT-LEAVE
      old old-u POLICY-ROOT-RELEASE POLICY-SEALED
   then
   had if
      old POLICY-ROOT-BUF old-u BYTE-COPY
      old-u POLICY-ROOT-U !
   else
      0 POLICY-ROOT-U !
      0 data-base INCLUDE-POLICY-REQUIRE-CELL + xt!
   then
   old old-u POLICY-ROOT-RELEASE
   prior prioru rc ROOT-LEAVE ;

;package

\ A restricted discovery pass sees a fresh require/provided registry, so a tool's
\ preloaded paths cannot dedup-hide a later user require. Raising REQUIRE-BASE to
\ the current count makes REQUIRE-KNOWN? ignore the tool's entries; RESTORE drops
\ the discovery entries and reinstates the tool's registry unchanged, keeping
\ warm-snapshot serialization intact.

: REQUIRE-SNAPSHOT ( -- )
   REQUIRE-N @ REQUIRE-SAVE-N !
   REQUIRE-BASE @ REQUIRE-SAVE-BASE !
   REQUIRE-N @ REQUIRE-BASE ! ;

: REQUIRE-RESTORE ( -- )
   REQUIRE-SAVE-BASE @ REQUIRE-BASE !
   REQUIRE-SAVE-N @ REQUIRE-REG:REWIND ;

\ COUNT remains the stable public registry cursor after the load path above.
package REQUIRE-REG

public

: COUNT ( -- n )
   REQUIRE-N @ ;

;package

\ What the ENGINE provides, as opposed to what this process loaded: the boot
\ prefix marks its own files `provided` before any user token runs, and freezing
\ the count at its end separates the two, which REQUIRE-KNOWN? cannot once a
\ tool's own dependencies are in the registry. tools/bundle-lib-core.f relies on
\ it: it bundles no file the engine loaded and skips none the engine lacks.
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


;using
