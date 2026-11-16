\ include.f: checked source loading. SOURCE-ROOT resolves paths against the owner
\ root, REQUIRE-REG records loaded files (engine rows frozen at boot), the event
\ log feeds a discovery pass, and the frames hold each open file. `include` only
\ composes source; package reopening owns shared namespace.
\
\ The engine's own source root: the working directory when it carries the
\ engine's first row; else the executable's tree when that does (`%`, two
\ levels above <tree>/bin/hb); else the working directory. The working
\ directory comes first because a stdin session's requires search it before
\ the root and only the root's physical copy of a row is frozen: with `%`
\ first, a copied tree there would load as application source. A build's fresh
\ registry holds no row, so its rows are relative to the working directory.
\ Derived on first need from the executable's path and realpath, not through
\ SOURCE-INPUT: it is a fact of this process. Deriving touches only JOIN, WORK,
\ Z and CANON scratch, so NORMAL-BUF and CANDIDATE-BUF survive.

PATH-CAP constant INCLUDE-PATH-CAP
$46 constant INCLUDE-EVAL-RC
-2110 constant INCLUDE-RANGE-RC
create INCLUDE-PATH INCLUDE-PATH-CAP 1 + allot

package REQUIRE-REG
private

\ FROZEN holds a DATA block of (offset,length) pairs followed by path bytes.
\ Process rows use two movable columns: ROWS holds row i's path pointer and
\ ROW-LENS its length. The paths live in chunks that never move.
PERSISTED-PTR-VARIABLE FROZEN
PERSISTED-PTR-U8-TABLE-VARIABLE ROWS
PERSISTED-PTR-VARIABLE ROW-LENS
variable ROW-CAP
PERSISTED-PTR-U8-TABLE-VARIABLE CHUNK
variable PREFIX-N

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
-1 REQUIRE-SAVE-N !                                \ no snapshot in effect

: INCLUDE-FALSE ( -- bool )
   0 0= 0= ;

: INCLUDE-TRUE ( -- bool )
   0 0= ;

\ Rows recorded while the boot registry is open describe what the ENGINE
\ carries; every other row, what a process loaded. Only the boot prefix opens
\ it. Loading this file is not the signal: a build
\ re-loads it into a booted engine, which would then record every later require
\ portably and stop recognising its own canonical rows.
: REQUIRE-BOOT-OPEN ( -- )
   INCLUDE-TRUE REQUIRE-BOOT-V ! ;

: REQUIRE-BOOT-OPEN? ( -- bool )
   REQUIRE-BOOT-V @ 0= 0= ;

\ While the registry is open every row so far is the engine's, because only the
\ prefix records there; once closed, the frozen count says.
: REQUIRE-BOOT-LIMIT ( -- n )
   REQUIRE-BOOT-OPEN? if REQUIRE-N @ exit then
   REQUIRE-BOOT-N @ ;

\ An invocation that owns source bytes supplies both filesystem answers through
\ this loader-local pair. A newly source-loaded target gets its own pair, so its
\ require registry and source frames remain independent of the retained host.
package SOURCE-INPUT
private

TYPED-VARIABLE CANON-XT [ ^u8 n -- ^u8 n bool ]
TYPED-VARIABLE READ-XT [ ^u8 n ^u8 n -- ^u8 n ]

public

: CANON ( ^u8 n -- ^u8 n bool ) CANON-XT @ execute ;
: READ ( ^u8 n ^u8 n -- ^u8 n ) READ-XT @ execute ;

: USE ( [ ^u8 n -- ^u8 n bool ] [ ^u8 n ^u8 n -- ^u8 n ] -- )
   {: canon read :}
   canon CANON-XT !
   read READ-XT ! ;

: WITH ( [ ^u8 n -- ^u8 n bool ] [ ^u8 n ^u8 n -- ^u8 n ] [ -- ] -- )
   {: canon read action :}
   CANON-XT @ READ-XT @ {: old-canon old-read :}
   canon read USE
   action catch {: rc:n :}
   old-canon old-read USE
   rc 0 <> if rc throw then ;

;package

: TOKEN-SPELLS? ( ^u8 n -- bool )
   SRCLOC:SPELLED nip nip ;

\ The place of a record of loader word w, given the path a u: the source a u
\ stands for, else the token the engine read last where it spells w, which it
\ does only when source calls w directly: called from a definition at run
\ time, that token is another word. Else no place, NULL-PTR 0.
: LOADER-PLACE$ ( ^u8 n ^u8 n -- ^u8 n ) {: w:^ wu:n a:^ u:n :}
   a u SRCLOC:ORIGIN$ {: t:^ tu:n :}
   t NULL-PTR <> if t tu exit then
   w wu TOKEN-SPELLS? if SRCLOC:TOKEN$ exit then
   NULL-PTR 0 ;

\ Places the record of word w, given the path a u, and opens its label, true;
\ the caller writes the rest, which reads after the path or after "that". With
\ no place the record has no label, false.
: LOADER-PATH-AT ( ^u8 n ^u8 n -- bool ) {: w:^ wu:n a:^ u:n :}
   w wu a u LOADER-PLACE$ {: t:^ tu:n :}
   t NULL-PTR = if 0 0= 0= exit then
   t tu REJECTION:AT
   a u SRCLOC:ORIGIN$ drop NULL-PTR = if s" passes a path that " REJECTION:EMIT-STR then
   0 0= ;

\ Word w's record for the path a u, kind E-PATH-NUL or E-PATH-RANGE: len 0 when empty,
\ -1 if too long once resolved, else the length over cap. Empty w names no word.
: LOADER-PATH-RECORD ( ^u8 n ^u8 n n n n -- )
   {: w:^ wu:n a:^ u:n kind:n len:n cap:n :}
   w wu SRCLOC:SPELLED drop {: name:^ nameu:n :}
   kind E-PATH-NUL = if
      s" E-PATH-NUL" REJECTION:OPEN
      wu 0= if
         s" a path holds a NUL byte" REJECTION:EMIT-STR
      else
         96 REJECTION:EMIT1  name nameu REJECTION:EMIT-STR
         s" ` got a path with a NUL byte" REJECTION:EMIT-STR
      then
      s" , which no file name holds" REJECTION:EMIT-STR
   else
      name nameu len cap REJECTION:PATH-RANGE
   then
   w wu a u LOADER-PATH-AT if
      kind E-PATH-NUL = if
         s" holds a NUL byte"
      else
         len 0= if
            s" is empty"
         else
            len 0 < if s" is too long once resolved" else s" is too long" then
         then
      then REJECTION:EMIT-STR
   then
   REJECTION:HELP
   kind E-PATH-NUL = if
      s" remove the NUL byte from the path" REJECTION:EMIT-STR
   else
      s" pass a path of 1 to " REJECTION:EMIT-STR
      cap REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
   then ;

\ Stops with word w's record for the path a u it cannot take.
: LOADER-PATH-STOP ( ^u8 n ^u8 n n n -- )
   INCLUDE-PATH-CAP LOADER-PATH-RECORD  CHECKER-REJECT-RC REJECTION:STOP ;

\ Whether a loader or ROOT! can take the path a u before resolving it: 0 0, else
\ the kind and len LOADER-PATH-STOP takes. Length is the resolver's to judge.
: LOADER-PATH-FAULT ( ^u8 n -- n n ) {: a:^ u:n :}
   u 0 <= if E-PATH-RANGE 0 exit then
   0 begin dup u < while
      dup a + c@ 0= if drop E-PATH-NUL 0 exit then
      1 +
   repeat
   drop 0 0 ;

\ Word w's path a u, placed at p pu: one no file name holds stops with w's record.
: LOADER-PATH-CHECK ( ^u8 n ^u8 n ^u8 n -- ) {: w:^ wu:n p:^ pu:n a:^ u:n :}
   a u LOADER-PATH-FAULT {: kind:n len:n :}
   kind 0 <> if w wu p pu kind len LOADER-PATH-STOP then ;

\ The path a u that loader w takes, found by q, which is given a u and the
\ place of w's record; one it cannot take stops with that record. An empty w
\ is a command-line entry, whose record has no place.
: LOADER-RESOLVE ( ^u8 n ^u8 n [ ^u8 n ^u8 n -- ^u8 n bool ] -- ^u8 n bool )
   {: w:^ wu:n a:^ u:n q :}
   wu 0= if NULL-PTR 0 else a u then {: p:^ pu:n :}
   wu 0= if NULL-PTR 0 else w wu a u LOADER-PLACE$ then {: t:^ tu:n :}
   w wu p pu a u LOADER-PATH-CHECK
   a u INCLUDE-FALSE [: drop t tu q execute ;] catch {: rc:n :}
   rc E-PATH-RANGE = if w wu p pu E-PATH-RANGE -1 LOADER-PATH-STOP then
   rc 0= if exit then
   rc throw ;

\ Canonical source paths and a dynamically scoped owner root. This bootstrap
\ layer uses only core bytes, mappings and the bounded realpath OS primitive.
package SOURCE-ROOT
EXPORT CHECK:ROOT-MOVER
private

\ Word w's path a u, judged as spelled: one no file name holds, or one longer
\ than PATH-CAP, stops with w's record.
: GIVEN-CHECK ( ^u8 n ^u8 n -- ) {: w:^ wu:n a:^ u:n :}
   w wu a u a u LOADER-PATH-CHECK
   u INCLUDE-PATH-CAP > if w wu a u E-PATH-RANGE u LOADER-PATH-STOP then ;

INCLUDE-PATH-CAP 1+ constant PATH-BYTES
create CWD-BUF PATH-BYTES allot
create CANON-BUF PATH-BYTES allot
create CANDIDATE-BUF PATH-BYTES allot
create OWNER-BUF PATH-BYTES allot
create OWNER-SAVE PATH-BYTES allot
create POLICY-ROOT-BUF PATH-BYTES allot
variable POLICY-ROOT-U
create POLICY-LF 10 c,
\ A path as written has no length limit, so these buffers are mappings each FIT
\ grows before a write; no address into one is kept across its FIT.
PTR-VARIABLE NORMAL-P   SCRATCH-VARIABLE NORMAL-CAP
PTR-VARIABLE JOIN-P     SCRATCH-VARIABLE JOIN-CAP
PTR-VARIABLE WORK-P     SCRATCH-VARIABLE WORK-CAP
PTR-VARIABLE Z-P        SCRATCH-VARIABLE Z-CAP
PTR-VARIABLE DENIED-P   SCRATCH-VARIABLE DENIED-CAP
PTR-VARIABLE REQUEST-P  SCRATCH-VARIABLE REQUEST-CAP
: NORMAL-BUF ( -- ^u8 ) NORMAL-P @ ;
: JOIN-BUF ( -- ^u8 ) JOIN-P @ ;
: WORK-BUF ( -- ^u8 ) WORK-P @ ;
: ZBUF ( -- ^u8 ) Z-P @ ;
: DENIED-PATH ( -- ^u8 ) DENIED-P @ ;
: REQUEST-BUF ( -- ^u8 ) REQUEST-P @ ;
: PATH-FIT ( ^^a ^n n -- ) PATH-BYTES swap ARENA-ROOM ;
: NORMAL-FIT ( n -- ) NORMAL-P NORMAL-CAP rot PATH-FIT ;
: JOIN-FIT ( n -- ) JOIN-P JOIN-CAP rot PATH-FIT ;
: WORK-FIT ( n -- ) WORK-P WORK-CAP rot PATH-FIT ;
: Z-FIT ( n -- ) Z-P Z-CAP rot PATH-FIT ;
: DENIED-FIT ( n -- ) DENIED-P DENIED-CAP rot PATH-FIT ;
: REQUEST-FIT ( n -- ) REQUEST-P REQUEST-CAP rot PATH-FIT ;
\ The u-byte path a as a C path in ZBUF.
: ZPATH-OF ( ^u8 n -- ^u8 ) {: a:^ u:n :}
   u 1+ Z-FIT  a u ZBUF u 1+ PATHZ  ZBUF ;
variable REQUEST-U
variable CWD-U
variable CANON-U
variable NORMAL-U
variable CANDIDATE-U
variable JOIN-U
variable OWNER-U
\ Readers reach the loader's record by SRCLOC:SOURCE-CELL, the running hb's
\ in a build.
0 constant REC-TOP
CELL constant REC-ROOT
2 cells constant REC-ROOTLEN
3 cells constant REC-BYTES
create LOADER-REC REC-BYTES SCRATCH-ALLOT
SRCLOC:SOURCE-CELL RESERVED-PTR-U8-CELL LOADER-CELL
LOADER-REC LOADER-CELL ptr-cell!            \ a build's store goes to its image
CAST: REC-CELLS ( ^u8 -- ^^u8 )
: LOADER ( -- ^u8 ) LOADER-CELL @ ;
: TOP ( -- ^^u8 ) LOADER REC-CELLS REC-TOP CELL / ptr-field ;
: CURRENT-A ( -- ^^u8 ) LOADER REC-CELLS REC-ROOT CELL / ptr-field ;
: CURRENT-U ( -- ^n ) LOADER REC-ROOTLEN + CELL-VIEW ;
create ENGINE-BUF PATH-BYTES allot
variable ENGINE-U
variable ENGINE-READ
\ The stack PUSHPATH fills: each saved path's mapped copy and length, 0 for the
\ top-level root, in two columns of SLOT-CAP cells that double together when full.
8 constant SLOT-FIRST
PTR-U8-TABLE-VARIABLE SLOT-A
PTR-VARIABLE SLOT-U
variable SLOT-CAP
variable SLOT-N
\ SLOT-N when the open scope began, 0 at the top level.
variable SLOT-BASE

: CURRENT-PTR ( -- ^u8 ) CURRENT-A @ ;


: CURRENT! ( ^u8 n -- )
   CURRENT-U ! CURRENT-A ! ;

\ The current path as set, null for the top-level root, which a scope saves
\ and restores: CURRENT$ would hand back CWD-BUF, which no top-level path may
\ hold (CD and POPPATH release the path they replace).
: CURRENT@ ( -- ^u8 n )
   CURRENT-PTR CURRENT-U @ ;

\ Throws kind, E-PATH-RANGE or E-PATH-NUL, carrying the record of the path a
\ u, which names no word, since its callers serve many public words: len and
\ cap as LOADER-PATH-RECORD takes them. Callers catch either code and go on.
: PATH-PASS ( ^u8 n n n n -- )
   {: a:^ u:n kind:n len:n cap:n :}
   NULL-PTR 0 a u kind len cap LOADER-PATH-RECORD
   REJECTION:SAVE  kind REJECTION:PASS ;

: CHECK ( ^u8 n -- )
   {: a:^ u:n :}
   u 0 <= if a u E-PATH-RANGE 0 INCLUDE-PATH-CAP PATH-PASS then ;


\ realpath of a path of any written length into CANON-BUF: its length, below 0
\ if it does not resolve; realpath fails on a result CANON-BUF cannot hold.
: CANON-N ( ^u8 n -- n )
   {: a:^ u:n :}
   a u CHECK
   a u LOADER-PATH-FAULT {: kind:n len:n :}
   kind 0 <> if a u kind len INCLUDE-PATH-CAP PATH-PASS then
   a u ZPATH-OF CANON-BUF PATH-BYTES realpath ;

public

: CANON-OS ( ^u8 n -- ^u8 n bool )
   {: a:^ u:n :}
   a u CANON-N {: n:n :}
   n 0 < if CANON-BUF 0 INCLUDE-FALSE exit then
   CANON-BUF n INCLUDE-TRUE ;

private

\ Opens word w's record of the path it got: w is a word the program called or
\ SOURCE-INPUT:CANON, for the canon it supplied; why follows "which".
: PATH-REJECT ( ^u8 n ^u8 n ^u8 n -- )
   {: w:^ wu:n path:^ pathu:n why:^ whyu:n :}
   s" E-SOURCE-ROOT" REJECTION:OPEN
   96 REJECTION:EMIT1  w wu REJECTION:EMIT-STR  s" ` got `" REJECTION:EMIT-STR
   path pathu REJECTION:EMIT-STR  s" `, which " REJECTION:EMIT-STR
   why whyu REJECTION:EMIT-STR ;

\ Word w got the path a u, shown as p pu: why reads after "which" and "that";
\ help says what the program should pass, or its canon answer.
: PATH-DIE ( ^u8 n ^u8 n ^u8 n ^u8 n ^u8 n -- )
   {: w:^ wu:n a:^ u:n p:^ pu:n why:^ whyu:n help:^ helpu:n :}
   w wu SRCLOC:SPELLED drop p pu why whyu PATH-REJECT
   w wu a u LOADER-PATH-AT if why whyu REJECTION:EMIT-STR then
   REJECTION:HELP  help helpu REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

: TRY-CANON ( ^u8 n -- bool )
   {: p:^ pu:n :}
   p pu SOURCE-INPUT:CANON {: a:^ u:n found:bool :}
   u PATH-BYTES >= if p pu E-PATH-RANGE -1 PATH-BYTES 1- PATH-PASS then
   found if
      a u LOADER-PATH-FAULT drop {: kind:n :}
      kind 0 <> if INCLUDE-TRUE else a c@ [char] / <> then if
         s" SOURCE-INPUT:CANON" p pu p pu
         kind E-PATH-NUL = if
            s" it answered with a path holding a NUL byte"
         else
            s" it answered with a path that does not begin with `/`"
         then
         s" have the canon given to `SOURCE-INPUT:USE` or `SOURCE-INPUT:WITH` answer an absolute path a file name can hold"
         PATH-DIE
      then
   then
   a CANON-BUF <> if a CANON-BUF u BYTE-COPY then
   u CANON-U !
   found ;


: CANON$ ( -- ^u8 n ) CANON-BUF CANON-U @ ;


\ The working directory must fit PATH-CAP as every root does; every path below
\ a longer one is longer still, so the engine rejects it rather than resolve
\ without it. realpath does not say why it fails, so the record names what a
\ removed, unsearchable or long directory all are not. realpath fails on a
\ longer one; a supplied canon's longer answer is rejected by TRY-CANON with
\ E-PATH-RANGE, which CWD-INIT turns into this record.
: CWD-DIE ( -- )
   s" E-SOURCE-ROOT" REJECTION:OPEN
   s" the working directory does not resolve to a searchable directory" REJECTION:EMIT-STR
   PATH-CAP s" bytes" REJECTION:LIMIT
   REJECTION:HELP
   s" run hb from a directory that exists, that you may search, and whose path fits the limit"
   REJECTION:EMIT-STR
   INCLUDE-IO-RC REJECTION:STOP ;


: CWD-READ ( -- )
   s" ." TRY-CANON 0= if CWD-DIE then
   CANON-BUF CWD-BUF CANON-U @ 1+ BYTE-COPY
   CANON-U @ CWD-U ! ;


: CWD-INIT ( -- )
   CWD-U @ 0= 0= if exit then
   [: CWD-READ ;] catch {: rc:n :}
   rc E-PATH-RANGE = if CWD-DIE then
   rc 0= 0= if rc throw then ;

: CWD-RECORD ( -- )
   CWD-INIT
   s" ." CWD-BUF CWD-U @ COMPILE-READS:ROOT+ ;

public

: CWD$ ( -- ^u8 n ) CWD-RECORD CWD-BUF CWD-U @ ;


private

: PARENT-U ( ^u8 n -- n ) {: a:^ u:n :}
   u
   begin dup 1 > while
      1- dup a + c@ 47 = if exit then
   repeat ;


\ The program file `hb prog.f` runs: argv[1], which the launcher leaves in
\ PROGRAM-FILE-CELL only on that route. PROGRAM? counts it as loaded:
\ RUN-PROGRAM gives it a frame but no registry row names it, since a row
\ appended on first use would vanish under the rollback of a failed require or
\ a discovery pass. Deriving writes only PROGRAM-BUF, so the caller's scratch
\ survives.
PROGRAM-FILE-CELL RESERVED-PTR-U8-CELL PROGRAM-ARG
create PROGRAM-BUF PATH-BYTES allot
variable PROGRAM-U

: PROGRAM-INIT ( -- )
   PROGRAM-U @ 0= 0= if exit then
   PROGRAM-ARG @ {: a:^ :}
   a 0= if exit then
   a PROGRAM-BUF PATH-BYTES realpath {: n:n :}
   n 0 <= if
      s" source root: the program file does not resolve within PATH-CAP bytes"
      INCLUDE-IO-RC die
   then
   n PROGRAM-U ! ;

\ Whether a canonical path is the program file. A fresh discovery registry
\ (REQUIRE-SNAPSHOT) hides it, as it hides every row recorded before it.
: PROGRAM? ( ^u8 n -- bool ) {: a:^ u:n :}
   REQUIRE-BASE @ 0= 0= if INCLUDE-FALSE exit then
   PROGRAM-INIT
   PROGRAM-U @ 0= if INCLUDE-FALSE exit then
   a u PROGRAM-BUF PROGRAM-U @ CORE-STR= ;

\ The working directory, the engine root, the program file and the resolver's
\ work belong to the process that derived them, so their cells are scratch: a
\ saved program, a cross-built one included, starts with none and derives its
\ own instead of resolving against its builder's directory.
: SCRATCH-BYTES ( ^u8 n -- ) {: a:^ u:n :}
   u 0 ?do a i + scratch-cell-mark 1 cells +loop ;

CWD-BUF PATH-BYTES SCRATCH-BYTES        CWD-U scratch-cell-mark
CANON-BUF PATH-BYTES SCRATCH-BYTES      CANON-U scratch-cell-mark
NORMAL-U scratch-cell-mark
CANDIDATE-BUF PATH-BYTES SCRATCH-BYTES  CANDIDATE-U scratch-cell-mark
JOIN-U scratch-cell-mark
OWNER-BUF PATH-BYTES SCRATCH-BYTES      OWNER-U scratch-cell-mark
OWNER-SAVE PATH-BYTES SCRATCH-BYTES
REQUEST-U scratch-cell-mark
ENGINE-BUF PATH-BYTES SCRATCH-BYTES     ENGINE-U scratch-cell-mark
ENGINE-READ scratch-cell-mark
PROGRAM-BUF PATH-BYTES SCRATCH-BYTES    PROGRAM-U scratch-cell-mark

public

: CURRENT$ ( -- ^u8 n )
   CURRENT-U @ 0 > if CURRENT-PTR CURRENT-U @ exit then
   PROGRAM-INIT
   PROGRAM-U @ 0= if CWD$ exit then
   PROGRAM-BUF PROGRAM-BUF PROGRAM-U @ PARENT-U ;

private

\ The root length is the caller's: one below 0, or whose sum with u wraps, is
\ rejected. Built in WORK-BUF, so a root or name in JOIN-BUF is read first.
: JOIN! ( ^u8 n ^u8 n -- ) {: root:^ rootu:n a:^ u:n :}
   rootu 0 < if root 0 E-PATH-RANGE 0 INCLUDE-PATH-CAP PATH-PASS then
   rootu u + 1+ {: n:n :}
   n 0 < if root 0 E-PATH-RANGE 0 INCLUDE-PATH-CAP PATH-PASS then
   n WORK-FIT
   root WORK-BUF rootu BYTE-COPY
   47 WORK-BUF rootu + c!
   a WORK-BUF rootu 1+ + u BYTE-COPY
   n JOIN-FIT
   n JOIN-U !
   WORK-BUF JOIN-BUF n BYTE-COPY ;


: ABSOLUTE! ( ^u8 n -- ) {: a:^ u:n :}
   a u CHECK
   a c@ 47 = if
      u JOIN-FIT  a JOIN-BUF u BYTE-COPY u JOIN-U ! exit
   then
   CWD$ a u JOIN! ;


\ p pu is the path being normalized.
: NORMAL-ROOM ( ^u8 n n n -- )
   {: p:^ pu:n n:n limit:n :}
   NORMAL-U @ n + limit > if p pu E-PATH-RANGE -1 limit PATH-PASS then
   NORMAL-U @ n + NORMAL-FIT ;


: NORMAL-PUT ( ^u8 n ^u8 n n -- )
   {: p:^ pu:n a:^ u:n limit:n :}
   NORMAL-U @ 1 > if
      p pu 1 limit NORMAL-ROOM
      47 NORMAL-BUF NORMAL-U @ + c!
      1 NORMAL-U +!
   then
   p pu u limit NORMAL-ROOM
   a NORMAL-BUF NORMAL-U @ + u BYTE-COPY
   u NORMAL-U +! ;


: NORMAL-SEG ( ^u8 n ^u8 n n -- )
   {: p:^ pu:n a:^ u:n limit:n :}
   u 0= if exit then
   a u s" ." CORE-STR= if exit then
   a u s" .." CORE-STR= if
      NORMAL-BUF NORMAL-U @ PARENT-U NORMAL-U ! exit
   then
   p pu a u limit NORMAL-PUT ;


: SEG-END ( ^u8 n n -- n ) {: a:^ u:n start:n :}
   start
   begin dup u < while
      dup a + c@ 47 = if exit then
      1+
   repeat ;


: NORMALIZE-LIMIT ( ^u8 n n -- )
   {: a:^ u:n limit:n :}
   1 NORMAL-FIT  47 NORMAL-BUF c! 1 NORMAL-U !
   0 begin dup u < while
      dup a u rot SEG-END {: start:n end:n :}
      a u  a start +  end start -  limit NORMAL-SEG
      end 1+
   repeat drop ;


: NORMALIZE ( ^u8 n -- ) INCLUDE-PATH-CAP NORMALIZE-LIMIT ;

\ A build compiles a second copy of this file into the program it builds, and
\ there LOADER answers hb's record: the copy's loading words would run its own
\ registry, buffers and frames over hb's. The build's loader does every load,
\ at a file's top level, so in a build the copy's loads refuse (BUILD-REFUSE).
: OWN-LOADER? ( -- bool ) LOADER LOADER-REC = ;

public

\ The resolver does not refuse: RESOLVE loads nothing, and in a build it
\ searches the process's current root, so a word may ask where a file lies.
\ Public so provided, which this file defines outside
\ SOURCE-ROOT, refuses here too: its row would lie outside the build's space,
\ which only the linker would catch. w is the word the program called.
: BUILD-REFUSE ( ^u8 n ^u8 n -- ) {: w:^ wu:n path:^ pathu:n :}
   OWN-LOADER? if exit then
   w wu path pathu path pathu
   s" a word may not load or provide while a build runs"
   s" load or provide the file at a file's top level, which the build's loader runs"
   PATH-DIE ;

private

\ Whether the bytes a s..e are an empty or `.` segment.
: DOT-SEG? ( ^u8 n n -- bool ) {: a:^ s:n e:n :}
   e s = if INCLUDE-TRUE exit then
   e s - 1 =  a s + c@ 46 =  and ;

\ The end of the run of empty and `.` segments of a u after end e; e if none.
: DOT-RUN-END ( ^u8 n n -- n ) {: a:^ u:n e:n :}
   e begin dup u < while
      dup a u rot 1+ SEG-END {: d:n f:n :}
      a d 1+ f DOT-SEG? 0= if d exit then
      f
   repeat ;

\ The real path in CANON-BUF, `/` and the segment p u, in WORK-BUF: its length.
: SEG-PATH ( ^u8 n -- n ) {: p:^ u:n :}
   CANON-U @ {: r:n :}
   r u + 1+ WORK-FIT
   CANON-BUF WORK-BUF r BYTE-COPY
   CANON-BUF r + 1- c@ 47 = if r else 47 WORK-BUF r + c!  r 1+ then {: h:n :}
   p WORK-BUF h + u BYTE-COPY
   h u + ;

\ The end of the segment of a u after end e, or of the run of empty and `.` ones
\ there, if the canon resolves it under CANON-BUF's real path; else e.
: CANON-STEP ( ^u8 n n -- n ) {: a:^ u:n e:n :}
   a u e DOT-RUN-END {: d:n :}
   CANON-U @ {: r:n :}
   d e > if
      d  s" ."
   else
      a u e 1+ SEG-END  a e 1+ +  over e - 1-
   then {: g:n seg:^ segu:n :}
   seg segu SEG-PATH {: tu:n :}
   WORK-BUF tu TRY-CANON if g exit then
   WORK-BUF CANON-BUF r BYTE-COPY  r CANON-U !  e ;

\ The length of the longest prefix of the absolute a u, cut at a segment end,
\ the canon resolves segment by segment, its real path in CANON-BUF; 0 if none.
: CANON-PREFIX ( ^u8 n -- n )
   {: a:^ u:n :}
   a 1 TRY-CANON 0= if 0 exit then
   0 begin dup u < while
      dup a u rot CANON-STEP
      2dup = if drop 1 max exit then
      nip
   repeat ;

\ The file system root always resolves through realpath; only a supplied canon
\ (SOURCE-INPUT:USE) can answer that no directory above a path does.
: EXISTING-PARENT ( -- n )
   JOIN-BUF  JOIN-BUF JOIN-U @ PARENT-U  CANON-PREFIX
   dup 0= if
      s" SOURCE-INPUT:CANON" JOIN-BUF JOIN-U @ JOIN-BUF JOIN-U @
      s" has no directory above it that the supplied canon resolves"
      s" have the canon given to `SOURCE-INPUT:USE` or `SOURCE-INPUT:WITH` answer true for `/`"
      PATH-DIE
   then ;


: MISSING-NORMALIZE ( -- )
   EXISTING-PARENT {: cut:n :}
   CANON-U @ JOIN-U @ cut - + {: total:n :}
   total WORK-FIT
   CANON-BUF WORK-BUF CANON-U @ BYTE-COPY
   JOIN-BUF cut + WORK-BUF CANON-U @ + JOIN-U @ cut - BYTE-COPY
   WORK-BUF CANON-U @ NORMALIZE
   NORMAL-U @ {: base:n :}
   \ The unresolved rest drops only empty and `.` segments: each `..` there
   \ follows a segment no lookup resolved, or a file, so it climbs from nothing.
   CANON-U @ begin dup total < while
      dup WORK-BUF total rot SEG-END {: start:n end:n :}
      WORK-BUF start end DOT-SEG? 0= if
         WORK-BUF total  WORK-BUF start +  end start -  INCLUDE-PATH-CAP NORMAL-PUT
      then
      end 1+
   repeat drop
   \ A rest of only empty and `.` segments follows a file the lookup cannot
   \ enter: one `.` stays, so the answer is never that file's own path.
   NORMAL-U @ base = if WORK-BUF total s" ." INCLUDE-PATH-CAP NORMAL-PUT then ;

public

\ The canonical path and whether it exists, until the next CANONICAL call; a
\ missing path's resolves only its longest existing prefix.
: CANONICAL ( ^u8 n -- ^u8 n bool )
   ABSOLUTE!
   JOIN-BUF JOIN-U @ TRY-CANON if
      CANON-U @ NORMAL-FIT
      CANON-BUF NORMAL-BUF CANON-U @ BYTE-COPY
      CANON-U @ NORMAL-U !
      NORMAL-BUF NORMAL-U @ INCLUDE-TRUE exit
   then
   MISSING-NORMALIZE
   NORMAL-BUF NORMAL-U @ INCLUDE-FALSE ;


\ A name with no slash lies in the current directory, so its directory is `.`;
\ PARENT-U alone would keep its first byte.
: DIRNAME ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   a u 0 SEG-END u = if s" ." exit then
   a a u PARENT-U ;


: JOIN ( ^u8 n ^u8 n -- ^u8 n ) {: a:^ u:n :}
   a u CHECK
   a c@ 47 = if 2drop a u exit then
   a u JOIN!
   JOIN-BUF JOIN-U @ ;


: BELOW? ( ^u8 n ^u8 n -- bool ) {: a:^ u:n root:^ rootu:n :}
   u rootu < if INCLUDE-FALSE exit then
   a rootu root rootu CORE-STR= 0= if INCLUDE-FALSE exit then
   u rootu = if INCLUDE-TRUE exit then
   rootu 1 = root c@ 47 = and if INCLUDE-TRUE exit then
   a rootu + c@ 47 = ;


: RELATIVE ( ^u8 n ^u8 n -- ^u8 n ) {: a:^ u:n root:^ rootu:n :}
   a u root rootu BELOW? 0= if a u exit then
   u rootu = if s" ." exit then
   rootu 1 = if a 1+ u 1- exit then
   a rootu 1+ + u rootu 1+ - ;


: RESOLVED-ROOT$ ( -- ^u8 n ) OWNER-BUF OWNER-U @ ;

private

: OWNER! ( ^u8 n -- ) {: a:^ u:n :}
   u INCLUDE-PATH-CAP > if E-PATH-RANGE throw then
   a OWNER-BUF u BYTE-COPY u OWNER-U ! ;


\ Whether the n canonical bytes in CANON-BUF name a directory this process can
\ search, which is all a root needs: the files below a directory resolve without
\ read permission on it. macOS realpath answers a file's own path for
\ `<file>/.`, so the kernel's lookup of `<path>/.` decides, and it passes only
\ through a searchable directory. It opens nothing, so a FIFO cannot make it
\ wait.
: ENGINE-DIR? ( n -- bool )
   {: n:n :}
   n 2 + WORK-FIT
   CANON-BUF WORK-BUF n BYTE-COPY
   47 WORK-BUF n + c!
   46 WORK-BUF n 1+ + c!
   WORK-BUF n 2 + ZPATH-OF 0 access 0= ;

\ A root's lookup: the canonical length in CANON-BUF of the spelling, -1 when it
\ does not resolve, -3 when ENGINE-DIR? rejects it.
: ROOT-N ( ^u8 n -- n )
   CANON-N {: n:n :}
   n 0 < if -1 exit then
   n ENGINE-DIR? 0= if -3 exit then
   n ;

\ ENGINE-DIR? decides a root on every platform; the canon of a path does not.
\ Neither says why a path fails, so the rejection names the absolute spelling
\ tried and what a missing, unsearchable or long directory and a file all are
\ not. w is the word the program called.
: ROOT-DIE ( ^u8 n -- )
   JOIN-BUF JOIN-U @ JOIN-BUF JOIN-U @
   s" does not resolve to a searchable directory within PATH-CAP bytes"
   s" name an existing directory this process can search"
   PATH-DIE ;

: ROOT-CANON ( ^u8 n ^u8 n -- ) {: w:^ wu:n a:^ u:n :}
   w wu a u GIVEN-CHECK
   a u ABSOLUTE!
   JOIN-BUF JOIN-U @ ROOT-N {: n:n :}
   n 0 < if w wu ROOT-DIE then
   n CANON-U ! ;

: OUTSIDE-ROOT ( ^u8 n ^u8 n -- ) {: a:^ u:n p:^ pu:n :}
   s" E-POLICY" REJECTION:OPEN
   96 REJECTION:EMIT1  a u REJECTION:EMIT-STR
   s" ` lies outside the source root" REJECTION:EMIT-STR
   pu 0 <> if p pu REJECTION:AT  s" outside the root" REJECTION:EMIT-STR then
   s" root" REJECTION:NOTE
   POLICY-ROOT-BUF POLICY-ROOT-U @ REJECTION:EMIT-STR
   REJECTION:HELP
   s" load only files under the source root, which this " REJECTION:EMIT-STR
   s" program's policy set with `POLICY-ROOT`" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

\ CONFINE of a path a program asked for as req, which a rejection names and
\ places at p, or nowhere when pu is 0.
: CONFINE-AS ( ^u8 n ^u8 n ^u8 n -- ^u8 n )
   {: a:^ u:n req:^ requ:n p:^ pu:n :}
   data-base POLICY-REQUIRE-CELL + @ 0= if a u exit then
   a u CANONICAL drop {: path:^ pathu:n :}
   path pathu POLICY-ROOT-BUF POLICY-ROOT-U @ BELOW? if path pathu exit then
   req requ p pu OUTSIDE-ROOT ;

public

\ ROOT-N's lookup, `.` being the working directory: the canonical directory and
\ true; false too for an empty spelling. Lasts until the next canon.
: ROOT? ( ^u8 n -- ^u8 n bool )
   {: a:^ u:n :}
   u 1 < if CANON-BUF 0 INCLUDE-FALSE exit then
   a u ROOT-N {: n:n :}
   n 0 < if CANON-BUF 0 INCLUDE-FALSE exit then
   CANON-BUF n INCLUDE-TRUE ;

: CONFINE ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   a u a u NULL-PTR 0 CONFINE-AS ;

private

: MAP-COPY ( ^u8 n -- ^u8 )
   {: a:^ u:n :}
   u 1+ map-anon 0= 0= if drop s" source root: cannot map a path copy" INCLUDE-IO-RC die then
   {: fresh:^ :}
   a fresh u 1+ BYTE-COPY
   fresh ;

: SLOT-FIELD ( n -- ^^u8 )
   SLOT-A @ swap ptr-field ;

: SLOT-LENS ( -- ^n ) SLOT-U @ ;

: SLOT-U! ( n n -- )
   cells SLOT-LENS + ! ;

: SLOT-U@ ( n -- n )
   cells SLOT-LENS + @ ;

: SLOT-RELEASE ( n -- ) {: ix:n :}
   ix SLOT-U@ {: u:n :}
   u 0 > if
      ix SLOT-FIELD @ u 1+ munmap 0 < if
         s" source root: cannot release a saved path" INCLUDE-IO-RC die
      then
   then
   NULL$ drop ix SLOT-FIELD !
   0 ix SLOT-U! ;

: ROOT-ENTER ( ^u8 n ^u8 n -- ^u8 n n )
   ROOT-CANON
   CANON-U @ {: u:n :}
   CANON-BUF u MAP-COPY {: fresh:^ :}
   CURRENT@
   SLOT-BASE @  SLOT-N @ SLOT-BASE !
   fresh u CURRENT! ;

\ The scope's current path at exit (its own, or one ROOT!, CD or POPPATH set) and
\ each path PUSHPATH saved in the scope are released once the caller's root is back.
: ROOT-LEAVE ( ^u8 n n -- )
   {: old:^ oldu:n base:n :}
   CURRENT@ {: cur:^ curu:n :}
   old oldu CURRENT!
   cur curu 1+ munmap 0 < if
      s" source root: cannot release a root mapping" INCLUDE-IO-RC die
   then
   SLOT-N @ SLOT-BASE @ ?do i SLOT-RELEASE loop
   SLOT-BASE @ SLOT-N !
   base SLOT-BASE ! ;

\ Clears the token read last and answers it, so no record a scope's callback
\ makes stands at the call that opened the scope.
: TOKEN-CLEAR ( -- ^u8 n )
   SRCLOC:TOKEN$ {: a:^ u:n :}
   NULL-PTR 0 SRCLOC:TOKEN!
   a u ;

public

\ The caller's root is back before the scope's is released, on a throw too.
: WITH ( ^u8 n [ -- ] -- ) {: a:^ u:n q :}
   s" SOURCE-ROOT:WITH" a u ROOT-ENTER {: old:^ oldu:n base:n :}
   JOIN-BUF JOIN-U @ CURRENT$ COMPILE-READS:ROOT+
   TOKEN-CLEAR {: t:^ tu:n :}
   q catch {: rc:n :}
   t tu SRCLOC:TOKEN!
   old oldu base ROOT-LEAVE
   rc 0= 0= if rc throw then ;

private

\ A current path is ROOT!'s mapping or POPPATH's, inside a scope also the scope's.
: RELEASE-CURRENT ( -- )
   CURRENT-U @ 0 > if
      CURRENT-PTR CURRENT-U @ 1+ munmap 0 < if
         s" source root: cannot release the current path" INCLUDE-IO-RC die
      then
   then
   NULL$ CURRENT! ;

: SLOT-GROW ( -- )
   SLOT-CAP @ {: oc:n :}
   oc 0= if
      SLOT-FIRST cells ARENA-ALLOC SLOT-A !
      SLOT-FIRST cells ARENA-ALLOC SLOT-U !
      SLOT-FIRST SLOT-CAP ! exit
   then
   SLOT-A @ oc cells oc 2 * cells ARENA-BYTES-GROW SLOT-A !
   SLOT-U @ oc cells oc 2 * cells ARENA-BYTES-GROW SLOT-U !
   oc 2 * SLOT-CAP ! ;

INP-CELL RESERVED-PTR-U8-CELL INPUT-AT

\ The tokenizer reads a newline as a blank and a piped stdin session is one
\ buffer, so a bare CD would otherwise take the next line's first word as its
\ directory.
: LINE-BLANK? ( -- bool )
   INPUT-AT @  REJECTION:INPUT$ + {: e:^ :}
   begin dup e < while
      dup c@ 10 = if drop INCLUDE-TRUE exit then
      dup c@ 32 > if drop INCLUDE-FALSE exit then
      1 +
   repeat
   drop INCLUDE-TRUE ;

\ Word w got the path a u that, alone or joined to the current root, is len bytes,
\ more than PATH-CAP.
: ROOT-LONG ( ^u8 n ^u8 n n -- ) {: w:^ wu:n a:^ u:n len:n :}
   w wu SRCLOC:SPELLED drop len INCLUDE-PATH-CAP REJECTION:PATH-RANGE
   w wu a u LOADER-PATH-AT if
      len u = if
         s" is too long"
      else
         s" is too long once joined to the current root"
      then REJECTION:EMIT-STR
   then
   REJECTION:HELP
   s" name a directory whose full path is at most " REJECTION:EMIT-STR
   INCLUDE-PATH-CAP REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

\ ROOT! for word w, the one the program called.
: ROOT-SET ( ^u8 n ^u8 n -- ) {: w:^ wu:n a:^ u:n :}
   w wu a u a u LOADER-PATH-CHECK
   u INCLUDE-PATH-CAP > if w wu a u u ROOT-LONG then
   CURRENT$ a u JOIN {: p:^ pu:n :}
   pu INCLUDE-PATH-CAP > if w wu a u pu ROOT-LONG then
   p pu ROOT-N {: n:n :}
   n -1 = if
      w wu a u p pu s" does not resolve to a directory"
      s" name an existing directory" PATH-DIE
   then
   n -3 = if
      w wu a u p pu s" is not a directory this process can search"
      s" name a directory, one this process may search" PATH-DIE
   then
   p pu CANON-BUF n COMPILE-READS:ROOT+
   CANON-BUF n MAP-COPY {: fresh:^ :}
   RELEASE-CURRENT
   fresh n CURRENT! ;

public

\ The directory is the current root, searched first by a relative require, until
\ the load or WITH body it runs in ends. hb's directory and engine root stay.
: ROOT! ( ^u8 n -- ) {: a:^ u:n :}
   s" SOURCE-ROOT:ROOT!" a u ROOT-SET ;
ROOT-MOVER

: CD ( -- )
   s" SOURCE-ROOT:CD" SRCLOC:SPELLED drop {: w:^ wu:n :}
   LINE-BLANK? if CURRENT$ type cr exit then
   w wu parse-name ROOT-SET ;
ROOT-MOVER

\ A null slot is the top-level root.
: PUSHPATH ( -- )
   SLOT-N @ SLOT-CAP @ >= if SLOT-GROW then
   SLOT-N @ {: ix:n :}
   CURRENT-U @ {: u:n :}
   u 0 > if CURRENT-PTR u MAP-COPY ix SLOT-FIELD ! then
   u ix SLOT-U!
   ix 1+ SLOT-N ! ;

: POPPATH ( -- )
   s" SOURCE-ROOT:POPPATH" SRCLOC:SPELLED drop {: w:^ wu:n :}
   SLOT-N @ SLOT-BASE @ = if
      s" E-SOURCE-ROOT" REJECTION:OPEN
      96 REJECTION:EMIT1  w wu REJECTION:EMIT-STR
      s" ` runs with no directory saved" REJECTION:EMIT-STR
      REJECTION:HELP
      s" save the current directory with `SOURCE-ROOT:PUSHPATH` first; " REJECTION:EMIT-STR
      s" a load or `SOURCE-ROOT:WITH` body pops only what it saved" REJECTION:EMIT-STR
      CHECKER-REJECT-RC REJECTION:STOP
   then
   SLOT-N @ 1- {: ix:n :}
   RELEASE-CURRENT
   ix SLOT-U@ {: u:n :}
   u 0 > if ix SLOT-FIELD @ u CURRENT! then
   NULL$ drop ix SLOT-FIELD !
   0 ix SLOT-U!
   ix SLOT-N ! ;
ROOT-MOVER

;package

using SOURCE-ROOT

: INCLUDE-DIE ( ^u8 n -- )
   INCLUDE-IO-RC die ;

: INCLUDE-CLOSE ( -- )
   INCLUDE-FD @ dup 0 >= if
      close
   else
      drop
   then
   -1 INCLUDE-FD ! ;

\ The loader word, the source token t tu, has no path after it. It is written
\ through its help, and the caller stops it.
: INCLUDE-NO-PATH ( ^u8 n -- ) {: t:^ tu:n :}
   t tu s" a path" t tu s" no path follows"
   REJECTION:MISSING-OPERAND
   s" write the file's path after the word, as " REJECTION:EMIT-STR
   s" `require src/lib/fs.f`" REJECTION:EMIT-STR ;

\ A loader word's parsed path of length u: a missing one is rejected at the word.
: INCLUDE-CHECK-PATH ( n -- ) {: u:n :}
   u 0 <= if
      SRCLOC:TOKEN$ INCLUDE-NO-PATH  CHECKER-REJECT-RC REJECTION:STOP
   then ;

package REQUIRE-REG
private

$7FFFFFFFFFFFFFFF constant MAX-BYTES
$7FFFFFFFFFFFFFFF 2 cells / constant ROW-LIMIT
3 cells constant CHUNK-HEAD
MAX-BYTES CHUNK-HEAD - constant CHUNK-LIMIT

\ A mapped chunk's room and used counts are the two cells after its prev
\ pointer, and its path bytes follow them at CHUNK-HEAD.
CAST: CHUNK-COUNTS ( ^^u8 -- ^n )

: SIZE-DIE ( -- )
   s" require: inventory size overflow" 76 die ;

: FROZEN-REC ( n -- ^n )
   2 * cells FROZEN @ swap + ;

: FROZEN-LEN ( n -- n )
   FROZEN-REC CELL + @ ;

: FROZEN-SLOT ( n -- ^u8 ) {: idx:n :}
   FROZEN @ idx FROZEN-REC @ + ;

: ROW-PTR ( n -- ^^u8 )
   ROWS @ swap ptr-field ;

: ROW-PTR@ ( n -- ^u8 )
   ROW-PTR @ ;

: ROW-LEN-ADDR ( n -- ^n )
   ROW-LENS @ swap cells + ;

: ROW-LEN@ ( n -- n )
   ROW-LEN-ADDR @ ;

: FROZEN-BYTES ( n -- n )
   2 * cells ;

: ROW-NEW-CAP ( n -- n ) {: need:n :}
   need $40 max
   ROW-CAP @ ROW-LIMIT 2 / <= if ROW-CAP @ 2 * max then ;

: COL-MAP ( n -- ^a )
   cells map-anon 0 <> if
      drop s" require: cannot map the row table" INCLUDE-DIE
   then ;

: COLS-UNMAP ( ^^u8 ^n n -- ) {: rows:^ lens:^ cap:n :}
   rows cap cells munmap {: rowsrc:n :}
   lens cap cells munmap {: lensrc:n :}
   rowsrc 0 < lensrc 0 < or if
      s" require: cannot release the row table" INCLUDE-DIE
   then ;

: ROW-GROW ( n -- ) {: need:n :}
   need ROW-LIMIT > if SIZE-DIE then
   need ROW-NEW-CAP {: cap:n :}
   ROWS @ ROW-LENS @ ROW-CAP @ {: old:^ oldlens:^ oldcap:n :}
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

: CHUNK-PREV@ ( ^^u8 -- ^^u8 )
   0 ptr-field @ ;

: CHUNK-ROOM@ ( ^^u8 -- n )
   CHUNK-COUNTS CELL + @ ;

: CHUNK-USED@ ( ^^u8 -- n )
   CHUNK-COUNTS 2 cells + @ ;

: CHUNK-USED! ( n ^^u8 -- )
   CHUNK-COUNTS 2 cells + ! ;

: CHUNK-DATA ( ^^u8 -- ^u8 )
   CHUNK-COUNTS byte-view CHUNK-HEAD + ;

: CHUNK-BYTES ( ^^u8 -- n )
   CHUNK-ROOM@ CHUNK-HEAD + ;

: CHUNK-NEW-ROOM ( n -- n ) {: u:n :}
   CHUNK @ NULL-PTR = if u $1000 max exit then
   CHUNK @ CHUNK-ROOM@ {: old:n :}
   old CHUNK-LIMIT 2 / <= if old 2 * u max else u then ;

: CHUNK-NEW ( n -- )
   CHUNK-NEW-ROOM {: room:n :}
   room CHUNK-LIMIT > if SIZE-DIE then
   CHUNK @ {: prev:^ :}
   room CHUNK-HEAD + map-anon 0 <> if
      drop s" require: cannot map a path chunk" INCLUDE-DIE
   then
   {: fresh:^ :}
   fresh CHUNK !
   prev CHUNK @ 0 ptr-field !
   room CHUNK @ CHUNK-COUNTS CELL + !
   0 CHUNK @ CHUNK-USED! ;

: CHUNK-SPACE ( n -- ^u8 ) {: u:n :}
   CHUNK @ NULL-PTR = if
      u CHUNK-NEW
   else
      u CHUNK @ CHUNK-ROOM@ CHUNK @ CHUNK-USED@ - > if u CHUNK-NEW then
   then
   CHUNK @ {: chunk:^ :}
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

: CHUNK-CONTAINS? ( ^^u8 ^u8 -- bool ) {: chunk:^ path:^ :}
   path chunk CHUNK-DATA - {: off:n :}
   off 0 >= off chunk CHUNK-ROOM@ < and ;

: CHUNK-TRIM-TO ( ^^u8 ^u8 n -- ) {: chunk:^ path:^ u:n :}
   path chunk CHUNK-DATA - u + chunk CHUNK-USED!
   chunk CHUNK ! ;

: CHUNKS-TRIM ( ^u8 n -- ) {: path:^ u:n :}
   CHUNK @ begin dup NULL-PTR <> while
      dup path CHUNK-CONTAINS? if path u CHUNK-TRIM-TO exit then
      dup CHUNK-PREV@ swap dup CHUNK-BYTES munmap 0 < if
         s" require: cannot release a path chunk" INCLUDE-DIE
      then
   repeat drop
   s" require: path chunk missing" 76 die ;

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

: PAD-BYTES ( ^u8 -- n )
   data-base - negate CELL 1- and ;

: TOTAL-BYTES ( n n -- n ) {: bytes:n pad:n :}
   bytes MAX-BYTES CELL 1- - pad - > if SIZE-DIE then
   pad bytes + ROUND-CELL ;

public

: SLOT ( n -- ^u8 ) {: idx:n :}
   idx PREFIX-N @ < if idx FROZEN-SLOT else idx ROW-PTR@ then ;

: LEN ( n -- n ) {: idx:n :}
   idx PREFIX-N @ < if idx FROZEN-LEN else idx ROW-LEN@ then ;

: APPEND ( ^u8 n -- ) {: a:^ u:n :}
   u 0 <= u INCLUDE-PATH-CAP > or if
      s" require: invalid path length" 76 die
   then
   REQUIRE-N @ {: idx:n :}
   idx ROW-LIMIT >= if SIZE-DIE then
   idx 1+ ROW-ENSURE
   u CHUNK-SPACE {: path:^ :}
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

\ Four other cells are cursors into rows; a bare REQUIRE-N ! would leave them
\ past the new end.
: TRUNCATE ( n -- )
   {: keep:n :}
   keep 0 < keep REQUIRE-N @ > or if
      s" require: truncate outside the registry" 76 die
   then
   keep REWIND
   REQUIRE-BOOT-N @ keep > if keep REQUIRE-BOOT-N ! then
   REQUIRE-BASE @ keep > if keep REQUIRE-BASE ! then
   REQUIRE-SAVE-N @ keep > if keep REQUIRE-SAVE-N ! then
   REQUIRE-SAVE-BASE @ keep > if keep REQUIRE-SAVE-BASE ! then ;

: MARK ( -- n ) REQUIRE-N @ ;

\ A failed require may have loaded a child in a reader or unit callback before
\ entering evaluation. Those child definitions survive that failure; remove
\ only the parent's provisional row and keep later rows in order.
: REMOVE ( n -- )
   {: idx:n :}
   idx PREFIX-N @ < idx REQUIRE-N @ >= or if
      s" require: remove outside live rows" 76 die
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
   here {: start:^ :}
   start PAD-BYTES {: pad:n :}
   bytes pad TOTAL-BYTES {: total:n :}
   total allot
   start pad + {: fresh:^ :}
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

;package

: REQUIRE-LEN@ ( n -- n )
   REQUIRE-REG:LEN ;

: REQUIRE-SLOT ( n -- ^u8 )
   REQUIRE-REG:SLOT ;

: REQUIRE-BYTE= ( ^u8 n n -- bool ) {: a:^ idx:n i:n :}
   a i ZBYTE@ idx REQUIRE-SLOT i ZBYTE@ = ;

: REQUIRE-PATH= ( ^u8 n n -- bool ) {: a:^ u:n idx:n :}
   idx REQUIRE-LEN@ u <> if INCLUDE-FALSE exit then
   0 begin dup u < while
      dup a idx rot REQUIRE-BYTE= 0= if drop INCLUDE-FALSE exit then
      1+
   repeat drop INCLUDE-TRUE ;

: REQUIRE-KNOWN? ( ^u8 n -- bool ) {: a:^ u:n :}
   REQUIRE-BASE @ begin dup REQUIRE-N @ < while
      dup a u rot REQUIRE-PATH= if drop INCLUDE-TRUE exit then
      1+
   repeat drop INCLUDE-FALSE ;

package SOURCE-ROOT
private

: LOADED? ( ^u8 n -- bool ) {: a:^ u:n :}
   a u PROGRAM? if INCLUDE-TRUE exit then
   a u REQUIRE-KNOWN? ;

\ The running engine's path: on macOS the one execve received, in the
\ executable_path= apple string past envp's terminator; on Linux
\ /proc/self/exe. Both name the engine, not the script, for a `#!` program
\ file. proc_pidpath needs FUNCTION:, which this layer cannot use.
: ENVP-END ( -- n )
   0 begin dup ENVP 0= 0= while 1+ repeat 1+ ;

\ Apple strings can be empty, so the scan ends at the null pointer.
: APPLE-EXE ( -- ^u8 n )
   ENVP-BASE 0= if NULL$ exit then
   ENVP-END begin dup ENVP 0= 0= while
      dup ENVP s" executable_path" ENV=? if
         ENVP s" executable_path=" nip ZPTR+ dup ZLEN exit
      then
      1+
   repeat drop NULL$ ;

\ `%` in CANON-BUF, as a length. 0 when there is no path, it is longer than
\ PATH-CAP or realpath rejects it; never a throw: the length is checked before
\ CANON-N, and neither path holds a NUL.
: EXE-ROOT ( ^u8 n -- n ) {: a:^ u:n :}
   u 0= u INCLUDE-PATH-CAP > or if 0 exit then
   a u CANON-N {: n:n :}
   n 0 <= if 0 exit then
   CANON-BUF CANON-BUF n PARENT-U PARENT-U ;

: TREE? ( ^u8 n -- bool )
   0 REQUIRE-SLOT 0 REQUIRE-LEN@ JOIN ZPATH-OF 0 access 0= ;

\ The root's length in CANON-BUF, 0 for the working directory.
: ENGINE-FIND-FOR ( ^u8 n -- n )
   REQUIRE-BOOT-LIMIT 0= if 2drop 0 exit then
   CWD$ TREE? if 2drop 0 exit then
   EXE-ROOT {: n:n :}
   n 0= if 0 exit then
   CANON-BUF n TREE? 0= if 0 exit then
   n ;

\ A target with no executable path, as Wasm, loads no source.
: ENGINE-FIND ( -- n )
   HB-TARGET-MACOS? if APPLE-EXE ENGINE-FIND-FOR exit then
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if
      s" /proc/self/exe" ENGINE-FIND-FOR exit
   then
   s" include: no engine path on this target" 76 die ;

: ENGINE-INIT ( -- )
   ENGINE-READ @ 0= 0= if exit then
   ENGINE-FIND {: n:n :}
   CANON-BUF ENGINE-BUF n BYTE-COPY
   n ENGINE-U !
   1 ENGINE-READ ! ;

public

\ The root is derived once, from whether the working directory is a Habu tree, so every answer
\ records that read: a part's reads do not depend on which part asked first.
: ENGINE$ ( -- ^u8 n )
   ENGINE-INIT
   ENGINE-U @ 0= if CWD$ exit then
   CWD-RECORD ENGINE-BUF ENGINE-U @ ;

\ Override the fallback root; the caller restores the old one after its load.
: ENGINE! ( ^u8 n -- ) {: a:^ u:n :}
   s" SOURCE-ROOT:ENGINE!" a u ROOT-CANON
   CANON-BUF ENGINE-BUF CANON-U @ BYTE-COPY
   CANON-U @ ENGINE-U !
   1 ENGINE-READ ! ;
ROOT-MOVER

private

: REQUEST! ( ^u8 n -- ) {: a:^ u:n :}
   a u CHECK
   u REQUEST-FIT  a REQUEST-BUF u BYTE-COPY u REQUEST-U ! ;

: REQUEST$ ( -- ^u8 n ) REQUEST-BUF REQUEST-U @ ;

\ Portable names describe only frozen engine facts, and a boot row IS its
\ portable name, so this reads the registry itself. Ordinary application facts
\ retain their canonical identity so identical names in distinct roots coexist.
: BOOT-KNOWN? ( ^u8 n n -- bool ) {: a:^ u:n first:n :}
   first begin dup REQUIRE-BOOT-LIMIT < while
      dup a u rot REQUIRE-PATH= if drop INCLUDE-TRUE exit then
      1+
   repeat drop INCLUDE-FALSE ;

: CANDIDATE$ ( -- ^u8 n ) CANDIDATE-BUF CANDIDATE-U @ ;

\ A request spelled under a root, normalized but not resolved, in NORMAL-BUF: no
\ longer than the join and a leading `/`, which may outrun PATH-CAP.
: UNDER ( ^u8 n ^u8 n -- ^u8 n )
   JOIN {: a:^ u:n :}
   a u u 1+ NORMALIZE-LIMIT
   NORMAL-BUF NORMAL-U @ ;

\ Whether a selected candidate is the engine root's own copy of a frozen row,
\ from row `first` on. Below the root its physical path spells the row. A
\ directory symlink inside the root can carry the root's spelling of a row
\ outside it, so the request spelled under the root is asked too, and counts
\ only when it resolves to the same physical candidate: symlink/.. can name
\ another file. Spelling the request reuses NORMAL-BUF, which holds the
\ candidate, so the candidate is copied first.
: BOOT-CANDIDATE ( ^u8 n ^u8 n ^u8 n n -- ^u8 n bool )
   {: a:^ u:n root:^ rootu:n req:^ requ:n first:n :}
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
: CANDIDATE ( ^u8 n n -- ^u8 n bool bool )
   {: first:n :}
   2dup OWNER!
   REQUEST$ JOIN CANONICAL {: exists:bool :}
   2dup LOADED? {: known:bool :}
   known 0= if
      ENGINE$ REQUEST$ first BOOT-CANDIDATE
   else known then
   dup exists or ;

: ENGINE-SEARCHED? ( -- bool ) ENGINE$ CURRENT$ CORE-STR= 0= ;
: CWD-SEARCHED? ( -- bool )
   CWD$ CURRENT$ CORE-STR= CWD$ ENGINE$ CORE-STR= or 0= ;

\ A relative request's roots in order, each once: its owner, the engine root,
\ then the working directory. The first root whose candidate stops the search
\ answers, found; when none does, the owner's answer, not found, names the
\ missing file. So a library the owner lacks is the engine's whatever the
\ working directory holds, and the working directory answers only what neither
\ holds: an application's entry below its tree, run from the tree, requires the
\ tree's files by their tree-relative names.
: SEARCH-ROOTS ( n -- ^u8 n bool bool )
   {: first:n :}
   CURRENT$ first CANDIDATE dup if exit then 2drop 2drop
   ENGINE-SEARCHED? if
      ENGINE$ first CANDIDATE dup if exit then 2drop 2drop
   then
   CWD-SEARCHED? if
      CWD$ first CANDIDATE dup if exit then 2drop 2drop
   then
   CURRENT$ first CANDIDATE ;

\ A missing file's directory need not be a root at all (missing, a file,
\ unsearchable), so its answer keeps the current root.
: ABS-OWNER ( ^u8 n bool -- )
   {: a:^ u:n exists:bool :}
   a u CURRENT$ BELOW? if CURRENT$ OWNER! exit then
   a u ENGINE$ BELOW? if ENGINE$ OWNER! exit then
   a u CWD$ BELOW? if CWD$ OWNER! exit then
   exists 0= if CURRENT$ OWNER! exit then
   a u DIRNAME OWNER! ;

\ The file a require of REQUEST$ selects, with frozen rows counted from row
\ `first`, whether it is known and whether it was found.
: SELECT ( n -- ^u8 n bool bool )
   {: first:n :}
   REQUEST-BUF c@ $2F = if
      REQUEST$ CANONICAL {: exists:bool :}
      2dup exists ABS-OWNER
      2dup LOADED? {: known:bool :}
      known 0= if
         ENGINE$ REQUEST$ first BOOT-CANDIDATE
      else known then
      dup exists or exit
   then
   first SEARCH-ROOTS ;

\ The source token an open that fails stands at: LOAD-RUN binds its caller's
\ request token, empty for one no frame holds; RUN-PROGRAM, WITH-BYTES and
\ WITH-SUPPLIED bind it empty. Each binds it for its load and puts the
\ previous one back when the load ends.
PTR-VARIABLE REQUEST-AT-A
variable REQUEST-AT-U
: REQUEST-AT! ( ^u8 n -- ) REQUEST-AT-U ! REQUEST-AT-A ! ;
: REQUEST-AT$ ( -- ^u8 n ) REQUEST-AT-A @ REQUEST-AT-U @ ;

public

\ The label of errno e, and the os error its record names: e where the label
\ does not name e, else 0.
: ERRNO-WHY$ ( n -- ^u8 n n ) {: e:n :}
   e 2 = if s" no such file" 0 exit then
   e 13 = if s" permission denied" 0 exit then
   e 20 = if s" a directory in the path is a file" 0 exit then
   e 21 = if s" is a directory" 0 exit then
   e 5 = if s" input/output error" 0 exit then
   e 23 = e 24 = or if s" too many open files" 0 exit then
   e 12 = if s" out of memory" 0 exit then
   s" the system call failed" e ;

\ Writes why, then os error e unless e is 0, as Rust names an OS error.
: UNREAD-WHY ( ^u8 n n -- ) {: why:^ whyu:n e:n :}
   why whyu REJECTION:EMIT-STR
   e 0= if exit then
   s"  (os error " REJECTION:EMIT-STR  e REJECTION:EMIT-N  41 REJECTION:EMIT1 ;

private

\ Writes a u as one single-quoted shell word: each `'` in it closes the quote,
\ is written escaped and reopens it, so no path byte acts in a shell.
: SH-QUOTED ( ^u8 n -- ) {: a:^ u:n :}
   39 REJECTION:EMIT1
   u 0 ?do
      a i + c@ dup 39 = if drop s" '\''" REJECTION:EMIT-STR else REJECTION:EMIT1 then
   loop
   39 REJECTION:EMIT1 ;

\ The help for path, whose open or read failed with errno e. EACCES's names no
\ owner: Habu looks none up, and the reader knows whose file it is.
: OPEN-HELP ( ^u8 n n -- ) {: path:^ pathu:n e:n :}
   e 2 = if s" create the file or correct the path" REJECTION:EMIT-STR exit then
   e 13 = if
      s" if the file is yours, make it readable with `chmod u+r " REJECTION:EMIT-STR
      path pathu SH-QUOTED
      s" `; else ask its owner to let you read it" REJECTION:EMIT-STR exit
   then
   e 20 = if
      s" correct the path, so that each name in it before the last is a directory"
      REJECTION:EMIT-STR exit
   then
   e 21 = if s" name a source file, not a directory" REJECTION:EMIT-STR exit then
   e 23 = e 24 = or if
      s" raise the open-file limit with `ulimit -n`, " REJECTION:EMIT-STR
      s" or close files this process holds open" REJECTION:EMIT-STR exit
   then
   e 12 = if s" free memory, then load it again" REJECTION:EMIT-STR exit then
   s" check that the file exists and is readable" REJECTION:EMIT-STR ;

\ The record of path, not read to its end, labelled why and os error e; at t tu
\ with path in a note labelled note, else unplaced; written through its help.
: UNREAD-OS-RECORD ( ^u8 n ^u8 n ^u8 n n ^u8 n -- )
   {: t:^ tu:n path:^ pathu:n why:^ whyu:n e:n note:^ noteu:n :}
   s" E-UNREADABLE-SOURCE" REJECTION:OPEN
   s" cannot read `" REJECTION:EMIT-STR
   tu 0= if
      path pathu REJECTION:EMIT-STR  s" `: " REJECTION:EMIT-STR
      why whyu e UNREAD-WHY  REJECTION:HELP exit
   then
   t tu REJECTION:EMIT-STR  96 REJECTION:EMIT1
   t tu REJECTION:AT  why whyu e UNREAD-WHY
   note noteu REJECTION:NOTE  path pathu REJECTION:EMIT-STR
   REJECTION:HELP ;

: UNREAD-RECORD ( ^u8 n ^u8 n ^u8 n -- ) 0 s" resolved" UNREAD-OS-RECORD ;

\ EISDIR for a path that names a directory, else e: an open of a directory
\ without read permission fails EACCES, but a stat of it succeeds.
: SOURCE-ERRNO ( ^u8 n n -- n )
   {: path:^ pathu:n e:n :}
   path pathu ZPATH-OF COMPILE-READS:STAMP drop nip 32 rshift
   FS-ABI:S-IFDIR = if 21 exit then e ;

public

\ Rejects path, found but failing its open or a read with errno e, at t tu, else
\ unplaced. Public so a walk that reads a found file rejects it with its record.
: UNREADABLE-SOURCE ( ^u8 n ^u8 n n -- )
   {: t:^ tu:n path:^ pathu:n e:n :}
   path pathu e SOURCE-ERRNO {: why:n :}
   t tu path pathu why ERRNO-WHY$ s" resolved" UNREAD-OS-RECORD
   path pathu why OPEN-HELP
   INCLUDE-IO-RC REJECTION:STOP ;

\ The file path changed while it was read, so its bytes may be no single
\ version of it. Placed as UNREADABLE-SOURCE places its record.
: CHANGED-SOURCE ( ^u8 n ^u8 n -- )
   s" changed while it was read" UNREAD-RECORD
   s" load it again once nothing is writing it" REJECTION:EMIT-STR
   INCLUDE-IO-RC REJECTION:STOP ;

\ The file path is a stream, not a regular file, so a reader that stamps what
\ it reads cannot stamp it. Placed as UNREADABLE-SOURCE places its record.
: UNSTAMPED-SOURCE ( ^u8 n ^u8 n -- )
   s" is not a regular file" UNREAD-RECORD
   s" load a regular file, whose bytes a stamp can name" REJECTION:EMIT-STR
   INCLUDE-IO-RC REJECTION:STOP ;

private

: INCLUDE-ERRNO ( -- n )   data-base FS-ABI:ERRNO-OFF + @ ;

: RESOLVED-U ( n -- n ) DENIED-PATH swap CANON-PREFIX ;

: SLASHES-END ( n n -- n ) {: w:n i:n :}
   i begin dup w < while
      dup DENIED-PATH + c@ 47 = 0= if exit then
      1+
   repeat ;

\ The w-byte DENIED-PATH with the link after its k-byte prefix, real path in
\ CANON-BUF, followed: the length, 0 for no link or a target past PATH-CAP.
: LINK-NEXT ( n n -- n )
   {: w:n k:n :}
   w k SLASHES-END {: c:n :}
   c w >= if 0 exit then
   DENIED-PATH w c SEG-END {: e:n :}
   CANON-U @ {: r:n :}
   DENIED-PATH c + e c - SEG-PATH {: lu:n :}
   WORK-BUF lu ZPATH-OF  PATH-BYTES WORK-FIT
   WORK-BUF PATH-BYTES readlink {: t:n :}
   t 1 <  t PATH-BYTES >=  or if 0 exit then
   WORK-BUF c@ 47 = if 0 else lu e c - - then {: head:n :}
   head t + w e - + {: total:n :}
   t w e - + WORK-FIT
   DENIED-PATH e + WORK-BUF t + w e - BYTE-COPY
   total DENIED-FIT
   head 0 > if
      CANON-BUF DENIED-PATH r BYTE-COPY
      head r > if 47 DENIED-PATH r + c! then
   then
   WORK-BUF DENIED-PATH head + total head - BYTE-COPY  total ;

\ The walk of the w-byte DENIED-PATH after links links: the longest prefix the
\ canon resolves either denies search or is followed by the link to replace.
: DENIED-FROM ( n n -- ^u8 n ) {: w:n links:n :}
   w RESOLVED-U {: k:n :}
   k 0= if DENIED-PATH 0 exit then
   CANON-U @ {: r:n :}
   r ENGINE-DIR? 0= INCLUDE-ERRNO 13 = and if
      r DENIED-FIT  CANON-BUF DENIED-PATH r BYTE-COPY  DENIED-PATH r exit
   then
   links 40 >= if DENIED-PATH 0 exit then
   w k LINK-NEXT {: next:n :}
   next 0= if DENIED-PATH 0 exit then
   next links 1+ RECURSE ;

\ The real directory whose search denied a lookup of path, symlinks followed;
\ 0 when no lookup is denied or the walk cannot reach that directory.
: SEARCH-DENIED ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   u DENIED-FIT  a DENIED-PATH u BYTE-COPY
   u 0 DENIED-FROM ;

\ Rejects the joined path a denied lookup searched, at t tu, else unplaced, by
\ its CANONICAL name; the help names dir, in DENIED-PATH, that CANONICAL spares.
: UNSEARCHABLE-SOURCE ( ^u8 n ^u8 n -- ) {: t:^ tu:n path:^ pathu:n :}
   path pathu SEARCH-DENIED {: dir:^ diru:n :}
   diru 0= if exit then
   t tu path pathu CANONICAL drop 13 ERRNO-WHY$ s" searched" UNREAD-OS-RECORD
   s" if the directory `" REJECTION:EMIT-STR  dir diru REJECTION:EMIT-STR
   s" ` is yours, make it searchable with `chmod u+x " REJECTION:EMIT-STR
   dir diru SH-QUOTED
   s" `; else ask its owner to let you search it" REJECTION:EMIT-STR
   INCLUDE-IO-RC REJECTION:STOP ;

public

\ Rejects request a u, which no root holds, at p, unplaced when pu is 0; as
\ unreadable where a denied lookup hides it. An entry is searched for and
\ helped as a command-line path. Public for check.f's and the verifier's walks.
: MISSING-SOURCE ( ^u8 n ^u8 n bool -- )
   {: a:^ u:n p:^ pu:n entry:bool :}
   entry  a c@ 47 =  or if
      p pu CWD$ a u JOIN UNSEARCHABLE-SOURCE
   else
      p pu CURRENT$ a u JOIN UNSEARCHABLE-SOURCE
      ENGINE-SEARCHED? if p pu ENGINE$ a u JOIN UNSEARCHABLE-SOURCE then
      CWD-SEARCHED? if p pu CWD$ a u JOIN UNSEARCHABLE-SOURCE then
   then
   s" E-MISSING-SOURCE" REJECTION:OPEN
   s" cannot open `" REJECTION:EMIT-STR  a u REJECTION:EMIT-STR
   96 REJECTION:EMIT1
   pu 0 <> if p pu REJECTION:AT  s" not found" REJECTION:EMIT-STR then
   s" searched" REJECTION:NOTE
   entry  a c@ 47 =  or if
      CWD$ a u JOIN CANONICAL drop REJECTION:EMIT-STR
   else
      CURRENT$ a u JOIN CANONICAL drop REJECTION:EMIT-STR
      ENGINE-SEARCHED? if
         s" , " REJECTION:EMIT-STR
         ENGINE$ a u JOIN CANONICAL drop REJECTION:EMIT-STR
      then
      CWD-SEARCHED? if
         s" , " REJECTION:EMIT-STR
         CWD$ a u JOIN CANONICAL drop REJECTION:EMIT-STR
      then
   then
   REJECTION:HELP
   entry if
      s" a path given on the command line is looked up from the " REJECTION:EMIT-STR
      s" directory hb runs in" REJECTION:EMIT-STR
   else
      s" check the path: a relative path is looked up under the root " REJECTION:EMIT-STR
      s" the including file was found under, then Habu's source root, " REJECTION:EMIT-STR
      s" then the directory hb runs in" REJECTION:EMIT-STR
   then
   CHECKER-REJECT-RC REJECTION:STOP ;

\ RESOLVE of a u whose rejection stands at t tu, unplaced when tu is 0. SELECT
\ and CANONICAL may reuse the buffer a u lies in, so a rejection names
\ REQUEST$, the request's copy.
: RESOLVE-AT ( ^u8 n ^u8 n -- ^u8 n bool ) {: a:^ u:n t:^ tu:n :}
   a u REQUEST! REQUIRE-BASE @ SELECT drop
   >r REQUEST$ t tu CONFINE-AS r> ;

\ A rejection stands at the source a u stands for, else nowhere.
: RESOLVE ( ^u8 n -- ^u8 n bool )
   2dup SRCLOC:ORIGIN$ RESOLVE-AT ;

private

: LOAD-RESOLVE ( ^u8 n ^u8 n -- ^u8 n bool ) {: a:^ u:n t:^ tu:n :}
   a u REQUEST! REQUIRE-BASE @ SELECT {: found:bool :}
   found 0= if REQUEST$ t tu INCLUDE-FALSE MISSING-SOURCE then
   >r REQUEST$ t tu CONFINE-AS r> ;

public

\ Command-line entries are relative to the invocation directory only;
\ dependencies beneath them inherit the entry directory as their primary root.
: ENTRY-RESOLVE ( ^u8 n -- ^u8 n bool ) {: a:^ u:n :}
   a u REQUEST!
   CWD$ REQUIRE-BASE @ CANDIDATE
   {: path:^ pathu:n known:bool found:bool :}
   found 0= if a u NULL-PTR 0 INCLUDE-TRUE MISSING-SOURCE then
   path pathu DIRNAME OWNER!
   path pathu CONFINE known ;

\ The selection counts every frozen row whatever REQUIRE-BASE is, so a frozen
\ row is the engine's even where its file is missing. RESOLVED-ROOT$ stays as
\ the caller's own RESOLVE left it.
: ENGINE-KNOWN? ( ^u8 n -- bool )
   OWNER-BUF OWNER-SAVE OWNER-U @ BYTE-COPY
   OWNER-U @ {: owner:n :}
   REQUEST! 0 SELECT 2drop
   ENGINE$ REQUEST$ 0 BOOT-CANDIDATE nip nip
   OWNER-SAVE OWNER-BUF owner BYTE-COPY
   owner OWNER-U ! ;

: BOOT-REJECT ( ^u8 n -- ) {: root:^ rootu:n :}
   s" E-SOURCE-ROOT" REJECTION:OPEN
   96 REJECTION:EMIT1  REQUEST$ REJECTION:EMIT-STR
   s" ` is a boot file outside the engine root" REJECTION:EMIT-STR
   s" root" REJECTION:NOTE  root rootu REJECTION:EMIT-STR
   REJECTION:HELP
   s" require it after `REQUIRE-BOOT-FREEZE`, or move it under `" REJECTION:EMIT-STR
   root rootu REJECTION:EMIT-STR  96 REJECTION:EMIT1
   CHECKER-REJECT-RC REJECTION:STOP ;

\ The row a boot file is recorded as: the spelling BOOT-CANDIDATE asks for. That
\ is its path below the engine root, or, for a file a directory symlink inside
\ the root carries from outside it, the request spelled under the root when that
\ resolves to this very file.
\ Any other file is rejected by name: its row could only be its canonical path,
\ and an engine carrying one takes that file for a stranger under every other
\ root.
: BOOT-ROW ( ^u8 n -- ^u8 n ^u8 n ) {: a:^ u:n :}
   u CANDIDATE-U ! a CANDIDATE-BUF u BYTE-COPY
   ENGINE$ {: root:^ rootu:n :}
   CANDIDATE$ root rootu BELOW? if
      CANDIDATE$ CANDIDATE$ root rootu RELATIVE exit
   then
   root rootu REQUEST$ UNDER root rootu BELOW? if
      NORMAL-BUF NORMAL-U @ CANONICAL drop CANDIDATE$ CORE-STR=
   else INCLUDE-FALSE then
   0= if root rootu BOOT-REJECT then
   CANDIDATE$ root rootu REQUEST$ UNDER root rootu RELATIVE ;

;package

\ A boot row is recorded portably, in the one spelling BOOT-KNOWN? asks for,
\ relative to the engine root; a build's fresh registry makes that root the
\ working directory. Against the load's own owner root a require issued by a
\ boot file would shorten to a bare basename no query spells, naming a
\ different file in every directory. A canonical absolute spelling would bake
\ the build directory into the engine, so two builds of one revision from two
\ directories would differ. An application row keeps its canonical identity;
\ the two cannot collide, as a canonical name begins with `/` and a portable
\ one never does. The path comes back in storage the store did not reuse, for
\ the caller's load.
: REQUIRE-STORE ( ^u8 n -- ^u8 n )
   REQUIRE-BOOT-OPEN? if BOOT-ROW else 2dup then
   REQUIRE-REG:APPEND ;

: INCLUDE-PATH0 ( ^u8 n -- ^u8 )
   {: a:^ u:n :}
   a u INCLUDE-PATH INCLUDE-PATH-CAP 1+ PATHZ
   u INCLUDE-PATH-U !
   INCLUDE-PATH ;

variable SCRIPT-NAMED-PEND

: SCRIPT-NAMED-PEND! ( bool -- )
   SCRIPT-NAMED-PEND ! ;


\ Every source load runs through the bound runner. The ordinary one keeps the
\ frame and scope behavior; a build binds its own once the loader has loaded.
package SOURCE-UNIT
private

TYPED-VARIABLE LOAD-XT [ ^u8 n ^u8 n ^u8 n [ n n -- ] -- ]

: ORDINARY ( ^u8 n ^u8 n ^u8 n [ n n -- ] -- )
   {: path:^ pathu:n root:^ rootu:n source:^ sourceu:n q :}
   0 sourceu q execute ;

public

: LOAD ( ^u8 n ^u8 n ^u8 n [ n n -- ] -- ) LOAD-XT @ execute ;
\ A runner calls the file's quotation as `0 sourceu q execute` with nothing of
\ its own beneath it; the quotation throws for a refused file.
: USE ( [ ^u8 n ^u8 n ^u8 n [ n n -- ] -- ] -- ) LOAD-XT ! ;
: CURRENT ( -- [ ^u8 n ^u8 n ^u8 n [ n n -- ] -- ] ) LOAD-XT @ ;
: RESET ( -- ) [: ORDINARY ;] LOAD-XT ! ;

;package

SOURCE-UNIT:RESET

\ Source-composition event log: one event per loader act, the request as spelled
\ and the path it resolved to, so a restricted discovery pass can rebuild include
\ multiplicity and require/provided registry state in order, and a build's
\ linker can carry the acts in a file's object.
\ During discovery the walker runs DISCOVERY-ON and passes the loader-token span
\ to DISC-TOK! and the span where the path is written to DISC-PATH!. A build's
\ linker reads the depth to tell a request made with cells on the stack.

15 constant EVENT-FIELDS
EVENT-FIELDS cells constant EVENT-BYTES
$1000 constant EVENT-ROOM-MIN                    \ a first mapping's bytes

0 constant EV-INCLUDED
1 constant EV-REQUIRED
2 constant EV-PROVIDED
0 constant EV-STATE-FRESH
1 constant EV-STATE-KNOWN

\ Mappings that grow as events arrive; the pool holds each event's paths and
\ roots, a root stored once. Each CAP counts its mapping's bytes, 0 unmapped.
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
variable DISC-PATH-A
variable DISC-PATH-U

: EVENT-ON? ( -- bool )   EVENT-ON-V @ 0= 0= ;
: DISCOVERY? ( -- bool )  EVENT-DISC-V @ 0= 0= ;
: EVENT-ON ( -- )         1 EVENT-ON-V ! ;
: EVENT-OFF ( -- )        0 EVENT-ON-V ! ;
: DISCOVERY-ON ( -- )     1 EVENT-DISC-V ! ;
: DISCOVERY-OFF ( -- )    0 EVENT-DISC-V ! ;
: DISC-TOK! ( n n -- )    DISC-TOK-U ! DISC-TOK-A ! ;
: DISC-PATH! ( n n -- )   DISC-PATH-U ! DISC-PATH-A ! ;
: EVENT-RECS@ ( -- ^u8 ) EVENT-RECS @ ;
: EVENT-POOL@ ( -- ^u8 ) EVENT-POOL @ ;

: EVENT-UNMAP ( ^u8 n -- ) {: a:^ u:n :}
   u 0= if exit then
   a u munmap 0 < if s" events: cannot release an event table" INCLUDE-DIE then ;

\ A fresh mapping of CAP bytes holding the first USED bytes of OLD, which maps
\ OLDCAP bytes and is released first; the caller then publishes the answer and
\ CAP, so a refusal ends the process while OLD is still the one published.
: EVENT-REMAP ( ^u8 n n n -- ^u8 )
   {: old:^ oldcap:n used:n cap:n :}
   cap map-anon 0= 0= if drop s" events: cannot map an event table" INCLUDE-DIE then
   {: fresh:^ :}
   old fresh used BYTE-COPY
   old oldcap EVENT-UNMAP
   fresh ;

\ Reset releases both mappings, so snapshot preparation leaves a captured image
\ no path of the machine that recorded them and no address of a mapping.
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

\ Outside discovery no reader takes the path's span: it is the loader token's.
: EVENT-PATH-SPAN ( -- n n )
   DISCOVERY? if DISC-PATH-A @ DISC-PATH-U @ exit then
   LOADER-TOKEN-SPAN ;

: EVENT-SLOT ( n n -- ^n )
   swap EVENT-FIELDS * + cells EVENT-RECS@ + CELL-VIEW ;

: EVENT-FIELD@ ( n n -- n )  EVENT-SLOT @ ;
: EVENT-FIELD! ( n n n -- )  EVENT-SLOT ! ;

: EVENT-POOL-AT ( n -- ^u8 )  EVENT-POOL@ + ;

: EVENT-GROWN ( n n -- n ) {: need:n cap:n :}
   cap 2 * need max EVENT-ROOM-MIN max ;

: EVENT-RECS-ROOM ( -- )
   EVENT-N @ 1 + EVENT-BYTES * {: need:n :}
   need EVENT-RECS-CAP @ <= if exit then
   need EVENT-RECS-CAP @ EVENT-GROWN {: cap:n :}
   EVENT-RECS@ EVENT-RECS-CAP @ EVENT-N @ EVENT-BYTES * cap EVENT-REMAP
   EVENT-RECS !
   cap EVENT-RECS-CAP ! ;

: EVENT-POOL-ROOM ( n -- ) {: u:n :}
   EVENT-POOL-N @ u + {: need:n :}
   u 0 < need 0 < or if
      s" events: a path length is negative or overflows the event pool" 76 die
   then
   need EVENT-POOL-CAP @ <= if exit then
   need EVENT-POOL-CAP @ EVENT-GROWN {: cap:n :}
   EVENT-POOL@ EVENT-POOL-CAP @ EVENT-POOL-N @ cap EVENT-REMAP
   EVENT-POOL !
   cap EVENT-POOL-CAP ! ;

: EVENT-COPY-PATH ( ^u8 n -- n n ) {: a:^ u:n :}
   u EVENT-POOL-ROOM
   EVENT-POOL-N @ {: off:n :}
   a off EVENT-POOL-AT u BYTE-COPY
   off u + EVENT-POOL-N !
   off u ;

package SOURCE-EVENT
public

: ROOT@ ( n -- ^u8 n ) {: ix:n :}
   ix 6 EVENT-FIELD@ EVENT-POOL-AT ix 7 EVENT-FIELD@ ;

\ The current root when the event was recorded.
: CURRENT@ ( n -- ^u8 n ) {: ix:n :}
   ix 11 EVENT-FIELD@ EVENT-POOL-AT ix 12 EVENT-FIELD@ ;

private

: COPY-SHARED ( ^u8 n -- n n ) {: a:^ u:n :}
   0 begin dup EVENT-N @ < while
      dup ROOT@ a u CORE-STR= if
         dup 6 EVENT-FIELD@ swap 7 EVENT-FIELD@ exit
      then
      dup CURRENT@ a u CORE-STR= if
         dup 11 EVENT-FIELD@ swap 12 EVENT-FIELD@ exit
      then
      1+
   repeat drop
   a u EVENT-COPY-PATH ;

public

: STORE-ROOT ( n -- ) {: ix:n :}
   RESOLVED-ROOT$ COPY-SHARED {: off:n len:n :}
   off ix 6 EVENT-FIELD!
   len ix 7 EVENT-FIELD!
   SOURCE-ROOT:CURRENT$ COPY-SHARED {: coff:n clen:n :}
   coff ix 11 EVENT-FIELD!
   clen ix 12 EVENT-FIELD! ;

;package

: EVENT-RECORD ( ^u8 n ^u8 n n n -- )
   {: a:^ u:n path:^ pathu:n kd:n st:n :}
   EVENT-ON? 0= if exit then
   EVENT-RECS-ROOM
   EVENT-N @ {: ix:n :}
   EVENT-SPAN {: toka:n toku:n :}
   EVENT-PATH-SPAN {: pat:n plen:n :}
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
   pat   ix 13 EVENT-FIELD!
   plen  ix 14 EVENT-FIELD!
   ix SOURCE-EVENT:STORE-ROOT
   ix 1 + EVENT-N ! ;

: REQUIRE-STATE ( bool -- n )
   if EV-STATE-KNOWN exit then EV-STATE-FRESH ;

: EVENT-COUNT ( -- n )       EVENT-N @ ;
: EVENT-KIND@ ( n -- n )     0 EVENT-FIELD@ ;
: EVENT-STATE@ ( n -- n )    5 EVENT-FIELD@ ;
: EVENT-PATH@ ( n -- ^u8 n ) {: ix:n :}
   ix 1 EVENT-FIELD@ EVENT-POOL-AT
   ix 2 EVENT-FIELD@ ;
: EVENT-SPELLED@ ( n -- ^u8 n ) {: ix:n :}
   ix 8 EVENT-FIELD@ EVENT-POOL-AT
   ix 9 EVENT-FIELD@ ;
: EVENT-TOK@ ( n -- n n ) {: ix:n :}
   ix 3 EVENT-FIELD@ ix 4 EVENT-FIELD@ ;
: EVENT-PATH-TOK@ ( n -- n n ) {: ix:n :}
   ix 13 EVENT-FIELD@ ix 14 EVENT-FIELD@ ;
: EVENT-DEPTH@ ( n -- n )    10 EVENT-FIELD@ ;

package SOURCE-ROOT
EXPORT CHECK:FILE-POS-CLEAR
private

\ A frame carries its own resolved path: a nested include overwrites
\ INCLUDE-PATH before the inner file is read, so only the frame names the outer
\ file to an inner refusal or a POP. The file's bytes follow the header in the
\ same mapping, sized to the file: FR-ROOM counts the source bytes it can
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
\ How many root programs are being read while this frame is the top.
13 cells constant FR-ROOT-RUNS
\ The file's stamp: change time seconds, nanoseconds and size.
14 cells constant FR-CTIME-SEC
15 cells constant FR-CTIME-NS
16 cells constant FR-STAMP-SIZE
\ The transient schema mark the file's load took (SCHEMA-REG:TRANSIENT-MARK).
17 cells constant FR-T-NODES
18 cells constant FR-T-ROOTS
19 cells constant FR-T-PARAMS
20 cells constant FR-PATH
FR-PATH INCLUDE-PATH-CAP + 1 cells + constant FR-ROOT
FR-ROOT INCLUDE-PATH-CAP + 1 cells + constant HEADER-BYTES
$1000 constant ROOM-MIN
\ A frame's FR-PREV, FR-OWNER, FR-EXEC-COPY and FR-EVAL-PTR cells hold
\ addresses; its other header cells hold counts, flags and stamps, and its bytes
\ follow them.
CAST: FRAME-CELLS ( ^u8 -- ^^u8 )
: FRAME-PREV ( ^u8 -- ^^u8 ) FRAME-CELLS FR-PREV CELL / ptr-field ;
: FRAME-OWNER ( ^u8 -- ^^u8 ) FRAME-CELLS FR-OWNER CELL / ptr-field ;
: FRAME-EXEC-COPY ( ^u8 -- ^^u8 )
   FRAME-CELLS FR-EXEC-COPY CELL / ptr-field ;
: FRAME-EVAL-PTR ( ^u8 -- ^^u8 )
   FRAME-CELLS FR-EVAL-PTR CELL / ptr-field ;
PTR-VARIABLE RANGE-OWNER
variable LAST-LOAD-EVAL-OK

: TOP@ ( -- ^u8 ) TOP @ ;
: TOP! ( ^u8 -- ) TOP ! ;

\ w is the word the program called.
: CHECK-ACTIVE ( ^u8 n -- ) {: w:^ wu:n :}
   TOP@ 0= 0= if exit then
   s" E-SOURCE-ROOT" REJECTION:OPEN
   96 REJECTION:EMIT1  w wu REJECTION:EMIT-STR
   s" ` runs with no file loading" REJECTION:EMIT-STR
   REJECTION:HELP  s" call it from code a load runs" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

: FRAME-EXEC-SOURCE ( ^u8 -- ^u8 )
   {: frame:^ :}
   frame FRAME-EXEC-COPY @ dup 0= if
      drop frame HEADER-BYTES +
   then ;

\ The engine's refusals name their place from these four cells and
\ nothing else decides when they are right: PUBLISH-LOCATION republishes them
\ when the top frame, its size, execution copy or root-run count changes, so
\ they name the file being read, or 0 for none (the REPL, a stdin session, the
\ boot prefix, a root program run over an open file: FR-ROOT-RUNS). A file
\ that cannot be opened or read publishes the frame that asked for it
\ (INCLUDE-UNREADABLE).
: PUBLISH-NONE ( -- )
   0 data-base SRCLOC:PATH-CELL + !
   0 data-base SRCLOC:PATHLEN-CELL + !
   0 data-base SRCLOC:OWNED-CELL + !
   0 data-base SRCLOC:OWNEDLEN-CELL + ! ;

: PUBLISH-FRAME ( ^u8 -- ) {: frame:^ :}
   FILE-POS-CLEAR
   frame 0= if PUBLISH-NONE exit then
   frame FR-ROOT-RUNS + CELL-VIEW @ 0 > if PUBLISH-NONE exit then
   frame FR-PATH + NULL-PTR - data-base SRCLOC:PATH-CELL + !
   frame FR-PATHLEN + CELL-VIEW @ data-base SRCLOC:PATHLEN-CELL + !
   frame FR-SIZE + CELL-VIEW @ dup 0 < if
      drop 0 data-base SRCLOC:OWNED-CELL + !
      0 data-base SRCLOC:OWNEDLEN-CELL + !
      exit
   then
   data-base SRCLOC:OWNEDLEN-CELL + !
   frame FRAME-EXEC-SOURCE NULL-PTR -
      data-base SRCLOC:OWNED-CELL + ! ;

: PUBLISH-LOCATION ( -- ) TOP@ PUBLISH-FRAME ;

: MAPPED ( ^u8 -- n ) FR-ROOM + CELL-VIEW @ HEADER-BYTES + ;

: ROOM ( -- n ) TOP@ FR-ROOM + CELL-VIEW @ ;

: PUSH ( -- )
   HEADER-BYTES ROOM-MIN + map-anon 0= 0= if drop s" include: cannot map a source frame" INCLUDE-DIE then
   {: frame:^ :}
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
   SCHEMA-REG:TRANSIENT-MARK {: nodes:n roots:n params:n :}
   nodes frame FR-T-NODES + CELL-VIEW !
   roots frame FR-T-ROOTS + CELL-VIEW !
   params frame FR-T-PARAMS + CELL-VIEW !
   INCLUDE-PATH frame FR-PATH + INCLUDE-PATH-U @ BYTE-COPY
   CURRENT$ {: root:^ rootu:n :}
   rootu frame FR-ROOTLEN + CELL-VIEW !
   root frame FR-ROOT + rootu BYTE-COPY
   frame TOP!
   INCLUDE-FALSE SCRIPT-NAMED-PEND!
   PUBLISH-LOCATION ;

: POP ( -- )
   s" SOURCE-ROOT:POP" CHECK-ACTIVE
   TOP@ {: frame:^ :}
   frame FRAME-OWNER @ RANGE-OWNER !
   frame FRAME-EXEC-COPY @ {: copy:^ :}
   frame FR-SIZE + CELL-VIEW @ 1 max {: copyu:n :}
   frame FRAME-PREV @ TOP!
   PUBLISH-LOCATION
   copy 0= if 0 else copy copyu munmap then {: copyrc:n :}
   frame dup MAPPED munmap 0 < copyrc 0 < or if
      s" include: cannot release a source frame" INCLUDE-DIE then ;

: SOURCE ( -- ^u8 ) s" SOURCE-ROOT:SOURCE" CHECK-ACTIVE TOP@ HEADER-BYTES + ;

\ The header and the first KEEP source bytes move to a fresh mapping; once the
\ old one is released, the fresh one becomes TOP and the published location,
\ the only two that point into a frame while it fills. Only memory refuses, by
\ name: a refusal ends the process while the old frame is still the one
\ published.
: GROW ( n n -- )
   {: want:n keep:n :}
   TOP@ {: frame:^ :}
   ROOM 2 * want max {: room:n :}
   HEADER-BYTES room + map-anon 0= 0= if drop s" include: cannot map a source frame" INCLUDE-DIE then
   {: fresh:^ :}
   frame fresh HEADER-BYTES keep + BYTE-COPY
   room fresh FR-ROOM + CELL-VIEW !
   frame dup MAPPED munmap 0 < if s" include: cannot release a source frame" INCLUDE-DIE then
   fresh TOP!
   PUBLISH-LOCATION ;

\ The open frame's stamp, from a stat of the path taken before the file is
\ opened, so a change during the read leaves a stamp no later load matches.
: STAMP! ( n n n -- ) {: sec:n ns:n size:n :}
   TOP@ {: frame:^ :}
   sec frame FR-CTIME-SEC + CELL-VIEW !
   ns frame FR-CTIME-NS + CELL-VIEW !
   size frame FR-STAMP-SIZE + CELL-VIEW ! ;

\ Rejects PATH unless the top frame awaits its bytes, FR-SIZE -1 from PUSH to HOLD:
\ a word's read finds its own file's frame held, or no frame; in a build, hb's.
: READ-REFUSE ( ^u8 n -- ) {: path:^ pathu:n :}
   TOP@ 0= 0= if TOP@ FR-SIZE + CELL-VIEW @ -1 = if exit then then
   s" SOURCE-ROOT:READ-OS" path pathu path pathu
   s" is not the file a load is opening"
   s" call it only from a reader installed with `SOURCE-INPUT:USE`, for the path the loader passes"
   PATH-DIE ;

\ Where a rejection of the file being opened stands: at the request in the file
\ that asked for it; an entry's has no place.
: ASKED-AT$ ( -- ^u8 n )
   TOP@ FRAME-PREV @ {: prev:^ :}
   prev 0= if NULL-PTR 0 else prev PUBLISH-FRAME REQUEST-AT$ then ;

\ The file a u the resolver found would not open or read, the system's last
\ call failing with errno e.
: INCLUDE-UNREADABLE ( ^u8 n n -- ) {: a:^ u:n e:n :}
   INCLUDE-CLOSE
   ASKED-AT$ a u e UNREADABLE-SOURCE ;

\ The file a u the resolver found is a stream (a FIFO, device or socket), whose
\ open would block or whose bytes no stamp names.
: INCLUDE-UNSTAMPED ( ^u8 n -- ) {: a:^ u:n :}
   ASKED-AT$ a u UNSTAMPED-SOURCE ;

\ The file a u the resolver found holds more bytes than its stamp's size.
: INCLUDE-CHANGED ( ^u8 n -- ) {: a:^ u:n :}
   INCLUDE-CLOSE
   ASKED-AT$ a u CHANGED-SOURCE ;

: INCLUDE-OPEN ( ^u8 n -- ) {: a:^ u:n :}
   a u INCLUDE-PATH0 open-rd INCLUDE-FD !
   INCLUDE-FD @ 0 < if a u INCLUDE-ERRNO INCLUDE-UNREADABLE then ;

\ A read stops once it holds more than SIZE bytes, the stamp's size.
: INCLUDE-READ-DONE? ( ^u8 n n -- bool ) {: a:^ u:n size:n :}
   INCLUDE-U @ ROOM = if INCLUDE-U @ 1+ INCLUDE-U @ GROW then
   INCLUDE-FD @ SOURCE INCLUDE-U @ + ROOM INCLUDE-U @ - read INCLUDE-RD !
   INCLUDE-RD @ 0 < if a u INCLUDE-ERRNO INCLUDE-UNREADABLE then
   INCLUDE-RD @ 0 = if INCLUDE-TRUE exit then
   INCLUDE-U @ INCLUDE-RD @ + INCLUDE-U !
   INCLUDE-U @ size > if a u INCLUDE-CHANGED then
   INCLUDE-FALSE ;

: INCLUDE-READ-ALL ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   a u INCLUDE-PATH0 COMPILE-READS:STAMP {: sec:n ns:n size:n :}
   sec ns size STAMP!
   sec ns size COMPILE-READS:STABLE? 0= if a u INCLUDE-UNSTAMPED then
   a u INCLUDE-OPEN
   0 INCLUDE-U !
   begin a u size INCLUDE-READ-DONE? 0= while repeat
   INCLUDE-CLOSE
   SOURCE INCLUDE-U @ ;

public

: ACTIVE-SOURCE ( -- ^u8 n ^u8 n ^u8 n )
   s" SOURCE-ROOT:ACTIVE-SOURCE" CHECK-ACTIVE
   TOP@ {: frame:^ :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @
   frame FR-ROOT + frame FR-ROOTLEN + CELL-VIEW @
   SOURCE frame FR-SIZE + CELL-VIEW @ ;

\ The active file's COMPILE-READS:STAMP as STAMP! took it; 0 0 -1 for bytes
\ READ-OS did not read, as WITH-SUPPLIED's are.
: ACTIVE-STAMP ( -- n n n )
   s" SOURCE-ROOT:ACTIVE-STAMP" CHECK-ACTIVE
   TOP@ {: frame:^ :}
   frame FR-CTIME-SEC + CELL-VIEW @
   frame FR-CTIME-NS + CELL-VIEW @
   frame FR-STAMP-SIZE + CELL-VIEW @ ;

\ Ends the transient schema nodes the active file's load made, keeping its
\ loader's.
: END-TYPES ( -- )
   s" SOURCE-ROOT:END-TYPES" CHECK-ACTIVE
   TOP@ {: frame:^ :}
   frame FR-T-NODES + CELL-VIEW @
   frame FR-T-ROOTS + CELL-VIEW @
   frame FR-T-PARAMS + CELL-VIEW @
   SCHEMA-REG:TRANSIENT-RELEASE ;

\ The loader's reader, public so a reader installed with SOURCE-INPUT:USE may
\ call it from READ-SOURCE; it fills the top frame only while that awaits its bytes.
: READ-OS ( ^u8 n ^u8 n -- ^u8 n )
   2drop s" SOURCE-ROOT:READ-OS" 2over GIVEN-CHECK 2dup READ-REFUSE INCLUDE-READ-ALL ;

private

\ A leading `#!` line reads as a comment: its two bytes become `\ `, so every
\ line and column stays the file's own.
: SHEBANG-COMMENT ( ^u8 n -- ) {: a:^ u:n :}
   u 2 < if exit then
   a c@ $23 <> if exit then
   a 1 + c@ $21 <> if exit then
   $5c a c!
   $20 a 1 + c! ;

\ The loop LOAD-UNIT runs a loaded file's bytes through; USE-INTERPRETER swaps it.
defer INCLUDE-INTERPRET ( ^u8 n n n -- )

\ The design seal is read where the engine's loop reads a token, and the loop
\ written in Habu reads it nowhere, so a file loaded once the seal is set is read
\ by the engine's loop whatever INCLUDE-INTERPRET holds. `0 <>`, not `0<>`: the
\ prefix loads src/lib/prelude.f, which defines `0<>`, after this file.
: POLICY-SEALED? ( -- bool )
   data-base POLICY-NDICT-CELL + @ 0 <> ;

\ The OS reader has filled the frame already, learning the size as it read;
\ another reader's bytes come with their length, and the frame grows to fit
\ them before they are copied in.
: HOLD ( ^u8 n -- ^u8 n )
   {: a:^ u:n :}
   a SOURCE <> if
      u ROOM > if u 0 GROW then
      a SOURCE u BYTE-COPY
   then
   u TOP@ FR-SIZE + CELL-VIEW !
   PUBLISH-LOCATION
   SOURCE u ;

: READ-SOURCE ( -- ^u8 n )
   INCLUDE-PATH INCLUDE-PATH-U @ RESOLVED-ROOT$ SOURCE-INPUT:READ HOLD ;

\ The original bytes remain in the frame while a shebang is rewritten in a
\ private execution copy. Range positions continue to name the full source.
: EXEC-SOURCE ( -- ^u8 )
   TOP@ FRAME-EXEC-SOURCE ;

EXPORT CHECK:CHECKER-SPLIT-STR
EXPORT CHECK:RESIDUE-AT
EXPORT CHECK:RESIDUE-NOTE
EXPORT CHECK:SPLIT-STR-FILE?
\ Every range of a file starts marked when the file's source is, whatever part
\ ran before it, so the marker is read from byte 0.
: RUN-SAVED-RANGE ( -- )
   TOP@ {: frame:^ :}
   frame FRAME-EVAL-PTR @
   frame FR-EVAL-SIZE + CELL-VIEW @
   2dup SPLIT-STR-FILE? CHECKER-SPLIT-STR !
   frame FR-EVAL-FIRST + CELL-VIEW @
   frame FR-EVAL-END + CELL-VIEW @
   POLICY-SEALED? if evaluate-range else INCLUDE-INTERPRET then ;

\ A file's range runs on an empty data stack, so an underflow cannot reach the
\ loader's cells; the loader's marker comes back as the range returns or throws.
: INTERPRET-SAVED-RANGE ( -- )
   CHECKER-SPLIT-STR @ {: split:n :}
   [: RUN-SAVED-RANGE ;] catch {: rc:n :}
   split CHECKER-SPLIT-STR !
   rc 0 <> if rc throw then ;

: CHECK-RANGE ( n n n -- ) {: size:n first:n end:n :}
   first 0 < first end > or end size > or if INCLUDE-RANGE-RC throw then ;

: INTERPRET-INSTALL ( -- ) [: evaluate-range ;] is INCLUDE-INTERPRET ;
INTERPRET-INSTALL

public

: USE-INTERPRETER ( [ ^u8 n n n -- ] -- ) is INCLUDE-INTERPRET ;

\ The process's root stream (stdin, or the `--load` stream) runs open, as the
\ engine's own loop runs it: it may leave cells on the stack and the process
\ still exits 0. The stream's text is no file's, so a refusal in it names none:
\ run while a file is open, it counts itself on that file's frame, which then
\ publishes no location until the stream returns or throws.
: ROOT-RUNS+ ( n -- ) {: k:n :}
   TOP@ 0= if exit then
   TOP@ {: frame:^ :}
   frame FR-ROOT-RUNS + CELL-VIEW @ k + frame FR-ROOT-RUNS + CELL-VIEW !
   PUBLISH-LOCATION ;

: RUN-ROOT ( ^u8 n -- ) {: a:^ u:n :}
   REJECTION:ROOT-TEXT$ {: sa:^ su:n :}
   a u REJECTION:ROOT-TEXT!
   1 ROOT-RUNS+
   a u 0 u [: INCLUDE-INTERPRET ;]
   [: -1 ROOT-RUNS+  sa su REJECTION:ROOT-TEXT! ;] finally ;

private

: LOAD-BYTES ( -- )
   READ-SOURCE {: a:^ u:n :}
   u 2 >= if
      a c@ $23 = a 1 + c@ $21 = and if
         u 1 max map-anon 0= 0= if drop INCLUDE-IO-RC throw then
         {: copy:^ :}
         a copy u BYTE-COPY
         copy u SHEBANG-COMMENT
         copy TOP@ FRAME-EXEC-COPY !
         PUBLISH-LOCATION
      then
   then ;

\ A failed fragment rolls back the require rows it added whichever loop read
\ it; the residue check stays outside, on the unit's stack.
: RUN-RANGE ( n n -- ) {: first:n end:n :}
   RANGE-OWNER @ 0= if INCLUDE-RANGE-RC throw then
   s" SOURCE-ROOT:RUN-RANGE" CHECK-ACTIVE
   TOP@ RANGE-OWNER @ <> if INCLUDE-RANGE-RC throw then
   TOP@ {: frame:^ :}
   frame FR-SIZE + CELL-VIEW @ {: size:n :}
   size first end CHECK-RANGE
   EXEC-SOURCE frame FRAME-EVAL-PTR !
   size frame FR-EVAL-SIZE + CELL-VIEW !
   first frame FR-EVAL-FIRST + CELL-VIEW !
   end frame FR-EVAL-END + CELL-VIEW !
   REQUIRE-REG:MARK {: mark:n :}
   [: INTERPRET-SAVED-RANGE ;] catch {: rc:n :}
   rc 0 <> if mark REQUIRE-REG:TRUNCATE rc throw then
   -1 frame FR-EVAL-OK + CELL-VIEW ! ;

: RESIDUE-COUNT ( n -- ) {: n:n :}
   n REJECTION:EMIT-U
   n 1 = if s"  cell" else s"  cells" then REJECTION:EMIT-STR ;

\ A nested file's rejection throws out of its loader's run before the loader
\ reaches its own check, so only the innermost file is named.
: RESIDUE-LEFT ( n -- ) {: n:n :}
   s" E-EVAL-RESIDUE" REJECTION:OPEN
   s" this file leaves " REJECTION:EMIT-STR  n RESIDUE-COUNT
   s"  on the stack" REJECTION:EMIT-STR
   RESIDUE-AT {: a:^ u:n :}
   u 0 > if
      a u REJECTION:AT  s" the cells left start here" REJECTION:EMIT-STR
   then
   n RESIDUE-NOTE
   REJECTION:HELP
   s" drop or consume them before the file ends: " REJECTION:EMIT-STR
   s" a value crosses a load only through a word or a variable"
   REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

: RESIDUE-TAKEN ( n -- ) {: n:n :}
   s" E-EVAL-RESIDUE" REJECTION:OPEN
   s" this file takes " REJECTION:EMIT-STR  n RESIDUE-COUNT
   s"  it did not push" REJECTION:EMIT-STR
   REJECTION:HELP
   s" a file starts on an empty stack: " REJECTION:EMIT-STR
   s" pass values through a word or a variable" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

\ Run at the end of the unit, where depth is what the file left.
: RESIDUE-CHECK ( -- )
   depth {: n:n :}
   n 0 > if n RESIDUE-LEFT then
   n 0 < if n negate RESIDUE-TAKEN then ;

\ The declaration open in the running source, as its declarer records it: the
\ source token that opened it, its closer's spelling and the action that
\ abandons it. CLOSER-U is 0 while none is open.
PTR-VARIABLE OPENER-A
variable OPENER-U
PTR-VARIABLE CLOSER-A
variable CLOSER-U
TYPED-VARIABLE ABANDON-XT [ -- ]
\ Counts the rows OPENED records: the open row is always the newest.
variable DECL-SERIAL

\ A row is open that the text whose mark is n opened.
: OPEN-SINCE? ( n -- bool )
   DECL-SERIAL @ <  CLOSER-U @ 0 <> and ;

: TICKED ( ^u8 n -- )
   96 REJECTION:EMIT1  REJECTION:EMIT-STR  96 REJECTION:EMIT1 ;

\ Opens the E-UNCLOSED record of the declaration t tu opened, through "is still
\ open when ".
: UNCLOSED-HEAD ( ^u8 n -- ) {: t:^ tu:n :}
   s" E-UNCLOSED" REJECTION:OPEN
   t tu TICKED  s"  is still open when " REJECTION:EMIT-STR ;

\ Places the record at t tu and opens its help, through "close it with `c` ".
: UNCLOSED-HELP ( ^u8 n ^u8 n -- ) {: t:^ tu:n c:^ cu:n :}
   t tu REJECTION:AT  s" opened here" REJECTION:EMIT-STR
   REJECTION:HELP  s" close it with " REJECTION:EMIT-STR
   c cu TICKED  32 REJECTION:EMIT1 ;

public

\ The record of the declaration t tu opened, which c cu closes, still open when
\ the token o ou opens another or, with ou 0, when its source ends; not stopped.
: UNCLOSED ( ^u8 n ^u8 n ^u8 n -- ) {: t:^ tu:n c:^ cu:n o:^ ou:n :}
   t tu UNCLOSED-HEAD
   ou 0= if
      s" the source ends" REJECTION:EMIT-STR
   else
      o ou TICKED  s"  opens" REJECTION:EMIT-STR
   then
   t tu c cu UNCLOSED-HELP
   ou 0= if
      s" before the source ends" REJECTION:EMIT-STR
   else
      s" before " REJECTION:EMIT-STR  o ou TICKED
   then ;

\ The record of the declaration t tu opened, which c cu closes, still open when
\ the load word w wu loads a file; not stopped.
: UNCLOSED-LOAD ( ^u8 n ^u8 n ^u8 n -- ) {: t:^ tu:n c:^ cu:n w:^ wu:n :}
   t tu UNCLOSED-HEAD
   w wu TICKED  s"  loads a file" REJECTION:EMIT-STR
   t tu c cu UNCLOSED-HELP
   s" before " REJECTION:EMIT-STR  w wu TICKED ;

: CLOSED ( -- )
   0 CLOSER-U ! ;

\ A declaration opening while another is open rejects the open one and
\ abandons both.
: OPENED ( ^u8 n ^u8 n [ -- ] -- ) {: t:^ tu:n c:^ cu:n q :}
   CLOSER-U @ 0 <> if
      OPENER-A @ OPENER-U @ CLOSER-A @ CLOSER-U @ t tu UNCLOSED
      ABANDON-XT @ execute  q execute  CLOSED
      CHECKER-REJECT-RC REJECTION:STOP
   then
   t OPENER-A !  tu OPENER-U !  c CLOSER-A !  cu CLOSER-U !  q ABANDON-XT !
   1 DECL-SERIAL +! ;

\ The mark a text takes as it starts, for TEXT-END and TEXT-UNWOUND.
: TEXT-MARK ( -- n )
   DECL-SERIAL @ ;

\ A text ending with a declaration it opened still open rejects it.
: TEXT-END ( n -- )
   OPEN-SINCE? 0= if exit then
   OPENER-A @ OPENER-U @ CLOSER-A @ CLOSER-U @ NULL-PTR 0 UNCLOSED
   ABANDON-XT @ execute  CLOSED
   CHECKER-REJECT-RC REJECTION:STOP ;

\ Load word w wu begins a load: a declaration still open is rejected at its
\ opener, so none spans a load.
: LOAD-BEGINS ( ^u8 n -- ) {: w:^ wu:n :}
   CLOSER-U @ 0= if exit then
   OPENER-A @ OPENER-U @ CLOSER-A @ CLOSER-U @  w wu SRCLOC:SPELLED drop
   UNCLOSED-LOAD
   ABANDON-XT @ execute  CLOSED
   CHECKER-REJECT-RC REJECTION:STOP ;

\ A text a throw left abandons the declaration it opened, with no record.
: TEXT-UNWOUND ( n -- )
   OPEN-SINCE? 0= if exit then
   ABANDON-XT @ execute  CLOSED ;

private

CAST: CLOSE-ACTION ( n -- [ -- ] )

\ The active checker owner closes the created body pending at a load boundary,
\ so the body's `,` grid stops there.
: BODY-CLOSE ( -- )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   CHECKER-OWNER-ABI:CLOSER {: close:n :}
   close 0= if exit then
   close CLOSE-ACTION execute ;

\ A file that ends with no definition open ends the transient schema nodes its
\ definers built definitions from; a build ends them at the file's last piece.
: TRANSIENT-END ( -- )
   data-base BOOT:IMAGE-BASE-CELL + @ 0 <> if exit then
   data-base PEND-CELL + @ 0 <> if exit then
   END-TYPES ;

\ The created body closes and the residue check runs inside the unit, so a runner's call
\ returns only for an accepted file; each compiled part starts in host.
: CALL-UNIT ( -- )
   TOP@ {: frame:^ :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @
   frame FR-ROOT + frame FR-ROOTLEN + CELL-VIEW @
   EXEC-SOURCE frame FR-SIZE + CELL-VIEW @
   [: 0 data-base SECTION-CELL + !  RUN-RANGE BODY-CLOSE RESIDUE-CHECK
      TRANSIENT-END ;]
   SOURCE-UNIT:LOAD ;

\ A loaded file's DATA starts and ends on a cell, so its bytes are a function
\ of its own source and a stored object installs whatever earlier files
\ allotted. A throw skips the end align, which does not matter: a load that
\ threw refuses the parent's object, so no stored segment continues the
\ parent's data from there.
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
   data-base SECTION-CELL + @ {: sec:n :}
   PUSH
   [: LOAD-UNIT ;] catch {: rc:n :}
   sec data-base SECTION-CELL + !
   rc 0 <> if TOP@ FR-EVAL-OK + CELL-VIEW @ LAST-LOAD-EVAL-OK ! then
   INCLUDE-CLOSE
   POP
   rc 0= 0= if rc throw then ;

\ A verifier can traverse the loader's resolved file frames without invoking
\ the interpreter. The callback receives bytes owned by the frame and its
\ canonical path; both remain valid throughout the callback, including nested
\ loads.
: READ-FOR ( [ ^u8 n ^u8 n -- ] -- [ ^u8 n ^u8 n -- ] )
   {: q :}
   READ-SOURCE TOP@ {: frame:^ :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @ q execute
   q ;

: COPY-FOR ( ^u8 n [ ^u8 n ^u8 n -- ] -- ^u8 n [ ^u8 n ^u8 n -- ] )
   {: a:^ u:n q :}
   a u HOLD TOP@ {: frame:^ :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @ q execute
   a u q ;

: WITH-BYTES-CURRENT ( [ ^u8 n ^u8 n -- ] -- [ ^u8 n ^u8 n -- ] ) {: q :}
   PUSH
   q
   [: READ-FOR ;] catch {: rc:n :}
   drop
   INCLUDE-CLOSE
   POP
   rc 0= 0= if rc throw then
   q ;

: WITH-SUPPLIED-CURRENT ( ^u8 n [ ^u8 n ^u8 n -- ] -- ^u8 n [ ^u8 n ^u8 n -- ] )
   {: a:^ u:n q :}
   PUSH a u q
   [: COPY-FOR ;] catch {: rc:n :}
   2drop drop
   INCLUDE-CLOSE
   POP
   rc 0= 0= if rc throw then
   a u q ;

: LOAD-RAW ( ^u8 n ^u8 n -- ) {: w:^ wu:n a:^ u:n :}
   a u CONFINE
   INCLUDE-PATH0 drop
   w wu RESOLVED-ROOT$ ROOT-ENTER {: old:^ oldu:n base:n :}
   [: LOAD-CURRENT ;] catch {: rc:n :}
   old oldu base ROOT-LEAVE
   rc 0= 0= if rc throw then ;

public

\ No WITH scope opens, so the frame's root is CURRENT$, at the top level the
\ program's directory.
: RUN-PROGRAM ( ^u8 n -- ) {: a:^ u:n :}
   s" SOURCE-ROOT:RUN-PROGRAM" a u BUILD-REFUSE
   NULL-PTR 0 a u [: 2drop CANONICAL ;] LOADER-RESOLVE {: path:^ pathu:n exists:bool :}
   path pathu a u NULL-PTR 0 CONFINE-AS INCLUDE-PATH0 drop
   exists 0= if a u NULL-PTR 0 INCLUDE-TRUE MISSING-SOURCE then
   PUSH
   REQUEST-AT$ {: ra:^ ru:n :}  NULL-PTR 0 REQUEST-AT!
   [: LOAD-UNIT ;] catch {: rc:n :}
   ra ru REQUEST-AT!
   INCLUDE-CLOSE
   POP
   rc 0= 0= if rc throw then ;

private

\ Loads a u for loader word w, the word the program called; t tu is the
\ request token an open of a u that fails stands at, or empty.
: LOAD-RUN ( ^u8 n ^u8 n ^u8 n -- )
   {: t:^ tu:n w:^ wu:n a:^ u:n :}
   w wu a u BUILD-REFUSE
   0 LAST-LOAD-EVAL-OK !
   REQUEST-AT$ {: ra:^ ru:n :}  t tu REQUEST-AT!
   [: w wu a u LOAD-RAW ;] catch {: rc:n :}
   ra ru REQUEST-AT!
   rc 0 <> if rc throw then
   0 LAST-LOAD-EVAL-OK ! ;

public

\ Whether the load LOAD-RUN last threw out of had completed its source range,
\ which leaves its definitions live; false after one that returned. A require
\ keeps its row in that case.
: COMMITTED? ( -- bool )
   LAST-LOAD-EVAL-OK @ 0 <> ;

: NAMED? ( -- bool )
   TOP@ 0= if INCLUDE-FALSE exit then
   TOP@ FR-NAMED + c@ 0= 0= ;

\ PATH has already been resolved by the loader's resolver. This opens a source
\ frame and makes its root current until the callback returns.
: WITH-BYTES ( ^u8 n [ ^u8 n ^u8 n -- ] -- )
   {: path:^ pathu:n q :}
   s" SOURCE-ROOT:WITH-BYTES" path pathu GIVEN-CHECK
   s" SOURCE-ROOT:WITH-BYTES" path pathu BUILD-REFUSE
   path pathu CONFINE INCLUDE-PATH0 drop
   s" SOURCE-ROOT:WITH-BYTES" RESOLVED-ROOT$ ROOT-ENTER {: old:^ oldu:n base:n :}
   REQUEST-AT$ {: ra:^ ru:n :}  NULL-PTR 0 REQUEST-AT!
   TOKEN-CLEAR {: t:^ tu:n :}
   q [: WITH-BYTES-CURRENT ;] catch {: rc:n :}
   drop
   t tu SRCLOC:TOKEN!
   ra ru REQUEST-AT!
   old oldu base ROOT-LEAVE
   rc 0= 0= if rc throw then ;

\ A caller that owns PATH's source bytes can open the same loader frame without
\ reading a different disk copy.
: WITH-SUPPLIED ( ^u8 n ^u8 n [ ^u8 n ^u8 n -- ] -- )
   {: path:^ pathu:n src:^ srcu:n q :}
   s" SOURCE-ROOT:WITH-SUPPLIED" path pathu GIVEN-CHECK
   s" SOURCE-ROOT:WITH-SUPPLIED" path pathu BUILD-REFUSE
   path pathu CONFINE INCLUDE-PATH0 drop
   s" SOURCE-ROOT:WITH-SUPPLIED" RESOLVED-ROOT$ ROOT-ENTER {: old:^ oldu:n base:n :}
   REQUEST-AT$ {: ra:^ ru:n :}  NULL-PTR 0 REQUEST-AT!
   TOKEN-CLEAR {: t:^ tu:n :}
   src srcu q [: WITH-SUPPLIED-CURRENT ;] catch {: rc:n :}
   2drop drop
   t tu SRCLOC:TOKEN!
   ra ru REQUEST-AT!
   old oldu base ROOT-LEAVE
   rc 0= 0= if rc throw then ;

;package

package SOURCE-INPUT
public

: RESET ( -- )
   [: SOURCE-ROOT:CANON-OS ;] CANON-XT !
   [: SOURCE-ROOT:READ-OS ;] READ-XT ! ;

;package

package SOURCE-ROOT
private

\ A discovery pass reads source and opens no file, so it records a missing
\ file's request like any other; the reader that opens the file rejects it.
: LOADER-FIND ( ^u8 n ^u8 n -- ^u8 n bool )
   DISCOVERY? if RESOLVE-AT exit then
   LOAD-RESOLVE ;

\ A path the registry already holds is skipped, so the pending flag has to be
\ cleared on every exit or it would leak into the next unrelated load. w is the
\ word the program called.
: REQUIRE-BODY ( ^u8 n ^u8 n ^u8 n bool -- )
   {: w:^ wu:n a:^ u:n path:^ pathu:n known:bool :}
   a u path pathu EV-REQUIRED known REQUIRE-STATE EVENT-RECORD
   known if INCLUDE-FALSE SCRIPT-NAMED-PEND! exit then
   REQUIRE-REG:MARK {: row:n :}
   path pathu REQUIRE-STORE {: row-path:^ row-pathu:n :}
   DISCOVERY? if INCLUDE-FALSE SCRIPT-NAMED-PEND! exit then
   a u SRCLOC:ORIGIN$ {: t:^ tu:n :}
   [: t tu w wu row-path row-pathu LOAD-RUN ;] catch {: rc:n :}
   rc 0 <> if
      COMMITTED? 0= if row REQUIRE-REG:REMOVE then
      rc throw
   then ;

public

\ `included` of a u for loader word w, the word the program called.
: INCLUDED-AS ( ^u8 n ^u8 n -- )
   {: w:^ wu:n a:^ u:n :}
   w wu a u [: LOADER-FIND ;] LOADER-RESOLVE drop {: path:^ pathu:n :}
   a u path pathu EV-INCLUDED EV-STATE-FRESH EVENT-RECORD
   DISCOVERY? if exit then
   a u SRCLOC:ORIGIN$
   w wu path pathu LOAD-RUN ;

\ `required` of a u for loader word w, the word the program called.
: REQUIRED-AS ( ^u8 n ^u8 n -- )
   {: w:^ wu:n a:^ u:n :}
   INCLUDE-FALSE SCRIPT-NAMED-PEND!
   w wu a u  w wu a u [: LOADER-FIND ;] LOADER-RESOLVE  REQUIRE-BODY ;

: ENTRY-REQUIRED ( ^u8 n -- )
   {: a:^ u:n :}
   s" script-required" LOAD-BEGINS
   INCLUDE-TRUE SCRIPT-NAMED-PEND!
   s" script-required" a u
   NULL-PTR 0 a u [: 2drop ENTRY-RESOLVE ;] LOADER-RESOLVE  REQUIRE-BODY ;

;package

: included ( ^u8 n -- )
   {: a:^ u:n :}
   s" included" SOURCE-ROOT:LOAD-BEGINS
   s" included" a u INCLUDED-AS ;

create SOURCE-ORIGINAL-INCLUDED-ENTRY 0 ,
' included SOURCE-ORIGINAL-INCLUDED-ENTRY xt!

: required ( ^u8 n -- )
   {: a:^ u:n :}
   s" required" SOURCE-ROOT:LOAD-BEGINS
   s" required" a u REQUIRED-AS ;

create SOURCE-ORIGINAL-REQUIRED-ENTRY 0 ,
' required SOURCE-ORIGINAL-REQUIRED-ENTRY xt!

\ The `--load` argv row.
: script-required ( ^u8 n -- ) ENTRY-REQUIRED ;

: SCRIPT-NAMED-LOAD? ( -- bool )
   SOURCE-ROOT:NAMED? ;

: provided ( ^u8 n -- )
   {: a:^ u:n :}
   s" provided" a u [: RESOLVE-AT ;] LOADER-RESOLVE {: path:^ pathu:n known:bool :}
   s" provided" path pathu SOURCE-ROOT:BUILD-REFUSE
   a u path pathu EV-PROVIDED known REQUIRE-STATE EVENT-RECORD
   known if exit then
   path pathu REQUIRE-STORE 2drop ;

\ Defined in CHECK's global section, as operand-rows.f defines `parses:`, so
\ INTRINSIC and the loader ids, which are CHECK's, tag them.
package CHECK
global
: include ( -- )
   s" include" SOURCE-ROOT:LOAD-BEGINS
   s" include" SRCLOC:SPELLED drop {: w:^ wu:n :}
   parse-name {: a:^ u:n :}
   u INCLUDE-CHECK-PATH
   w wu a u INCLUDED-AS ;
immediate
INTRINSIC-INCLUDE INTRINSIC

create SOURCE-ORIGINAL-INCLUDE-ENTRY 0 ,
' include SOURCE-ORIGINAL-INCLUDE-ENTRY xt!

: require ( -- )
   s" require" SOURCE-ROOT:LOAD-BEGINS
   s" require" SRCLOC:SPELLED drop {: w:^ wu:n :}
   parse-name {: a:^ u:n :}
   u INCLUDE-CHECK-PATH
   w wu a u REQUIRED-AS ;
immediate
INTRINSIC-REQUIRE INTRINSIC

create SOURCE-ORIGINAL-REQUIRE-ENTRY 0 ,
' require SOURCE-ORIGINAL-REQUIRE-ENTRY xt!
;package

package SOURCE-UNIT
public
0 constant LOAD-INCLUDE
1 constant LOAD-REQUIRE
0 constant NONE
1 constant PARSED-PATH
2 constant STACK-PATH

: LOADER-KIND ( n -- n ) {: entry:n :}
   entry SOURCE-ORIGINAL-INCLUDE-ENTRY @ =
   entry SOURCE-ORIGINAL-REQUIRE-ENTRY @ = or if PARSED-PATH exit then
   entry SOURCE-ORIGINAL-INCLUDED-ENTRY @ =
   entry SOURCE-ORIGINAL-REQUIRED-ENTRY @ = or if STACK-PATH exit then
   NONE ;

: LOAD-PATH ( n ^u8 n -- ) {: mode:n path:^ size:n :}
   SOURCE-ROOT:ACTIVE-SOURCE 2drop 2drop 2drop
   mode LOAD-INCLUDE = if path size included exit then
   mode LOAD-REQUIRE = if path size required exit then
   INCLUDE-EVAL-RC throw ;
;package

package SOURCE-ROOT
private

: POLICY-SEALED ( -- )
   s" E-POLICY" REJECTION:OPEN
   s" the source policy is already sealed" REJECTION:EMIT-STR
   SRCLOC:TOKEN$ REJECTION:AT  s" sealed" REJECTION:EMIT-STR
   REJECTION:HELP
   s" call `POLICY-ROOT` before the policy seals the dictionary" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

: POLICY-ROOT-SET ( [ -- ] -- )
   data-base POLICY-REQUIRE-CELL + xt!
   CANON-BUF POLICY-ROOT-BUF CANON-U @ BYTE-COPY
   CANON-U @ POLICY-ROOT-U ! ;

: POLICY-ROOT-RELEASE ( ^u8 n -- )
   1+ munmap 0 < if s" policy: cannot release a root copy" INCLUDE-IO-RC die then ;

public

\ Configure confinement before checking or sealing. The policy library passes
\ the ordinary require xt it captured when loaded.
: POLICY-ROOT ( ^u8 n [ -- ] -- )
   {: path:^ u:n require-xt :}
   data-base POLICY-NDICT-CELL + @ 0 <> if POLICY-SEALED then
   s" SOURCE-ROOT:POLICY-ROOT" path u ROOT-CANON
   require-xt POLICY-ROOT-SET ;

\ Scope a root and require owner for reusable source checks. A callback that
\ seals leaves the new policy root and refuses the attempted return to the old.
: POLICY-WITH-ROOT ( ^u8 n [ -- ] [ -- ] -- )
   {: path:^ u:n q require-xt :}
   data-base POLICY-NDICT-CELL + @ 0 <> if POLICY-SEALED then
   s" SOURCE-ROOT:POLICY-WITH-ROOT" path u ROOT-ENTER {: prior:^ prioru:n prior-base:n :}
   data-base POLICY-REQUIRE-CELL + @ 0= 0= {: had:bool :}
   POLICY-ROOT-U @ {: old-u:n :}
   POLICY-ROOT-BUF old-u MAP-COPY {: old:^ :}
   require-xt POLICY-ROOT-SET
   TOKEN-CLEAR {: t:^ tu:n :}
   q catch {: rc:n :}
   t tu SRCLOC:TOKEN!
   data-base POLICY-NDICT-CELL + @ 0 <> if
      prior prioru prior-base ROOT-LEAVE
      old old-u POLICY-ROOT-RELEASE POLICY-SEALED
   then
   had if
      old POLICY-ROOT-BUF old-u BYTE-COPY
      old-u POLICY-ROOT-U !
   else
      0 POLICY-ROOT-U !
      0 data-base POLICY-REQUIRE-CELL + xt!
   then
   old old-u POLICY-ROOT-RELEASE
   prior prioru prior-base ROOT-LEAVE
   rc 0= 0= if rc throw then ;

;package

\ A restricted discovery pass sees a fresh require/provided registry, so a tool's
\ preloaded paths cannot dedup-hide a later user require. One snapshot is in
\ effect at a time, since one pair is saved: a second would lose the first.

: REQUIRE-SNAPSHOT ( -- )
   REQUIRE-SAVE-N @ 0 >= if s" require: a registry snapshot is already in effect" 76 die then
   REQUIRE-N @ REQUIRE-SAVE-N !
   REQUIRE-BASE @ REQUIRE-SAVE-BASE !
   REQUIRE-N @ REQUIRE-BASE ! ;

: REQUIRE-RESTORE ( -- )
   REQUIRE-SAVE-N @ 0< if s" require: no registry snapshot to restore" 76 die then
   REQUIRE-SAVE-BASE @ REQUIRE-BASE !
   REQUIRE-SAVE-N @ REQUIRE-REG:REWIND
   -1 REQUIRE-SAVE-N ! ;

package REQUIRE-REG

public

: COUNT ( -- n )
   REQUIRE-N @ ;

;package

\ What the ENGINE provides, as opposed to what this process loaded: the boot
\ prefix marks its own files `provided` before any user token runs, and freezing
\ the count at its end separates the two, which REQUIRE-KNOWN? cannot once a
\ tool's own dependencies are in the registry.
: REQUIRE-BOOT-FREEZE ( -- )
   REQUIRE-N @ REQUIRE-BOOT-N !
   INCLUDE-FALSE REQUIRE-BOOT-V ! ;

: ENGINE-PROVIDES? ( ^u8 n -- bool )
   ENGINE-KNOWN? ;

\ A file that builds for some targets only states them on its first code line,
\ before its requires; on another target its load stops there.
: ?TARGET ( bool -- )
   if exit then
   s" E-TARGET" REJECTION:OPEN
   s" this file does not build for " REJECTION:EMIT-STR  HB-TARGET$ REJECTION:EMIT-STR
   s" ?TARGET" TOKEN-SPELLS? if
      SRCLOC:TOKEN$ REJECTION:AT  s" the target test fails here" REJECTION:EMIT-STR
   then
   REJECTION:HELP
   s" build this file only for a target its test admits, or widen the test"
   REJECTION:EMIT-STR
   INCLUDE-IO-RC REJECTION:STOP ;


;using
