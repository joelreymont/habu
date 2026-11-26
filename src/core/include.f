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

package LOADER

private

create INCLUDE-PATH PATH-CAP 1 + allot

;package

package REQUIRE-REG
private

\ A DATA block of (offset, length, file id) triples followed by path bytes.
PERSISTED-PTR-VARIABLE FROZEN
\ One movable mapping: row i's path pointer, ROW-CAP cells on (ROW-LENS) its
\ length, ROW-CAP further (ROW-IDS) its file id; path chunks never move.
PERSISTED-PTR-U8-TABLE-VARIABLE ROWS
PERSISTED-PTR-VARIABLE ROW-LENS
PERSISTED-PTR-VARIABLE ROW-IDS
variable ROW-CAP
PERSISTED-PTR-U8-TABLE-VARIABLE CHUNK
variable PREFIX-N

;package

package LOADER

variable INCLUDE-FD
variable INCLUDE-U
variable INCLUDE-RD
variable INCLUDE-PATH-U

-1 INCLUDE-FD !

: INCLUDE-FALSE ( -- bool )
   0 0= 0= ;

: INCLUDE-TRUE ( -- bool )
   0 0= ;

;package

package REQUIRE-REG
EXPORT LOADER:INCLUDE-FALSE
EXPORT LOADER:INCLUDE-TRUE

variable REQUIRE-N
variable REQUIRE-BOOT-N
variable REQUIRE-BOOT-V
variable REQUIRE-BASE
variable REQUIRE-SAVE-N
variable REQUIRE-SAVE-BASE

-1 REQUIRE-SAVE-N !                                \ no snapshot in effect

public

\ Rows recorded while the boot registry is open describe what the ENGINE
\ carries; every other row, what a process loaded. Only the boot prefix opens
\ it. Loading this file is not the signal: a build
\ re-loads it into a booted engine, which would then record every later require
\ portably and stop recognising its own canonical rows.
: BOOT-OPEN ( -- )
   INCLUDE-TRUE REQUIRE-BOOT-V ! ;

private

: REQUIRE-BOOT-OPEN? ( -- bool )
   REQUIRE-BOOT-V @ 0= 0= ;

\ While the registry is open every row so far is the engine's, because only the
\ prefix records there; once closed, the frozen count says.
: REQUIRE-BOOT-LIMIT ( -- n )
   REQUIRE-BOOT-OPEN? if REQUIRE-N @ exit then
   REQUIRE-BOOT-N @ ;

;package

\ An invocation that owns source bytes supplies both filesystem answers through
\ this loader-local pair. A newly source-loaded target gets its own pair, so its
\ require registry and source frames remain independent of the retained host.
package SOURCE-INPUT
private

TYPED-VARIABLE CANON-XT [ str -- str bool ]
TYPED-VARIABLE READ-XT [ str -- str ]

public

: CANON ( str -- str bool ) CANON-XT @ execute ;
: READ ( str -- str ) READ-XT @ execute ;

: USE ( [ str -- str bool ] [ str -- str ] -- )
   {: canon read :}
   canon CANON-XT !
   read READ-XT ! ;

: WITH ( [ str -- str bool ] [ str -- str ] [ -- ] -- )
   {: canon read action :}
   CANON-XT @ READ-XT @ {: old-canon old-read :}
   canon read USE
   action catch {: rc:n :}
   old-canon old-read USE
   rc 0 <> if rc throw then ;

;package

package LOADER

: TOKEN-SPELLS? ( str -- bool )
   dup STR:DATA swap STR:LEN SRCLOC:SPELLED nip nip ;

\ The place of a record of loader word w, given the path a u: the source a u
\ stands for, else the token the engine read last where it spells w, which it
\ does only when source calls w directly: called from a definition at run
\ time, that token is another word. Else no place, NULL-PTR 0.
: LOADER-PLACE$ ( str str -- str ) {: w:str s:str :}
   s SRCLOC:ORIGIN$ {: t:str :}
   t STR:DATA NULL-PTR <> if t exit then
   w TOKEN-SPELLS? if SRCLOC:TOKEN$ >str exit then
   NULL-PTR 0 >str ;

\ Places the record of word w, given the path a u, and opens its label, true;
\ the caller writes the rest, which reads after the path or after "that". With
\ no place the record has no label, false.
: LOADER-PATH-AT ( str str -- bool ) {: w:str s:str :}
   w s LOADER-PLACE$ {: t:str :}
   t STR:DATA NULL-PTR = if 0 0= 0= exit then
   t REJECTION:AT
   s SRCLOC:ORIGIN$ STR:DATA NULL-PTR = if
      s" passes a path that " REJECTION:EMIT-STR
   then
   0 0= ;

\ Word w's record for the path a u, kind E-PATH-NUL or E-PATH-RANGE: len is the
\ length given, unread when resolved is set because only the path a u resolves
\ to is over cap. Empty w names no word.
: LOADER-PATH-RECORD ( str str n n bool n -- )
   {: w:str s:str kind:n len:n resolved:bool cap:n :}
   w STR:DATA w STR:LEN SRCLOC:SPELLED drop >str {: name:str :}
   kind E-PATH-NUL = if
      s" E-PATH-NUL" REJECTION:OPEN
      w STR:LEN 0= if
         s" a path holds a NUL byte" REJECTION:EMIT-STR
      else
         96 REJECTION:EMIT1  name REJECTION:EMIT-STR
         s" ` got a path with a NUL byte" REJECTION:EMIT-STR
      then
      s" , which no file name holds" REJECTION:EMIT-STR
   else
      resolved if
         name cap REJECTION:PATH-RESOLVED-RANGE
      else
         name len cap REJECTION:PATH-RANGE
      then
   then
   w s LOADER-PATH-AT if
      kind E-PATH-NUL = if
         s" holds a NUL byte"
      else
         resolved if
            s" is too long once resolved"
         else
            len 0= if
               s" is empty"
            else
               len 0 < if s" has a length below 0" else s" is too long" then
            then
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
: LOADER-PATH-STOP ( str str n n bool -- )
   PATH-CAP LOADER-PATH-RECORD  EXIT-STATUS:REJECTED REJECTION:STOP ;

public

\ Whether a loader or ROOT! can take the path a u before resolving it: 0 0, else
\ the kind and len LOADER-PATH-STOP takes. Length is the resolver's to judge.
: PATH-FAULT ( str -- n n ) {: s:str :}
   s STR:LEN 0 <= if E-PATH-RANGE s STR:LEN exit then
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ 0= if drop E-PATH-NUL 0 exit then
      1 +
   repeat
   drop 0 0 ;

private

\ Word w's path a u, placed at p pu: one no file name holds stops with w's record.
: LOADER-PATH-CHECK ( str str str -- ) {: w:str p:str s:str :}
   s PATH-FAULT {: kind:n len:n :}
   kind 0 <> if w p kind len INCLUDE-FALSE LOADER-PATH-STOP then ;

public

\ The path a u that loader w takes, found by q, which is given a u and the
\ place of w's record; one it cannot take stops with that record. An empty w
\ is a command-line entry, whose record has no place.
: RESOLVE ( str str [ str str -- str bool ] -- str bool )
   {: w:str s:str q :}
   w STR:LEN 0= if NULL-PTR 0 >str else s then {: p:str :}
   w STR:LEN 0= if NULL-PTR 0 >str else w s LOADER-PLACE$ then {: t:str :}
   w p s LOADER-PATH-CHECK
   [: s t q execute ;] catch {: rc:n :}
   rc E-PATH-RANGE = if w p E-PATH-RANGE 0 INCLUDE-TRUE LOADER-PATH-STOP then
   rc 0= if exit then
   rc throw ;

;package

\ Canonical source paths and a dynamically scoped owner root. This bootstrap
\ layer uses only core bytes, mappings and the bounded realpath OS primitive.
package SOURCE-ROOT
EXPORT BOOT:data-base
EXPORT CHECK:ROOT-MOVER
EXPORT LOADER:INCLUDE-FALSE
EXPORT LOADER:INCLUDE-TRUE
EXPORT LOADER:LOADER-PATH-AT
EXPORT LOADER:LOADER-PATH-CHECK
EXPORT LOADER:LOADER-PATH-RECORD
EXPORT LOADER:LOADER-PATH-STOP
EXPORT REQUIRE-REG:REQUIRE-BASE
private

\ Word w's path a u, judged as spelled: one no file name holds, or one longer
\ than PATH-CAP, stops with w's record.
: GIVEN-CHECK ( str str -- ) {: w:str s:str :}
   w s s LOADER-PATH-CHECK
   s STR:LEN PATH-CAP > if w s E-PATH-RANGE s STR:LEN INCLUDE-FALSE LOADER-PATH-STOP then ;

PATH-CAP 1+ constant PATH-BYTES
create CWD-BUF PATH-BYTES allot
create CANON-BUF PATH-BYTES allot
create CANDIDATE-BUF PATH-BYTES allot
create OWNER-BUF PATH-BYTES allot
create OWNER-SAVE PATH-BYTES allot
create POLICY-ROOT-BUF PATH-BYTES allot
variable POLICY-ROOT-U
POLICY-ROOT-BUF POLICY-ROOT-U REQUIRES:POLICY-ROOT!
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
: ZPATH-OF ( str -- ^u8 ) {: s:str :}
   s STR:LEN 1+ Z-FIT  s ZBUF s STR:LEN 1+ PATHZ  ZBUF ;
variable REQUEST-U
variable CWD-U
variable CANON-U
variable NORMAL-U
variable CANDIDATE-U
variable JOIN-U
variable OWNER-U
\ Readers reach the loader's record by SRCLOC:SOURCE-CELL, the running hb's
\ in a build.
SRCLOC:REC-TOP constant REC-TOP
SRCLOC:REC-ROOT constant REC-ROOT
SRCLOC:REC-ROOTLEN constant REC-ROOTLEN
3 cells constant REC-BYTES
create LOADER-REC REC-BYTES SCRATCH-ALLOT
EXPORT POINTER-STORAGE:RESERVED-PTR-U8-CELL
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


: CURRENT! ( str -- )
   dup STR:LEN CURRENT-U !  STR:DATA CURRENT-A ! ;

\ The current path as set, null for the top-level root, which a scope saves
\ and restores: CURRENT$ would hand back CWD-BUF, which no top-level path may
\ hold (CD and POPPATH release the path they replace).
: CURRENT@ ( -- str )
   CURRENT-PTR CURRENT-U @ >str ;

\ Throws kind, E-PATH-RANGE or E-PATH-NUL, carrying the record of the path a
\ u, which names no word, since its callers serve many public words: len,
\ resolved and cap as LOADER-PATH-RECORD takes them. Callers catch either code
\ and go on.
: PATH-PASS ( str n n bool n -- )
   {: s:str kind:n len:n resolved:bool cap:n :}
   NULL-PTR 0 >str s kind len resolved cap LOADER-PATH-RECORD
   REJECTION:SAVE  kind REJECTION:PASS ;

: CHECK ( str -- )
   {: s:str :}
   s STR:LEN 0 <= if s E-PATH-RANGE s STR:LEN INCLUDE-FALSE PATH-CAP PATH-PASS then ;


\ realpath of a path of any written length into CANON-BUF: its length, below 0
\ if it does not resolve; realpath fails on a result CANON-BUF cannot hold.
: CANON-N ( str -- n )
   {: s:str :}
   s CHECK
   s LOADER:PATH-FAULT {: kind:n len:n :}
   kind 0 <> if s kind len INCLUDE-FALSE PATH-CAP PATH-PASS then
   s ZPATH-OF CANON-BUF PATH-BYTES >span realpath ;

public

: CANON-OS ( str -- str bool )
   {: s:str :}
   s CANON-N {: n:n :}
   n 0 < if CANON-BUF 0 >str INCLUDE-FALSE exit then
   CANON-BUF n >str INCLUDE-TRUE ;

private

\ Opens word w's record of the path it got: w is a word the program called or
\ SOURCE-INPUT:CANON, for the canon it supplied; why follows "which".
: PATH-REJECT ( str str str -- )
   {: w:str path:str why:str :}
   s" E-SOURCE-ROOT" REJECTION:OPEN
   96 REJECTION:EMIT1  w REJECTION:EMIT-STR  s" ` got `" REJECTION:EMIT-STR
   path REJECTION:EMIT-STR  s" `, which " REJECTION:EMIT-STR
   why REJECTION:EMIT-STR ;

\ Word w got the path a u, shown as p pu: why reads after "which" and "that";
\ help says what the program should pass, or its canon answer.
: PATH-DIE ( str str str str str -- )
   {: w:str s:str p:str why:str help:str :}
   w STR:DATA w STR:LEN SRCLOC:SPELLED drop >str p why PATH-REJECT
   w s LOADER-PATH-AT if why REJECTION:EMIT-STR then
   REJECTION:HELP  help REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: TRY-CANON ( str -- bool )
   {: p:str :}
   p SOURCE-INPUT:CANON {: c:str found:bool :}
   c STR:LEN PATH-BYTES >= if
      p E-PATH-RANGE 0 INCLUDE-TRUE PATH-BYTES 1- PATH-PASS
   then
   found if
      c LOADER:PATH-FAULT drop {: kind:n :}
      kind 0 <> if INCLUDE-TRUE else c STR:DATA c@ [char] / <> then if
         s" SOURCE-INPUT:CANON" p p
         kind E-PATH-NUL = if
            s" it answered with a path holding a NUL byte"
         else
            s" it answered with a path that does not begin with `/`"
         then
         s" have the canon given to `SOURCE-INPUT:USE` or `SOURCE-INPUT:WITH` answer an absolute path a file name can hold"
         PATH-DIE
      then
   then
   c STR:DATA CANON-BUF <> if c STR:DATA CANON-BUF c STR:LEN BYTE-COPY then
   c STR:LEN CANON-U !
   found ;


: CANON$ ( -- str ) CANON-BUF CANON-U @ >str ;


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
   EXIT-STATUS:IOERR REJECTION:STOP ;


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
   s" ." CWD-BUF CWD-U @ >str COMPILE-READS:ROOT+ ;

public

: CWD$ ( -- str ) CWD-RECORD CWD-BUF CWD-U @ >str ;


private

: PARENT-U ( str -- n ) {: s:str :}
   s STR:LEN
   begin dup 1 > while
      1- dup s STR:DATA + c@ 47 = if exit then
   repeat ;


\ The program file `hb prog.f` runs. No registry row names it: one appended on
\ first use would vanish as a failed require or a discovery pass rolls back.
PROGRAM-FILE-CELL RESERVED-PTR-U8-CELL PROGRAM-ARG
create PROGRAM-BUF PATH-BYTES allot
variable PROGRAM-U
SCRATCH-VARIABLE PROGRAM-ID

\ Of the path buffers it writes only PROGRAM-BUF, so the caller's survive.
: PROGRAM-INIT ( -- )
   PROGRAM-U @ 0= 0= if exit then
   PROGRAM-ARG @ {: a:^ :}
   a 0= if exit then
   a PROGRAM-BUF PATH-BYTES >span realpath {: n:n :}
   n 0 <= if
      s" source root: the program file does not resolve within PATH-CAP bytes"
      EXIT-STATUS:IOERR die
   then
   n PROGRAM-U ! ;

\ Whether a canonical path is the program file. A fresh discovery registry
\ (REQUIRE-REG:SNAPSHOT) hides it, as it hides every row recorded before it.
: PROGRAM? ( str -- bool ) {: s:str :}
   REQUIRE-BASE @ 0= 0= if INCLUDE-FALSE exit then
   PROGRAM-INIT
   PROGRAM-U @ 0= if INCLUDE-FALSE exit then
   s PROGRAM-BUF PROGRAM-U @ >str STR:= ;

\ The working directory, the engine root, the program file and the resolver's
\ work belong to the process that derived them, so their cells are scratch: a
\ saved program, a cross-built one included, starts with none and derives its
\ own instead of resolving against its builder's directory.
: SCRATCH-BYTES ( ^a n -- ) {: a:^ u:n :}
   u 0 ?do a i + scratch-cell-mark 1 cells +loop ;

CWD-BUF PATH-BYTES SCRATCH-BYTES        CWD-U scratch-cell-mark
CWD-BUF CWD-U REQUIRES:CWD!
CANON-BUF PATH-BYTES SCRATCH-BYTES      CANON-U scratch-cell-mark
NORMAL-U scratch-cell-mark
CANDIDATE-BUF PATH-BYTES SCRATCH-BYTES  CANDIDATE-U scratch-cell-mark
JOIN-U scratch-cell-mark
OWNER-BUF PATH-BYTES SCRATCH-BYTES      OWNER-U scratch-cell-mark
OWNER-SAVE PATH-BYTES SCRATCH-BYTES
REQUEST-U scratch-cell-mark
ENGINE-BUF PATH-BYTES SCRATCH-BYTES     ENGINE-U scratch-cell-mark
ENGINE-READ scratch-cell-mark
ENGINE-BUF ENGINE-U REQUIRES:ENGINE!
PROGRAM-BUF PATH-BYTES SCRATCH-BYTES    PROGRAM-U scratch-cell-mark

public

: CURRENT$ ( -- str )
   CURRENT-U @ 0 > if CURRENT-PTR CURRENT-U @ >str exit then
   PROGRAM-INIT
   PROGRAM-U @ 0= if CWD$ exit then
   PROGRAM-BUF PROGRAM-BUF PROGRAM-U @ >str PARENT-U >str ;

private

\ The root length is the caller's: one below 0 is rejected, and one whose sum
\ with u wraps is a joined path too long. Built in WORK-BUF, so a root or name
\ in JOIN-BUF is read first.
: JOIN! ( str str -- ) {: root:str s:str :}
   root STR:LEN 0 < if
      root STR:DATA 0 >str E-PATH-RANGE root STR:LEN INCLUDE-FALSE PATH-CAP PATH-PASS
   then
   root STR:LEN s STR:LEN + 1+ {: n:n :}
   n 0 < if root STR:DATA 0 >str E-PATH-RANGE 0 INCLUDE-TRUE PATH-CAP PATH-PASS then
   n WORK-FIT
   root STR:DATA WORK-BUF root STR:LEN BYTE-COPY
   47 WORK-BUF root STR:LEN + c!
   s STR:DATA WORK-BUF root STR:LEN 1+ + s STR:LEN BYTE-COPY
   n JOIN-FIT
   n JOIN-U !
   WORK-BUF JOIN-BUF n BYTE-COPY ;


: ABSOLUTE! ( str -- ) {: s:str :}
   s CHECK
   s STR:DATA c@ 47 = if
      s STR:LEN JOIN-FIT  s STR:DATA JOIN-BUF s STR:LEN BYTE-COPY s STR:LEN JOIN-U ! exit
   then
   CWD$ s JOIN! ;


\ p pu is the path being normalized.
: NORMAL-ROOM ( str n n -- )
   {: p:str n:n limit:n :}
   NORMAL-U @ n + limit > if p E-PATH-RANGE 0 INCLUDE-TRUE limit PATH-PASS then
   NORMAL-U @ n + NORMAL-FIT ;


: NORMAL-PUT ( str str n -- )
   {: p:str s:str limit:n :}
   NORMAL-U @ 1 > if
      p 1 limit NORMAL-ROOM
      47 NORMAL-BUF NORMAL-U @ + c!
      1 NORMAL-U +!
   then
   p s STR:LEN limit NORMAL-ROOM
   s STR:DATA NORMAL-BUF NORMAL-U @ + s STR:LEN BYTE-COPY
   s STR:LEN NORMAL-U +! ;


: NORMAL-SEG ( str str n -- )
   {: p:str s:str limit:n :}
   s STR:LEN 0= if exit then
   s s" ." STR:= if exit then
   s s" .." STR:= if
      NORMAL-BUF NORMAL-U @ >str PARENT-U NORMAL-U ! exit
   then
   p s limit NORMAL-PUT ;


: SEG-END ( str n -- n ) {: s:str start:n :}
   start
   begin dup s STR:LEN < while
      dup s STR:DATA + c@ 47 = if exit then
      1+
   repeat ;


: NORMALIZE-LIMIT ( str n -- )
   {: s:str limit:n :}
   1 NORMAL-FIT  47 NORMAL-BUF c! 1 NORMAL-U !
   0 begin dup s STR:LEN < while
      dup s rot SEG-END {: start:n end:n :}
      s  s STR:DATA start +  end start - >str  limit NORMAL-SEG
      end 1+
   repeat drop ;


: NORMALIZE ( str -- ) PATH-CAP NORMALIZE-LIMIT ;

\ A build compiles a second copy of this file into the program it builds, and
\ there LOADER answers hb's record: the copy's loading words would run its own
\ registry, buffers and frames over hb's. The build's loader does every load,
\ at a file's top level, so in a build the copy's loads reject (BUILD-REJECT).
: OWN-LOADER? ( -- bool ) LOADER LOADER-REC = ;

public

\ The resolver does not reject: RESOLVE loads nothing, and in a build it
\ searches the process's current root, so a word may ask where a file lies.
\ Each loading word rejects before it writes a row: a row a caught rejection
\ left would lie outside the build's space. Public for provided, which this
\ file defines outside SOURCE-ROOT. w is the word the program called.
: BUILD-REJECT ( str str -- ) {: w:str path:str :}
   OWN-LOADER? if exit then
   w path path
   s" a word may not load or provide while a build runs"
   s" load or provide the file at a file's top level, which the build's loader runs"
   PATH-DIE ;

private

\ Whether the segment g is empty or `.`.
: DOT-SEG? ( str -- bool ) {: g:str :}
   g STR:LEN 0= if INCLUDE-TRUE exit then
   g STR:LEN 1 =  g STR:DATA c@ 46 =  and ;

\ The end of the run of empty and `.` segments of a u after end e; e if none.
: DOT-RUN-END ( str n -- n ) {: s:str e:n :}
   e begin dup s STR:LEN < while
      dup s rot 1+ SEG-END {: d:n f:n :}
      s STR:DATA d 1+ +  f d 1+ - >str DOT-SEG? 0= if d exit then
      f
   repeat ;

\ The real path in CANON-BUF, `/` and the segment p u, in WORK-BUF: its length.
: SEG-PATH ( str -- n ) {: s:str :}
   CANON-U @ {: r:n :}
   r s STR:LEN + 1+ WORK-FIT
   CANON-BUF WORK-BUF r BYTE-COPY
   CANON-BUF r + 1- c@ 47 = if r else 47 WORK-BUF r + c!  r 1+ then {: h:n :}
   s STR:DATA WORK-BUF h + s STR:LEN BYTE-COPY
   h s STR:LEN + ;

\ The end of the segment of a u after end e, or of the run of empty and `.` ones
\ there, if the canon resolves it under CANON-BUF's real path; else e.
: CANON-STEP ( str n -- n ) {: s:str e:n :}
   s e DOT-RUN-END {: d:n :}
   CANON-U @ {: r:n :}
   d e > if
      d  s" ."
   else
      s e 1+ SEG-END  s STR:DATA e 1+ +  over e - 1- >str
   then {: g:n seg:str :}
   seg SEG-PATH {: tu:n :}
   WORK-BUF tu >str TRY-CANON if g exit then
   WORK-BUF CANON-BUF r BYTE-COPY  r CANON-U !  e ;

\ The length of the longest prefix of the absolute a u, cut at a segment end,
\ the canon resolves segment by segment, its real path in CANON-BUF; 0 if none.
: CANON-PREFIX ( str -- n )
   {: s:str :}
   s STR:DATA 1 >str TRY-CANON 0= if 0 exit then
   0 begin dup s STR:LEN < while
      dup s rot CANON-STEP
      2dup = if drop 1 max exit then
      nip
   repeat ;

\ The file system root always resolves through realpath; only a supplied canon
\ (SOURCE-INPUT:USE) can answer that no directory above a path does.
: EXISTING-PARENT ( -- n )
   JOIN-BUF  JOIN-BUF JOIN-U @ >str PARENT-U >str  CANON-PREFIX
   dup 0= if
      s" SOURCE-INPUT:CANON" JOIN-BUF JOIN-U @ >str JOIN-BUF JOIN-U @ >str
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
   WORK-BUF CANON-U @ >str NORMALIZE
   NORMAL-U @ {: base:n :}
   \ The unresolved rest drops only empty and `.` segments: each `..` there
   \ follows a segment no lookup resolved, or a file, so it climbs from nothing.
   CANON-U @ begin dup total < while
      dup WORK-BUF total >str rot SEG-END {: start:n end:n :}
      WORK-BUF start +  end start - >str {: seg:str :}
      seg DOT-SEG? 0= if
         WORK-BUF total >str  seg  PATH-CAP NORMAL-PUT
      then
      end 1+
   repeat drop
   \ A rest of only empty and `.` segments follows a file the lookup cannot
   \ enter: one `.` stays, so the answer is never that file's own path.
   NORMAL-U @ base = if WORK-BUF total >str s" ." PATH-CAP NORMAL-PUT then ;

public

\ The canonical path and whether it exists, until the next CANONICAL call; a
\ missing path's resolves only its longest existing prefix.
: CANONICAL ( str -- str bool )
   ABSOLUTE!
   JOIN-BUF JOIN-U @ >str TRY-CANON if
      CANON-U @ NORMAL-FIT
      CANON-BUF NORMAL-BUF CANON-U @ BYTE-COPY
      CANON-U @ NORMAL-U !
      NORMAL-BUF NORMAL-U @ >str INCLUDE-TRUE exit
   then
   MISSING-NORMALIZE
   NORMAL-BUF NORMAL-U @ >str INCLUDE-FALSE ;


\ A name with no slash lies in the current directory, so its directory is `.`;
\ PARENT-U alone would keep its first byte.
: DIRNAME ( str -- str ) {: s:str :}
   s 0 SEG-END s STR:LEN = if s" ." exit then
   s STR:DATA s PARENT-U >str ;


: JOIN ( str str -- str ) {: s:str :}
   s CHECK
   s STR:DATA c@ 47 = if drop s exit then
   s JOIN!
   JOIN-BUF JOIN-U @ >str ;


: BELOW? ( str str -- bool ) {: s:str root:str :}
   s STR:LEN root STR:LEN < if INCLUDE-FALSE exit then
   s STR:DATA root STR:LEN >str root STR:= 0= if INCLUDE-FALSE exit then
   s STR:LEN root STR:LEN = if INCLUDE-TRUE exit then
   root STR:LEN 1 = root STR:DATA c@ 47 = and if INCLUDE-TRUE exit then
   s STR:DATA root STR:LEN + c@ 47 = ;


: RELATIVE ( str str -- str ) {: s:str root:str :}
   s root BELOW? 0= if s exit then
   s STR:LEN root STR:LEN = if s" ." exit then
   root STR:LEN 1 = if s STR:DATA 1+ s STR:LEN 1- >str exit then
   s STR:DATA root STR:LEN 1+ + s STR:LEN root STR:LEN 1+ - >str ;


: RESOLVED-ROOT$ ( -- str ) OWNER-BUF OWNER-U @ >str ;

private

: OWNER! ( str -- ) {: s:str :}
   s STR:LEN PATH-CAP > if E-PATH-RANGE throw then
   s STR:DATA OWNER-BUF s STR:LEN BYTE-COPY s STR:LEN OWNER-U ! ;


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
   WORK-BUF n 2 + >str ZPATH-OF 0 access 0= ;

\ A root's lookup: the canonical length in CANON-BUF of the spelling, -1 when it
\ does not resolve, -3 when ENGINE-DIR? rejects it.
: ROOT-N ( str -- n )
   CANON-N {: n:n :}
   n 0 < if -1 exit then
   n ENGINE-DIR? 0= if -3 exit then
   n ;

\ ENGINE-DIR? decides a root on every platform; the canon of a path does not.
\ Neither says why a path fails, so the rejection names the absolute spelling
\ tried and what a missing, unsearchable or long directory and a file all are
\ not. w is the word the program called.
: ROOT-DIE ( str -- )
   JOIN-BUF JOIN-U @ >str JOIN-BUF JOIN-U @ >str
   s" does not resolve to a searchable directory within PATH-CAP bytes"
   s" name an existing directory this process can search"
   PATH-DIE ;

: ROOT-CANON ( str str -- ) {: w:str s:str :}
   w s GIVEN-CHECK
   s ABSOLUTE!
   JOIN-BUF JOIN-U @ >str ROOT-N {: n:n :}
   n 0 < if w ROOT-DIE then
   n CANON-U ! ;

: OUTSIDE-ROOT ( str str -- ) {: s:str p:str :}
   s" E-POLICY" REJECTION:OPEN
   96 REJECTION:EMIT1  s REJECTION:EMIT-STR
   s" ` lies outside the source root" REJECTION:EMIT-STR
   p STR:LEN 0 <> if p REJECTION:AT  s" outside the root" REJECTION:EMIT-STR then
   s" root" REJECTION:NOTE
   POLICY-ROOT-BUF POLICY-ROOT-U @ >str REJECTION:EMIT-STR
   REJECTION:HELP
   s" load only files under the source root, which this " REJECTION:EMIT-STR
   s" program's policy set with `POLICY-ROOT`" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ CONFINE of a path a program asked for as req, which a rejection names and
\ places at p, or nowhere when pu is 0.
: CONFINE-AS ( str str str -- str )
   {: s:str req:str p:str :}
   data-base POLICY-REQUIRE-CELL + @ 0= if s exit then
   s CANONICAL drop {: path:str :}
   path POLICY-ROOT-BUF POLICY-ROOT-U @ >str BELOW? if path exit then
   req p OUTSIDE-ROOT ;

public

\ ROOT-N's lookup, `.` being the working directory: the canonical directory and
\ true; false too for an empty spelling. Lasts until the next canon.
: ROOT? ( str -- str bool )
   {: s:str :}
   s STR:LEN 1 < if CANON-BUF 0 >str INCLUDE-FALSE exit then
   s ROOT-N {: n:n :}
   n 0 < if CANON-BUF 0 >str INCLUDE-FALSE exit then
   CANON-BUF n >str INCLUDE-TRUE ;

: CONFINE ( str -- str ) {: s:str :}
   s s NULL-PTR 0 >str CONFINE-AS ;

private

: MAP-COPY ( str -- ^u8 )
   {: s:str :}
   s STR:LEN 1+ map-anon 0= 0= if drop s STR:LEN 1+ s" a path copy" s" " REJECTION:MAP-PASS-FILE then
   {: fresh:^ :}
   s STR:DATA fresh s STR:LEN 1+ BYTE-COPY
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
         s" source root: cannot release a saved path" EXIT-STATUS:IOERR die
      then
   then
   NULL$ STR:DATA ix SLOT-FIELD !
   0 ix SLOT-U! ;

: ROOT-ENTER ( str str -- str n )
   ROOT-CANON
   CANON-U @ {: u:n :}
   CANON-BUF u >str MAP-COPY {: fresh:^ :}
   CURRENT@
   SLOT-BASE @  SLOT-N @ SLOT-BASE !
   fresh u >str CURRENT! ;

\ The scope's current path at exit (its own, or one ROOT!, CD or POPPATH set) and
\ each path PUSHPATH saved in the scope are released once the caller's root is back.
: ROOT-LEAVE ( str n -- )
   {: old:str base:n :}
   CURRENT@ {: cur:str :}
   old CURRENT!
   cur STR:DATA cur STR:LEN 1+ munmap 0 < if
      s" source root: cannot release a root mapping" EXIT-STATUS:IOERR die
   then
   SLOT-N @ SLOT-BASE @ ?do i SLOT-RELEASE loop
   SLOT-BASE @ SLOT-N !
   base SLOT-BASE ! ;

\ Clears the token read last and answers it, so no record a scope's callback
\ makes stands at the call that opened the scope.
: TOKEN-CLEAR ( -- str )
   SRCLOC:TOKEN$ >str {: t:str :}
   NULL-PTR 0 SRCLOC:TOKEN!
   t ;

public

\ The caller's root is back before the scope's is released, on a throw too.
: WITH ( str [ -- ] -- ) {: s:str q :}
   s" SOURCE-ROOT:WITH" s ROOT-ENTER {: old:str base:n :}
   JOIN-BUF JOIN-U @ >str CURRENT$ COMPILE-READS:ROOT+
   TOKEN-CLEAR {: t:str :}
   q catch {: rc:n :}
   t STR:DATA t STR:LEN SRCLOC:TOKEN!
   old base ROOT-LEAVE
   rc 0= 0= if rc throw then ;

private

\ A current path is ROOT!'s mapping or POPPATH's, inside a scope also the scope's.
: RELEASE-CURRENT ( -- )
   CURRENT-U @ 0 > if
      CURRENT-PTR CURRENT-U @ 1+ munmap 0 < if
         s" source root: cannot release the current path" EXIT-STATUS:IOERR die
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
   INPUT-AT @  REJECTION:INPUT$ {: in:str :}
   in STR:DATA in STR:LEN + {: e:^ :}
   begin dup e < while
      dup c@ 10 = if drop INCLUDE-TRUE exit then
      dup c@ 32 > if drop INCLUDE-FALSE exit then
      1 +
   repeat
   drop INCLUDE-TRUE ;

\ Word w got the path a u that, alone or joined to the current root, is len bytes,
\ more than PATH-CAP.
: ROOT-LONG ( str str n -- ) {: w:str s:str len:n :}
   w STR:DATA w STR:LEN SRCLOC:SPELLED drop >str len PATH-CAP REJECTION:PATH-RANGE
   w s LOADER-PATH-AT if
      len s STR:LEN = if
         s" is too long"
      else
         s" is too long once joined to the current root"
      then REJECTION:EMIT-STR
   then
   REJECTION:HELP
   s" name a directory whose full path is at most " REJECTION:EMIT-STR
   PATH-CAP REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ ROOT! for word w, the one the program called.
: ROOT-SET ( str str -- ) {: w:str s:str :}
   w s s LOADER-PATH-CHECK
   s STR:LEN PATH-CAP > if w s s STR:LEN ROOT-LONG then
   CURRENT$ s JOIN {: p:str :}
   p STR:LEN PATH-CAP > if w s p STR:LEN ROOT-LONG then
   p ROOT-N {: n:n :}
   n -1 = if
      w s p s" does not resolve to a directory"
      s" name an existing directory" PATH-DIE
   then
   n -3 = if
      w s p s" is not a directory this process can search"
      s" name a directory, one this process may search" PATH-DIE
   then
   p CANON-BUF n >str COMPILE-READS:ROOT+
   CANON-BUF n >str MAP-COPY {: fresh:^ :}
   RELEASE-CURRENT
   fresh n >str CURRENT! ;

public

\ The directory is the current root, searched first by a relative require, until
\ the load or WITH body it runs in ends. hb's directory and engine root stay.
: ROOT! ( str -- ) {: s:str :}
   s" SOURCE-ROOT:ROOT!" s ROOT-SET ;
ROOT-MOVER

: CD ( -- )
   s" SOURCE-ROOT:CD" dup STR:DATA swap STR:LEN SRCLOC:SPELLED drop >str {: w:str :}
   LINE-BLANK? if CURRENT$ type cr exit then
   w parse-name ROOT-SET ;
ROOT-MOVER

\ A null slot is the top-level root.
: PUSHPATH ( -- )
   SLOT-N @ SLOT-CAP @ >= if SLOT-GROW then
   SLOT-N @ {: ix:n :}
   CURRENT-U @ {: u:n :}
   u 0 > if CURRENT-PTR u >str MAP-COPY ix SLOT-FIELD ! then
   u ix SLOT-U!
   ix 1+ SLOT-N ! ;

: POPPATH ( -- )
   s" SOURCE-ROOT:POPPATH" dup STR:DATA swap STR:LEN SRCLOC:SPELLED drop >str
   {: w:str :}
   SLOT-N @ SLOT-BASE @ = if
      s" E-SOURCE-ROOT" REJECTION:OPEN
      96 REJECTION:EMIT1  w REJECTION:EMIT-STR
      s" ` runs with no directory saved" REJECTION:EMIT-STR
      REJECTION:HELP
      s" save the current directory with `SOURCE-ROOT:PUSHPATH` first; " REJECTION:EMIT-STR
      s" a load or `SOURCE-ROOT:WITH` body pops only what it saved" REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   SLOT-N @ 1- {: ix:n :}
   RELEASE-CURRENT
   ix SLOT-U@ {: u:n :}
   u 0 > if ix SLOT-FIELD @ u >str CURRENT! then
   NULL$ STR:DATA ix SLOT-FIELD !
   0 ix SLOT-U!
   ix SLOT-N ! ;
ROOT-MOVER

;package

package LOADER

: INCLUDE-DIE ( str -- )
   EXIT-STATUS:IOERR die ;

: INCLUDE-CLOSE ( -- )
   INCLUDE-FD @ dup 0 >= if
      close
   else
      drop
   then
   -1 INCLUDE-FD ! ;

public

\ The loader word, the source token t tu, has no path after it. It is written
\ through its help, and the caller stops it.
: NO-PATH ( str -- ) {: t:str :}
   t s" a path" t s" no path follows"
   REJECTION:MISSING-OPERAND
   s" write the file's path after the word, as " REJECTION:EMIT-STR
   s" `require src/lib/fs.f`" REJECTION:EMIT-STR ;

private

\ A loader word's parsed path of length u: a missing one is rejected at the word.
: INCLUDE-CHECK-PATH ( n -- ) {: u:n :}
   u 0 <= if
      SRCLOC:TOKEN$ >str NO-PATH  EXIT-STATUS:REJECTED REJECTION:STOP
   then ;

;package

package REQUIRE-REG
private
EXPORT BOOT:data-base
EXPORT LOADER:INCLUDE-DIE

$7FFFFFFFFFFFFFFF constant MAX-BYTES
$7FFFFFFFFFFFFFFF 3 cells / constant ROW-LIMIT
3 cells constant CHUNK-HEAD
MAX-BYTES CHUNK-HEAD - constant CHUNK-LIMIT

\ A mapped chunk's room and used counts are the two cells after its prev
\ pointer, and its path bytes follow them at CHUNK-HEAD.
CAST: CHUNK-COUNTS ( ^^u8 -- ^n )

: FROZEN-REC ( n -- ^n )
   3 * cells FROZEN @ swap + ;

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

: ROW-ID-ADDR ( n -- ^n )
   ROW-IDS @ swap cells + ;

: FROZEN-BYTES ( n -- n )
   3 * cells ;

: ROW-NEW-CAP ( n -- n ) {: need:n :}
   need $40 max
   ROW-CAP @ ROW-LIMIT 2 / <= if ROW-CAP @ 2 * max then ;

: COLS-MAP ( n -- ^^u8 )
   3 * cells {: bytes:n :}
   bytes map-anon 0 <> if
      drop bytes s" the loader's row table" s" " REJECTION:MAP-PASS-FILE
   then ;

: COLS-UNMAP ( ^^u8 n -- ) {: rows:^ cap:n :}
   rows cap 3 * cells munmap 0 < if
      s" require: cannot release the row table" INCLUDE-DIE
   then ;

: ROW-GROW ( n -- ) {: need:n :}
   need ROW-NEW-CAP {: cap:n :}
   ROWS @ ROW-LENS @ ROW-IDS @ ROW-CAP @
   {: old:^ oldlens:^ oldids:^ oldcap:n :}
   cap COLS-MAP {: base:^ :}
   base BYTE-VIEW cap cells + CELL-VIEW {: lens:^ :}
   base BYTE-VIEW cap 2 * cells + CELL-VIEW {: ids:^ :}
   oldcap 0 ?do
      old i ptr-field @ base i ptr-field !
      oldlens i cells + @ lens i cells + !
      oldids i cells + @ ids i cells + !
   loop
   base ROWS !
   lens ROW-LENS !
   ids ROW-IDS !
   cap ROW-CAP !
   oldcap 0 > if old oldcap COLS-UNMAP then ;

: ROW-ENSURE ( n -- ) {: need:n :}
   need ROW-CAP @ > if need ROW-GROW then ;

: ROWS-RELEASE ( -- )
   ROW-CAP @ 0= if exit then
   ROWS @ ROW-CAP @ COLS-UNMAP
   NULL-PTR ROWS !
   NULL-PTR ROW-LENS !
   NULL-PTR ROW-IDS !
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
   CHUNK @ {: prev:^ :}
   room CHUNK-HEAD + map-anon 0 <> if
      drop room CHUNK-HEAD + s" a path chunk" s" " REJECTION:MAP-PASS-FILE
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

: CHUNK-TRIM-TO ( ^^u8 str -- ) {: chunk:^ path:str :}
   path STR:DATA chunk CHUNK-DATA - path STR:LEN + chunk CHUNK-USED!
   chunk CHUNK ! ;

: CHUNKS-TRIM ( str -- ) {: path:str :}
   CHUNK @ begin dup NULL-PTR <> while
      dup path STR:DATA CHUNK-CONTAINS? if path CHUNK-TRIM-TO exit then
      dup CHUNK-PREV@ swap dup CHUNK-BYTES munmap 0 < if
         s" require: cannot release a path chunk" INCLUDE-DIE
      then
   repeat drop ;

: ROUND-CELL ( n -- n ) {: bytes:n :}
   bytes CELL mod {: rem:n :}
   rem 0= if bytes exit then
   bytes CELL rem - + ;

: CLAMP ( -- )
   PREFIX-N @ REQUIRE-N @ > if REQUIRE-N @ PREFIX-N ! then ;

: LIVE-BYTES ( -- n )
   0 REQUIRE-N @ 0 ?do
      i PREFIX-N @ < if i FROZEN-LEN else i ROW-LEN@ then +
   loop ;

: PACK-BYTES ( n -- n ) REQUIRE-N @ FROZEN-BYTES + ;

: PAD-BYTES ( ^u8 -- n )
   data-base - negate CELL 1- and ;

: TOTAL-BYTES ( n n -- n ) + ROUND-CELL ;

public

: SLOT ( n -- ^u8 ) {: idx:n :}
   idx PREFIX-N @ < if idx FROZEN-SLOT else idx ROW-PTR@ then ;

: LEN ( n -- n ) {: idx:n :}
   idx PREFIX-N @ < if idx FROZEN-LEN else idx ROW-LEN@ then ;

\ Row idx's file id: 0 for a row no require or end file minted one for.
: ID ( n -- n ) {: idx:n :}
   idx PREFIX-N @ < if idx FROZEN-REC 2 cells + @ exit then
   idx ROW-ID-ADDR @ ;

: APPEND ( str -- ) {: s:str :}
   REQUIRE-N @ {: idx:n :}
   idx 1+ ROW-ENSURE
   s STR:LEN CHUNK-SPACE {: path:^ :}
   s STR:DATA path s STR:LEN BYTE-COPY
   path idx ROW-PTR !
   s STR:LEN idx ROW-LEN-ADDR !
   0 idx ROW-ID-ADDR !
   idx 1+ REQUIRE-N ! ;

\ APPEND of a file the image carries, which a build's end file mints its id
\ for: the rows' order is their ids'.
: APPEND-FILE ( str -- ) {: s:str :}
   s APPEND
   s NULL$ REQUIRES:MINT  REQUIRE-N @ 1- ROW-ID-ADDR ! ;

: REWIND ( n -- ) {: keep:n :}
   keep PREFIX-N @ <= if
      CHUNKS-RELEASE
   else
      keep 1- dup ROW-PTR@ swap ROW-LEN@ >str CHUNKS-TRIM
   then
   keep REQUIRE-N !
   CLAMP ;

\ Four other cells are cursors into rows; a bare REQUIRE-N ! would leave them
\ past the new end.
: TRUNCATE ( n -- )
   {: keep:n :}
   keep 0 < keep REQUIRE-N @ > or if
      s" require: truncate outside the registry" EXIT-STATUS:INTERNAL die
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
   idx REQUIRE-N @ 1- = if
      idx REWIND
   else
      REQUIRE-N @ idx 1+ ?do
         i ROW-PTR@ i 1- ROW-PTR !
         i ROW-LEN@ i 1- ROW-LEN-ADDR !
         i ROW-ID-ADDR @ i 1- ROW-ID-ADDR !
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
      cursor fresh i 3 * cells + CELL-VIEW !
      u fresh i 3 * 1+ cells + CELL-VIEW !
      i ID fresh i 3 * 2 + cells + CELL-VIEW !
      cursor u +
   loop drop
   CHUNKS-RELEASE
   ROWS-RELEASE
   fresh FROZEN !
   REQUIRE-N @ PREFIX-N ! ;

private

: REQUIRE-BYTE= ( ^u8 n n -- bool ) {: a:^ idx:n i:n :}
   a i ZBYTE@ idx SLOT i ZBYTE@ = ;

: REQUIRE-PATH= ( str n -- bool ) {: s:str idx:n :}
   idx LEN s STR:LEN <> if INCLUDE-FALSE exit then
   0 begin dup s STR:LEN < while
      dup s STR:DATA idx rot REQUIRE-BYTE= 0= if drop INCLUDE-FALSE exit then
      1+
   repeat drop INCLUDE-TRUE ;

public

\ The known row holding path a u, or -1.
: ROW-OF ( str -- n ) {: s:str :}
   REQUIRE-BASE @ begin dup REQUIRE-N @ < while
      dup s rot REQUIRE-PATH= if exit then
      1+
   repeat drop -1 ;

private

: REQUIRE-KNOWN? ( str -- bool ) ROW-OF 0 >= ;

;package

package SOURCE-ROOT
EXPORT REQUIRE-REG:REQUIRE-BOOT-LIMIT
EXPORT REQUIRE-REG:REQUIRE-KNOWN?
EXPORT REQUIRE-REG:REQUIRE-PATH=
private

: LOADED? ( str -- bool ) {: s:str :}
   s PROGRAM? if INCLUDE-TRUE exit then
   s REQUIRE-KNOWN? ;

\ The running engine's path: on macOS the one execve received, in the
\ executable_path= apple string past envp's terminator; on Linux
\ /proc/self/exe. Both name the engine, not the script, for a `#!` program
\ file. proc_pidpath needs FUNCTION:, which this layer cannot use.
: ENVP-END ( -- n )
   0 begin dup ENVP 0= 0= while 1+ repeat 1+ ;

\ Apple strings can be empty, so the scan ends at the null pointer.
: APPLE-EXE ( -- str )
   ENVP-BASE 0= if NULL$ exit then
   ENVP-END begin dup ENVP 0= 0= while
      dup ENVP s" executable_path" ENV=? if
         ENVP s" executable_path=" STR:LEN ZPTR+ dup ZLEN >str exit
      then
      1+
   repeat drop NULL$ ;

\ `%` in CANON-BUF, as a length. 0 when there is no path, it is longer than
\ PATH-CAP or realpath rejects it; never a throw: the length is checked before
\ CANON-N, and neither path holds a NUL.
: EXE-ROOT ( str -- n ) {: s:str :}
   s STR:LEN 0= s STR:LEN PATH-CAP > or if 0 exit then
   s CANON-N {: n:n :}
   n 0 <= if 0 exit then
   CANON-BUF CANON-BUF n >str PARENT-U >str PARENT-U ;

: TREE? ( str -- bool )
   0 REQUIRE-REG:SLOT 0 REQUIRE-REG:LEN >str JOIN ZPATH-OF 0 access 0= ;

\ The root's length in CANON-BUF, 0 for the working directory.
: ENGINE-FIND-FOR ( str -- n )
   REQUIRE-BOOT-LIMIT 0= if drop 0 exit then
   CWD$ TREE? if drop 0 exit then
   EXE-ROOT {: n:n :}
   n 0= if 0 exit then
   CANON-BUF n >str TREE? 0= if 0 exit then
   n ;

\ A target with no executable path, as Wasm, loads no source.
: ENGINE-FIND ( -- n )
   HB-TARGET-MACOS? if APPLE-EXE ENGINE-FIND-FOR exit then
   HB-TARGET-LINUX? if
      s" /proc/self/exe" ENGINE-FIND-FOR exit
   then
   s" include: no engine path on this target" EXIT-STATUS:INTERNAL die ;

: ENGINE-INIT ( -- )
   ENGINE-READ @ 0= 0= if exit then
   ENGINE-FIND {: n:n :}
   CANON-BUF ENGINE-BUF n BYTE-COPY
   n ENGINE-U !
   1 ENGINE-READ ! ;

public

\ The root is derived once, from whether the working directory is a Habu tree, so every answer
\ records that read: a part's reads do not depend on which part asked first.
: ENGINE$ ( -- str )
   ENGINE-INIT
   ENGINE-U @ 0= if CWD$ exit then
   CWD-RECORD ENGINE-BUF ENGINE-U @ >str ;

\ Override the fallback root; the caller restores the old one after its load.
: ENGINE! ( str -- ) {: s:str :}
   s" SOURCE-ROOT:ENGINE!" s ROOT-CANON
   CANON-BUF ENGINE-BUF CANON-U @ BYTE-COPY
   CANON-U @ ENGINE-U !
   1 ENGINE-READ ! ;
ROOT-MOVER

private

\ A new id for the file at path a u, whose requires resolve under root.
: FILE-ID ( str str -- n ) {: s:str root:str :}
   root ENGINE$ STR:= if s ENGINE$ RELATIVE else s then {: p:str :}
   p STR:DATA c@ 47 = if root else NULL$ then {: r:str :}
   p r REQUIRES:MINT ;

: REQUEST! ( str -- ) {: s:str :}
   s CHECK
   s STR:LEN REQUEST-FIT  s STR:DATA REQUEST-BUF s STR:LEN BYTE-COPY s STR:LEN REQUEST-U ! ;

: REQUEST$ ( -- str ) REQUEST-BUF REQUEST-U @ >str ;

\ The boot row from `first` on holding a u, or -1; a boot row is its portable
\ name, so this reads the registry itself.
: BOOT-ROW-OF ( str n -- n ) {: s:str first:n :}
   first begin dup REQUIRE-BOOT-LIMIT < while
      dup s rot REQUIRE-PATH= if exit then
      1+
   repeat drop -1 ;

: CANDIDATE$ ( -- str ) CANDIDATE-BUF CANDIDATE-U @ >str ;

\ A request spelled under a root, normalized but not resolved, in NORMAL-BUF: no
\ longer than the join and a leading `/`, which may outrun PATH-CAP.
: UNDER ( str str -- str )
   JOIN {: j:str :}
   j j STR:LEN 1+ NORMALIZE-LIMIT
   NORMAL-BUF NORMAL-U @ >str ;

\ The frozen row from `first` on whose engine-root copy the candidate is, or -1;
\ the request spelled under the root counts only if it resolves to the candidate.
: BOOT-MATCH ( str str str n -- str n )
   {: s:str root:str req:str first:n :}
   first REQUIRE-BOOT-LIMIT >= if s -1 exit then
   s root RELATIVE first BOOT-ROW-OF {: row:n :}
   row 0 >= if s row exit then
   s STR:LEN CANDIDATE-U ! s STR:DATA CANDIDATE-BUF s STR:LEN BYTE-COPY
   root req UNDER root BELOW? if
      NORMAL-BUF NORMAL-U @ >str root RELATIVE first BOOT-ROW-OF {: under:n :}
      under 0 >= if
         NORMAL-BUF NORMAL-U @ >str CANONICAL drop
         CANDIDATE$ STR:= if CANDIDATE$ under exit then
         CANDIDATE$ -1 exit
      then
   then
   CANDIDATE$ -1 ;

\ Whether a selected candidate is the engine root's own copy of a frozen row.
: BOOT-CANDIDATE ( str str str n -- str bool )
   BOOT-MATCH 0 >= ;

\ One root's candidate for REQUEST$: its canonical path, whether it is known,
\ and whether a search stops there (known, or on disk). Under any root the
\ candidate can be the engine root's physical copy of a frozen row, counted
\ from row `first`: the root's files reached through its parent directory or a
\ symlinked working directory are still the engine's, and BOOT-CANDIDATE's
\ physical test keeps another directory's file with an engine file's name its
\ own.
: CANDIDATE ( str n -- str bool bool )
   {: first:n :}
   dup OWNER!
   REQUEST$ JOIN CANONICAL {: exists:bool :}
   dup LOADED? {: known:bool :}
   known 0= if
      ENGINE$ REQUEST$ first BOOT-CANDIDATE
   else known then
   dup exists or ;

: ENGINE-SEARCHED? ( -- bool ) ENGINE$ CURRENT$ STR:= 0= ;
: CWD-SEARCHED? ( -- bool )
   CWD$ CURRENT$ STR:= CWD$ ENGINE$ STR:= or 0= ;

\ A relative request's roots in order, each once: its owner, the engine root,
\ then the working directory. The first root whose candidate stops the search
\ answers, found; when none does, the owner's answer, not found, names the
\ missing file. So a library the owner lacks is the engine's whatever the
\ working directory holds, and the working directory answers only what neither
\ holds: an application's entry below its tree, run from the tree, requires the
\ tree's files by their tree-relative names.
: SEARCH-ROOTS ( n -- str bool bool )
   {: first:n :}
   CURRENT$ first CANDIDATE dup if exit then 2drop drop
   ENGINE-SEARCHED? if
      ENGINE$ first CANDIDATE dup if exit then 2drop drop
   then
   CWD-SEARCHED? if
      CWD$ first CANDIDATE dup if exit then 2drop drop
   then
   CURRENT$ first CANDIDATE ;

\ A missing file's directory need not be a root at all (missing, a file,
\ unsearchable), so its answer keeps the current root.
: ABS-OWNER ( str bool -- )
   {: s:str exists:bool :}
   s CURRENT$ BELOW? if CURRENT$ OWNER! exit then
   s ENGINE$ BELOW? if ENGINE$ OWNER! exit then
   s CWD$ BELOW? if CWD$ OWNER! exit then
   exists 0= if CURRENT$ OWNER! exit then
   s DIRNAME OWNER! ;

\ The file a require of REQUEST$ selects, with frozen rows counted from row
\ `first`, whether it is known and whether it was found.
: SELECT ( n -- str bool bool )
   {: first:n :}
   REQUEST-BUF c@ $2F = if
      REQUEST$ CANONICAL {: exists:bool :}
      dup exists ABS-OWNER
      dup LOADED? {: known:bool :}
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
: REQUEST-AT! ( str -- ) dup STR:LEN REQUEST-AT-U !  STR:DATA REQUEST-AT-A ! ;
: REQUEST-AT$ ( -- str ) REQUEST-AT-A @ REQUEST-AT-U @ >str ;

public

\ The label of errno e, and the os error its record names: e where the label
\ does not name e, else 0.
: ERRNO-WHY$ ( n -- str n ) {: e:n :}
   e 2 = if s" no such file" 0 exit then
   e 13 = if s" permission denied" 0 exit then
   e 20 = if s" a directory in the path is a file" 0 exit then
   e 21 = if s" is a directory" 0 exit then
   e 5 = if s" input/output error" 0 exit then
   e 23 = e 24 = or if s" too many open files" 0 exit then
   e 12 = if s" out of memory" 0 exit then
   s" the system call failed" e ;

\ Writes why, then os error e unless e is 0, as Rust names an OS error.
: UNREAD-WHY ( str n -- ) {: why:str e:n :}
   why REJECTION:EMIT-STR
   e 0= if exit then
   s"  (os error " REJECTION:EMIT-STR  e REJECTION:EMIT-N  41 REJECTION:EMIT1 ;

private

\ Writes a u as one single-quoted shell word: each `'` in it closes the quote,
\ is written escaped and reopens it, so no path byte acts in a shell.
: SH-QUOTED ( str -- ) {: s:str :}
   39 REJECTION:EMIT1
   s STR:LEN 0 ?do
      s STR:DATA i + c@ dup 39 = if drop s" '\''" REJECTION:EMIT-STR else REJECTION:EMIT1 then
   loop
   39 REJECTION:EMIT1 ;

\ The help for path, whose open or read failed with errno e. EACCES's names no
\ owner: Habu looks none up, and the reader knows whose file it is.
: OPEN-HELP ( str n -- ) {: path:str e:n :}
   e 2 = if s" create the file or correct the path" REJECTION:EMIT-STR exit then
   e 13 = if
      s" if the file is yours, make it readable with `chmod u+r " REJECTION:EMIT-STR
      path SH-QUOTED
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
: UNREAD-OS-RECORD ( str str str n str -- )
   {: t:str path:str why:str e:n note:str :}
   s" E-UNREADABLE-SOURCE" REJECTION:OPEN
   s" cannot read `" REJECTION:EMIT-STR
   t STR:LEN 0= if
      path REJECTION:EMIT-STR  s" `: " REJECTION:EMIT-STR
      why e UNREAD-WHY  REJECTION:HELP exit
   then
   t REJECTION:EMIT-STR  96 REJECTION:EMIT1
   t REJECTION:AT  why e UNREAD-WHY
   note REJECTION:NOTE  path REJECTION:EMIT-STR
   REJECTION:HELP ;

: UNREAD-RECORD ( str str str -- ) 0 s" resolved" UNREAD-OS-RECORD ;

\ EISDIR for a path that names a directory, else e: an open of a directory
\ without read permission fails EACCES, but a stat of it succeeds.
: SOURCE-ERRNO ( str n -- n )
   {: path:str e:n :}
   path ZPATH-OF COMPILE-READS:STAMP drop nip 32 rshift
   FS-ABI:S-IFDIR = if 21 exit then e ;

public

\ Rejects path, found but failing its open or a read with errno e, at t tu, else
\ unplaced. Public so a walk that reads a found file rejects it with its record.
: UNREADABLE-SOURCE ( str str n -- )
   {: t:str path:str e:n :}
   path e SOURCE-ERRNO {: why:n :}
   t path why ERRNO-WHY$ s" resolved" UNREAD-OS-RECORD
   path why OPEN-HELP
   EXIT-STATUS:IOERR REJECTION:STOP ;

\ The file path changed while it was read, so its bytes may be no single
\ version of it. Placed as UNREADABLE-SOURCE places its record.
: CHANGED-SOURCE ( str str -- )
   s" changed while it was read" UNREAD-RECORD
   s" load it again once nothing is writing it" REJECTION:EMIT-STR
   EXIT-STATUS:IOERR REJECTION:STOP ;

\ The file path is a stream, not a regular file, so a reader that stamps what
\ it reads cannot stamp it. Placed as UNREADABLE-SOURCE places its record.
: UNSTAMPED-SOURCE ( str str -- )
   s" is not a regular file" UNREAD-RECORD
   s" load a regular file, whose bytes a stamp can name" REJECTION:EMIT-STR
   EXIT-STATUS:IOERR REJECTION:STOP ;

private

: INCLUDE-ERRNO ( -- n )   data-base FS-ABI:ERRNO-OFF + @ ;

: RESOLVED-U ( n -- n ) DENIED-PATH swap >str CANON-PREFIX ;

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
   DENIED-PATH w >str c SEG-END {: e:n :}
   CANON-U @ {: r:n :}
   DENIED-PATH c + e c - >str SEG-PATH {: lu:n :}
   WORK-BUF lu >str ZPATH-OF  PATH-BYTES WORK-FIT
   WORK-BUF PATH-BYTES >span readlink {: t:n :}
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
: DENIED-FROM ( n n -- str ) {: w:n links:n :}
   w RESOLVED-U {: k:n :}
   k 0= if DENIED-PATH 0 >str exit then
   CANON-U @ {: r:n :}
   r ENGINE-DIR? 0= INCLUDE-ERRNO 13 = and if
      r DENIED-FIT  CANON-BUF DENIED-PATH r BYTE-COPY  DENIED-PATH r >str exit
   then
   links 40 >= if DENIED-PATH 0 >str exit then
   w k LINK-NEXT {: next:n :}
   next 0= if DENIED-PATH 0 >str exit then
   next links 1+ RECURSE ;

\ The real directory whose search denied a lookup of path, symlinks followed;
\ 0 when no lookup is denied or the walk cannot reach that directory.
: SEARCH-DENIED ( str -- str ) {: s:str :}
   s STR:LEN DENIED-FIT  s STR:DATA DENIED-PATH s STR:LEN BYTE-COPY
   s STR:LEN 0 DENIED-FROM ;

\ Rejects the joined path a denied lookup searched, at t tu, else unplaced, by
\ its CANONICAL name; the help names dir, in DENIED-PATH, that CANONICAL spares.
: UNSEARCHABLE-SOURCE ( str str -- ) {: t:str path:str :}
   path SEARCH-DENIED {: dir:str :}
   dir STR:LEN 0= if exit then
   t path CANONICAL drop 13 ERRNO-WHY$ s" searched" UNREAD-OS-RECORD
   s" if the directory `" REJECTION:EMIT-STR  dir REJECTION:EMIT-STR
   s" ` is yours, make it searchable with `chmod u+x " REJECTION:EMIT-STR
   dir SH-QUOTED
   s" `; else ask its owner to let you search it" REJECTION:EMIT-STR
   EXIT-STATUS:IOERR REJECTION:STOP ;

public

\ Rejects request a u, which no root holds, at p, unplaced when pu is 0; as
\ unreadable where a denied lookup hides it. An entry is searched for and
\ helped as a command-line path. Public for check.f's and the verifier's walks.
: MISSING-SOURCE ( str str bool -- )
   {: s:str p:str entry:bool :}
   entry  s STR:DATA c@ 47 =  or if
      p CWD$ s JOIN UNSEARCHABLE-SOURCE
   else
      p CURRENT$ s JOIN UNSEARCHABLE-SOURCE
      ENGINE-SEARCHED? if p ENGINE$ s JOIN UNSEARCHABLE-SOURCE then
      CWD-SEARCHED? if p CWD$ s JOIN UNSEARCHABLE-SOURCE then
   then
   s" E-MISSING-SOURCE" REJECTION:OPEN
   s" cannot open `" REJECTION:EMIT-STR  s REJECTION:EMIT-STR
   96 REJECTION:EMIT1
   p STR:LEN 0 <> if p REJECTION:AT  s" not found" REJECTION:EMIT-STR then
   s" searched" REJECTION:NOTE
   entry  s STR:DATA c@ 47 =  or if
      CWD$ s JOIN CANONICAL drop REJECTION:EMIT-STR
   else
      CURRENT$ s JOIN CANONICAL drop REJECTION:EMIT-STR
      ENGINE-SEARCHED? if
         s" , " REJECTION:EMIT-STR
         ENGINE$ s JOIN CANONICAL drop REJECTION:EMIT-STR
      then
      CWD-SEARCHED? if
         s" , " REJECTION:EMIT-STR
         CWD$ s JOIN CANONICAL drop REJECTION:EMIT-STR
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
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ RESOLVE of a u whose rejection stands at t tu, unplaced when tu is 0. SELECT
\ and CANONICAL may reuse the buffer a u lies in, so a rejection names
\ REQUEST$, the request's copy.
: RESOLVE-AT ( str str -- str bool ) {: s:str t:str :}
   s REQUEST! REQUIRE-BASE @ SELECT drop
   >r REQUEST$ t CONFINE-AS r> ;

\ A rejection stands at the source a u stands for, else nowhere.
: RESOLVE ( str -- str bool )
   dup SRCLOC:ORIGIN$ RESOLVE-AT ;

private

: LOAD-RESOLVE ( str str -- str bool ) {: s:str t:str :}
   s REQUEST! REQUIRE-BASE @ SELECT {: found:bool :}
   found 0= if REQUEST$ t INCLUDE-FALSE MISSING-SOURCE then
   >r REQUEST$ t CONFINE-AS r> ;

public

\ Command-line entries are relative to the invocation directory only;
\ dependencies beneath them inherit the entry directory as their primary root.
: ENTRY-RESOLVE ( str -- str bool ) {: s:str :}
   s REQUEST!
   CWD$ REQUIRE-BASE @ CANDIDATE
   {: path:str known:bool found:bool :}
   found 0= if s NULL-PTR 0 >str INCLUDE-TRUE MISSING-SOURCE then
   path DIRNAME OWNER!
   path CONFINE known ;

\ The selection counts every frozen row whatever REQUIRE-BASE is, so a frozen
\ row is the engine's even where its file is missing. RESOLVED-ROOT$ stays as
\ the caller's own RESOLVE left it.
: ENGINE-KNOWN? ( str -- bool )
   OWNER-BUF OWNER-SAVE OWNER-U @ BYTE-COPY
   OWNER-U @ {: owner:n :}
   REQUEST! 0 SELECT 2drop
   ENGINE$ REQUEST$ 0 BOOT-CANDIDATE nip
   OWNER-SAVE OWNER-BUF owner BYTE-COPY
   owner OWNER-U ! ;

: BOOT-REJECT ( str -- ) {: root:str :}
   s" E-SOURCE-ROOT" REJECTION:OPEN
   96 REJECTION:EMIT1  REQUEST$ REJECTION:EMIT-STR
   s" ` is a boot file outside the engine root" REJECTION:EMIT-STR
   s" root" REJECTION:NOTE  root REJECTION:EMIT-STR
   REJECTION:HELP
   s" require it after `REQUIRE-REG:BOOT-FREEZE`, or move it under `" REJECTION:EMIT-STR
   root REJECTION:EMIT-STR  96 REJECTION:EMIT1
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The row a boot file is recorded as: the spelling BOOT-CANDIDATE asks for. That
\ is its path below the engine root, or, for a file a directory symlink inside
\ the root carries from outside it, the request spelled under the root when that
\ resolves to this very file.
\ Any other file is rejected by name: its row could only be its canonical path,
\ and an engine carrying one takes that file for a stranger under every other
\ root.
: BOOT-ROW ( str -- str str ) {: s:str :}
   s STR:LEN CANDIDATE-U ! s STR:DATA CANDIDATE-BUF s STR:LEN BYTE-COPY
   ENGINE$ {: root:str :}
   CANDIDATE$ root BELOW? if
      CANDIDATE$ CANDIDATE$ root RELATIVE exit
   then
   root REQUEST$ UNDER root BELOW? if
      NORMAL-BUF NORMAL-U @ >str CANONICAL drop CANDIDATE$ STR:=
   else INCLUDE-FALSE then
   0= if root BOOT-REJECT then
   CANDIDATE$ root REQUEST$ UNDER root RELATIVE ;

;package

package REQUIRE-REG
EXPORT SOURCE-ROOT:FILE-ID
public

\ Mints row idx's file id, its requires resolved under root; the id leaves with
\ its row.
: MINT ( n str -- n ) {: idx:n root:str :}
   idx SLOT idx LEN >str root FILE-ID {: id:n :}
   id idx ROW-ID-ADDR !
   id ;

\ A boot row is recorded portably, in the one spelling BOOT-ROW-OF asks for,
\ relative to the engine root; a build's fresh registry makes that root the
\ working directory. Against the load's own owner root a require issued by a
\ boot file would shorten to a bare basename no query spells, naming a
\ different file in every directory. A canonical absolute spelling would bake
\ the build directory into the engine, so two builds of one revision from two
\ directories would differ. An application row keeps its canonical identity;
\ the two cannot collide, as a canonical name begins with `/` and a portable
\ one never does. The path comes back in storage the store did not reuse, for
\ the caller's load.
: STORE ( str -- str )
   REQUIRE-BOOT-OPEN? if SOURCE-ROOT:BOOT-ROW else dup then
   APPEND ;

;package

package LOADER

: INCLUDE-PATH0 ( str -- ^u8 )
   {: s:str :}
   s INCLUDE-PATH PATH-CAP 1+ PATHZ
   s STR:LEN INCLUDE-PATH-U !
   INCLUDE-PATH ;

;package

\ Every source load runs through the bound runner. The ordinary one keeps the
\ frame and scope behavior; a build binds its own once the loader has loaded.
package SOURCE-UNIT
private

TYPED-VARIABLE LOAD-XT [ str str str [ n n -- nominal ] -- renders nominal roots ]

: ORDINARY ( str str str [ n n -- ] -- )
   {: path:str root:str source:str q :}
   0 source STR:LEN q execute ;

public

: LOAD ( str str str [ n n -- nominal ] -- ) LOAD-XT @ execute ;
\ A runner calls the file's quotation as `0 sourceu q execute` with nothing of
\ its own beneath it; the quotation throws for a rejected file.
: USE ( [ str str str [ n n -- nominal ] -- renders nominal roots ] -- ) LOAD-XT ! ;
: CURRENT ( -- [ str str str [ n n -- nominal ] -- renders nominal roots ] ) LOAD-XT @ ;
: RESET ( -- ) [: ORDINARY ;] LOAD-XT ! ;

;package

SOURCE-UNIT:RESET

package LOADER
EXPORT BOOT:data-base

: LOADER-TOK-A ( -- n )   data-base TKA-CELL + @ ;
: LOADER-TOK-U ( -- n )   data-base TKL-CELL + @ ;
: LOADER-TOKEN-SPAN ( -- n n )  LOADER-TOK-A LOADER-TOK-U ;

;package

\ One event per loader act: the request as spelled and the path it resolved to,
\ which a discovery pass replays in order and a build's linker carries per file.
package SOURCE-EVENT
EXPORT LOADER:INCLUDE-DIE
EXPORT LOADER:LOADER-TOKEN-SPAN

13 constant EVENT-FIELDS
EVENT-FIELDS cells constant EVENT-BYTES
$1000 constant EVENT-ROOM-MIN                    \ a first mapping's bytes

public

0 constant KIND-INCLUDED
1 constant KIND-REQUIRED
2 constant KIND-PROVIDED
3 constant KIND-TEXT-REQUIRED        \ by text a word evaluated: it grants no file
0 constant STATE-FRESH
1 constant STATE-KNOWN

private

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

: EVENT-ON? ( -- bool )   EVENT-ON-V @ 0= 0= ;
: DISCOVERY? ( -- bool )  EVENT-DISC-V @ 0= 0= ;

public

: ON ( -- )               1 EVENT-ON-V ! ;
: OFF ( -- )              0 EVENT-ON-V ! ;
: DISCOVERY-ON ( -- )     1 EVENT-DISC-V ! ;
: DISCOVERY-OFF ( -- )    0 EVENT-DISC-V ! ;

private

: EVENT-RECS@ ( -- ^u8 ) EVENT-RECS @ ;
: EVENT-POOL@ ( -- ^u8 ) EVENT-POOL @ ;

: EVENT-UNMAP ( ^a n -- ) {: a:^ u:n :}
   u 0= if exit then
   a u munmap 0 < if s" events: cannot release an event table" INCLUDE-DIE then ;

\ The caller publishes the answer and CAP once this returns, so a rejection
\ throws while OLD is still the one published.
: EVENT-REMAP ( ^u8 n n n -- ^u8 )
   {: old:^ oldcap:n used:n cap:n :}
   cap map-anon 0= 0= if drop cap s" an event table" s" " REJECTION:MAP-PASS-FILE then
   {: fresh:^ :}
   old fresh used BYTE-COPY
   old oldcap EVENT-UNMAP
   fresh ;

public

\ Reset releases both mappings, so snapshot preparation leaves a captured image
\ no path of the machine that recorded them and no address of a mapping.
: RESET ( -- )
   EVENT-RECS@ EVENT-RECS-CAP @ EVENT-UNMAP
   NULL-PTR EVENT-RECS !  0 EVENT-RECS-CAP !  0 EVENT-N !
   EVENT-POOL@ EVENT-POOL-CAP @ EVENT-UNMAP
   NULL-PTR EVENT-POOL !  0 EVENT-POOL-CAP !  0 EVENT-POOL-N ! ;

private

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
   need EVENT-POOL-CAP @ <= if exit then
   need EVENT-POOL-CAP @ EVENT-GROWN {: cap:n :}
   EVENT-POOL@ EVENT-POOL-CAP @ EVENT-POOL-N @ cap EVENT-REMAP
   EVENT-POOL !
   cap EVENT-POOL-CAP ! ;

: EVENT-COPY-PATH ( str -- n n ) {: s:str :}
   s STR:LEN EVENT-POOL-ROOM
   EVENT-POOL-N @ {: off:n :}
   s STR:DATA off EVENT-POOL-AT s STR:LEN BYTE-COPY
   off s STR:LEN + EVENT-POOL-N !
   off s STR:LEN ;

public

: ROOT@ ( n -- str ) {: ix:n :}
   ix 6 EVENT-FIELD@ EVENT-POOL-AT ix 7 EVENT-FIELD@ >str ;

\ The current root when the event was recorded.
: CURRENT@ ( n -- str ) {: ix:n :}
   ix 11 EVENT-FIELD@ EVENT-POOL-AT ix 12 EVENT-FIELD@ >str ;

private

: COPY-SHARED ( str -- n n ) {: s:str :}
   0 begin dup EVENT-N @ < while
      dup ROOT@ s STR:= if
         dup 6 EVENT-FIELD@ swap 7 EVENT-FIELD@ exit
      then
      dup CURRENT@ s STR:= if
         dup 11 EVENT-FIELD@ swap 12 EVENT-FIELD@ exit
      then
      1+
   repeat drop
   s EVENT-COPY-PATH ;

public

: STORE-ROOT ( n -- ) {: ix:n :}
   SOURCE-ROOT:RESOLVED-ROOT$ COPY-SHARED {: off:n len:n :}
   off ix 6 EVENT-FIELD!
   len ix 7 EVENT-FIELD!
   SOURCE-ROOT:CURRENT$ COPY-SHARED {: coff:n clen:n :}
   coff ix 11 EVENT-FIELD!
   clen ix 12 EVENT-FIELD! ;

private

: EVENT-RECORD ( str str n n -- )
   {: s:str path:str kd:n st:n :}
   EVENT-ON? 0= if exit then
   EVENT-RECS-ROOM
   EVENT-N @ {: ix:n :}
   LOADER-TOKEN-SPAN {: toka:n toku:n :}
   path EVENT-COPY-PATH {: off:n len:n :}
   s EVENT-COPY-PATH {: soff:n slen:n :}
   kd    ix 0 EVENT-FIELD!
   off   ix 1 EVENT-FIELD!
   len   ix 2 EVENT-FIELD!
   toka  ix 3 EVENT-FIELD!
   toku  ix 4 EVENT-FIELD!
   st    ix 5 EVENT-FIELD!
   soff  ix 8 EVENT-FIELD!
   slen  ix 9 EVENT-FIELD!
   depth ix 10 EVENT-FIELD!     \ the callers hold their arguments in locals: the requester's depth
   ix STORE-ROOT
   ix 1 + EVENT-N ! ;

public

: COUNT ( -- n )             EVENT-N @ ;
: KIND@ ( n -- n )           0 EVENT-FIELD@ ;

\ Whether event n is a require, of a file's text or of one a word evaluated.
: REQUIRE? ( n -- bool )
   KIND@ {: kind:n :}
   kind KIND-REQUIRED =  kind KIND-TEXT-REQUIRED =  or ;

: STATE@ ( n -- n )          5 EVENT-FIELD@ ;
: PATH@ ( n -- str ) {: ix:n :}
   ix 1 EVENT-FIELD@ EVENT-POOL-AT
   ix 2 EVENT-FIELD@ >str ;

private

: EVENT-SPELLED@ ( n -- str ) {: ix:n :}
   ix 8 EVENT-FIELD@ EVENT-POOL-AT
   ix 9 EVENT-FIELD@ >str ;
: EVENT-TOK@ ( n -- n n ) {: ix:n :}
   ix 3 EVENT-FIELD@ ix 4 EVENT-FIELD@ ;
: EVENT-DEPTH@ ( n -- n )    10 EVENT-FIELD@ ;

;package

package REQUIRE-REG

: REQUIRE-STATE ( bool -- n )
   if SOURCE-EVENT:STATE-KNOWN exit then SOURCE-EVENT:STATE-FRESH ;

;package

package SOURCE-ROOT
EXPORT CHECK:FILE-POS-CLEAR
EXPORT SCHEMA-REG:TRANSIENT-MARK
EXPORT SCHEMA-REG:TRANSIENT-RELEASE
EXPORT LOADER:INCLUDE-CLOSE
EXPORT LOADER:INCLUDE-DIE
EXPORT LOADER:INCLUDE-FD
EXPORT LOADER:INCLUDE-PATH
EXPORT LOADER:INCLUDE-PATH-U
EXPORT LOADER:INCLUDE-PATH0
EXPORT LOADER:INCLUDE-RD
EXPORT LOADER:INCLUDE-U
private

\ A frame carries its own resolved path: a nested include overwrites
\ INCLUDE-PATH before the inner file is read, so only the frame names the outer
\ file to an inner rejection or a POP. Its mapping is sized to the file:
\ FR-ROOM counts the source bytes it can hold, FR-SIZE the file's.
SRCLOC:FR-PREV constant FR-PREV
SRCLOC:FR-PATHLEN constant FR-PATHLEN
1 cells constant FR-ROOM
SRCLOC:FR-SIZE constant FR-SIZE
3 cells constant FR-ROOTLEN
SRCLOC:FR-EXEC-COPY constant FR-EXEC-COPY
5 cells constant FR-EVAL-OK
7 cells constant FR-EVAL-PTR
8 cells constant FR-EVAL-SIZE
9 cells constant FR-EVAL-FIRST
10 cells constant FR-EVAL-END
\ How many root programs are being read while this frame is the top.
11 cells constant FR-ROOT-RUNS
\ The file's stamp: change time seconds, nanoseconds and size.
12 cells constant FR-CTIME-SEC
13 cells constant FR-CTIME-NS
14 cells constant FR-STAMP-SIZE
\ The transient schema mark the file's load took (SCHEMA-REG:TRANSIENT-MARK).
15 cells constant FR-T-NODES
16 cells constant FR-T-ROOTS
17 cells constant FR-T-PARAMS
\ The reader its text is read by (REQUIRES:CURRENT@).
18 cells constant FR-FILE
\ The file id current when it opened, current again once it closes.
19 cells constant FR-WAS
SRCLOC:FR-PATH constant FR-PATH
SRCLOC:FR-ROOT constant FR-ROOT
SRCLOC:HEADER-BYTES constant HEADER-BYTES
$1000 constant ROOM-MIN
\ A frame's FR-PREV, FR-EXEC-COPY and FR-EVAL-PTR cells hold addresses; its
\ other header cells hold counts, flags and stamps, and its bytes follow them.
CAST: FRAME-CELLS ( ^u8 -- ^^u8 )
: FRAME-PREV ( ^u8 -- ^^u8 ) FRAME-CELLS FR-PREV CELL / ptr-field ;
: FRAME-EXEC-COPY ( ^u8 -- ^^u8 )
   FRAME-CELLS FR-EXEC-COPY CELL / ptr-field ;
: FRAME-EVAL-PTR ( ^u8 -- ^^u8 )
   FRAME-CELLS FR-EVAL-PTR CELL / ptr-field ;
variable LAST-LOAD-EVAL-OK

: TOP@ ( -- ^u8 ) TOP @ ;
: TOP! ( ^u8 -- ) TOP ! ;

\ w is the word the program called.
: CHECK-ACTIVE ( str -- ) {: w:str :}
   TOP@ 0= 0= if exit then
   s" E-SOURCE-ROOT" REJECTION:OPEN
   96 REJECTION:EMIT1  w REJECTION:EMIT-STR
   s" ` runs with no file loading" REJECTION:EMIT-STR
   REJECTION:HELP  s" call it from code a load runs" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: FRAME-EXEC-SOURCE ( ^u8 -- ^u8 )
   {: frame:^ :}
   frame FRAME-EXEC-COPY @ dup 0= if
      drop frame HEADER-BYTES +
   then ;

\ The engine's rejections name their place from these four cells and
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

: TOP-PATH$ ( -- str )
   TOP@ {: frame:^ :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @ >str ;

: FRAME-FILE ( ^u8 -- n ) {: frame:^ :}
   frame 0= if 0 exit then
   frame FR-FILE + CELL-VIEW @ ;

public

\ The reader of the frame the newest one opened in (REQUIRES); 0 for an entry.
: OUTER-FILE ( -- n ) TOP@ FRAME-PREV @ FRAME-FILE ;

private

\ Opens a frame whose text is read by reader id (REQUIRES); POP makes current
\ again the reader current here.
: PUSH ( n -- )
   {: id:n :}
   HEADER-BYTES ROOM-MIN + {: bytes:n :}
   bytes map-anon 0= 0= if
      drop bytes s" a source frame" INCLUDE-PATH INCLUDE-PATH-U @ >str
      REJECTION:MAP-PASS-FILE
   then
   {: frame:^ :}
   TOP@ frame FRAME-PREV !
   INCLUDE-PATH-U @ frame FR-PATHLEN + CELL-VIEW !
   ROOM-MIN frame FR-ROOM + CELL-VIEW !
   -1 frame FR-SIZE + CELL-VIEW !
   NULL-PTR frame FRAME-EXEC-COPY !
   0 frame FR-EVAL-OK + CELL-VIEW !
   0 frame FR-ROOT-RUNS + CELL-VIEW !
   0 frame FR-CTIME-SEC + CELL-VIEW !
   0 frame FR-CTIME-NS + CELL-VIEW !
   -1 frame FR-STAMP-SIZE + CELL-VIEW !
   id frame FR-FILE + CELL-VIEW !
   TRANSIENT-MARK {: nodes:n roots:n params:n :}
   nodes frame FR-T-NODES + CELL-VIEW !
   roots frame FR-T-ROOTS + CELL-VIEW !
   params frame FR-T-PARAMS + CELL-VIEW !
   INCLUDE-PATH frame FR-PATH + INCLUDE-PATH-U @ BYTE-COPY
   CURRENT$ {: root:str :}
   root STR:LEN frame FR-ROOTLEN + CELL-VIEW !
   root STR:DATA frame FR-ROOT + root STR:LEN BYTE-COPY
   REQUIRES:CURRENT@ frame FR-WAS + CELL-VIEW !
   frame TOP!
   id REQUIRES:CURRENT!
   PUBLISH-LOCATION ;

: POP ( -- )
   s" SOURCE-ROOT:POP" CHECK-ACTIVE
   TOP@ {: frame:^ :}
   frame FRAME-EXEC-COPY @ {: copy:^ :}
   frame FR-SIZE + CELL-VIEW @ 1 max {: copyu:n :}
   frame FRAME-PREV @ TOP!
   frame FR-WAS + CELL-VIEW @ REQUIRES:CURRENT!
   PUBLISH-LOCATION
   copy 0= if 0 else copy copyu munmap then {: copyrc:n :}
   frame dup MAPPED munmap 0 < copyrc 0 < or if
      s" include: cannot release a source frame" INCLUDE-DIE then ;

: SOURCE ( -- ^u8 ) s" SOURCE-ROOT:SOURCE" CHECK-ACTIVE TOP@ HEADER-BYTES + ;

\ TOP and the published location, the only pointers into a filling frame, move
\ only once the fresh mapping is made: a rejection leaves the old one published.
: GROW ( n n -- )
   {: want:n keep:n :}
   TOP@ {: frame:^ :}
   ROOM 2 * want max {: room:n :}
   HEADER-BYTES room + map-anon 0= 0= if
      drop HEADER-BYTES room + s" a source frame" TOP-PATH$ REJECTION:MAP-PASS-FILE
   then
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
: READ-REJECT ( str -- ) {: path:str :}
   TOP@ 0= 0= if TOP@ FR-SIZE + CELL-VIEW @ -1 = if exit then then
   s" SOURCE-ROOT:READ-OS" path path
   s" is not the file a load is opening"
   s" call it only from a reader installed with `SOURCE-INPUT:USE`, for the path the loader passes"
   PATH-DIE ;

\ Where a rejection of the file being opened stands: at the request in the file
\ that asked for it; an entry's has no place.
: ASKED-AT$ ( -- str )
   TOP@ FRAME-PREV @ {: prev:^ :}
   prev 0= if NULL-PTR 0 >str else prev PUBLISH-FRAME REQUEST-AT$ then ;

\ The file a u the resolver found would not open or read, the system's last
\ call failing with errno e.
: INCLUDE-UNREADABLE ( str n -- ) {: s:str e:n :}
   INCLUDE-CLOSE
   ASKED-AT$ s e UNREADABLE-SOURCE ;

\ The file a u the resolver found is a stream (a FIFO, device or socket), whose
\ open would block or whose bytes no stamp names.
: INCLUDE-UNSTAMPED ( str -- ) {: s:str :}
   ASKED-AT$ s UNSTAMPED-SOURCE ;

\ The file a u the resolver found holds more bytes than its stamp's size.
: INCLUDE-CHANGED ( str -- ) {: s:str :}
   INCLUDE-CLOSE
   ASKED-AT$ s CHANGED-SOURCE ;

: INCLUDE-OPEN ( str -- ) {: s:str :}
   s INCLUDE-PATH0 open-rd INCLUDE-FD !
   INCLUDE-FD @ 0 < if s INCLUDE-ERRNO INCLUDE-UNREADABLE then ;

\ A read stops once it holds more than SIZE bytes, the stamp's size.
: INCLUDE-READ-DONE? ( str n -- bool ) {: s:str size:n :}
   INCLUDE-U @ ROOM = if INCLUDE-U @ 1+ INCLUDE-U @ GROW then
   INCLUDE-FD @ SOURCE INCLUDE-U @ + ROOM INCLUDE-U @ - read INCLUDE-RD !
   INCLUDE-RD @ 0 < if s INCLUDE-ERRNO INCLUDE-UNREADABLE then
   INCLUDE-RD @ 0 = if INCLUDE-TRUE exit then
   INCLUDE-U @ INCLUDE-RD @ + INCLUDE-U !
   INCLUDE-U @ size > if s INCLUDE-CHANGED then
   INCLUDE-FALSE ;

: INCLUDE-READ-ALL ( str -- str ) {: s:str :}
   s INCLUDE-PATH0 COMPILE-READS:STAMP {: sec:n ns:n size:n :}
   sec ns size STAMP!
   sec ns size COMPILE-READS:STABLE? 0= if s INCLUDE-UNSTAMPED then
   s INCLUDE-OPEN
   0 INCLUDE-U !
   begin s size INCLUDE-READ-DONE? 0= while repeat
   INCLUDE-CLOSE
   SOURCE INCLUDE-U @ >str ;

public

: ACTIVE-SOURCE ( -- str str str )
   s" SOURCE-ROOT:ACTIVE-SOURCE" CHECK-ACTIVE
   TOP@ {: frame:^ :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @ >str
   frame FR-ROOT + frame FR-ROOTLEN + CELL-VIEW @ >str
   SOURCE frame FR-SIZE + CELL-VIEW @ >str ;

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
   TRANSIENT-RELEASE ;

\ The loader's reader, public so a reader installed with SOURCE-INPUT:USE may
\ call it from READ-SOURCE; it fills the top frame only while that awaits its bytes.
: READ-OS ( str -- str )
   s" SOURCE-ROOT:READ-OS" over GIVEN-CHECK dup READ-REJECT INCLUDE-READ-ALL ;

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
defer INCLUDE-INTERPRET ( str n n -- nominal )

\ Once the seal is set a file is read by `evaluate-range`, which reads the seal,
\ whatever INCLUDE-INTERPRET holds. `0 <>`: prelude.f's `0<>` loads after this.
: POLICY-SEALED? ( -- bool )
   data-base POLICY-NDICT-CELL + @ 0 <> ;

\ The OS reader has filled the frame already, learning the size as it read;
\ another reader's bytes come with their length, and the frame grows to fit
\ them before they are copied in.
: HOLD ( str -- str )
   {: s:str :}
   s STR:DATA SOURCE <> if
      s STR:LEN ROOM > if s STR:LEN 0 GROW then
      s STR:DATA SOURCE s STR:LEN BYTE-COPY
   then
   s STR:LEN TOP@ FR-SIZE + CELL-VIEW !
   PUBLISH-LOCATION
   SOURCE s STR:LEN >str ;

: READ-SOURCE ( -- str )
   INCLUDE-PATH INCLUDE-PATH-U @ >str SOURCE-INPUT:READ HOLD ;

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
   frame FR-EVAL-SIZE + CELL-VIEW @ >str
   dup SPLIT-STR-FILE? CHECKER-SPLIT-STR !
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

: INTERPRET-INSTALL ( -- ) [: evaluate-range ;] is INCLUDE-INTERPRET ;
INTERPRET-INSTALL

public

: USE-INTERPRETER ( [ str n n -- nominal ] -- ) is INCLUDE-INTERPRET ;

\ The process's root stream (stdin, or the `--load` stream) runs open, as the
\ engine's own loop runs it: it may leave cells on the stack and the process
\ still exits 0. The stream's text is no file's, so a rejection in it names none:
\ run while a file is open, it counts itself on that file's frame, which then
\ publishes no location until the stream returns or throws.
: ROOT-RUNS+ ( n -- ) {: k:n :}
   TOP@ 0= if exit then
   TOP@ {: frame:^ :}
   frame FR-ROOT-RUNS + CELL-VIEW @ k + frame FR-ROOT-RUNS + CELL-VIEW !
   PUBLISH-LOCATION ;

: RUN-ROOT ( str -- ) {: s:str :}
   REJECTION:ROOT-TEXT$ {: was:str :}
   s REJECTION:ROOT-TEXT!
   1 ROOT-RUNS+
   s 0 s STR:LEN [: INCLUDE-INTERPRET ;]
   [: -1 ROOT-RUNS+  was REJECTION:ROOT-TEXT! ;] finally ;

private

: LOAD-BYTES ( -- )
   READ-SOURCE {: s:str :}
   s STR:DATA {: a:^ :}  s STR:LEN {: u:n :}
   u 2 >= if
      a c@ $23 = a 1 + c@ $21 = and if
         u 1 max map-anon 0= 0= if
            drop u 1 max s" a copy of the source" TOP-PATH$ REJECTION:MAP-PASS-FILE
         then
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
   TOP@ {: frame:^ :}
   frame FR-SIZE + CELL-VIEW @ {: size:n :}
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
   RESIDUE-AT {: r:str :}
   r STR:LEN 0 > if
      r REJECTION:AT  s" the cells left start here" REJECTION:EMIT-STR
   then
   n RESIDUE-NOTE
   REJECTION:HELP
   s" drop or consume them before the file ends: " REJECTION:EMIT-STR
   s" a value crosses a load only through a word or a variable"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: RESIDUE-TAKEN ( n -- ) {: n:n :}
   s" E-EVAL-RESIDUE" REJECTION:OPEN
   s" this file takes " REJECTION:EMIT-STR  n RESIDUE-COUNT
   s"  it did not push" REJECTION:EMIT-STR
   REJECTION:HELP
   s" a file starts on an empty stack: " REJECTION:EMIT-STR
   s" pass values through a word or a variable" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

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

: TICKED ( str -- )
   96 REJECTION:EMIT1  REJECTION:EMIT-STR  96 REJECTION:EMIT1 ;

\ Opens the E-UNCLOSED record of the declaration t tu opened, through "is still
\ open ".
: UNCLOSED-HEAD ( str -- ) {: t:str :}
   s" E-UNCLOSED" REJECTION:OPEN
   t TICKED  s"  is still open " REJECTION:EMIT-STR ;

\ Places the record at t tu and opens its help, through "close it with `c` ".
: UNCLOSED-HELP ( str str -- ) {: t:str c:str :}
   t REJECTION:AT  s" opened here" REJECTION:EMIT-STR
   REJECTION:HELP  s" close it with " REJECTION:EMIT-STR
   c TICKED  32 REJECTION:EMIT1 ;

public

\ The record of the declaration t tu opened, which c cu closes, still open at the
\ token o ou, which may not come inside it, or, with ou 0, when its source ends;
\ not stopped.
: UNCLOSED ( str str str -- ) {: t:str c:str o:str :}
   t UNCLOSED-HEAD
   o STR:LEN 0= if
      s" when the source ends" REJECTION:EMIT-STR
   else
      s" at " REJECTION:EMIT-STR  o TICKED
   then
   t c UNCLOSED-HELP
   o STR:LEN 0= if
      s" before the source ends" REJECTION:EMIT-STR
   else
      s" before " REJECTION:EMIT-STR  o TICKED
   then ;

\ The record of the declaration t tu opened, which c cu closes, still open when
\ the load word w wu loads a file; not stopped.
: UNCLOSED-LOAD ( str str str -- ) {: t:str c:str w:str :}
   t UNCLOSED-HEAD
   s" when " REJECTION:EMIT-STR  w TICKED  s"  loads a file" REJECTION:EMIT-STR
   t c UNCLOSED-HELP
   s" before " REJECTION:EMIT-STR  w TICKED ;

private

: CLOSED ( -- )
   0 CLOSER-U ! ;

\ A declaration opening while another is open rejects the open one and
\ abandons both.
: NESTED-REJECT ( str [ -- ] -- ) {: t:str q :}
   CLOSER-U @ 0 <> if
      OPENER-A @ OPENER-U @ >str CLOSER-A @ CLOSER-U @ >str t UNCLOSED
      ABANDON-XT @ execute  q execute  CLOSED
      EXIT-STATUS:REJECTED REJECTION:STOP
   then ;

\ Registers the declaration t tu opens, which c cu closes and q abandons.
: OPENED ( str str [ -- ] -- ) {: t:str c:str q :}
   t q NESTED-REJECT
   t STR:DATA OPENER-A !  t STR:LEN OPENER-U !
   c STR:DATA CLOSER-A !  c STR:LEN CLOSER-U !  q ABANDON-XT !
   1 DECL-SERIAL +! ;

public

\ The open declaration's opener token; empty when none is open.
: OPENER$ ( -- str )
   CLOSER-U @ 0= if NULL-PTR 0 >str exit then
   OPENER-A @ OPENER-U @ >str ;

private

\ The mark a text takes as it starts, for TEXT-END and TEXT-UNWOUND.
: TEXT-MARK ( -- n )
   DECL-SERIAL @ ;

\ A text ending with a declaration it opened still open rejects it.
: TEXT-END ( n -- )
   OPEN-SINCE? 0= if exit then
   OPENER-A @ OPENER-U @ >str CLOSER-A @ CLOSER-U @ >str NULL-PTR 0 >str UNCLOSED
   ABANDON-XT @ execute  CLOSED
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Load word w wu begins a load: a declaration still open is rejected at its
\ opener, so none spans a load.
: LOAD-BEGINS ( str -- ) {: w:str :}
   CLOSER-U @ 0= if exit then
   OPENER-A @ OPENER-U @ >str CLOSER-A @ CLOSER-U @ >str
   w STR:DATA w STR:LEN SRCLOC:SPELLED drop >str
   UNCLOSED-LOAD
   ABANDON-XT @ execute  CLOSED
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ A text a throw left abandons the declaration it opened, with no record.
: TEXT-UNWOUND ( n -- )
   OPEN-SINCE? 0= if exit then
   ABANDON-XT @ execute  CLOSED ;

CAST: CLOSE-ACTION ( n -- [ -- ] )

\ A file that ends with no definition open ends the transient schema nodes its
\ definers built definitions from; a build ends them at the file's last piece.
: TRANSIENT-END ( -- )
   data-base BOOT:IMAGE-BASE-CELL + @ 0 <> if exit then
   data-base PEND-CELL + @ 0 <> if exit then
   END-TYPES ;

\ The residue check runs inside the unit, so a runner's call returns only for
\ an accepted file; each compiled part starts in host.
: CALL-UNIT ( -- )
   TOP@ {: frame:^ :}
   frame FR-PATH + frame FR-PATHLEN + CELL-VIEW @ >str
   frame FR-ROOT + frame FR-ROOTLEN + CELL-VIEW @ >str
   EXEC-SOURCE frame FR-SIZE + CELL-VIEW @ >str
   [: 0 data-base SECTION-CELL + !  RUN-RANGE RESIDUE-CHECK
      TRANSIENT-END ;]
   SOURCE-UNIT:LOAD ;

\ A loaded file's DATA starts and ends on a cell, so its bytes are a function
\ of its own source and a stored object installs whatever earlier files
\ allotted. A throw skips the end align, which does not matter: a load that
\ threw rejects the parent's object, so no stored segment continues the
\ parent's data from there.
: LOAD-UNIT ( -- )
   LOAD-BYTES
   align
   [: CALL-UNIT ;] source-file-run
   align ;

: LOAD-CURRENT ( n -- )
   {: id:n :}
   data-base SECTION-CELL + @ {: sec:n :}
   id PUSH
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
: READ-FOR ( [ str str -- ] -- [ str str -- ] )
   {: q :}
   READ-SOURCE TOP-PATH$ q execute
   q ;

: COPY-FOR ( str [ str str -- ] -- str [ str str -- ] )
   {: s:str q :}
   s HOLD TOP-PATH$ q execute
   s q ;

\ The frame is read by reader id (REQUIRES).
: WITH-BYTES-CURRENT ( n [ str str -- ] -- n [ str str -- ] ) {: id:n q :}
   id PUSH
   q
   [: READ-FOR ;] catch {: rc:n :}
   drop
   INCLUDE-CLOSE
   POP
   rc 0= 0= if rc throw then
   id q ;

: WITH-SUPPLIED-CURRENT ( str n [ str str -- ] -- str n [ str str -- ] )
   {: s:str id:n q :}
   id PUSH s q
   [: COPY-FOR ;] catch {: rc:n :}
   drop drop
   INCLUDE-CLOSE
   POP
   rc 0= 0= if rc throw then
   s id q ;

: LOAD-RAW ( str str n -- ) {: w:str s:str id:n :}
   s CONFINE
   INCLUDE-PATH0 drop
   w RESOLVED-ROOT$ ROOT-ENTER {: old:str base:n :}
   [: id LOAD-CURRENT ;] catch {: rc:n :}
   old base ROOT-LEAVE
   rc 0= 0= if rc throw then ;

public

\ No WITH scope opens, so the frame's root is CURRENT$, at the top level the
\ program's directory.
: RUN-PROGRAM ( str -- ) {: s:str :}
   s" SOURCE-ROOT:RUN-PROGRAM" s BUILD-REJECT
   PROGRAM-ID @ {: was:n :}
   NULL-PTR 0 >str s [: drop CANONICAL ;] LOADER:RESOLVE {: path:str exists:bool :}
   path s NULL-PTR 0 >str CONFINE-AS INCLUDE-PATH0 drop
   exists 0= if s NULL-PTR 0 >str INCLUDE-TRUE MISSING-SOURCE then
   path CURRENT$ FILE-ID {: id:n :}
   path PROGRAM? if id PROGRAM-ID ! then
   id PUSH
   REQUEST-AT$ {: ra:str :}  NULL-PTR 0 >str REQUEST-AT!
   [: LOAD-UNIT ;] catch {: rc:n :}
   ra REQUEST-AT!
   INCLUDE-CLOSE
   POP
   was 0 <> if was PROGRAM-ID ! then  \ an outermost run's id stays, for an exit hook
   rc 0= 0= if rc throw then ;

private

\ Loads a u, read by reader id, for w, the word the program called; an open of a
\ u that fails stands at the request token t tu, or nowhere when it is empty.
: LOAD-RUN ( str str str n -- )
   {: t:str w:str s:str id:n :}
   0 LAST-LOAD-EVAL-OK !
   REQUEST-AT$ {: ra:str :}  t REQUEST-AT!
   [: w s id LOAD-RAW ;] catch {: rc:n :}
   ra REQUEST-AT!
   rc 0 <> if rc throw then
   0 LAST-LOAD-EVAL-OK ! ;

public

\ Whether the load LOAD-RUN last threw out of had completed its source range,
\ which leaves its definitions live; false after one that returned. A require
\ keeps its row in that case.
: COMMITTED? ( -- bool )
   LAST-LOAD-EVAL-OK @ 0 <> ;

\ PATH has already been resolved by the loader's resolver, under ROOT. This
\ opens a source frame read by reader id and makes ROOT current until the
\ callback returns.
: WITH-BYTES ( str str n [ str str -- ] -- )
   {: root:str path:str id:n q :}
   s" SOURCE-ROOT:WITH-BYTES" path GIVEN-CHECK
   s" SOURCE-ROOT:WITH-BYTES" path BUILD-REJECT
   path CONFINE INCLUDE-PATH0 drop
   s" SOURCE-ROOT:WITH-BYTES" root ROOT-ENTER {: old:str base:n :}
   REQUEST-AT$ {: ra:str :}  NULL-PTR 0 >str REQUEST-AT!
   TOKEN-CLEAR {: t:str :}
   id q [: WITH-BYTES-CURRENT ;] catch {: rc:n :}
   2drop
   t STR:DATA t STR:LEN SRCLOC:TOKEN!
   ra REQUEST-AT!
   old base ROOT-LEAVE
   rc 0= 0= if rc throw then ;

\ WITH-BYTES from bytes the caller owns, read by reader id, in place of a disk
\ copy.
: WITH-SUPPLIED ( str str str n [ str str -- ] -- )
   {: root:str path:str src:str id:n q :}
   s" SOURCE-ROOT:WITH-SUPPLIED" path GIVEN-CHECK
   s" SOURCE-ROOT:WITH-SUPPLIED" path BUILD-REJECT
   path CONFINE INCLUDE-PATH0 drop
   s" SOURCE-ROOT:WITH-SUPPLIED" root ROOT-ENTER {: old:str base:n :}
   REQUEST-AT$ {: ra:str :}  NULL-PTR 0 >str REQUEST-AT!
   TOKEN-CLEAR {: t:str :}
   src id q [: WITH-SUPPLIED-CURRENT ;] catch {: rc:n :}
   2drop drop
   t STR:DATA t STR:LEN SRCLOC:TOKEN!
   ra REQUEST-AT!
   old base ROOT-LEAVE
   rc 0= 0= if rc throw then ;

;package

package SOURCE-INPUT
public

: RESET ( -- )
   [: SOURCE-ROOT:CANON-OS ;] CANON-XT !
   [: SOURCE-ROOT:READ-OS ;] READ-XT ! ;

;package

package SOURCE-ROOT
EXPORT SOURCE-EVENT:DISCOVERY?
EXPORT SOURCE-EVENT:EVENT-RECORD
EXPORT REQUIRE-REG:REQUIRE-STATE
private

\ A discovery pass reads source and opens no file, so it records a missing
\ file's request like any other; the reader that opens the file rejects it.
: LOADER-FIND ( str str -- str bool )
   DISCOVERY? if RESOLVE-AT exit then
   LOAD-RESOLVE ;

public

\ The file id of the program file, or of the registry row or the frozen row, a
\ require of a u selected as path; 0 for none of them.
: REQUIRED-ID ( str str -- n ) {: s:str path:str :}
   path PROGRAM? if PROGRAM-ID @ exit then
   path REQUIRE-REG:ROW-OF {: row:n :}
   row 0 >= if row REQUIRE-REG:ID exit then
   s REQUEST!
   path ENGINE$ REQUEST$ REQUIRE-BASE @ BOOT-MATCH nip {: boot:n :}
   boot 0 < if 0 exit then
   boot REQUIRE-REG:ID ;

\ The current text requires path, which a require of a u selected and the
\ registry holds.
: REQUIRE-HELD ( str str -- )
   {: s:str path:str :}
   s  s path REQUIRED-ID  REQUIRES:EDGE ;

\ Row, which a require of a u just stored, gets a new file id, its requires
\ resolved under root, and the current text requires it; answers the id.
: REQUIRE-STORED ( str n str -- n )
   REQUIRE-REG:MINT {: s:str id:n :}
   s id REQUIRES:EDGE
   id ;

private

\ The kind of a require the current text makes. A discovery reports what a file
\ asks for, not who runs it.
: REQUIRED-KIND ( -- n )
   DISCOVERY? 0=  REQUIRES:CURRENT@ 0 <  and if SOURCE-EVENT:KIND-TEXT-REQUIRED exit then
   SOURCE-EVENT:KIND-REQUIRED ;

\ A known path is skipped; either way the current text requires its file. w is
\ the word the program called; the resolver left RESOLVED-ROOT$.
: REQUIRE-BODY ( str str str bool -- )
   {: w:str s:str path:str known:bool :}
   s path REQUIRED-KIND known REQUIRE-STATE EVENT-RECORD
   known if s path REQUIRE-HELD exit then
   w path BUILD-REJECT
   REQUIRE-REG:MARK {: row:n :}
   path REQUIRE-REG:STORE {: row-path:str :}
   DISCOVERY? if exit then
   s row RESOLVED-ROOT$ REQUIRE-STORED {: id:n :}
   s SRCLOC:ORIGIN$ {: t:str :}
   [: t w row-path id LOAD-RUN ;] catch {: rc:n :}
   rc 0 <> if
      COMMITTED? 0= if row REQUIRE-REG:REMOVE then
      rc throw
   then ;

public

\ `included` of a u for loader word w, the word the program called. Its text is
\ its includer's, even when a program provided the file.
: INCLUDED-AS ( str str -- )
   {: w:str s:str :}
   w s [: LOADER-FIND ;] LOADER:RESOLVE drop {: path:str :}
   w path BUILD-REJECT
   s path SOURCE-EVENT:KIND-INCLUDED SOURCE-EVENT:STATE-FRESH EVENT-RECORD
   DISCOVERY? if exit then
   s SRCLOC:ORIGIN$
   w path REQUIRES:CURRENT@ LOAD-RUN ;

\ `required` of a u for loader word w, the word the program called.
: REQUIRED-AS ( str str -- )
   {: w:str s:str :}
   w s  w s [: LOADER-FIND ;] LOADER:RESOLVE
   REQUIRE-BODY ;

: ENTRY-REQUIRED ( str -- )
   {: s:str :}
   s" script-required" LOAD-BEGINS
   s" script-required" s
   NULL-PTR 0 >str s [: drop ENTRY-RESOLVE ;] LOADER:RESOLVE  REQUIRE-BODY ;

;package

package LOADER
EXPORT SOURCE-EVENT:EVENT-RECORD
EXPORT REQUIRE-REG:REQUIRE-STATE
EXPORT SOURCE-ROOT:LOAD-BEGINS

global

: included ( str -- )
   {: s:str :}
   s" included" LOAD-BEGINS
   s" included" s SOURCE-ROOT:INCLUDED-AS ;

: required ( str -- )
   {: s:str :}
   s" required" LOAD-BEGINS
   s" required" s SOURCE-ROOT:REQUIRED-AS ;

\ The `--load` argv row.
: script-required ( str -- ) SOURCE-ROOT:ENTRY-REQUIRED ;

: provided ( str -- )
   {: s:str :}
   s" provided" s [: SOURCE-ROOT:RESOLVE-AT ;] RESOLVE {: path:str known:bool :}
   s" provided" path SOURCE-ROOT:BUILD-REJECT
   s path SOURCE-EVENT:KIND-PROVIDED known REQUIRE-STATE EVENT-RECORD
   known if exit then
   path REQUIRE-REG:STORE drop ;

private

create SOURCE-ORIGINAL-INCLUDED-ENTRY 0 ,
' included SOURCE-ORIGINAL-INCLUDED-ENTRY xt!

create SOURCE-ORIGINAL-REQUIRED-ENTRY 0 ,
' required SOURCE-ORIGINAL-REQUIRED-ENTRY xt!

;package

\ Defined in CHECK's global section, as operand-rows.f defines `parses:`, so
\ INTRINSIC and the loader ids, which are CHECK's, tag them.
package CHECK
EXPORT LOADER:INCLUDE-CHECK-PATH
EXPORT SOURCE-ROOT:LOAD-BEGINS
global
: include ( -- )
   s" include" LOAD-BEGINS
   s" include" dup STR:DATA swap STR:LEN SRCLOC:SPELLED drop >str {: w:str :}
   parse-name {: s:str :}
   s STR:LEN INCLUDE-CHECK-PATH
   w s SOURCE-ROOT:INCLUDED-AS ;
immediate
INTRINSIC-INCLUDE INTRINSIC

create SOURCE-ORIGINAL-INCLUDE-ENTRY 0 ,
' include SOURCE-ORIGINAL-INCLUDE-ENTRY xt!

: require ( -- )
   s" require" LOAD-BEGINS
   s" require" dup STR:DATA swap STR:LEN SRCLOC:SPELLED drop >str {: w:str :}
   parse-name {: s:str :}
   s STR:LEN INCLUDE-CHECK-PATH
   w s SOURCE-ROOT:REQUIRED-AS ;
immediate
INTRINSIC-REQUIRE INTRINSIC

create SOURCE-ORIGINAL-REQUIRE-ENTRY 0 ,
' require SOURCE-ORIGINAL-REQUIRE-ENTRY xt!
;package

package SOURCE-UNIT
EXPORT LOADER:SOURCE-ORIGINAL-INCLUDED-ENTRY
EXPORT LOADER:SOURCE-ORIGINAL-REQUIRED-ENTRY
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

: LOAD-PATH ( n str -- ) {: mode:n path:str :}
   SOURCE-ROOT:ACTIVE-SOURCE drop drop drop
   mode LOAD-INCLUDE = if path included exit then
   mode LOAD-REQUIRE = if path required exit then
   EXIT-STATUS:REJECTED throw ;
;package

package SOURCE-ROOT
private

: POLICY-SEALED ( -- )
   s" E-POLICY" REJECTION:OPEN
   s" the source policy is already sealed" REJECTION:EMIT-STR
   SRCLOC:TOKEN$ >str REJECTION:AT  s" sealed" REJECTION:EMIT-STR
   REJECTION:HELP
   s" call `POLICY-ROOT` before the policy seals the dictionary" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: POLICY-ROOT-SET ( [ -- renders parses nominal roots ] -- )
   data-base POLICY-REQUIRE-CELL + xt!
   CANON-BUF POLICY-ROOT-BUF CANON-U @ BYTE-COPY
   CANON-U @ POLICY-ROOT-U ! ;

: POLICY-ROOT-RELEASE ( ^u8 n -- )
   1+ munmap 0 < if s" policy: cannot release a root copy" EXIT-STATUS:IOERR die then ;

public

\ Configure confinement before checking or sealing. The policy library passes
\ the ordinary require xt it captured when loaded.
: POLICY-ROOT ( str [ -- renders parses nominal roots ] -- )
   {: path:str require-xt :}
   data-base POLICY-NDICT-CELL + @ 0 <> if POLICY-SEALED then
   s" SOURCE-ROOT:POLICY-ROOT" path ROOT-CANON
   require-xt POLICY-ROOT-SET ;

\ Scope a root and require owner for reusable source checks. A callback that
\ seals leaves the new policy root and rejects the attempted return to the old.
: POLICY-WITH-ROOT ( str [ -- ] [ -- renders parses nominal roots ]
   -- )
   {: path:str q require-xt :}
   data-base POLICY-NDICT-CELL + @ 0 <> if POLICY-SEALED then
   data-base POLICY-REQUIRE-CELL + @ 0= 0= {: had:bool :}
   POLICY-ROOT-U @ {: old-u:n :}
   POLICY-ROOT-BUF old-u >str MAP-COPY {: old:^ :}
   s" SOURCE-ROOT:POLICY-WITH-ROOT" {: w:str :}
   [: w path ROOT-ENTER ;] catch
   {: p pbase rc:n :}
   rc 0= if p pbase else old old-u POLICY-ROOT-RELEASE rc throw then
   {: prior:str prior-base:n :}
   require-xt POLICY-ROOT-SET
   TOKEN-CLEAR {: t:str :}
   q catch {: rc:n :}
   t STR:DATA t STR:LEN SRCLOC:TOKEN!
   data-base POLICY-NDICT-CELL + @ 0 <> if
      prior prior-base ROOT-LEAVE
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
   prior prior-base ROOT-LEAVE
   rc 0= 0= if rc throw then ;

;package

\ A restricted discovery pass sees a fresh require/provided registry, so a tool's
\ preloaded paths cannot dedup-hide a later user require. One snapshot is in
\ effect at a time, since one pair is saved: a second would lose the first.
package REQUIRE-REG
public

: SNAPSHOT ( -- )
   REQUIRE-N @ REQUIRE-SAVE-N !
   REQUIRE-BASE @ REQUIRE-SAVE-BASE !
   REQUIRE-N @ REQUIRE-BASE ! ;

: RESTORE ( -- )
   REQUIRE-SAVE-BASE @ REQUIRE-BASE !
   REQUIRE-SAVE-N @ REWIND
   -1 REQUIRE-SAVE-N ! ;

: COUNT ( -- n )
   REQUIRE-N @ ;

\ The first row the registry counts as known; SNAPSHOT sets it to the count.
: BASE ( -- n )
   REQUIRE-BASE @ ;

\ What the ENGINE provides, as opposed to what this process loaded: the boot
\ prefix marks its own files `provided` before any user token runs, and freezing
\ the count at its end separates the two, which REQUIRE-KNOWN? cannot once a
\ tool's own dependencies are in the registry.
: BOOT-FREEZE ( -- )
   REQUIRE-N @ REQUIRE-BOOT-N !
   INCLUDE-FALSE REQUIRE-BOOT-V ! ;

: ENGINE-PROVIDES? ( str -- bool )
   SOURCE-ROOT:ENGINE-KNOWN? ;

;package

package LOADER
global

\ A file that builds for some targets only states them on its first code line,
\ before its requires; on another target its load stops there.
: ?TARGET ( bool -- )
   if exit then
   s" E-TARGET" REJECTION:OPEN
   s" this file does not build for " REJECTION:EMIT-STR  HB-TARGET$ REJECTION:EMIT-STR
   s" ?TARGET" TOKEN-SPELLS? if
      SRCLOC:TOKEN$ >str REJECTION:AT  s" the target test fails here" REJECTION:EMIT-STR
   then
   REJECTION:HELP
   s" build this file only for a target its test admits, or widen the test"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

;package
