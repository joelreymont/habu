\ build-fixpoint.f - checked self-rebuild orchestration: emits the stage sources, certifies
\ them, builds the capture host and the product, and keys the refresh stamp.
\ The caller composes it as one --load line (tools/bootstrap.sh) after the library preamble
\ BF-NEED-PREAMBLE names, so this file requires none of that preamble.

require src/habu/verify-source.f
require tools/build-target.f              \ the target the emitted sources are for
require tools/build-certify.f            \ the certify report and census lines

\ Everything below is private to BUILD-FIXPOINT; the export block at the end names the
\ surface other files call. COMPILER-BUILD closes and reopens BUILD-FIXPOINT, since
\ packages do not nest, and imports what it needs with `using BUILD-FIXPOINT`.
package BUILD-FIXPOINT

32768 constant BF-CMP-CAP
$10000 constant BF-CERT-DIAG-CAP
4 constant BF-MAX-GENS
10 constant BF-LF
64 constant BF-USAGE-RC
74 constant BF-BUILD-RC
34 constant BF-DQ
64 constant BF-STAMP-HEX-U
12 constant BF-STAMP-PREFIX-U
32 constant BF-STAMP-DG-U
256 constant BF-STAMP-CAP
128 constant BF-PIN-CAP

\ The preamble is --load-ed by the caller, never required here, so an old native seed can
\ supply the minimal set its primitives support (src/lib/process-env.f). Without it this
\ load would die at a bare `E-UNDEFINED: PATH-CAP`; the guard names the load list instead.
\ PATH-CAP alone proves nothing, since source-lex.f loads fs.f: codesign.f requires every
\ other module but process-env.f, so one public word from each branch is checked.
: BF-NEED-PREAMBLE ( -- )
   s" CODESIGN:TOOL" CHECKER-RESOLVES?
   s" PROC-ENV-INHERIT-MISSING" CHECKER-RESOLVES? and if exit then
   s" build-fixpoint: missing required load; --load src/lib/errors.f src/lib/string.f src/lib/memory.f src/lib/fs.f src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f src/lib/process-env.f src/lib/codesign.f tools/build-fixpoint.f tools/build-fixpoint-main.f before the build verb" BF-USAGE-RC die ;
BF-NEED-PREAMBLE

\ The stage source rewinds to the mark src/core/lower-cert-seal.f records at the core
\ prefix's end and carries no copy of that prefix, so a host engine without the mark cannot
\ build this tree: refuse before a byte is emitted, naming the file and the repair.
\ CHECKER-RESOLVES? is the only boot-prefix existence query that answers for a
\ package-qualified name: `search-wl` answers 0 for PKG:TAIL whatever the package holds
\ (measured against STR:LENGTH).
: BF-WATERMARK? ( -- bool )
   s" PREFIX-MARK:CURSORS" CHECKER-RESOLVES? ;

\ A name resolves whether or not anything wrote through it, so the mark's value is read
\ too: src/core/lower-cert-seal.f assigns it once, from CHECKER-BOUND:CURSORS, and the mark
\ is top-level in a prefix file every booted host reached, so zero means the boundary set
\ never landed in this engine. The value is read through an evaluated text after the
\ resolvability check, since a host without the word cannot compile a body naming it.
\ The recorded width is not compared with the seam's live one: CHECKER-BOUND sits behind
\ the lower-cert seal (`s" CHECKER-BOUND:CURSORS" CHECKER-RESOLVES?` answers 0 on a booted
\ engine, -1 for PREFIX-MARK:CURSORS), so no consumer can hold both numbers.
variable BF-MARK-N

public

\ BF-MARK-CURSORS's text hands the value to this hook. The text runs in its
\ caller's scope, which is outside this package when BF-PREFLIGHT runs, so it
\ names the hook qualified. No other file calls it.
: BF-MARK! ( n -- )
   BF-MARK-N ! ;

private

: BF-MARK-CURSORS ( -- n )
   s" PREFIX-MARK:CURSORS BUILD-FIXPOINT:BF-MARK!" evaluate-closed
   BF-MARK-N @ ;

: BF-REQUIRE-WATERMARK ( -- )
   BF-WATERMARK? 0= if
      s" build-fixpoint: host engine has no core-prefix watermark (src/core/lower-cert-seal.f); reseed bin/hb from a current engine and refresh" BF-BUILD-RC die
   then
   BF-MARK-CURSORS 0= if
      s" build-fixpoint: host engine's core-prefix mark carries no checker boundary (src/core/lower-cert-seal.f never recorded CHECKER-BOUND:CURSORS); reseed bin/hb from a current engine and refresh" BF-BUILD-RC die
   then ;

BF-REQUIRE-WATERMARK

;package

\ The package closes for these requires, and not for the guard above: the
\ files open packages of their own, and `package` inside an open package is
\ rejected as nesting. They also come after the guard: process-cwd.f requires
\ process-env.f, which must be loaded by the caller for the guard to pass.
require src/lib/process-cwd.f
require src/lib/engine-id.f                  \ PATH$: the engine a certify child runs
require src/lib/span.f                       \ SPAN-BUFFER: destinations for FS:SUFFIX-PATH
require tools/build-support.f            \ shared path, child env and source scratch
require src/lib/content-key.f                \ the chain fold
require tools/event-closure-lib.f        \ the chain fold
require src/lib/tree-copy.f                  \ the candidate's copied boot tree
require tools/build-out.f                \ BASE$: the out root the refresh stamp lives under

package BUILD-FIXPOINT

create BF-LF-BUF 1 allot
create BF-CAP-SINK 1 allot
create BF-PIN-KEYS BF-PIN-CAP BF-STAMP-DG-U * allot
create BF-PIN-DIGS BF-PIN-CAP BF-STAMP-DG-U * allot
create BF-PIN-KEYBUF 40 allot
create BF-PIN-DIGBUF 40 allot
create BF-STAMP-KEY 80 allot
create BF-STAMP-OLD 80 allot
create BF-STAMP-DG 40 allot
create BF-REC-PREFIX-DG 40 allot
create BF-REC-STAGE-DG 40 allot
create BF-REC-STDIN-DG 40 allot
create BF-CERT-DIAG BF-CERT-DIAG-CAP allot
create BF-STAMP-BUF BF-STAMP-CAP allot
create BF-SHA-CTX SHA256-CTX-BYTES allot
create BF-FSHA-CTX SHA256-FILE-CTX-BYTES allot
create BF-STAMP-PATH-BUF PATH-CAP allot
create BF-STAMP-DIR-BUF PATH-CAP allot
create BF-STAMP-DEF-BUF PATH-CAP allot
PATH-CAP SPAN-BUFFER: BF-HOST-DST-BUF       \ a FS:SUFFIX-PATH destination
BF-LF BF-LF-BUF c!

variable BF-CMP-A-BUF-A
variable BF-CMP-B-BUF-A
variable BF-FDA
variable BF-FDB
variable BF-APP-IN
variable BF-APP-OUT
variable BF-RA
variable BF-RB
variable BF-APP-RD
variable BF-APP-WR
variable BF-APP-OFF
variable BF-GEN
variable BF-FOUND
variable BF-PID
variable BF-IN          \ the child's stdin BF-SPAWN-IN closes
variable BF-STAMP-PATH-U
variable BF-STAMP-DIR-U
variable BF-STAMP-DEF-U
variable BF-STAMP-U
variable BF-REC-PREFIX?
variable BF-REC-STAGE?
variable BF-REC-STDIN?
variable BF-HOST-DST-U
variable BF-FORCE
variable BF-PIN-N
variable BF-PIN-ON
variable BF-CERT-RC
variable BF-CERT-DIAG-U
TYPED-VARIABLE BF-CERT-LAB-A ptr u8
variable BF-CERT-LAB-U
TYPED-VARIABLE BF-CERT-PATH-A ptr u8
variable BF-CERT-PATH-U

: BF-CERT-LAB-A@ ( -- ptr u8 )
   BF-CERT-LAB-A @ ;

: BF-CERT-LAB-A! ( ptr u8 -- )
   BF-CERT-LAB-A ! ;

: BF-CERT-PATH-A@ ( -- ptr u8 )
   BF-CERT-PATH-A @ ;

: BF-CERT-PATH-A! ( ptr u8 -- )
   BF-CERT-PATH-A ! ;

: BF-CMP-A ( -- ptr u8 )
   BF-CMP-A-BUF-A BF-CMP-CAP BF-BUF ;

: BF-CMP-B ( -- ptr u8 )
   BF-CMP-B-BUF-A BF-CMP-CAP BF-BUF ;

: BF-TRUE ( -- bool )
   0 0= ;

: BF-FALSE ( -- bool )
   0 0= 0= ;

\ A child's completion code. PROC-TIMEOUT-RC is a deadline that expired in the
\ child (native-build's smoke check, src/lib/process.f), so it throws E-PROC-TIMEOUT
\ again here instead of becoming one more failed child.
: BF-RC0 ( n -- ) {: rc:n :}
   rc 0= if exit then
   rc PROC-TIMEOUT-RC = if E-PROC-TIMEOUT throw then
   E-BUILD-STATUS throw ;

: BF-RENAME-TMP ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu BF-A$ dst dstu BF-B$ FS:RENAME-FILE ;

: BF-CHMOD-X-TMP ( ptr u8 n -- )
   BF-A$ FS:CHMOD-X ;

: BF-OPEN-INPUT ( ptr u8 n -- n )
   FS:>PATHZ open-rd dup 0 < if E-BUILD-PATH throw then ;

: BF-FINISH-PID ( pid -- n )
   PROC-WAIT-RC MATCH result ok OF ENDOF err OF ENDOF ;MATCH ;   \ completion code (ok 0 / err nonzero)

\ The request for pathz with the staged argv and only the staged env, which are
\ reset once the request holds its copy; the child inherits stdin, stdout and
\ stderr.
: BF-REQUEST ( ptr u8 ptr ptr u8 ptr ptr u8 -- PROCESS:request )
   {: pathz:ptr argv:ptr envp:ptr :}
   pathz dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   argv PROCESS:ARGV-ROWS
   envp PROCESS:ENV-ROWS
   PROC-ARGV-ENV-RESET ;

\ Spawns pathz with the staged argv and env. A refusal throws E-PROC-SPAWN.
: BF-SPAWN ( ptr u8 ptr ptr u8 ptr ptr u8 -- pid )
   BF-REQUEST PROCESS:SPAWN ;

: BF-CLOSE-IN ( -- )
   BF-IN @ close ;

\ BF-SPAWN with the child's stdin on infd, which is closed once the spawn
\ answers or throws.
: BF-SPAWN-IN ( ptr u8 ptr ptr u8 ptr ptr u8 n -- pid )
   BF-IN !
   [: BF-REQUEST 0 >FD BF-IN @ >FD PROCESS:FD PROCESS:SPAWN ;] [: BF-CLOSE-IN ;] finally ;

: BF-RUN-ENV-EXE ( ptr u8 n -- n ) {: exe:ptr exeu :}
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   exe exeu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE
   BF-SPAWN BF-FINISH-PID ;

: BF-RUN-ENV-INFD ( ptr u8 n n -- n ) {: exe:ptr exeu infd :}
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   exe exeu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE infd
   BF-SPAWN-IN BF-FINISH-PID ;

\ Spawns pathz with the staged argv and env as the capture child, its stdout on
\ outfd and its stderr on the capture pipe, and records its pid. Stack-preserving
\ under catch, so a refused spawn reaches PROC-THROW-CAPTURE.
: BF-OUTFD-LAUNCH ( ptr u8 ptr ptr u8 ptr ptr u8 n -- ptr u8 ptr ptr u8 ptr ptr u8 n )
   {: pathz:ptr argv:ptr envp:ptr outfd:n :}
   pathz argv envp BF-REQUEST
   1 >FD outfd >FD PROCESS:FD
   2 >FD PROC-ERR-W @ >FD PROCESS:FD
   PROCESS:SPAWN PID>N PROC-PID !
   pathz argv envp outfd ;

\ PROC-SPAWN-ARGV-ENV-CAPTURE with the child's stdout on a caller fd instead of
\ the capture pipe. The out pipe the capture machine opened is never handed to
\ the child and is closed here, so it reads EOF at once and the drain below ends
\ on stderr alone.
: BF-SPAWN-OUTFD-CAPTURE ( ptr u8 ptr ptr u8 ptr ptr u8 n -- )
   [: BF-OUTFD-LAUNCH ;] catch {: code:n :}
   drop drop drop drop
   code 0<> if code PROC-THROW-CAPTURE then
   PROC-PID @ >PID PROC-CAPTURE-PID!
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

\ Spawn the argv and env the caller built, stdout on a file and stderr captured under the
\ deadline every child here gets. A file, unlike a capture buffer, does not bound a child's
\ output before it runs, and FS:FILE-SIZE reads its size afterwards: the only way to read a
\ source transform's output back. The answer is an outcome, so a deadline expiry is an arm.
: BF-RUN-ARGV-ENV-OUTFILE ( ptr u8 n ptr u8 n ptr u8 len ms -- len outcome )
   {: exe:ptr exeu:n out:ptr outu:n err:ptr errcap:len timeout:ms :}
   out outu BF-LF-BUF 0 FS:WRITE-ALL
   out outu FS:OPEN-APPEND-FD {: outfd:n :}
   exe exeu >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv envp outfd BF-SPAWN-OUTFD-CAPTURE
   outfd close
   BF-CAP-SINK 1 >LEN err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME             \ ( len len outcome )
   rot drop ;                              \ the sunk stdout length is nobody's business

: BF-PREPARE-STAGE-ARGV ( ptr u8 n -- ptr u8 ptr ptr u8 )
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   BF-TMP$ >LEN PROC-ARGV+
   >LEN PROC-ARGV-PREPARE ;

: BF-RUN-STAGE-ENV-INFD ( ptr u8 n n -- n ) {: exe:ptr exeu infd :}
   BF-PREPARE-ENV
   exe exeu BF-PREPARE-STAGE-ARGV
   PROC-ENV-PREPARE infd
   BF-SPAWN-IN BF-FINISH-PID ;

: BF-RUN-STAGE-ENV-EXE ( ptr u8 n -- n )
   BF-PREPARE-ENV
   BF-PREPARE-STAGE-ARGV
   PROC-ENV-PREPARE
   BF-SPAWN BF-FINISH-PID ;

: BF-RUN-ENV-PATH-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu src srcu BF-OPEN-INPUT BF-RUN-ENV-INFD ;

: BF-RUN-STAGE-PATH-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu src srcu BF-OPEN-INPUT BF-RUN-STAGE-ENV-INFD ;

: BF-RUN-ENV-TMP ( ptr u8 n -- n )
   BF-A$ BF-RUN-ENV-EXE ;

: BF-RUN-STAGE-TMP ( ptr u8 n -- n )
   BF-A$ BF-RUN-STAGE-ENV-EXE ;

: BF-CODESIGN-VERIFY-TMP ( ptr u8 n -- ) {: a:ptr u :}
   a u BF-A$ CODESIGN:VERIFY ;

: BF-RESET-OUT ( ptr u8 n -- )
   BF-OUT$ BF-SOURCE-BUF 0 FS:WRITE-ALL ;

: BF-APPEND-BYTES ( ptr u8 n ptr u8 n -- ) {: out:ptr outu a:ptr u :}
   out outu BF-OUT$ a u FS:APPEND-FILE ;

: BF-APPEND-LF ( ptr u8 n -- ) {: out:ptr outu :}
   out outu BF-OUT$ BF-LF-BUF 1 FS:APPEND-FILE ;

: BF-APPEND-LINE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n a:ptr u:n :}
   out outu a u BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

\ Public for the sibling packages further down, which reach these through
\ `using BUILD-FIXPOINT`; the repository's surface is the export block at the end.
public
EXPORT BF-APPEND-LINE
EXPORT BF-FINISH-PID
EXPORT BF-SPAWN
private

;package

package COMPILER-BUILD
using BUILD-FIXPOINT

: ARGV ( ptr u8 n ptr u8 n -- ptr u8 ptr ptr u8 ) {: exe:ptr exeu:n src:ptr srcu:n :}
   PROC-ARGV-RESET
   s" --build" >LEN PROC-ARGV+
   src srcu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   BF-TMP$ >LEN PROC-ARGV+
   exe exeu >LEN PROC-ARGV-PREPARE ;

public

: RUN ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu:n src:ptr srcu:n :}
   BF-PREPARE-ENV
   exe exeu src srcu ARGV
   PROC-ENV-PREPARE
   BF-SPAWN BF-FINISH-PID ;

: RUN-TMP ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu:n src:ptr srcu:n :}
   exe exeu BF-A$ src srcu BF-B$ RUN ;

: SEAL ( ptr u8 n -- )
   s" SEAL-FRIEND" BF-APPEND-LINE ;

;using
;package

package BUILD-FIXPOINT

: BF-APP-CLOSE ( ptr n -- ) {: p:ptr :}
   p @ dup 0 >= if close else drop then
   -1 p ! ;

: BF-APP-THROW ( n -- )
   BF-APP-IN BF-APP-CLOSE
   BF-APP-OUT BF-APP-CLOSE
   throw ;

: BF-APP-RESET ( -- )
   -1 BF-APP-IN !
   -1 BF-APP-OUT ! ;

: BF-APP-OPEN ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu FS:>PATHZ open-rd BF-APP-IN !
   BF-APP-IN @ 0 < if E-FS-OPEN BF-APP-THROW then
   dst dstu FS:OPEN-APPEND-FD BF-APP-OUT !
   BF-APP-OUT @ 0 < if E-FS-OPEN BF-APP-THROW then ;

: BF-APP-WRITE-CHUNK ( n -- ) {: u :}
   0 BF-APP-OFF !
   begin BF-APP-OFF @ u < while
      BF-APP-OUT @ BF-SOURCE-BUF BF-APP-OFF @ + u BF-APP-OFF @ - write BF-APP-WR !
      BF-APP-WR @ 0 <= if E-FS-IO BF-APP-THROW then
      BF-APP-WR @ u BF-APP-OFF @ - > if E-FS-IO BF-APP-THROW then
      BF-APP-OFF @ BF-APP-WR @ + BF-APP-OFF !
   repeat ;

: BF-APPEND-FILE-STREAM ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   BF-APP-RESET
   src srcu dst dstu BF-APP-OPEN
   begin
      BF-APP-IN @ BF-SOURCE-BUF BF-SOURCE-CAP read BF-APP-RD !
      BF-APP-RD @ 0 < if E-FS-IO BF-APP-THROW then
      BF-APP-RD @ BF-SOURCE-CAP > if E-FS-IO BF-APP-THROW then
      BF-APP-RD @ 0 >
   while
      BF-APP-RD @ BF-APP-WRITE-CHUNK
   repeat
   BF-APP-IN BF-APP-CLOSE
   BF-APP-OUT BF-APP-CLOSE ;

\ ---------------------------------------------------------------------------
\ Boot-prefix hash pin. Every checkout source emitted into a stage image is pinned before the
\ append or split helpers copy it: the boot-prefix files are re-read across stage2, stdin,
\ snap and the stamp-key re-emit, and an edit between reads would build stage N and N+1 from
\ different revisions. The pin keys each path by its SHA-256 (32 bytes whatever its length),
\ records its content digest on first read and throws E-BUILD-BOOT-DRIFT on a changed
\ reload, naming the drifted path.
: BF-PIN-RESET ( -- )
   0 BF-PIN-N ! ;

: BF-PIN-ON! ( -- )
   BF-TRUE BF-PIN-ON ! ;

: BF-PIN-OFF! ( -- )
   BF-FALSE BF-PIN-ON ! ;

: BF-PIN-KEY@ ( n -- ptr u8 )
   BF-STAMP-DG-U * BF-PIN-KEYS + ;

: BF-PIN-DIG@ ( n -- ptr u8 )
   BF-STAMP-DG-U * BF-PIN-DIGS + ;

: BF-PIN-COMPUTE ( ptr u8 n -- ) {: a:ptr u:n :}
   BF-SHA-CTX a u BF-PIN-KEYBUF SHA256-IN
   BF-FSHA-CTX a u BF-PIN-DIGBUF SHA256-FILE-IN dup 0 <> if throw then drop ;

: BF-PIN-MATCH? ( n -- bool ) {: row:n :}
   BF-PIN-KEYBUF BF-STAMP-DG-U row BF-PIN-KEY@ BF-STAMP-DG-U STR= ;

: BF-PIN-FIND ( -- n )
   0 begin dup BF-PIN-N @ < while
      dup BF-PIN-MATCH? if exit then
      1 +
   repeat drop -1 ;

: BF-PIN-APPEND ( -- )
   BF-PIN-N @ BF-PIN-CAP >= if E-STR-CAPACITY throw then
   BF-PIN-KEYBUF BF-PIN-N @ BF-PIN-KEY@ BF-STAMP-DG-U BYTE-COPY
   BF-PIN-DIGBUF BF-PIN-N @ BF-PIN-DIG@ BF-STAMP-DG-U BYTE-COPY
   BF-PIN-N @ 1 + BF-PIN-N ! ;

: BF-PIN-VERIFY-AT ( n -- )
   BF-PIN-DIG@ BF-STAMP-DG-U BF-PIN-DIGBUF BF-STAMP-DG-U STR= 0= if
      E-BUILD-BOOT-DRIFT throw
   then ;

: BF-PIN-FILE ( ptr u8 n -- )
   BF-PIN-ON @ 0= if 2drop exit then
   BF-PIN-COMPUTE
   BF-PIN-FIND dup 0 < if drop BF-PIN-APPEND exit then
   BF-PIN-VERIFY-AT ;

: BF-APPEND-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu src:ptr srcu :}
   src srcu BF-PIN-FILE
   src srcu out outu BF-OUT$ BF-APPEND-FILE-STREAM
   out outu BF-APPEND-LF ;

: BF-APPEND-MODULE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n src:ptr srcu:n :}
   out outu src srcu BF-APPEND-SOURCE
   out outu S\" s\" " BF-APPEND-BYTES
   out outu src srcu BF-APPEND-BYTES
   out outu S\" \" provided" BF-APPEND-LINE ;

public
EXPORT BF-APPEND-SOURCE
EXPORT BF-BUILD-RC
private

;package

package BUILD-FIXPOINT

: BF-SRC-C@ ( n -- u8 )
   BF-SOURCE-BUF + c@ ;

: BF-PREFLIGHT ( -- )
   BF-REQUIRE-WATERMARK ;

: BF-TARGET-UNKNOWN ( -- )
   s" build-fixpoint: unknown target" BF-BUILD-RC die ;

\ The stage source's target seam reads the build's target (tools/build-target.f),
\ not this engine's predicates. Nothing here points it elsewhere, so it is this
\ engine's own target: the stage compiler is this machine's engine.
: BF-APPEND-TARGET-LAYOUT ( ptr u8 n -- ) {: out:ptr outu :}
   BUILD-TARGET:LINUX? BUILD-TARGET:LINUX-X86-64? or if
      out outu s" src/os/linux/layout-constants.f" BF-APPEND-SOURCE
      out outu s" src/os/linux/layout.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:MACOS? if
      out outu s" src/os/macos/layout.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-SYS ( ptr u8 n -- ) {: out:ptr outu :}
   BUILD-TARGET:LINUX? if
      out outu s" src/os/linux/sys.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:MACOS? if
      out outu s" src/os/macos/sys.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:LINUX-X86-64? if
      out outu s" src/os/linux-x86-64/sys.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-PROC-WATCH ( ptr u8 n -- ) {: out:ptr outu:n :}
   BUILD-TARGET:LINUX? if
      out outu s" src/os/linux/proc-watch.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:MACOS? if
      out outu s" src/os/macos/proc-watch.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:LINUX-X86-64? if
      out outu s" src/os/linux-x86-64/proc-watch.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-PROC-CONTROL ( ptr u8 n -- ) {: out:ptr outu:n :}
   BUILD-TARGET:LINUX? if
      out outu s" src/os/linux/proc-control.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:MACOS? if
      out outu s" src/os/macos/proc-control.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:LINUX-X86-64? if
      out outu s" src/os/linux-x86-64/proc-control.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-FLAG ( ptr u8 n -- ) {: out:ptr outu :}
   BUILD-TARGET:LINUX? if
      out outu s" src/os/linux/target.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:MACOS? if
      out outu s" src/os/macos/target.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:LINUX-X86-64? if
      out outu s" src/os/linux-x86-64/target.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-IMAGE-BYTES ( ptr u8 n -- ) {: out:ptr outu :}
   BUILD-TARGET:LINUX-X86-64? if
      out outu s" using X64CODE" BF-APPEND-LINE
   else
      out outu s" using A64ICODE" BF-APPEND-LINE
   then
   out outu s" src/os/image-bytes.f" BF-APPEND-MODULE
   out outu s" ;using" BF-APPEND-LINE ;

: BF-APPEND-TARGET-IMAGE ( ptr u8 n -- ) {: out:ptr outu :}
   BUILD-TARGET:LINUX? if
      out outu s" src/os/linux/elf.f" BF-APPEND-SOURCE
      out outu s" src/os/linux/sign.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:MACOS? if
      out outu s" src/os/macos/macho.f" BF-APPEND-SOURCE
      out outu s" src/os/macos/sign2.f" BF-APPEND-SOURCE
      exit
   then
   BUILD-TARGET:LINUX-X86-64? if
      out outu s" src/os/linux-x86-64/elf.f" BF-APPEND-SOURCE
      out outu s" src/os/linux-x86-64/sign.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-ROLES ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/core/roles.f" BF-APPEND-SOURCE ;

: BF-APPEND-CHECKER-BOOT ( ptr u8 n -- )
   {: out:ptr outu:n :}
   out outu s" src/core/util.f" BF-APPEND-SOURCE
   out outu s" src/core/cell.f" BF-APPEND-SOURCE
   out outu s" src/core/pointer-storage.f" BF-APPEND-SOURCE
   out outu s" src/core/engine-error.f" BF-APPEND-SOURCE
   out outu s" src/core/exec-vector.f" BF-APPEND-SOURCE
   out outu s" src/core/checker-fetch-abi.f" BF-APPEND-SOURCE
   out outu s" src/core/checker-owner-extension.f" BF-APPEND-SOURCE
   out outu s" src/core/checker-owner-abi.f" BF-APPEND-SOURCE
   out outu s" src/habu/created-body-abi.f" BF-APPEND-SOURCE
   out outu s" src/habu/prims.f" BF-APPEND-SOURCE
   out outu s" src/core/does-clause.f" BF-APPEND-SOURCE
   out outu s" src/core/checker.f" BF-APPEND-SOURCE
   out outu s" src/core/engine-error-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/lower-cert-base.f" BF-APPEND-SOURCE
   out outu s" src/core/type-schema.f" BF-APPEND-SOURCE
   out outu s" src/core/type-family.f" BF-APPEND-SOURCE
   out outu s" src/core/render.f" BF-APPEND-SOURCE
   out outu s" src/core/sumtype.f" BF-APPEND-SOURCE
   out outu s" src/core/layout-buffer.f" BF-APPEND-SOURCE
   out outu s" src/core/layout-valid.f" BF-APPEND-SOURCE
   out outu s" src/core/check-hook.f" BF-APPEND-SOURCE
   out outu s" src/core/cell-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/declaration-transaction.f" BF-APPEND-SOURCE
   out outu s" src/core/generated-declaration.f" BF-APPEND-SOURCE ;

: BF-APPEND-DECL-FILES ( ptr u8 n -- ) {: out:ptr outu:n :}
   \ All after the checker hook; the STRUCTURE declarer calls the constructor
   \ generator, so it follows it.
   out outu s" src/core/decl-event.f"     BF-APPEND-SOURCE
   out outu s" src/core/structure-make.f" BF-APPEND-SOURCE
   out outu s" src/core/structure-decl.f" BF-APPEND-SOURCE
   out outu s" src/core/arena.f"          BF-APPEND-SOURCE
   out outu s" src/core/enum-decl.f"      BF-APPEND-SOURCE ;

: BF-APPEND-CORE-FILES ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/core/structures.f" BF-APPEND-SOURCE ;

: BF-APPEND-CORE-BYTES ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/core/bytes.f" BF-APPEND-SOURCE ;

: BF-APPEND-HABU-LAYOUT ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/habu/stack-abi.f" BF-APPEND-SOURCE
   out outu s" src/habu/layout.f" BF-APPEND-SOURCE ;

: BF-APPEND-ENV-BASE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/os/env-base.f" BF-APPEND-SOURCE ;

: BF-APPEND-SCRIPT-ARGV ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/os/script-argv.f" BF-APPEND-SOURCE ;

: BF-APPEND-COMBINATORS ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/core/combinators.f" BF-APPEND-SOURCE ;

\ The whole-span descriptor write, ahead of driver-io.f (its only caller in a
\ builder) and ahead of aot-decl.f, which is where the AOT artifact writer's
\ closure begins.
: BF-APPEND-FDIO ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/habu/fdio.f" BF-APPEND-MODULE ;   \ aot-file.f requires it

: BF-APPEND-INCLUDE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/core/include.f" BF-APPEND-SOURCE
   out outu s" SOURCE-INPUT:RESET" BF-APPEND-LINE ;

: BF-APPEND-ENUMS ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/core/enums.f" BF-APPEND-SOURCE ;

\ The boot prefix in the engine's own load order, the build's mirror of src/habu/habu2.f
\ PFX-LOAD-BASE-FILES: BF-APPEND-CHECKER-BOOT is PFX-LOAD-CHECKER-FILES, BF-APPEND-DECL-FILES
\ PFX-LOAD-DECL-FILES, and BF-APPEND-BOOT-CORE PFX-LOAD-CORE-FILES and the final core seal,
\ without the include registry runtime registers before that seal. Certify-only: the
\ compiling host carries these definitions and the stage source rewinds to them
\ (src/habu/prefix-rewind.f), so this static check is the only gate they have.
: BF-APPEND-BOOT-CORE ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-APPEND-CORE-FILES
   out outu BF-APPEND-ROLES
   out outu BF-APPEND-CORE-BYTES
   out outu s" src/core/dynamic-storage.f" BF-APPEND-SOURCE
   out outu BF-APPEND-TARGET-FLAG
   out outu BF-APPEND-TARGET-LAYOUT
   out outu BF-APPEND-HABU-LAYOUT
   out outu BF-APPEND-ENV-BASE
   out outu BF-APPEND-INCLUDE
   out outu BF-APPEND-ENUMS
   out outu s" src/core/sha256.f" BF-APPEND-SOURCE
   out outu s" src/core/source-producer.f" BF-APPEND-SOURCE
   out outu BF-APPEND-COMBINATORS
   out outu s" src/habu/code-span.f" BF-APPEND-SOURCE
   out outu s" src/habu/xref.f" BF-APPEND-SOURCE
   out outu s" src/core/cell-body.f" BF-APPEND-SOURCE
   out outu s" src/core/generated-declaration-dictionary.f" BF-APPEND-SOURCE
   out outu s" src/core/generated-declaration-protection.f" BF-APPEND-SOURCE
   out outu s" src/core/layout-buffer-seal.f" BF-APPEND-SOURCE
   out outu s" src/core/checker-owner-guard.f" BF-APPEND-SOURCE
   out outu s" src/core/lower-cert-seal.f" BF-APPEND-SOURCE ;

: BF-APPEND-BOOT-PREFIX ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-APPEND-CHECKER-BOOT
   out outu BF-APPEND-DECL-FILES
   out outu BF-APPEND-BOOT-CORE ;

\ The verifier reads the emitted buffer and does not follow require, so the required build
\ modules go in at their dependency boundaries, in habu1.f's require order: regalloc-abi.f,
\ address-cells.f and prof-abi.f, which data-claims.f requires, then data-claims.f. A module
\ habu1.f requires but this list omits certifies as `undefined word`.
: BF-APPEND-HABU1 ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/habu/primitive-registry.f" BF-APPEND-MODULE
   out outu s" src/habu/regalloc-abi.f" BF-APPEND-MODULE
   out outu s" src/habu/address-cells.f" BF-APPEND-MODULE
   out outu s" src/habu/prof-abi.f" BF-APPEND-MODULE
   out outu s" src/habu/native-host-cells.f" BF-APPEND-MODULE
   out outu s" src/habu/data-claims.f" BF-APPEND-MODULE
   out outu s" src/habu/arith-abi.f" BF-APPEND-MODULE
   out outu s" src/habu/task-abi.f" BF-APPEND-MODULE
   out outu s" src/habu/spawn-abi.f" BF-APPEND-MODULE
   out outu s" src/habu/habu1.f" BF-APPEND-MODULE ;

\ fmt requires float and string; float requires string and option, string requires errors
\ and option. num-types.f, num-arithmetic.f and memory.f, which fmt does not reach, come in
\ the prefix's order (src/habu/habu2.f PFX-LOAD-STDLIB-FILES) because num-types.f and
\ memory.f (its WITH-BYTES `frame` STRUCTURE) declare type families and the seed refuses a
\ captured registry that does not start where the prefix's does (src/core/type-family.f
\ REG-AOT-BASE-BAD). Prelude and errors are restored already; each provided fact makes the
\ remaining require a no-op.
: BF-APPEND-FMT ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/lib/adt/option.f" BF-APPEND-MODULE
   out outu s" src/lib/num-types.f" BF-APPEND-MODULE
   out outu s" src/lib/num-arithmetic.f" BF-APPEND-MODULE
   out outu s" src/lib/string.f" BF-APPEND-MODULE
   out outu s" src/lib/memory.f" BF-APPEND-MODULE
   out outu s" src/lib/float.f" BF-APPEND-MODULE
   out outu s" src/lib/fmt.f" BF-APPEND-MODULE ;

\ Core-prefix files the compiling host carries from its own boot are absent: the payload
\ rewinds to the mark at that prefix's end (src/habu/prefix-rewind.f, BF-APPEND-RUN-PRELUDE).
\ script-argv, prelude and errors load after that mark, so the payload brings them back.
\ The absent files are checked as their own phase, BF-APPEND-BOOT-PREFIX.
: BF-RECOVERY-ONLY ( -- )
   HB-TARGET-LINUX-X86-64? BUILD-TARGET:LINUX-X86-64? or if
      s" build-fixpoint: ARM source recovery only; use tools/native-build.f or tools/two-generation-build.f on x86-64"
         BF-BUILD-RC die
   then ;

: BF-APPEND-COMMON ( ptr u8 n -- ) {: out:ptr outu :}
   BF-RECOVERY-ONLY
   \ The rewind removed the prelude. Restore it before build-side dependencies
   \ such as habu1.f use its checked flag words.
   out outu s" src/lib/prelude.f" BF-APPEND-MODULE
   out outu s" src/arch/arm64/asm.f" BF-APPEND-MODULE   \ icode.f requires it
   out outu s" src/arch/arm64/icode.f" BF-APPEND-MODULE   \ aot-decl.f requires it
   out outu s" src/arch/arm64/mnem.f" BF-APPEND-SOURCE
   out outu BF-APPEND-TARGET-SYS
   out outu BF-APPEND-SCRIPT-ARGV
   out outu s" src/habu/rt.f" BF-APPEND-SOURCE
   out outu s" src/habu/crash.f" BF-APPEND-SOURCE
   out outu BF-APPEND-IMAGE-BYTES
   out outu BF-APPEND-TARGET-IMAGE
   out outu BF-APPEND-TARGET-PROC-WATCH
   out outu BF-APPEND-TARGET-PROC-CONTROL
   out outu BF-APPEND-HABU1
   out outu s" src/habu/prof.f" BF-APPEND-SOURCE
   out outu s" src/habu/regalloc.f" BF-APPEND-SOURCE
   out outu s" src/habu/jit.f" BF-APPEND-SOURCE
   out outu BF-APPEND-FDIO
   out outu s" src/lib/errors.f" BF-APPEND-MODULE
   \ The span type family, in the engine prefix's position right after errors.f
   \ (src/habu/habu2.f PFX-LOAD-STDLIB-FILES says why the registry needs it).
   out outu s" src/lib/span.f" BF-APPEND-MODULE
   out outu s" src/habu/snapshot-format.f" BF-APPEND-MODULE
   out outu s" src/habu/address-carrier.f" BF-APPEND-MODULE
   out outu s" src/habu/cell-grid.f" BF-APPEND-MODULE
   out outu s" src/habu/aot-decl.f" BF-APPEND-MODULE   \ aot-capture.f requires it
   out outu s" src/habu/aot-runtime.f" BF-APPEND-MODULE   \ habu2.f uses its completeness query
   out outu s" src/habu/aot-ident.f" BF-APPEND-MODULE   \ aot-file.f requires it
   out outu BF-APPEND-FMT
   out outu s" src/habu/habu2.f" BF-APPEND-SOURCE ;

: BF-APPEND-DRIVER-IO ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/habu/sign-id.f" BF-APPEND-MODULE   \ driver-io.f requires it
   out outu s" src/habu/driver-io.f" BF-APPEND-SOURCE ;

: BF-APPEND-RUN-PRELUDE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" 0 set-check" BF-APPEND-LINE
   out outu s" src/habu/prefix-rewind.f" BF-APPEND-SOURCE
   out outu s" LOWER-CERT-HOOK:INSTALL" BF-APPEND-LINE ;

: BF-APPEND-STDIN-RUN-PRELUDE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu BF-APPEND-RUN-PRELUDE ;

: BF-EMIT-PREFIX-SOURCE ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-BOOT-PREFIX ;

: BF-EMIT-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu COMPILER-BUILD:SEAL
   out outu BF-APPEND-DRIVER-IO
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-EMIT-RUN-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu COMPILER-BUILD:SEAL
   out outu BF-APPEND-DRIVER-IO
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-EMIT-STDIN-RUN-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-STDIN-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu COMPILER-BUILD:SEAL
   out outu BF-APPEND-DRIVER-IO
   out outu s" src/habu/aot-arm.f" BF-APPEND-MODULE   \ aot-capture.f requires it
   out outu s" src/habu/terminal-call.f" BF-APPEND-MODULE
   out outu s" src/lib/le.f" BF-APPEND-MODULE
   out outu s" src/habu/sites.f" BF-APPEND-MODULE
   out outu s" src/habu/aot-capture.f" BF-APPEND-SOURCE
   out outu s" src/habu/aot-file.f" BF-APPEND-SOURCE
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-CLOSE-CMP ( -- )
   BF-FDA @ dup 0 >= if close else drop then
   BF-FDB @ dup 0 >= if close else drop then
   -1 BF-FDA !
   -1 BF-FDB ! ;

: BF-OPEN-CMP ( ptr u8 n ptr u8 n -- ) {: a:ptr au b:ptr bu :}
   -1 BF-FDA !
   -1 BF-FDB !
   a au FS:>PATHZ open-rd BF-FDA !
   BF-FDA @ 0 < if E-BUILD-PATH throw then
   b bu FS:>PATHZ open-rd BF-FDB !
   BF-FDB @ 0 < if
      BF-FDA @ close -1 BF-FDA ! E-BUILD-PATH throw
   then ;

: BF-READ-A ( -- n )
   BF-FDA @ BF-CMP-A BF-CMP-CAP read BF-RA !
   BF-RA @ 0 < if BF-CLOSE-CMP E-FS-IO throw then
   BF-RA @ BF-CMP-CAP > if BF-CLOSE-CMP E-FS-IO throw then
   BF-RA @ ;

: BF-READ-B ( -- n )
   BF-FDB @ BF-CMP-B BF-CMP-CAP read BF-RB !
   BF-RB @ 0 < if BF-CLOSE-CMP E-FS-IO throw then
   BF-RB @ BF-CMP-CAP > if BF-CLOSE-CMP E-FS-IO throw then
   BF-RB @ ;

: BF-FILE= ( ptr u8 n ptr u8 n -- bool )
   BF-OPEN-CMP
   begin
      BF-READ-A BF-RA !
      BF-READ-B BF-RB !
      BF-RA @ BF-RB @ <> if BF-CLOSE-CMP BF-FALSE exit then
      BF-RA @ 0= if BF-CLOSE-CMP BF-TRUE exit then
      BF-CMP-A BF-RA @ BF-CMP-B BF-RB @ STR= 0= if
         BF-CLOSE-CMP BF-FALSE exit
      then
   again ;

: BF-TMP-FILE= ( ptr u8 n ptr u8 n -- bool ) {: an:ptr anu bn:ptr bnu :}
   an anu BF-A-PATH BF-TMP> BF-A-LEN !
   bn bnu BF-B-PATH BF-TMP> BF-B-LEN !
   BF-A-PATH BF-A-LEN @ BF-B-PATH BF-B-LEN @ BF-FILE= ;

: BF-PREFIX-SOURCE ( -- )
   s" prefix-src" BF-EMIT-PREFIX-SOURCE ;

: BF-STAGE2-SOURCE ( -- )
   s" stage2-src" s" src/habu/stage2.f" BF-EMIT-SOURCE ;

: BF-STDIN-SOURCE ( -- )
   s" stage2-src" s" src/habu/stdin.f" BF-EMIT-STDIN-RUN-SOURCE ;

: BF-SNAP-SOURCE ( -- )
   s" hb-snap-src" BF-RESET-OUT
   s" hb-snap-src" s" require src/habu/app-image.f" BF-APPEND-LINE
   s" hb-snap-src" s" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE" BF-APPEND-LINE ;

: BF-CERT-LABEL$ ( -- ptr u8 n )
   BF-CERT-LAB-A@ BF-CERT-LAB-U @ ;

: BF-CERT-PATH$ ( -- ptr u8 n )
   BF-CERT-PATH-A@ BF-CERT-PATH-U @ ;

: BF-CERTIFY-INPUT! ( ptr u8 n ptr u8 n -- )
   BF-CERT-PATH-U !
   BF-CERT-PATH-A!
   BF-CERT-LAB-U !
   BF-CERT-LAB-A! ;

: BF-CERTIFY-ACT ( -- )
   BF-CERT-LABEL$ DIAG-FILE!
   BF-FALSE DIAG-JSON!
   BF-CERT-DIAG BF-CERT-DIAG-CAP DIAG-BUFFER!
   BF-CERT-PATH$ BF-READ-SOURCE
   BF-SOURCE-BUF BF-SOURCE-LEN @ VERIFY:SOURCE-BUF ;

: BF-CERTIFY-RC ( ptr u8 n ptr u8 n -- n )
   BF-CERTIFY-INPUT!
   0 BF-CERT-DIAG-U !
   [: BF-CERTIFY-ACT ;] catch BF-CERT-RC !
   DIAG-BUFFER$ nip BF-CERT-DIAG-U !
   DIAG-BUFFER-OFF
   BF-CERT-RC @ ;

\ A generated stage source that fails VERIFY:SOURCE-BUF kills the build (E-BUILD-CERTIFY)
\ after reporting the diagnostic: the running stage-N engine certifies the exact stage-N+1
\ bytes the build consumes, checker-boot prelude included, before any stage compile. No
\ escape hatch: the gforth recovery lane (docs/bootstrap.md "No-Binary Recovery") reaches
\ this refresh only after a working bin/hb exists, and a rejected tree must be repaired.
: BF-CERTIFY-GENERATED ( ptr u8 n ptr u8 n -- )
   BF-CERTIFY-RC
   0= IF exit THEN
   BF-CERT-LABEL$ BF-CERT-RC @ BF-CERT-DIAG BF-CERT-DIAG-U @ BUILD-CERTIFY:REPORT
   E-BUILD-CERTIFY throw ;

: BF-CERTIFY-DRIVER$ ( -- ptr u8 n )
   s" tools/build-fixpoint-certify.f" ;

\ Generated sources redeclare the stdlib, so they are verified against the core prefix in a
\ child of the running engine (ENGINE-ID:PATH$), not BF-ENGINE$, the product maker a fixture
\ points at this build's own output. Every source body is checked without filename
\ exemptions. tools/build-fixpoint-certify.f rewinds the child with src/habu/prefix-rewind.f's
\ text; the rewind keeps code and DATA alive, so the child's compiled verifier stays valid
\ after its entries retire. A nonempty PHASE has the child print its census line, since
\ VERIFY:CENSUS exists only in the child, with this process's target, a cross build's. The
\ child exits 0 for a certified source and 70 after printing the rejection and its
\ diagnostic; any status but 0 fails the build.
: BF-CERTIFY-GENERATED-CORE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: lab:ptr labu:n path:ptr pathu:n phase:ptr phaseu:n :}
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   BF-CERTIFY-DRIVER$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   lab labu >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+
   phaseu 0 > if
      phase phaseu >LEN PROC-ARGV+
      BUILD-CERTIFY:TARGET$ >LEN PROC-ARGV+
   then
   ENGINE-ID:PATH$ >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE
   BF-SPAWN BF-FINISH-PID
   0<> if E-BUILD-CERTIFY throw then ;

\ The boot prefix's certify phase: its bytes go to their own stage-input path and are read
\ back through VERIFY:SOURCE-BUF; no build step compiles that file.
: BF-CERTIFY-PREFIX ( -- )
   s" prefix-src" s" prefix-src" BF-A$ BF-CERTIFY-GENERATED
   s" boot prefix" BUILD-CERTIFY:TARGET$ BUILD-CERTIFY:CENSUS ;

\ The stage engine reads its source from the fixed `stage2-src` path in the temp root
\ (BF-PREPARE-STAGE-ARGV passes only `-- <tmp>`), so BF-STDIN-SOURCE overwrites the stage2
\ source there before BF-CERTIFY-STDIN: a certify's first argument names its phase, not a file.
: BF-CERTIFY-STAGE2 ( -- )
   s" stage2-src" s" stage2-src" BF-A$ s" assembled" BF-CERTIFY-GENERATED-CORE ;

: BF-CERTIFY-STDIN ( -- )
   s" stdin-src" s" stage2-src" BF-A$ s" " BF-CERTIFY-GENERATED-CORE ;

: BF-SRC-DIGEST ( ptr u8 n ptr u8 -- ) {: a:ptr u:n dg:ptr :}
   BF-FSHA-CTX a u BF-A$ dg SHA256-FILE-IN dup 0 <> if throw then drop ;

: BF-STAGE2-DIGEST ( ptr u8 -- ) {: dg:ptr :}
   s" stage2-src" dg BF-SRC-DIGEST ;

: BF-PREFIX-DIGEST ( ptr u8 -- ) {: dg:ptr :}
   s" prefix-src" dg BF-SRC-DIGEST ;

: BF-RECORD-RESET ( -- )
   0 BF-REC-PREFIX? !
   0 BF-REC-STAGE? !
   0 BF-REC-STDIN? ! ;

: BF-RECORD-PREFIX ( -- )
   BF-REC-PREFIX-DG BF-PREFIX-DIGEST
   -1 BF-REC-PREFIX? ! ;

: BF-RECORD-STAGE ( -- )
   BF-REC-STAGE-DG BF-STAGE2-DIGEST
   -1 BF-REC-STAGE? ! ;

: BF-RECORD-STDIN ( -- )
   BF-REC-STDIN-DG BF-STAGE2-DIGEST
   -1 BF-REC-STDIN? ! ;

: BF-BOOTSTRAP-STAGE ( -- )
   s" stage2-got" BF-REMOVE-TMP
   s" hb-stage" BF-REMOVE-TMP
   BF-ENGINE$ s" stage2-src" BF-A$ COMPILER-BUILD:RUN BF-RC0
   s" stage2-got" BF-EXPECT
   s" stage2-got" s" hb-stage" BF-RENAME-TMP
   s" hb-stage" BF-CHMOD-X-TMP ;

: BF-RUN-STAGE ( -- )
   s" stage2-got" BF-REMOVE-TMP
   s" hb-stage" BF-RUN-STAGE-TMP BF-RC0
   s" stage2-got" BF-EXPECT ;

: BF-PROMOTE-STAGE ( -- )
   s" stage2-got" s" hb-stage" BF-RENAME-TMP
   s" hb-stage" BF-CHMOD-X-TMP ;

: BF-VERIFY-STAGE ( -- )
   s" hb-stage" BF-CODESIGN-VERIFY-TMP ;

: BF-STAGE-MATCH? ( -- bool )
   s" hb-stage" s" stage2-got" BF-TMP-FILE= ;

: BF-STAGE-FIXPOINT-FROM-SOURCE ( -- )
   BF-BOOTSTRAP-STAGE
   0 BF-GEN !
   0 BF-FOUND !
   begin BF-GEN @ BF-MAX-GENS < while
      BF-RUN-STAGE
      BF-STAGE-MATCH? if
         BF-VERIFY-STAGE
         -1 BF-FOUND !
         BF-MAX-GENS BF-GEN !
      else
         BF-PROMOTE-STAGE
         BF-GEN @ 1 + BF-GEN !
      then
   repeat
   BF-FOUND @ 0= if s" FIXPOINT BROKEN: no convergence after 4 generations" BF-BUILD-RC die then
   s" bin/hb refresh OK: compiler fixpoint" type cr ;

: BF-STAGE-FIXPOINT ( -- )
   BF-PREFLIGHT
   BF-PREFIX-SOURCE
   BF-CERTIFY-PREFIX
   BF-RECORD-PREFIX
   BF-STAGE2-SOURCE
   BF-CERTIFY-STAGE2
   BF-RECORD-STAGE
   BF-STAGE-FIXPOINT-FROM-SOURCE ;

\ ---------------------------------------------------------------------------
\ The chain capture, and the driver that bakes it. A build source must not read paths from
\ the environment (docs/forth.md), so the artifact and the engine that produced it reach
\ src/habu/stdin.f as a call spliced into the generated source; tools/aot-chain-bake.f calls
\ this generator. The two literals go in a colon body, never on a top-level line: an
\ interpret-mode `s"` allots at HERE, which CAPTURE-REPL latches as the capture window's DATA
\ base, so the engine would depend on the paths' length (two temporary roots gave engines
\ 12081 bytes apart). In a body the literal lands in code below the window, which the seed's
\ canonical CODE-B0 absorbs. src/habu/stdin.f DP-MARK refuses the top-level shape.
create BF-CHAIN-DRV-BUF PATH-CAP allot   variable BF-CHAIN-DRV-U
create BF-ART-BUF PATH-CAP allot         variable BF-ART-U
variable BF-DRV-R

: BF-CHAIN-DRIVER$ ( -- ptr u8 n )
   BF-CHAIN-DRV-BUF BF-CHAIN-DRV-U @ ;

: BF-CHAIN-DRIVER-PATH! ( -- )
   s" chain-driver.f" BF-CHAIN-DRV-BUF BF-TMP> BF-CHAIN-DRV-U ! ;

: BF-CHAIN-ART$ ( -- ptr u8 n )
   s" chain.aot" BF-ART-BUF BF-TMP> BF-ART-U !
   BF-ART-BUF BF-ART-U @ ;

: BF-CAPTURE-TOOL$ ( -- ptr u8 n )
   s" tools/aot-chain-capture.f" ;

: BF-PREPARE-CAPTURE-ARGV ( ptr u8 n ptr u8 n -- ptr u8 ptr ptr u8 ) {: exe:ptr exeu:n art:ptr artu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   BF-CAPTURE-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   art artu >LEN PROC-ARGV+
   exe exeu >LEN PROC-ARGV-PREPARE ;

: BF-RUN-CAPTURE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu:n art:ptr artu:n :}
   BF-PREPARE-ENV
   exe exeu art artu BF-PREPARE-CAPTURE-ARGV
   PROC-ENV-PREPARE
   BF-SPAWN BF-FINISH-PID ;

: BF-CAPTURE-INTO ( ptr u8 n -- ) {: art:ptr artu:n :}
   s" hb-host" BF-A$ art artu BF-RUN-CAPTURE BF-RC0 ;

: BF-DRV-WS? ( n -- bool ) {: c:n :}
   c 32 = c 9 = or c 13 = or c 10 = or ;

: BF-DRV-LAST ( -- n )
   BF-SOURCE-LEN @ 1 -
   begin dup 0 >= while
      dup BF-SRC-C@ BF-DRV-WS? 0= if exit then
      1 -
   repeat ;

: BF-DRV-TAIL-BAD ( -- )
   s" build-fixpoint: src/habu/stdin.f no longer ends with STDIN-DRIVER:RUN" BF-BUILD-RC die ;

: BF-DRV-TAIL$ ( -- ptr u8 n ) s" STDIN-DRIVER:RUN" ;

\ Everything up to the trailing `STDIN-DRIVER:RUN`, which the generated driver
\ replaces with the artifact declaration plus the same call. Fail closed if the
\ tail moved: a driver assembled around a tail that is no longer there would run
\ some other program.
: BF-DRV-KEEP ( -- n )
   BF-DRV-LAST {: l:n :}
   BF-DRV-TAIL$ {: t:ptr tu:n :}
   l 1 + tu < if BF-DRV-TAIL-BAD then
   l 1 + tu - BF-SOURCE-BUF + tu  t tu STR= 0= if BF-DRV-TAIL-BAD then
   l 1 + tu > if l tu - BF-SRC-C@ BF-DRV-WS? 0= if BF-DRV-TAIL-BAD then then
   l 1 + tu - ;

\ The one split of the stdin driver: reads src/habu/stdin.f into BF-SOURCE-BUF and answers
\ how many bytes a generated driver keeps, up to the terminal `STDIN-DRIVER:RUN` it replaces.
\ This file's chain driver, tools/aot-chain-bake.f and test/aot-wid-build.f all ask it.
: BF-DRV-SOURCE-KEEP ( -- n )
   s" src/habu/stdin.f" BF-READ-SOURCE
   BF-DRV-KEEP ;

\ A path holding a `"` would end the generated string literal early and the driver
\ would compile some other program. Refuse rather than escape.
: BF-DRV-PATH? ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BF-DRV-R !
   begin BF-DRV-R @ u < while
      a BF-DRV-R @ + c@ BF-DQ = if
         s" build-fixpoint: a path with a quote cannot be spliced into the driver"
         BF-BUILD-RC die
      then
      BF-DRV-R @ 1 + BF-DRV-R !
   repeat ;

: BF-DRV+ ( ptr u8 n -- ) {: a:ptr u:n :}
   BF-CHAIN-DRIVER$ a u FS:APPEND-FILE ;

\ ( artifact-path producer-engine-path -- ): stdin.f with its tail replaced by the
\ declaration of THIS artifact and the same call.
: BF-CHAIN-DRIVER-FROM ( ptr u8 n ptr u8 n -- ) {: art:ptr artu:n eng:ptr engu:n :}
   art artu BF-DRV-PATH?
   eng engu BF-DRV-PATH?
   BF-CHAIN-DRIVER-PATH!
   BF-DRV-SOURCE-KEEP {: keep:n :}
   BF-CHAIN-DRIVER$ BF-SOURCE-BUF keep FS:WRITE-ALL
   S\" : ART-DECL ( -- ) s\" " BF-DRV+
   art artu BF-DRV+
   S\" \" s\" " BF-DRV+
   eng engu BF-DRV+
   S\" \" STDIN-DRIVER:ARTIFACT! ;\nART-DECL\n" BF-DRV+
   BF-DRV-TAIL$ BF-DRV+
   BF-LF-BUF 1 BF-DRV+ ;

: BF-CHAIN-DRIVER! ( -- )
   BF-CHAIN-ART$ s" hb-host" BF-B$ BF-CHAIN-DRIVER-FROM ;

\ ---------------------------------------------------------------------------
\ Two engines, one prefix. Once the chain is baked, `require src/compiler/native/compiler.f`
\ is a registry no-op in the product and src/core/include.f dies on duplicates, so the
\ product cannot capture its own chain. The prefix is emitted twice, differing only in the
\ artifact the driver declares (src/habu/stdin.f STDIN-DRIVER:ARTIFACT!):
\   hb-host    the capture host: no artifact, never installed; the capture runs in it.
\   hb-stdin   the product, seeded from the artifact captured in hb-host; BF-INSTALL-HB
\              checks it before promoting it.
: BF-EMIT-ENGINE ( ptr u8 n -- ) {: drv:ptr drvu:n :}
   s" stage2-src" drv drvu BF-EMIT-STDIN-RUN-SOURCE
   BF-CERTIFY-STDIN
   BF-RUN-STAGE
   s" stage2-got" s" hb-stdin-mk" BF-RENAME-TMP
   s" hb-stdin-mk" BF-CHMOD-X-TMP
   s" hb-stdin-got" BF-REMOVE-TMP
   s" hb-stdin-mk" BF-RUN-ENV-TMP BF-RC0
   s" hb-stdin-got" BF-EXPECT
   s" hb-stdin-got" s" hb-stdin" BF-RENAME-TMP
   s" hb-stdin" BF-CHMOD-X-TMP
   s" hb-stdin" BF-CODESIGN-VERIFY-TMP ;

: BF-KEEP-HOST ( -- )
   s" hb-stdin" s" hb-host" BF-RENAME-TMP
   s" hb-stdin-mk" BF-REMOVE-TMP ;

: BF-BUILD-STDIN-FROM-STAGE ( -- )
   s" src/habu/stdin.f" BF-EMIT-ENGINE
   BF-RECORD-STDIN                              \ the HOST's source is the stamp's: the
   BF-KEEP-HOST                                 \ product's names a temporary artifact path
   BF-CHAIN-ART$ BF-CAPTURE-INTO
   BF-CHAIN-DRIVER!
   BF-CHAIN-DRIVER$ BF-EMIT-ENGINE ;

: BF-BUILD-STDIN ( -- )
   BF-PREFLIGHT
   BF-BUILD-STDIN-FROM-STAGE ;

: BF-BUILD-STDIN-FRESH ( -- )
   BF-STAGE-FIXPOINT
   BF-BUILD-STDIN-FROM-STAGE ;

\ Rebuild the complete native runtime before taking its snapshot. APP-IMAGE owns
\ preparation and provenance; the source-only recovery host is not an image.
: BF-BUILD-NATIVE ( -- )
   s" hb-native" BF-REMOVE-TMP
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/native-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" hb-native" BF-A$ >LEN PROC-ARGV+
   BF-ENGINE$ >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE
   BF-SPAWN BF-FINISH-PID BF-RC0
   s" hb-native" BF-EXPECT ;

: BF-SAVE-SNAPSHOT ( -- )
   BF-SNAP-SOURCE
   s" hb-new" BF-REMOVE-TMP
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-new" BF-A$ >LEN PROC-ARGV+
   s" hb-snap-src" BF-A$ BF-OPEN-INPUT {: infd:n :}
   s" hb-native" BF-A$ >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE infd
   BF-SPAWN-IN BF-FINISH-PID BF-RC0
   s" hb-new" BF-EXPECT
   s" hb-new" BF-CODESIGN-VERIFY-TMP ;

: BF-BUILD-SNAP-FRESH ( -- )
   BF-BUILD-NATIVE
   BF-SAVE-SNAPSHOT
   s" snapshot image OK: candidate validated" type cr ;

create BF-BOOT-ROOT PATH-CAP allot
variable BF-BOOT-ROOT-U
$100 constant BF-BOOT-OUT-CAP
$1000 constant BF-BOOT-ERR-CAP
30000 constant BF-BOOT-TIMEOUT-MS
create BF-BOOT-OUT BF-BOOT-OUT-CAP allot
create BF-BOOT-ERR BF-BOOT-ERR-CAP allot

: BF-BOOT-ROOT$ ( -- ptr u8 n ) BF-BOOT-ROOT BF-BOOT-ROOT-U @ ;

\ A real copied tree makes a captured build-directory path fail here, before
\ the candidate can replace the working engine. Symlinks back to the original
\ sources would conceal that mismatch through canonical path resolution.
: BF-BOOT-TREE ( -- )
   s" src" BF-BOOT-ROOT$ TREE-COPY:TREE ;

: BF-BOOT-PROGRAM$ ( -- ptr u8 n )
   \ Exercise provided quotation storage in the copied tree before the
   \ candidate replaces the working engine.
   S\" require src/core/quotation-storage.f\nrequire src/compiler/native/compiler.f\n1 set-tier\n: BF-CANDIDATE-INC ( n -- n ) 1+ ;\n41 BF-CANDIDATE-INC . cr\n" ;

: BF-BOOT-RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N ENDOF
   ;MATCH ;

: BF-BOOT-RUN ( ptr u8 n -- )
   {: cand:ptr candu:n :}
   BF-BOOT-TREE
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   cand candu SOURCE-ROOT:CANONICAL drop >LEN
   BF-BOOT-ROOT$ >LEN BF-BOOT-PROGRAM$ >LEN
   BF-BOOT-OUT BF-BOOT-OUT-CAP >LEN BF-BOOT-ERR BF-BOOT-ERR-CAP >LEN
   BF-BOOT-TIMEOUT-MS >MS PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE
   BF-BOOT-RESULT {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or BF-BOOT-OUT outu S\" 42\n\n" STR= 0= or if
      s" build-fixpoint: candidate failed its copied-tree boot check" type cr
      BF-BOOT-OUT outu type BF-BOOT-ERR erru type
      E-BUILD-STATUS throw
   then ;

: BF-BOOT-CLEAN ( -- ) BF-BOOT-ROOT$ FS:REMOVE-TREE ;

: BF-CHECK-CANDIDATE ( ptr u8 n -- )
   {: cand:ptr candu:n :}
   BF-TMP$ s" boot-check" FS:MKTEMP-DIR-IN SOURCE-ROOT:CANONICAL drop
   {: root:ptr size:n :}
   root BF-BOOT-ROOT size BYTE-COPY size BF-BOOT-ROOT-U !
   cand candu [: BF-BOOT-RUN ;] [: BF-BOOT-CLEAN ;] finally ;

\ The candidate is copied into the engine's sibling, booted there and renamed
\ over the engine only once it passed (src/lib/fs-mutate.f FS:REPLACE-STAGED), so a
\ failed install leaves the engine as it was and no sibling.
: BF-INSTALL-HB-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   s" hb-stdin" BF-A$ tmp tmpu FS:COPY-FILE-STREAM
   tmp tmpu FS:CHMOD-X
   tmp tmpu BF-CHECK-CANDIDATE ;

: BF-INSTALL-HB ( -- )
   BF-ENGINE$ [: BF-INSTALL-HB-FILL ;] FS:REPLACE-STAGED
   s" hb-stdin" BF-REMOVE-TMP ;

\ The capture host is kept beside the engine it captured for: a fixture that source-loads
\ the chain cannot run against the product, which provides it, and rebuilding a host per
\ fixture is the cost this file pays once. The path derives from BF-ENGINE$, so a fixture
\ install under HABU_FIXPOINT_ENGINE keeps its host in its own sandbox.
: BF-HOST-DST$ ( -- ptr u8 n )
   BF-ENGINE$ s" -host" BF-HOST-DST-BUF FS:SUFFIX-PATH BF-HOST-DST-U !
   BF-HOST-DST-BUF BF-HOST-DST-U @ SPAN:TAKE SPAN:$ ;

: BF-INSTALL-HOST-FILL ( ptr u8 n -- )
   {: tmp:ptr tmpu:n :}
   s" hb-host" BF-A$ tmp tmpu FS:COPY-FILE-STREAM
   tmp tmpu FS:CHMOD-X ;

: BF-INSTALL-HOST ( -- )
   BF-HOST-DST$ [: BF-INSTALL-HOST-FILL ;] FS:REPLACE-STAGED ;

: BF-BIN-HB? ( ptr u8 n -- bool )
   2dup s" bin/hb" STR= if 2drop BF-TRUE exit then
   s" bin/hb-host" STR= ;

: BF-REMOVE-BIN-OTHER ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FS:FILE? if
      path pathu BF-BIN-HB? 0= if path pathu FS:REMOVE-FILE then
   then ;

: BF-CLEAN-BIN ( -- )
   BF-ENGINE$ s" bin/hb" STR= 0= if exit then
   s" bin" [: BF-REMOVE-BIN-OTHER ;] FS:WALK-FILES ;

: BF-INSTALL ( -- )
   BF-BUILD-STDIN-FRESH
   BF-INSTALL-HB
   BF-INSTALL-HOST
   BF-CLEAN-BIN
   s" bin/hb ready (small checked engine, tty REPL + stdin)" type cr ;

: BF-STAMP-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-BUILD-PATH throw then
   u PATH-CAP > if E-BUILD-PATH throw then
   a BF-STAMP-PATH-BUF u BYTE-COPY
   u BF-STAMP-PATH-U ! ;

: BF-STAMP-PATH-RESET ( -- )
   0 BF-STAMP-PATH-U ! ;

: BF-STAMP-DIR$ ( -- ptr u8 n )
   BUILD-OUT:BASE$ s" fixpoint" BF-STAMP-DIR-BUF FS:JOIN-PATH BF-STAMP-DIR-U !
   BF-STAMP-DIR-BUF BF-STAMP-DIR-U @ ;

: BF-STAMP-PATH$ ( -- ptr u8 n )
   BF-STAMP-PATH-U @ 0 > if BF-STAMP-PATH-BUF BF-STAMP-PATH-U @ exit then
   BF-STAMP-DIR$ s" stamp" BF-STAMP-DEF-BUF FS:JOIN-PATH BF-STAMP-DEF-U !
   BF-STAMP-DEF-BUF BF-STAMP-DEF-U @ ;

: BF-STAMP-ENSURE-DIR ( -- )
   BF-STAMP-PATH$ SOURCE-ROOT:DIRNAME FS:MAKE-DIRS ;

: BF-STAMP-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   BF-STAMP-U @ u + BF-STAMP-CAP > if E-STR-CAPACITY throw then
   a BF-STAMP-BUF BF-STAMP-U @ + u BYTE-COPY
   BF-STAMP-U @ u + BF-STAMP-U ! ;

: BF-STAMP-C+ ( n -- ) {: c:n :}
   BF-STAMP-U @ 1 + BF-STAMP-CAP > if E-STR-CAPACITY throw then
   c BF-STAMP-BUF BF-STAMP-U @ + c!
   BF-STAMP-U @ 1 + BF-STAMP-U ! ;

: BF-STAMP-FRAG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   u BF-STAMP-C+
   a u BF-STAMP-BYTES+ ;

: BF-STAMP-DG+ ( ptr u8 n ptr u8 -- ) {: tag:ptr tagu:n dg:ptr :}
   tag tagu BF-STAMP-FRAG+
   dg BF-STAMP-DG-U BF-STAMP-FRAG+ ;

: BF-STAMP-ENGINE+ ( -- )
   BF-FSHA-CTX BF-ENGINE$ BF-STAMP-DG SHA256-FILE-IN dup 0 <> if throw then drop
   s" engine" BF-STAMP-DG BF-STAMP-DG+ ;

\ ---------------------------------------------------------------------------
\ The stamp key: the refresh's preimage and its comparison with the stored stamp. The chain
\ capture reads sources from the checkout in the host engine, so editing the chain changes
\ no emitted byte. The capture tool's whole ordered require/include closure is one preimage
\ row, covering the chain since the tool requires the chain entry inside its window.
\ Discovery is the hb-build artifact cache's walk (tools/event-closure-lib.f over
\ tools/source-discovery.f) and refuses fail-closed, so a closure that cannot be reproduced
\ fails the refresh. The closure digest is captured once per process, before the build: a
\ mid-build edit read afterwards would be stamped over an engine built from the older
\ chain. Capturing first can only over-invalidate.

create CHAIN-DG 40 allot
variable CHAIN-DONE?
variable CHAIN-I

: ENTRY$ ( -- ptr u8 n )
   BF-CAPTURE-TOOL$ ;

: CLOSURE+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold ) {: a:ptr u:n :}
   a u EC:BUILD
   0 CHAIN-I !
   begin CHAIN-I @ EC:COUNT < while
      CHAIN-I @ EC:PATH$ CONTENT-KEY:FILE+
      CHAIN-I @ 1+ CHAIN-I !
   repeat ;

: CHAIN-DIGEST-BODY ( ptr u8 n ptr u8 CONTENT-KEY:fold -- )
   >r {: a:ptr u:n dst:ptr :} r>
   s" native-chain-closure-v1" CONTENT-KEY:TEXT+
   a u CLOSURE+
   dst CONTENT-KEY:FINAL ;

\ The ordered closure of one entry file, hashed into a 32-byte digest.
: CHAIN-DIGEST! ( ptr u8 n ptr u8 -- )
   [: CHAIN-DIGEST-BODY ;] CONTENT-KEY:WITH-FOLD ;

: CHAIN-RECORD ( -- )
   CHAIN-DONE? @ if exit then
   ENTRY$ CHAIN-DG CHAIN-DIGEST!
   BF-TRUE CHAIN-DONE? ! ;

\ The preimage tag for the capture-source digest, a word because tools/build-fixpoint-test.f
\ rebuilds the same framed field: two literals drift apart on a rename unseen, since the
\ fixture's negative case passes on an absent tag either way.
: BF-STAMP-CAPTURE-TAG$ ( -- ptr u8 n )
   s" capture-src" ;

: BF-STAMP-KEY-BEGIN ( -- )
   CHAIN-RECORD
   0 BF-STAMP-U !
   s" build-fixpoint-stamp-v2" BF-STAMP-FRAG+
   BF-STAMP-ENGINE+
   BF-STAMP-CAPTURE-TAG$ CHAIN-DG BF-STAMP-DG+ ;

\ The boot prefix is a keyed build input in its own right. Its bytes reach no
\ stage source, so without this row an edit to checker.f would leave every
\ other stamp input identical and the refresh would answer `fixpoint: cached`
\ over a product whose prefix had changed underneath it.
: BF-STAMP-PREFIX-KEY+ ( -- )
   BF-STAMP-DG BF-PREFIX-DIGEST
   s" prefix-src" BF-STAMP-DG BF-STAMP-DG+ ;

: BF-STAMP-STAGE-KEY+ ( -- )
   BF-STAMP-DG BF-STAGE2-DIGEST
   s" stage2-src" BF-STAMP-DG BF-STAMP-DG+ ;

: BF-STAMP-STDIN-KEY+ ( -- )
   BF-STAMP-DG BF-STAGE2-DIGEST
   s" stdin-src" BF-STAMP-DG BF-STAMP-DG+ ;

: BF-STAMP-KEY-END ( -- )
   BF-SHA-CTX BF-STAMP-BUF BF-STAMP-U @ BF-STAMP-DG SHA256-IN
   BF-STAMP-DG BF-STAMP-KEY SHA256>HEX ;

: BF-STAMP-KEY! ( -- )
   BF-STAMP-KEY-BEGIN
   BF-PREFIX-SOURCE
   BF-STAMP-PREFIX-KEY+
   BF-STAGE2-SOURCE
   BF-STAMP-STAGE-KEY+
   BF-STDIN-SOURCE
   BF-STAMP-STDIN-KEY+
   BF-STAMP-KEY-END ;

: BF-STAMP-RECORDED-KEY! ( -- )
   BF-REC-PREFIX? @ 0= if E-BUILD-STATUS throw then
   BF-REC-STAGE? @ 0= if E-BUILD-STATUS throw then
   BF-REC-STDIN? @ 0= if E-BUILD-STATUS throw then
   BF-STAMP-KEY-BEGIN
   s" prefix-src" BF-REC-PREFIX-DG BF-STAMP-DG+
   s" stage2-src" BF-REC-STAGE-DG BF-STAMP-DG+
   s" stdin-src" BF-REC-STDIN-DG BF-STAMP-DG+
   BF-STAMP-KEY-END ;

: BF-STAMP-READ? ( -- bool )
   BF-STAMP-PATH$ FS:FILE? 0= if BF-FALSE exit then
   BF-STAMP-PATH$ FS:FILE-SIZE BF-STAMP-HEX-U 1 + <> if BF-FALSE exit then
   BF-STAMP-PATH$ BF-STAMP-OLD BF-STAMP-HEX-U 1 + FS:READ-ALL BF-STAMP-HEX-U 1 + <> if BF-FALSE exit then
   BF-STAMP-OLD BF-STAMP-HEX-U + c@ BF-LF <> if BF-FALSE exit then
   BF-TRUE ;

\ Every stamped verb asks this first, so it is where the chain capture lands
\ before the build starts. `--force` answers false without deriving a key, so
\ the capture has to happen ahead of that answer rather than inside the key.
: BF-STAMP-MATCH? ( -- bool )
   CHAIN-RECORD
   BF-FORCE @ 0 <> if BF-FALSE exit then
   BF-STAMP-READ? 0= if BF-FALSE exit then
   BF-STAMP-KEY!
   BF-STAMP-OLD BF-STAMP-HEX-U BF-STAMP-KEY BF-STAMP-HEX-U STR= ;

: BF-STAMP-WRITE ( -- )
   BF-STAMP-ENSURE-DIR
   BF-STAMP-RECORDED-KEY!
   BF-LF BF-STAMP-KEY BF-STAMP-HEX-U + c!
   BF-STAMP-PATH$ BF-STAMP-KEY BF-STAMP-HEX-U 1 + FS:ATOMIC-WRITE-FILE ;

: BF-STAMP-CACHED ( -- )
   s" fixpoint: cached " type
   BF-STAMP-KEY BF-STAMP-PREFIX-U type cr ;

: BF-STDIN-HB= ( -- bool )
   s" hb-stdin" BF-A$ BF-ENGINE$ BF-FILE= ;

: BF-ALL-STAMP ( -- )
   BF-STDIN-HB= if BF-STAMP-WRITE then ;

: BF-BUILD-ALL-CACHED ( -- )
   BF-STAMP-MATCH? if BF-STAMP-CACHED exit then
   BF-RECORD-RESET
   BF-BUILD-STDIN-FRESH
   BF-ALL-STAMP ;

: BF-INSTALL-CACHED ( -- )
   BF-STAMP-MATCH? if BF-STAMP-CACHED exit then
   BF-RECORD-RESET
   BF-INSTALL
   BF-STAMP-WRITE ;

: BF-USAGE ( -- )
   s" usage: tools/build-fixpoint.f [all|install|stage|stdin|snap] [--force]" BF-USAGE-RC die ;

: BF-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: BF-ARGN= ( n ptr u8 n -- bool ) {: idx:n a:ptr u:n :}
   idx SCRIPT-ARGV$ a u STR= ;

: BF-PARSE-FORCE ( -- n )
   0 BF-FORCE !
   SCRIPT-ARGC 0= if 0 exit then
   SCRIPT-ARGC 1 - s" --force" BF-ARGN= if
      -1 BF-FORCE !
      SCRIPT-ARGC 1 - exit
   then
   SCRIPT-ARGC ;

: BF-MAIN ( -- )
   BF-PARSE-FORCE {: argn:n :}
   BF-PIN-RESET BF-PIN-ON!
   argn 0= if BF-RECOVERY-ONLY BF-BUILD-ALL-CACHED exit then
   argn 1 <> if BF-USAGE then
   s" all" BF-ARG0= if BF-RECOVERY-ONLY BF-BUILD-ALL-CACHED exit then
   s" install" BF-ARG0= if BF-RECOVERY-ONLY BF-INSTALL-CACHED exit then
   s" stage" BF-ARG0= if BF-RECOVERY-ONLY BF-STAGE-FIXPOINT exit then
   s" stdin" BF-ARG0= if BF-RECOVERY-ONLY BF-BUILD-STDIN-FRESH exit then
   s" snap" BF-ARG0= if BF-BUILD-SNAP-FRESH exit then
   BF-USAGE ;

\ Fail-closed CLI boundary: BF-CLI catches every throw out of BF-MAIN, names it on stderr
\ and dies with the tool's exit contract: PROC-TIMEOUT-RC (src/lib/process.f) for a deadline
\ that expired here or in a child that exited with it (BF-RC0), BF-BUILD-RC for any other
\ throw, BF-USAGE-RC for a refused command line.
$80 constant BF-FAIL-CAP
$18 constant BF-FAIL-DG-CAP
create BF-FAIL-BUF BF-FAIL-CAP allot
create BF-FAIL-DG BF-FAIL-DG-CAP allot
variable BF-FAIL-U
variable BF-FAIL-V
variable BF-FAIL-N

: BF-FAIL-C+ ( n -- ) {: c:n :}
   BF-FAIL-U @ 1 + BF-FAIL-CAP > if s" build-fixpoint: fail message overflow" BF-BUILD-RC die then
   c BF-FAIL-BUF BF-FAIL-U @ + c!
   BF-FAIL-U @ 1 + BF-FAIL-U ! ;

: BF-FAIL+ ( ptr u8 n -- ) {: a:ptr u:n :}
   BF-FAIL-U @ u + BF-FAIL-CAP > if s" build-fixpoint: fail message overflow" BF-BUILD-RC die then
   a BF-FAIL-BUF BF-FAIL-U @ + u BYTE-COPY
   BF-FAIL-U @ u + BF-FAIL-U ! ;

: BF-FAIL-DIGITS+ ( -- )
   0 BF-FAIL-N !
   BF-FAIL-V @ 0 = if $30 BF-FAIL-C+ exit then
   begin BF-FAIL-V @ 0 > while
      BF-FAIL-V @ 10 mod $30 +  BF-FAIL-DG BF-FAIL-N @ + c!
      BF-FAIL-N @ 1 + BF-FAIL-N !
      BF-FAIL-V @ 10 / BF-FAIL-V !
   repeat
   begin BF-FAIL-N @ 0 > while
      BF-FAIL-N @ 1 - BF-FAIL-N !
      BF-FAIL-DG BF-FAIL-N @ + c@ BF-FAIL-C+
   repeat ;

: BF-FAIL-CODE+ ( n -- ) {: rc:n :}
   rc 0 < if $2D BF-FAIL-C+ then
   rc 0 < if 0 rc - else rc then BF-FAIL-V !
   BF-FAIL-DIGITS+ ;

: BF-FAIL-NAME+ ( n -- ) {: rc:n :}
   rc E-BUILD-STATUS = if s"  (E-BUILD-STATUS: refresh child failed)" BF-FAIL+ exit then
   rc E-BUILD-PATH = if s"  (E-BUILD-PATH: build artifact missing)" BF-FAIL+ exit then
   rc E-BUILD-CERTIFY = if s"  (E-BUILD-CERTIFY: generated stage source rejected)" BF-FAIL+ exit then
   rc E-PROC-TIMEOUT = if s"  (E-PROC-TIMEOUT: a deadline expired)" BF-FAIL+ exit then ;

: BF-FAIL-DIE ( n -- ) {: rc:n :}
   0 BF-FAIL-U !
   s" build-fixpoint: failed: uncaught throw code " BF-FAIL+
   rc BF-FAIL-CODE+
   rc BF-FAIL-NAME+
   BF-FAIL-BUF BF-FAIL-U @ rc BF-BUILD-RC PROC-EXIT-RC die ;

variable BF-CLI-RAN

: BF-CLI ( -- )
   BF-CLI-RAN @ if exit then          \ run the CLI at most once per process: the
   BF-TRUE BF-CLI-RAN !                \ tail self-dispatch and either explicit
   [: BF-MAIN ;] catch {: rc:n :}      \ entry wrapper must not both drive a build
   rc 0 <> if rc BF-FAIL-DIE then ;

;package

package BUILD-FIXPOINT

\ Fail-closed CLI self-dispatch. tools/build-fixpoint-main.f and tools/build-fixpoint-refresh.f
\ call BF-CLI; without self-dispatch a recovery command omitting them leaves the loaded stdin
\ engine reading its program from closed stdin and exiting 0, having built nothing. Run the
\ CLI when the command line named this file (SCRIPT-NAMED-LOAD?; a required file stays
\ inert) and a build verb follows `--`. INCLUDE-DEPTH 0 cannot answer this: `--load` loads
\ named files at depth 1 through the require registry. The verb gate keeps a co-loaded tool
\ that consumes its own `--` arguments (tools/hb-build.f under tools/hb-build-test-lib.f)
\ from being hijacked, and BF-CLI runs once per process.
: BF-CLI-VERB-ARG0? ( -- bool )
   SCRIPT-ARGC 0 <= if BF-FALSE exit then
   s" all"     BF-ARG0= if BF-TRUE exit then
   s" install" BF-ARG0= if BF-TRUE exit then
   s" stage"   BF-ARG0= if BF-TRUE exit then
   s" stdin"   BF-ARG0= if BF-TRUE exit then
   s" snap"    BF-ARG0= if BF-TRUE exit then
   BF-FALSE ;

: BF-CLI-SELF-DISPATCH ( -- )
   SCRIPT-NAMED-LOAD? 0= if exit then
   BF-CLI-VERB-ARG0? 0= if exit then
   BF-CLI ;

\ ---------------------------------------------------------------------------
\ The BUILD-FIXPOINT surface. Shared build helpers are exported by tools/build-support.f.
\ Consumers require this file and `using BUILD-FIXPOINT`; the tool's own test reopens the
\ package to drive internals no caller sees.

public

EXPORT BF-APPEND-BYTES
EXPORT BF-APPEND-BOOT-PREFIX
EXPORT BF-APPEND-COMMON
EXPORT BF-APPEND-DRIVER-IO
EXPORT BF-APPEND-LF
EXPORT BF-APPEND-RUN-PRELUDE
EXPORT BF-BUILD-STDIN-FROM-STAGE
EXPORT BF-CERTIFY-DRIVER$
EXPORT BF-CERTIFY-STDIN
EXPORT BF-CHAIN-DRIVER$
EXPORT BF-CHAIN-DRIVER-FROM
EXPORT BF-CHMOD-X-TMP
EXPORT BF-CLI
EXPORT BF-CLI-SELF-DISPATCH
EXPORT BF-CODESIGN-VERIFY-TMP
EXPORT BF-DRV-SOURCE-KEEP
EXPORT BF-EMIT-ENGINE
EXPORT BF-EMIT-STDIN-RUN-SOURCE
EXPORT BF-PREFLIGHT
EXPORT BF-RC0
EXPORT BF-RENAME-TMP
EXPORT BF-RESET-OUT
EXPORT BF-RUN-ARGV-ENV-OUTFILE
EXPORT BF-RUN-ENV-TMP
EXPORT BF-RUN-STAGE
EXPORT BF-STAGE-FIXPOINT
EXPORT BF-STAGE-FIXPOINT-FROM-SOURCE
EXPORT BF-STAGE2-SOURCE

;package

\ Run the CLI with the package CLOSED. A build certifies its own generated
\ sources in this process (BF-CERTIFY-ACT calls VERIFY:SOURCE-BUF), and the
\ checker records and resolves names in whatever package scope is open when it
\ runs, so driving a build from inside BUILD-FIXPOINT would verify the emitted
\ engine source against this package's wordlist instead of the global one.
BUILD-FIXPOINT:BF-CLI-SELF-DISPATCH
