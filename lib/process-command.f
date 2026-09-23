\ process-command.f - checked command contexts and the one shared command.
\
\ STORAGE CLASS. `package CMD` is CALLER-OWNED. A command context is what
\ `CMD:COMMAND NAME` declares: a `PTR-U8-TABLE` whose cell 0 holds the address of
\ the byte region the same declaration allots, and whose remaining cells are the
\ child's argv and envp vectors. Every CMD word takes that handle, so any number
\ of tasks may each build and run their own command at once, and a caller may
\ pair its own storage with `CMD:BIND` instead of declaring it.
\
\ `package PROC-CMD` is PROCESS-WIDE. Its words are the same surface over one
\ static context, CTX0, so every caller that shares them is single-task, as it
\ always was. Neither package writes the process-wide argv/env staging of
\ lib/process-argv.f and lib/process-env.f: an owned run spawns from the
\ context's own vectors. See docs/threads.md.
\
\ External callers use the qualified public API: the command-builder words
\ (PROC-CMD:RESET, PROC-CMD:WIPE, PROC-CMD:ARG+, PROC-CMD:ENV+,
\ PROC-CMD:ENV-ENTRY+, PROC-CMD:ENV-HERMETIC, PROC-CMD:IN!, PROC-CMD:CWD!,
\ PROC-CMD:RUN-OUTCOME, PROC-CMD:RUN-RC, PROC-CMD:OUT$, PROC-CMD:ERR$,
\ PROC-CMD:OUTCOME@, PROC-CMD:RC@) and the same tails under CMD with the handle
\ deepest. Each package's storage and staging helpers are package-private.
require lib/errors.f
require lib/codegen.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/span.f

package CMD

public

255 constant ARG-MAX                     \ positional argv slots a command may carry
2048 constant ENV-ROWS                   \ envp cells: caller rows, defaults, inherited, the NULL

private

\ The vector. Cell 0 holds the byte region; the argv vector follows (slot 0 the
\ path, slots 1..ARG-MAX the arguments, one more for the NULL), then the envp
\ vector. A vector cell holds a `ptr u8`, which is why the whole context is a
\ declared PTR-U8-TABLE: a pointer stored in or fetched from an undeclared cell
\ is refused (E-RAW-CELL-PTR).
1 constant ARGV-SLOT
ARGV-SLOT ARG-MAX 2 + + constant ENVP-SLOT

public

ENVP-SLOT ENV-ROWS + constant VEC-CELLS

private

\ The byte region, every member at the width it had as a module buffer. The
\ counts are the leading cells, read through CELL-VIEW of the region's base:
\ `create ... allot` is zeroed and cell-aligned, so a fresh context starts at
\ zero and the view is aligned.
16 constant COUNT-CELLS
COUNT-CELLS cells constant PATHZ-OFF
PROC-PATHZ-CAP constant PATHZ-CAP
PATHZ-OFF PATHZ-CAP + constant CWDZ-OFF
PATHZ-CAP constant CWDZ-CAP
CWDZ-OFF CWDZ-CAP + constant ARG-BUF-OFF
PROC-ARGV-BUF-CAP constant ARG-BUF-CAP
ARG-BUF-OFF ARG-BUF-CAP + constant ENV-BUF-OFF
PROC-ENV-EXTRA-BYTES constant ENV-BUF-CAP
ENV-BUF-OFF ENV-BUF-CAP + constant IN-OFF
131072 constant IN-CAP
IN-OFF IN-CAP + constant OUT-OFF
32768 constant OUT-CAP
OUT-OFF OUT-CAP + constant ERR-OFF
32768 constant ERR-CAP

public

ERR-OFF ERR-CAP + constant BYTES

private

: BASE ( ptr ptr u8 -- ptr u8 )
   @ ;

: COUNT-CELL ( ptr ptr u8 n -- ptr n ) {: h:ptr i:n :}
   h BASE i cells + CELL-VIEW ;

\ The count cells, by index in the sixteen-cell header.
: ARG-N-CELL ( ptr ptr u8 -- ptr n ) 0 COUNT-CELL ;
: ENV-N-CELL ( ptr ptr u8 -- ptr n ) 1 COUNT-CELL ;
: ARG-OFF-CELL ( ptr ptr u8 -- ptr n ) 2 COUNT-CELL ;
: ENV-OFF-CELL ( ptr ptr u8 -- ptr n ) 3 COUNT-CELL ;
: IN-LEN-CELL ( ptr ptr u8 -- ptr n ) 4 COUNT-CELL ;
: CWD-LEN-CELL ( ptr ptr u8 -- ptr n ) 5 COUNT-CELL ;   \ 0: the child inherits this process's directory
: OUT-LEN-CELL ( ptr ptr u8 -- ptr n ) 6 COUNT-CELL ;
: ERR-LEN-CELL ( ptr ptr u8 -- ptr n ) 7 COUNT-CELL ;
: EXITED-CELL ( ptr ptr u8 -- ptr n ) 8 COUNT-CELL ;    \ 1: completed by exit (vs signal) when not timed out
: TIMED-OUT-CELL ( ptr ptr u8 -- ptr n ) 9 COUNT-CELL ; \ 1: capture deadline hit
: CODE-CELL ( ptr ptr u8 -- ptr n ) 10 COUNT-CELL ;     \ exit code or signal number; 0 for timeout
: RC-CELL ( ptr ptr u8 -- ptr n ) 11 COUNT-CELL ;
: INHERIT-CELL ( ptr ptr u8 -- ptr n ) 12 COUNT-CELL ;
\ Rows in the envp vector as PREPARE leaves it: the caller's own ENV-N rows plus
\ whatever inheritance appended for this run. ENV-N stays the caller's count, so
\ a second run inherits into the same place rather than after the last run's copy.
: ENV-FILL-CELL ( ptr ptr u8 -- ptr n ) 13 COUNT-CELL ;

: PATHZ ( ptr ptr u8 -- ptr u8 ) BASE PATHZ-OFF + ;
: CWDZ ( ptr ptr u8 -- ptr u8 ) BASE CWDZ-OFF + ;
: ARG-BUF ( ptr ptr u8 -- ptr u8 ) BASE ARG-BUF-OFF + ;
: ENV-BUF ( ptr ptr u8 -- ptr u8 ) BASE ENV-BUF-OFF + ;
: IN-BUF ( ptr ptr u8 -- ptr u8 ) BASE IN-OFF + ;
: OUT-BUF ( ptr ptr u8 -- ptr u8 ) BASE OUT-OFF + ;
: ERR-BUF ( ptr ptr u8 -- ptr u8 ) BASE ERR-OFF + ;

: ARGV-VEC ( ptr ptr u8 -- ptr ptr u8 ) {: h:ptr :} ARGV-SLOT cells h + ;
: ENVP-VEC ( ptr ptr u8 -- ptr ptr u8 ) {: h:ptr :} ENVP-SLOT cells h + ;

: ARGV-CELL ( ptr ptr u8 n -- ptr ptr u8 ) {: h:ptr i:n :}
   i 0 < i ARG-MAX 1 + > or if E-PROC-OUTPUT throw then
   i cells h ARGV-VEC + ;

: ENVP-CELL ( ptr ptr u8 n -- ptr ptr u8 ) {: h:ptr i:n :}
   i 0 < i ENV-ROWS 1 - > or if E-PROC-ENV throw then
   i cells h ENVP-VEC + ;

: CAPTURE-RESET ( ptr ptr u8 -- ) {: h:ptr :}
   0 h OUT-LEN-CELL !
   0 h ERR-LEN-CELL !
   1 h EXITED-CELL !
   0 h TIMED-OUT-CELL !
   0 h CODE-CELL !
   0 h RC-CELL ! ;

public

: RESET ( ptr ptr u8 -- ) {: h:ptr :}
   0 h ARG-N-CELL !
   0 h ARG-OFF-CELL !
   0 h ENV-N-CELL !
   0 h ENV-FILL-CELL !
   0 h ENV-OFF-CELL !
   0 h IN-LEN-CELL !
   0 h CWD-LEN-CELL !
   1 h INHERIT-CELL !
   h CAPTURE-RESET ;

\ Explicitly erase stdin and captures, including bytes beyond the live lengths.
\ RESET and RUN keep their existing lifetime; the caller chooses when to wipe.
: WIPE ( ptr ptr u8 -- ) {: h:ptr :}
   0 h IN-BUF IN-CAP SPAN:MAKE SPAN:FILL
   0 h OUT-BUF OUT-CAP SPAN:MAKE SPAN:FILL
   0 h ERR-BUF ERR-CAP SPAN:MAKE SPAN:FILL
   0 h IN-LEN-CELL !
   0 h OUT-LEN-CELL !
   0 h ERR-LEN-CELL ! ;

private

\ An argument is copied NUL-terminated into the context's ARG-BUF and its
\ address installed straight into the argv vector: no lib/process-argv.f
\ staging is on this path.
: ARG-ZCOPY ( ptr ptr u8 ptr u8 len -- ptr u8 ) {: h:ptr a:ptr u:len :}
   u LEN>N 0 < if E-PROC-OUTPUT throw then
   h ARG-OFF-CELL @ {: off:n :}
   off u LEN>N 1 + + ARG-BUF-CAP > if E-PROC-OUTPUT throw then
   a u h ARG-BUF off + ARG-BUF-CAP off - >LEN PROC-ZCOPY {: z:ptr :}
   off u LEN>N 1 + + h ARG-OFF-CELL !
   z ;

public

: ARG+ ( ptr ptr u8 ptr u8 len -- ) {: h:ptr a:ptr u:len :}
   h ARG-N-CELL @ ARG-MAX >= if E-PROC-OUTPUT throw then
   h a u ARG-ZCOPY h h ARG-N-CELL @ 1 + ARGV-CELL !
   h ARG-N-CELL @ 1 + h ARG-N-CELL ! ;

private

\ A row that would leave no cell for the envp NULL is refused, and the refusal
\ names the ceiling and the row on stderr the way lib/process-env.f's do. The
\ diagnostic buffer is that module's: one shared line renderer, used only on a
\ refusal path that ends in a throw.
: REPORT-FULL ( n -- ) {: saw:n :}
   PROC-ENV-DIAG CODEGEN:RESET
   s" process-command: command environment full at " PROC-ENV-DIAG+
   ENV-ROWS PROC-ENV-DIAG-N
   s"  rows: entry " PROC-ENV-DIAG+
   saw PROC-ENV-DIAG-N
   s"  refused" PROC-ENV-DIAG+
   PROC-ENV-DIAG-LINE ;

: ENV-CHECK-ROW ( n -- ) {: have:n :}
   have ENV-ROWS 1 - < if exit then
   have 1 + REPORT-FULL
   E-PROC-ENV throw ;

: ENV-CHECK-EXTRA ( ptr ptr u8 -- ) {: h:ptr :}
   h ENV-N-CELL @ ENV-CHECK-ROW ;

: ENV-INSTALL-Z ( ptr ptr u8 ptr u8 -- ) {: h:ptr z:ptr :}
   h ENV-CHECK-EXTRA
   z h h ENV-N-CELL @ ENVP-CELL !
   h ENV-N-CELL @ 1 + h ENV-N-CELL ! ;

: ENV-STORE-Z ( ptr ptr u8 ptr u8 len -- ptr u8 ) {: h:ptr a:ptr u:len :}
   u LEN>N 0 < if E-PROC-ENV throw then
   h ENV-OFF-CELL @ {: off:n :}
   off u LEN>N 1 + + ENV-BUF-CAP > if E-PROC-ENV throw then
   a h ENV-BUF off + u LEN>N BYTE-COPY
   0 h ENV-BUF off + u LEN>N + c!
   off u LEN>N 1 + + h ENV-OFF-CELL !
   h ENV-BUF off + ;

public

: ENV-ENTRY+ ( ptr ptr u8 ptr u8 len -- ) {: h:ptr a:ptr u:len :}
   a u PROC-ENV-CHECK-ENTRY
   h ENV-CHECK-EXTRA
   h h a u ENV-STORE-Z ENV-INSTALL-Z ;

: ENV+ ( ptr ptr u8 ptr u8 len ptr u8 len -- )
   {: h:ptr name:ptr nameu:len val:ptr valu:len :}
   name nameu PROC-ENV-CHECK-NAME
   valu LEN>N 0 < if E-PROC-ENV throw then
   h ENV-CHECK-EXTRA
   h ENV-OFF-CELL @ {: off:n :}
   off nameu LEN>N valu LEN>N + 2 + + ENV-BUF-CAP > if E-PROC-ENV throw then
   name h ENV-BUF off + nameu LEN>N BYTE-COPY
   PROC-ENV-EQUAL h ENV-BUF off + nameu LEN>N + c!
   val h ENV-BUF off + nameu LEN>N + 1 + valu LEN>N BYTE-COPY
   0 h ENV-BUF off + nameu LEN>N + 1 + valu LEN>N + c!
   h h ENV-BUF off + ENV-INSTALL-Z
   off nameu LEN>N valu LEN>N + 2 + + h ENV-OFF-CELL ! ;

: ENV-HERMETIC ( ptr ptr u8 -- ) {: h:ptr :}
   0 h INHERIT-CELL ! ;

private

: CWD-CHECK ( ptr u8 len -- ) {: a:ptr u:len :}
   u LEN>N 0 <= if E-PROC-PATH throw then
   u LEN>N CWDZ-CAP 1 - > if E-PROC-PATH throw then
   a u LEN>N DIR? 0= if E-PROC-PATH throw then ;

public

\ Run the child from this directory instead of the loader's. A missing path or a
\ non-directory is refused here, before anything is spawned; RESET clears it.
: CWD! ( ptr ptr u8 ptr u8 len -- ) {: h:ptr a:ptr u:len :}
   a u CWD-CHECK
   a u h CWDZ CWDZ-CAP >LEN PROC-ZCOPY drop
   u LEN>N h CWD-LEN-CELL ! ;

: IN! ( ptr ptr u8 ptr u8 len -- ) {: h:ptr a:ptr u:len :}
   u LEN>N 0 < if E-PROC-OUTPUT throw then
   u LEN>N IN-CAP > if E-PROC-OUTPUT throw then
   a h IN-BUF u LEN>N BYTE-COPY
   u LEN>N h IN-LEN-CELL ! ;

private

\ Inheritance appends ADDRESSES, never copies: a default row is read where
\ PROC-ENV-DEFAULT+ wrote it, in the process-wide read-only policy table, and a
\ parent row is read in the parent's own environment. Only the caller's own rows
\ occupy the context's ENV-BUF.
: ENV-HAS-NAME? ( ptr u8 len ptr ptr u8 -- bool ) {: a:ptr u:len h:ptr :}
   a u h ENVP-VEC h ENV-FILL-CELL @ PROC-ENV-VEC-HAS-NAME? ;

: ENV-INHERIT-Z ( ptr ptr u8 ptr u8 -- ) {: h:ptr z:ptr :}
   z z ZLEN >LEN h ENV-HAS-NAME? if exit then
   h ENV-FILL-CELL @ ENV-CHECK-ROW
   z h h ENV-FILL-CELL @ ENVP-CELL !
   h ENV-FILL-CELL @ 1 + h ENV-FILL-CELL ! ;

: ENV-INHERIT-DEFAULT-ONE ( n ptr ptr u8 -- ) {: i:n h:ptr :}
   h i >IDX PROC-ENV-DEF-SLOT @ ENV-INHERIT-Z ;

: ENV-INHERIT-DEFAULTS ( ptr ptr u8 -- ) {: h:ptr :}
   0 begin dup PROC-ENV-DEF-N @ COUNT>N < while
      dup h ENV-INHERIT-DEFAULT-ONE
      1 +
   repeat drop ;

: ENV-INHERIT-PARENT-ONE ( n ptr ptr u8 -- ) {: i:n h:ptr :}
   h i ENVP ENV-INHERIT-Z ;

: ENV-INHERIT-MISSING ( ptr ptr u8 -- ) {: h:ptr :}
   h ENV-INHERIT-DEFAULTS
   0 begin dup ENVP 0= 0= while
      dup h ENV-INHERIT-PARENT-ONE
      1 +
   repeat drop ;

\ The vectors the child receives: argv slot 0 is the path and slot n+1 the NULL,
\ envp ends at the NULL after the last row. Both terminators are written here,
\ so a refused run leaves no half-built vector behind.
: PREPARE ( ptr ptr u8 ptr u8 len -- ) {: h:ptr path:ptr pathu:len :}
   path pathu h PATHZ PATHZ-CAP >LEN PROC-ZCOPY h 0 ARGV-CELL !
   NULL-PTR h h ARG-N-CELL @ 1 + ARGV-CELL !
   h ENV-N-CELL @ h ENV-FILL-CELL !
   h INHERIT-CELL @ 0<> if h ENV-INHERIT-MISSING then
   NULL-PTR h h ENV-FILL-CELL @ ENVP-CELL ! ;

: CHECK-RUN ( ptr u8 len ms -- ) {: path:ptr pathu:len timeout:ms :}
   path pathu PROC-ARGV-CHECK-PATH
   timeout MS>N 0 < if E-PROC-TIMEOUT throw then ;

\ The spawn cores take the context's own vectors and reset no process-wide
\ staging; the capture machinery around them is lib/process.f's task-local row.
: SPAWN ( ptr ptr u8 -- ) {: h:ptr :}
   h CWD-LEN-CELL @ 0 > if
      h PATHZ h ARGV-VEC h ENVP-VEC h CWDZ
      PROC-CWD:SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE-CORE
   else
      h PATHZ h ARGV-VEC h ENVP-VEC PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE-CORE
   then
   PROC-CAPTURE-ADOPT-SPAWN ;

\ Decompose the outcome sum into exited/timed-out flags plus the code cell
\ (all one cell, lossless): exit codes >= 128 stay distinct from signal
\ deaths, unlike the retired rc-based pair store.
: STORE-RUN ( len len outcome ptr ptr u8 -- ) {: h:ptr :}
   MATCH outcome
     exited OF h CODE-CELL ! 1 h EXITED-CELL ! 0 h TIMED-OUT-CELL ! ENDOF
     signaled OF h CODE-CELL ! 0 h EXITED-CELL ! 0 h TIMED-OUT-CELL ! ENDOF
     timeout OF 0 h CODE-CELL ! 0 h EXITED-CELL ! 1 h TIMED-OUT-CELL ! ENDOF
   ;MATCH
   LEN>N h ERR-LEN-CELL !
   LEN>N h OUT-LEN-CELL ! ;

public

: OUTCOME@ ( ptr ptr u8 -- outcome ) {: h:ptr :}
   h TIMED-OUT-CELL @ 0<> if OUTCOME:TIMEOUT exit then
   h EXITED-CELL @ 0<> if h CODE-CELL @ OUTCOME:EXITED exit then
   h CODE-CELL @ OUTCOME:SIGNALED ;

: RUN-OUTCOME ( ptr ptr u8 ptr u8 len ms -- outcome )
   {: h:ptr path:ptr pathu:len timeout:ms :}
   path pathu timeout CHECK-RUN
   h CAPTURE-RESET
   h path pathu PREPARE
   timeout PROC-STDIN-CAPTURE-BEGIN
   h SPAWN
   h IN-BUF h IN-LEN-CELL @ >LEN
   h OUT-BUF OUT-CAP >LEN
   h ERR-BUF ERR-CAP >LEN PROC-RUN-STDIN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME h STORE-RUN
   h OUTCOME@ dup PROC-OUTCOME>RC RC>N h RC-CELL ! ;

\ Wrap the stored completion rc into a result<n,n>: ok = clean exit (0), err =
\ the nonzero completion code (a nonzero exit code, or 128+signal). The captured
\ output stays in the context's own buffers (read via OUT$/ERR$), so the return
\ carries only the code - no capture product here, unlike the RUN-*-CAPTURE
\ words that return the lengths.
private

: RC>RESULT ( n -- result<n,n> ) {: rc:n :}
   rc 0 = if rc RESULT:OK else rc RESULT:ERR then ;

public

: RC@ ( ptr ptr u8 -- result<n,n> ) {: h:ptr :}
   h RC-CELL @ RC>RESULT ;

: RUN-RC ( ptr ptr u8 ptr u8 len ms -- result<n,n> )
   {: h:ptr path:ptr pathu:len timeout:ms :}
   h path pathu timeout RUN-OUTCOME drop
   h RC@ ;

: OUT$ ( ptr ptr u8 -- ptr u8 n ) {: h:ptr :}
   h OUT-BUF h OUT-LEN-CELL @ ;

: ERR$ ( ptr ptr u8 -- ptr u8 n ) {: h:ptr :}
   h ERR-BUF h ERR-LEN-CELL @ ;

\ Bind a byte region to a vector and start it reset. Public so a caller may pair
\ its own storage - a MEM:ALLOC-BYTES region of BYTES bytes and a
\ MEM-ALLOC-CELLS vector of VEC-CELLS cells - for a command that is not declared.
: BIND ( ptr u8 ptr ptr u8 -- ) {: b:ptr h:ptr :}
   b h !
   h RESET ;

private

$200 CODEGEN:BUFFER GEN

: GEN+ ( ptr u8 n -- )
   GEN CODEGEN:APPEND-STRING ;

: COMMAND-NAME ( -- ptr u8 n )
   parse-name dup 0= if E-PROC-OUTPUT throw then ;

public

\ `CMD:COMMAND NAME` declares one command context and publishes NAME as its
\ handle. The source is built in a CODEGEN buffer and handed to the loader's
\ audited evaluate, lib/task.f `+USER`'s shape, because a definer cannot both
\ parse a name and run two storage definers on it:
\
\    CMD:VEC-CELLS PTR-U8-TABLE NAME#VEC
\    create NAME#BUF CMD:BYTES allot
\    : NAME ( -- ptr ptr u8 ) NAME#VEC ;
\    NAME#BUF NAME#VEC CMD:BIND
\
\ A name this definer generates is invisible to tools/check.f: its preverify
\ (VERIFY:SOURCE-BUF-IN-SCOPE) reads the source without running the load-time
\ INCLUDE-EVALUATE, so a later mention of NAME refuses E-UNDEFINED unless the
\ engine image already carries NAME. Measured on a source the engine does not
\ carry, lib/task.f's `+USER` row refuses the same way; a command context is
\ verified through the real load path, lib/process-command-test.f.
: COMMAND ( -- )
   COMMAND-NAME {: name:ptr nameu:n :}
   GEN CODEGEN:RESET
   s" CMD:VEC-CELLS PTR-U8-TABLE " GEN+
   name nameu GEN+
   s\" #VEC\ncreate " GEN+
   name nameu GEN+
   s\" #BUF CMD:BYTES allot\n: " GEN+
   name nameu GEN+
   s"  ( -- ptr ptr u8 ) " GEN+
   name nameu GEN+
   s\" #VEC ;\n" GEN+
   name nameu GEN+
   s" #BUF " GEN+
   name nameu GEN+
   s" #VEC CMD:BIND" GEN+
   GEN CODEGEN:CONTENTS INCLUDE-EVALUATE ;

;package

package PROC-CMD

public

CMD:ARG-MAX constant ARG-MAX             \ public: max positional argv slots a command may carry

private

\ The one shared command every PROC-CMD word works on.
CMD:COMMAND CTX0

public

: RESET ( -- )
   CTX0 CMD:RESET ;

: WIPE ( -- )
   CTX0 CMD:WIPE ;

: ARG+ ( ptr u8 len -- ) {: a:ptr u:len :}
   CTX0 a u CMD:ARG+ ;

: ENV-ENTRY+ ( ptr u8 len -- ) {: a:ptr u:len :}
   CTX0 a u CMD:ENV-ENTRY+ ;

: ENV+ ( ptr u8 len ptr u8 len -- ) {: name:ptr nameu:len val:ptr valu:len :}
   CTX0 name nameu val valu CMD:ENV+ ;

: ENV-HERMETIC ( -- )
   CTX0 CMD:ENV-HERMETIC ;

: CWD! ( ptr u8 len -- ) {: a:ptr u:len :}
   CTX0 a u CMD:CWD! ;

: IN! ( ptr u8 len -- ) {: a:ptr u:len :}
   CTX0 a u CMD:IN! ;

: OUTCOME@ ( -- outcome )
   CTX0 CMD:OUTCOME@ ;

: RUN-OUTCOME ( ptr u8 len ms -- outcome ) {: path:ptr pathu:len timeout:ms :}
   CTX0 path pathu timeout CMD:RUN-OUTCOME ;

: RUN-RC ( ptr u8 len ms -- result<n,n> ) {: path:ptr pathu:len timeout:ms :}
   CTX0 path pathu timeout CMD:RUN-RC ;

: OUT$ ( -- ptr u8 n )
   CTX0 CMD:OUT$ ;

: ERR$ ( -- ptr u8 n )
   CTX0 CMD:ERR$ ;

: RC@ ( -- result<n,n> )
   CTX0 CMD:RC@ ;

;package
