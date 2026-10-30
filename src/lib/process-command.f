\ process-command.f - scoped child-process commands under a linear token.
\
\ STORAGE CLASS. TASK-LOCAL. `CMD:WITH-COMMAND` allocates one command context,
\ hands the body a linear `CMD:command` naming it, and releases the context and
\ everything it grew when the body returns or throws. Every member grows: the
\ argument, environment and stdin bytes, the argv/envp vectors built from them,
\ and the stdout/stderr captures. No caller-visible ceiling remains.
\
\ The open contexts of a task form a chain through each context's OUTER cell,
\ headed by the task's INNER cell; RUNNING names the context whose run is in
\ progress, so the capture sinks (quotations, which carry no data) find it.
\ Scopes nest, and tasks never share a context. A halted task unwinds through
\ each scope's `finally` as a throw does, so the scope closes its context then too.
\
\ The body hands back the token it was given; any other, such as an outer
\ scope's token swapped in, is refused with CMD:E-HANDLE. Either way the scope
\ closes its own context, so each scope frees exactly the context it opened.
\
\ A throw out of the body loses the token with the stack, and the cleanup frees
\ by pointer, so `catch` belongs outside the scope word.
\
\ A run inherits this process's environment and the PROC-ENV defaults unless
\ ENV-HERMETIC turns that off; UNSETENV keeps one named variable from both.
\ No word here writes the process-wide argv/env staging of src/lib/process-argv.f
\ and src/lib/process-env.f: a run spawns from the context's own vectors.
\
\ CANCELLATION. CMD:CANCEL-FD! lends a pipe's read end to the next run. A byte
\ or a closed writer cancels it: the run kills the child's tree, reaps the
\ child, closes its captures and throws E-PROC-CANCELLED. A completion already
\ collected wins, and PROC-STOP is answered first. The lent descriptor is never
\ read or closed, so a byte left from one request cancels the next run: drain
\ or replace the pipe before lending it again.
\
\ PLACEMENT. `child parent FD+` makes the child's fd `child` this process's fd
\ `parent` in every run until RESET or the scope's end, applied after the stdio
\ slots as PROCESS:FD applies it (src/lib/spawn.f). Each scope holds its own
\ placements; an outer scope's runs never see an inner one's. The parent keeps
\ ownership: CMD never closes a placed fd, so it must stay open through each run.
\ A run refuses a closed one before it opens its own descriptors, which could
\ take the closed number: PROCESS:CHECK-FD throws E-PROC-SPAWN with EBADF in
\ PROCESS:ERRNO, and no child runs.
\
\ A run that throws, cancelled or not, keeps its code: OUTCOME@ and RC@ throw it
\ again until the next run or RESET. A path, deadline or closed placed fd refused
\ before the run starts leaves the previous outcome in place.
\
\ RUN is the run that must exit clean: any other completion throws the caller's
\ nonzero code after the captured stderr is copied to this process's, and a
\ deadline kill throws E-PROC-TIMEOUT. As in RUN-OUTCOME, a spawn failure, a
\ refused path, deadline or placed fd and cancellation throw their own codes
\ before any run completes.
\
\ OUT$ and ERR$ are borrowed until the next run, RESET or the end of the scope.
require src/lib/errors.f
require src/lib/memory.f
require src/lib/task.f
require src/lib/fs.f
require src/lib/fd.f
require src/lib/byte-buffer.f
require src/lib/vector.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/spawn.f

\ Scoped commands: -9580..-9589, minted here beside the package its codes belong
\ to (docs/forth-card.md § 6).
-9580 constant E-CMD-FIRST
-9589 constant E-CMD-LAST

package CMD

public

-9580 constant E-HANDLE   \ a WITH-COMMAND body handed back a token other than the one it was given

DEFLINEAR CMD:command

private

\ ---- task-local chain ---------------------------------------------------------
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 2 cells TASK:+USER CMD-TASK drop

: INNER ( -- ptr ptr n ) CMD-TASK 0 ptr-field ;
: RUNNING ( -- ptr ptr n ) CMD-TASK 1 ptr-field ;

\ ---- the context record -------------------------------------------------------
\ Header cells, then the PATHZ and CWDZ byte regions. A member that holds an
\ address is a typed `ptr-field`; a pointer kept in an undeclared cell is refused.
: OUTER ( ptr n -- ptr ptr n ) 0 ptr-field ;
: INHERIT ( ptr n -- ptr n ) 1 cells + ;
: CWD-LEN ( ptr n -- ptr n ) 2 cells + ;       \ 0: the child inherits this process's directory
: EXITED ( ptr n -- ptr n ) 3 cells + ;        \ 1: completed by exit (vs signal) when not timed out
: TIMED-OUT ( ptr n -- ptr n ) 4 cells + ;     \ 1: capture deadline hit
: CODE ( ptr n -- ptr n ) 5 cells + ;          \ exit code or signal number; 0 for timeout
: CANCEL-FD ( ptr n -- ptr n ) 6 cells + ;
: FAILED ( ptr n -- ptr n ) 7 cells + ;        \ the last run's throw code; 0: it completed
: OUT-LEN ( ptr n -- ptr n ) 8 cells + ;
: ERR-LEN ( ptr n -- ptr n ) 9 cells + ;
: OUT-BASE ( ptr n -- ptr ptr u8 ) 10 ptr-field ;
: OUT-CAP ( ptr n -- ptr n ) 11 cells + ;
: ERR-BASE ( ptr n -- ptr ptr u8 ) 12 ptr-field ;
: ERR-CAP ( ptr n -- ptr n ) 13 cells + ;
: ARG-BUF ( ptr n -- ptr n ) 14 cells + ;      \ NUL-terminated arguments, back to back
: ENV-BUF ( ptr n -- ptr n ) 17 cells + ;      \ NUL-terminated caller rows, back to back
: IN-BUF ( ptr n -- ptr n ) 20 cells + ;
: ARGV-VEC ( ptr n -- ptr ptr u8 ) 23 ptr-field ;  \ the child's argv, built by PREPARE
: ENVP-VEC ( ptr n -- ptr ptr u8 ) 26 ptr-field ;  \ the child's envp, built by PREPARE
: ARG-OFF ( ptr n -- ptr n ) 29 cells + ;      \ each argument's offset in ARG-BUF
: ENV-OFF ( ptr n -- ptr n ) 32 cells + ;      \ each caller row's offset in ENV-BUF
: PLACED ( ptr n -- ptr n ) 35 cells + ;       \ FD+'s child/parent pairs, back to back
: UNSET-BUF ( ptr n -- ptr n ) 38 cells + ;    \ UNSETENV's NUL-terminated names, back to back
: UNSET-OFF ( ptr n -- ptr n ) 41 cells + ;    \ each unset name's offset in UNSET-BUF
44 constant HEAD-CELLS

PROC-PATHZ-CAP constant PATHZ-CAP
HEAD-CELLS PATHZ-CAP 2 * 7 + 8 / + constant CTX-CELLS

: PATHZ ( ptr n -- ptr u8 ) BYTE-VIEW HEAD-CELLS cells + ;
: CWDZ ( ptr n -- ptr u8 ) PATHZ PATHZ-CAP + ;

\ Growing members start small; the first byte, row or capture grows them.
64 constant BYTES-START
16 constant ITEMS-START

CAST: ITEM>N ( NUM:item-count -- n )

: N>ITEM ( n -- NUM:item-count )
   NUM:ITEM-COUNT
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;

: BUF-LEN ( ptr a -- n ) BUF:LEN@ BUF:BLEN>N ;
: BUF-BASE ( ptr a -- ptr u8 ) BUF:SPAN$ drop ;

LINEAR: MINT ( ptr n -- CMD:command )
LINEAR: ERASE ( CMD:command -- ptr n )

: CTX ( CMD:command -- CMD:command ptr n )
   ERASE dup MINT swap ;

: CAPTURE-RESET ( ptr n -- ) {: h:ptr :}
   0 h OUT-LEN !
   0 h ERR-LEN !
   0 h FAILED !
   1 h EXITED !
   0 h TIMED-OUT !
   0 h CODE ! ;

: CLEAR-BUILD ( ptr n -- )
   {: h:ptr :}
   h ARG-BUF BUF:CLEAR
   h ENV-BUF BUF:CLEAR
   h IN-BUF BUF:CLEAR
   h ARG-OFF VEC:CLEAR
   h ENV-OFF VEC:CLEAR
   h PLACED VEC:CLEAR
   h UNSET-BUF BUF:CLEAR
   h UNSET-OFF VEC:CLEAR
   0 h CWD-LEN !
   1 h INHERIT !
   PROC-NO-FD h CANCEL-FD !
   h CAPTURE-RESET ;

\ ---- opening and closing --------------------------------------------------------
: INIT-MEMBERS ( ptr n -- )
   {: h:ptr :}
   h ARG-BUF BYTES-START BUF:N>BLEN BUF:INIT
   h ENV-BUF BYTES-START BUF:N>BLEN BUF:INIT
   h IN-BUF BYTES-START BUF:N>BLEN BUF:INIT
   h ARGV-VEC ITEMS-START N>ITEM VEC:INIT
   h ENVP-VEC ITEMS-START N>ITEM VEC:INIT
   h ARG-OFF ITEMS-START N>ITEM VEC:INIT
   h ENV-OFF ITEMS-START N>ITEM VEC:INIT
   h PLACED ITEMS-START N>ITEM VEC:INIT
   h UNSET-BUF BYTES-START BUF:N>BLEN BUF:INIT
   h UNSET-OFF ITEMS-START N>ITEM VEC:INIT
   h CLEAR-BUILD ;

: RELEASE-REGION ( ptr u8 n -- ) {: base:ptr cap:n :}
   cap 0= if exit then
   base cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

\ Pop the context first, so a failing disposer cannot leave a freed context on
\ the chain; a member never initialised is a dead header, which DISPOSE ignores.
: CLOSE ( -- )
   INNER @ {: h:ptr :}
   h OUTER @ INNER !
   h ARG-BUF BUF:DISPOSE
   h ENV-BUF BUF:DISPOSE
   h IN-BUF BUF:DISPOSE
   h ARGV-VEC VEC:DISPOSE
   h ENVP-VEC VEC:DISPOSE
   h ARG-OFF VEC:DISPOSE
   h ENV-OFF VEC:DISPOSE
   h PLACED VEC:DISPOSE
   h UNSET-BUF BUF:DISPOSE
   h UNSET-OFF VEC:DISPOSE
   h OUT-BASE @ h OUT-CAP @ RELEASE-REGION
   h ERR-BASE @ h ERR-CAP @ RELEASE-REGION
   h CTX-CELLS MEM:CELLS-ALLOC-COUNT MEM:RELEASE-CELLS ;

\ Everything that can throw runs here, inside the region CLOSE protects.
: RUN-BODY ( R [ R CMD:command -- S CMD:command ] -- S )
   INNER @ INIT-MEMBERS
   >r INNER @ MINT r> execute ERASE INNER @ <> if E-HANDLE throw then ;

\ Allocation and linking only, so nothing between them can throw.
: OPEN ( -- )
   CTX-CELLS MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: h:ptr :}
   INNER @ h OUTER !
   h INNER ! ;

public

\ Run the body with a fresh command and release the command, with everything it
\ grew, when the body returns or throws.
: WITH-COMMAND ( R [ R CMD:command -- S CMD:command ] -- S )
   OPEN
   [: RUN-BODY ;] [: CLOSE ;] finally ;

\ Forget the arguments, rows, unset names, stdin, directory, placements, lent
\ descriptor and last outcome; the storage stays for reuse.
: RESET ( CMD:command -- CMD:command )
   CTX CLEAR-BUILD ;

\ Lend a pipe's read end until the next run returns or throws. Another task
\ writes or closes its writer; it never mutates a running command. PROC-NO-FD
\ disconnects. The owner must keep the read end open through run completion.
: CANCEL-FD! ( CMD:command fd -- CMD:command ) {: fd:fd :}
   CTX {: h:ptr :}
   fd FD>N h CANCEL-FD ! ;

private

\ Record where the next NUL-terminated entry starts; PREPARE resolves offsets to
\ addresses once the bytes stop moving.
: MARK ( ptr n ptr n -- ) {: buf:ptr offs:ptr :}
   buf BUF-LEN offs VEC:PUSH drop ;

public

: ARG+ ( CMD:command ptr u8 n -- CMD:command ) {: a:ptr u:n :}
   CTX {: h:ptr :}
   u 0 < if E-PROC-OUTPUT throw then
   h ARG-BUF h ARG-OFF MARK
   a u BUF:N>BLEN h ARG-BUF BUF:APPEND-SPAN
   0 h ARG-BUF BUF:APPEND-BYTE ;

: ENV-ENTRY+ ( CMD:command ptr u8 n -- CMD:command ) {: a:ptr u:n :}
   CTX {: h:ptr :}
   u 0 < if E-PROC-ENV throw then
   a u >LEN PROC-ENV-CHECK-ENTRY
   h ENV-BUF h ENV-OFF MARK
   a u BUF:N>BLEN h ENV-BUF BUF:APPEND-SPAN
   0 h ENV-BUF BUF:APPEND-BYTE ;

: ENV+ ( CMD:command ptr u8 n ptr u8 n -- CMD:command )
   {: name:ptr nameu:n val:ptr valu:n :}
   CTX {: h:ptr :}
   nameu 0 < valu 0 < or if E-PROC-ENV throw then
   name nameu >LEN PROC-ENV-CHECK-NAME
   h ENV-BUF h ENV-OFF MARK
   name nameu BUF:N>BLEN h ENV-BUF BUF:APPEND-SPAN
   PROC-ENV-EQUAL h ENV-BUF BUF:APPEND-BYTE
   val valu BUF:N>BLEN h ENV-BUF BUF:APPEND-SPAN
   0 h ENV-BUF BUF:APPEND-BYTE ;

: ENV-HERMETIC ( CMD:command -- CMD:command )
   CTX 0 swap INHERIT ! ;


\ The next runs' child inherits no variable of this name, neither from this
\ process's environment nor from the PROC-ENV defaults, until RESET. A caller row
\ with the same name (ENV+, ENV-ENTRY+) still reaches the child: explicit beats
\ unset. Under ENV-HERMETIC nothing is inherited, so UNSETENV changes nothing.
\ An empty name, a name holding `=` or NUL, or a negative length: E-PROC-ENV.
: UNSETENV ( CMD:command ptr u8 n -- CMD:command )
   {: a:ptr u:n :}
   CTX {: h:ptr :}
   a u >LEN PROC-ENV-CHECK-NAME
   h UNSET-BUF h UNSET-OFF MARK
   a u BUF:N>BLEN h UNSET-BUF BUF:APPEND-SPAN
   0 h UNSET-BUF BUF:APPEND-BYTE ;

private

: CWD-CHECK ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 <= if E-PROC-PATH throw then
   u PATHZ-CAP 1 - > if E-PROC-PATH throw then
   a u FS:DIR? 0= if E-PROC-PATH throw then ;

public

\ Run the child from this directory instead of the loader's. A missing path or a
\ non-directory is refused here, before anything is spawned; RESET clears it.
: CWD! ( CMD:command ptr u8 n -- CMD:command ) {: a:ptr u:n :}
   CTX {: h:ptr :}
   a u CWD-CHECK
   a u >LEN h CWDZ PATHZ-CAP >LEN PROC-ZCOPY drop
   u h CWD-LEN ! ;

: IN! ( CMD:command ptr u8 n -- CMD:command ) {: a:ptr u:n :}
   CTX {: h:ptr :}
   u 0 < if E-PROC-OUTPUT throw then
   a u BUF:N>BLEN h IN-BUF BUF:REPLACE ;


\ The child's fd child is the parent's fd parent in the next runs (PLACEMENT
\ above). Either negative: E-PROC-OUTPUT.
: FD+ ( CMD:command fd fd -- CMD:command )
   {: child:fd parent:fd :}
   CTX {: h:ptr :}
   child FD>N 0 < parent FD>N 0 < or if E-PROC-OUTPUT throw then
   child FD>N h PLACED VEC:PUSH drop
   parent FD>N h PLACED VEC:PUSH drop ;

private

\ ---- the child's vectors ----------------------------------------------------------
: ARG-ADDR+ ( ptr n NUM:index n -- ptr n ) {: off:n :} drop {: h:ptr :}
   h ARG-BUF BUF-BASE off + h ARGV-VEC VEC:PUSH drop
   h ;

: ENV-ADDR+ ( ptr n NUM:index n -- ptr n ) {: off:n :} drop {: h:ptr :}
   h ENV-BUF BUF-BASE off + h ENVP-VEC VEC:PUSH drop
   h ;


: UNSET-NAME ( ptr n n -- ptr u8 len )
   {: h:ptr i:n :}
   h UNSET-BUF BUF-BASE h UNSET-OFF i VEC:N>INDEX VEC:@ + dup ZLEN >LEN ;


: UNSET? ( ptr n ptr u8 -- bool )
   {: h:ptr z:ptr :}
   0 begin dup h UNSET-OFF VEC:LEN@ ITEM>N < while
      h over UNSET-NAME z z ZLEN >LEN PROC-ENV-SAME-NAME? if drop PROC-ENV-TRUE exit then
      1+
   repeat drop PROC-ENV-FALSE ;


\ Inheritance appends ADDRESSES, never copies: a default row is read where
\ PROC-ENV-DEFAULT+ wrote it, in the process-wide read-only policy table, and a
\ parent row is read in the parent's own environment. A row whose name is already
\ present or unset is skipped; the vector is re-read for each check because PUSH
\ may move it. Caller rows are pushed before inheritance and never checked against
\ the unset names, so an explicit row beats UNSETENV.
: INHERIT-Z ( ptr n ptr u8 -- )
   {: h:ptr z:ptr :}
   z z ZLEN >LEN h ENVP-VEC VEC:DATA@ h ENVP-VEC VEC:LEN@ ITEM>N PROC-ENV-VEC-HAS-NAME?
   if exit then
   h z UNSET? if exit then
   z h ENVP-VEC VEC:PUSH drop ;


: INHERIT-DEFAULTS ( ptr n -- ) {: h:ptr :}
   0 begin dup PROC-ENV-DEF-N @ COUNT>N < while
      h over >IDX PROC-ENV-DEF-SLOT @ INHERIT-Z
      1 +
   repeat drop ;

: INHERIT-PARENT ( ptr n -- ) {: h:ptr :}
   0 begin dup ENVP 0= 0= while
      h over ENVP INHERIT-Z
      1 +
   repeat drop ;

\ argv: the path, each argument, the NULL. envp: the caller's rows, then the
\ inherited ones, the NULL. Both are rebuilt for every run.
: PREPARE ( ptr n ptr u8 n -- )
   {: h:ptr path:ptr pathu:n :}
   path pathu >LEN h PATHZ PATHZ-CAP >LEN PROC-ZCOPY {: z:ptr :}
   h ARGV-VEC VEC:CLEAR
   z h ARGV-VEC VEC:PUSH drop
   h h ARG-OFF [: ARG-ADDR+ ;] VEC:EACH drop
   NULL-PTR h ARGV-VEC VEC:PUSH drop
   h ENVP-VEC VEC:CLEAR
   h h ENV-OFF [: ENV-ADDR+ ;] VEC:EACH drop
   h INHERIT @ 0<> if h INHERIT-DEFAULTS h INHERIT-PARENT then
   NULL-PTR h ENVP-VEC VEC:PUSH drop ;

: CHECK-RUN ( ptr u8 n ms -- )
   {: path:ptr pathu:n timeout:ms :}
   path pathu >LEN PROC-ARGV-CHECK-PATH
   timeout MS>N 0 < if E-PROC-TIMEOUT throw then ;


: PAIRS ( ptr n -- ptr n n )
   {: h:ptr :}
   h PLACED VEC:DATA@ h PLACED VEC:LEN@ ITEM>N 2 / ;


\ The pair at idx of PLACED's array: the child's fd and the parent's.
: PAIR ( ptr n n -- fd fd )
   2 * cells + {: p:ptr :}
   p @ >FD p cell+ @ >FD ;


: CHECK-PLACED ( ptr n -- )
   PAIRS 0 ?do dup i PAIR nip PROCESS:CHECK-FD loop drop ;


\ The capture pipes at the child's 0, 1 and 2.
: STDIO ( PROCESS:request -- PROCESS:request )
   0 >FD PROC-IN-R @ >FD PROCESS:FD
   1 >FD PROC-OUT-W @ >FD PROCESS:FD
   2 >FD PROC-ERR-W @ >FD PROCESS:FD ;


: PLACE ( PROCESS:request ptr n -- PROCESS:request )
   PAIRS {: fds:ptr n :}
   n 0 ?do fds i PAIR PROCESS:FD loop ;


\ The run's request from the context's own storage; FD+'s placements apply after
\ the stdio slots.
: REQUEST ( ptr n -- PROCESS:request )
   {: h:ptr :}
   h PATHZ dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   h ARGV-VEC VEC:DATA@ PROCESS:ARGV-ROWS
   h ENVP-VEC VEC:DATA@ PROCESS:ENV-ROWS
   h CWD-LEN @ 0 > if h CWDZ h CWD-LEN @ PROCESS:CWD then
   STDIO
   h PLACE ;


\ A refused request or spawn closes the capture descriptors before its code goes
\ on (PROC-THROW-CAPTURE); a child becomes this task's capture child.
: SPAWN ( ptr n -- )
   0 >PID [: drop dup REQUEST PROCESS:SPAWN ;] catch {: rc:n :}
   rc 0<> if drop drop rc PROC-THROW-CAPTURE then
   nip PROC-CAPTURE-ADOPT-SPAWN ;


\ ---- captures ---------------------------------------------------------------------
\ process.f's growing loop asks for room for `need` bytes and keeps the length
\ itself; the bytes already captured are carried into the larger region.
: GROWN ( n n -- n ) {: need:n cap:n :}
   cap 2 * need max ;

: ROOM ( n ptr ptr u8 ptr n n -- ptr u8 ) {: need:n base:ptr cap:ptr len:n :}
   need cap @ <= if base @ exit then
   need cap @ GROWN {: size:n :}
   size MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: fresh:ptr :}
   base @ fresh len BYTE-COPY
   base @ cap @ RELEASE-REGION
   fresh base !
   size cap !
   fresh ;

: OUT-ROOM ( n -- ptr u8 ) RUNNING @ {: h:ptr :} h OUT-BASE h OUT-CAP PROC-OUT-LEN @ ROOM ;
: ERR-ROOM ( n -- ptr u8 ) RUNNING @ {: h:ptr :} h ERR-BASE h ERR-CAP PROC-ERR-LEN @ ROOM ;

\ Decompose the outcome sum into exited/timed-out flags plus the code cell
\ (all one cell, lossless): exit codes >= 128 stay distinct from signal deaths.
: STORE-OUTCOME ( outcome ptr n -- )
   {: h:ptr :}
   MATCH outcome
     exited OF h CODE ! 1 h EXITED ! 0 h TIMED-OUT ! ENDOF
     signaled OF h CODE ! 0 h EXITED ! 0 h TIMED-OUT ! ENDOF
     timeout OF 0 h CODE ! 0 h EXITED ! 1 h TIMED-OUT ! ENDOF
   ;MATCH ;

\ The command disconnects at entry; release the capture's borrowed reference
\ after its cleanup. A throw has no outcome variant: the getters throw its code.
: RUN-DONE ( n ptr n -- )
   {: code:n h:ptr :}
   PROC-NO-FD PROC-CANCEL-FD !
   code h FAILED !
   code 0<> if code throw then ;

: START-CANCEL ( fd -- )
   FD>N PROC-CANCEL-FD !
   PROC-STOP-ASK ;

: CAPTURE ( ptr n fd ms -- ptr n fd ms )
   {: h:ptr cancel:fd timeout:ms :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   cancel START-CANCEL
   h SPAWN
   h IN-BUF BUF-BASE h IN-BUF BUF-LEN >LEN
   [: OUT-ROOM ;] [: ERR-ROOM ;] PROC-RUN-STDIN-GROWING-BOTH-CAPTURE-OUTCOME-LOOP
   h cancel timeout ;

: OUTCOME-OF ( ptr n -- outcome ) {: h:ptr :}
   h FAILED @ dup 0<> if throw then drop
   h TIMED-OUT @ 0<> if OUTCOME:TIMEOUT exit then
   h EXITED @ 0<> if h CODE @ OUTCOME:EXITED exit then
   h CODE @ OUTCOME:SIGNALED ;

\ A refused path, deadline or closed placed fd throws before CAPTURE-RESET
\ forgets the last run, so a refused run keeps the previous outcome; from the
\ reset on, every throw reaches RUN-DONE. CHECK-PLACED also runs before CAPTURE
\ opens the capture pipes, which could take a closed placed fd's number and hand
\ the child a capture pipe in its place. RUNNING names the context only while its
\ capture runs, and is cleared on every path out, so no task row keeps a
\ context's address past its run.
: RUN-CAPTURE ( ptr n ptr u8 n ms -- )
   {: h:ptr path:ptr pathu:n timeout:ms :}
   h CANCEL-FD @ >FD {: cancel:fd :}
   PROC-NO-FD h CANCEL-FD !
   path pathu timeout CHECK-RUN
   h CHECK-PLACED
   h path pathu PREPARE
   h CAPTURE-RESET
   h RUNNING !
   h cancel timeout [: CAPTURE ;] catch {: code:n :} drop 2drop
   NULL-PTR RUNNING !
   PROC-OUT-LEN @ h OUT-LEN !
   PROC-ERR-LEN @ h ERR-LEN !
   code h RUN-DONE
   PROC-CAPTURE-FINISH-OUTCOME h STORE-OUTCOME 2drop ;

: RC>RESULT ( n -- result<n,n> ) {: rc:n :}
   rc 0 = if rc RESULT:OK else rc RESULT:ERR then ;

public

: OUTCOME@ ( CMD:command -- CMD:command outcome )
   CTX OUTCOME-OF ;

\ Run the prepared command at path, feed it the stdin and capture both streams
\ whole; a deadline-killed run answers OUTCOME:TIMEOUT.
: RUN-OUTCOME ( CMD:command ptr u8 n ms -- CMD:command outcome )
   {: path:ptr pathu:n timeout:ms :}
   CTX {: h:ptr :}
   h path pathu timeout RUN-CAPTURE
   h OUTCOME-OF ;

\ The stored outcome's completion rc as a result<n,n>: ok = clean exit (0), err =
\ the nonzero completion code (a nonzero exit code, or 128+signal). A run its
\ deadline killed has no completion code: RC@ and RUN-RC throw E-PROC-TIMEOUT for
\ it (PROC-OUTCOME>RC), and only a caller that acts on the deadline reads it as
\ data through OUTCOME@ or RUN-OUTCOME.
: RC@ ( CMD:command -- CMD:command result<n,n> )
   OUTCOME@ PROC-OUTCOME>RC RC>N RC>RESULT ;

: RUN-RC ( CMD:command ptr u8 n ms -- CMD:command result<n,n> )
   RUN-OUTCOME drop RC@ ;

: OUT$ ( CMD:command -- CMD:command ptr u8 n )
   CTX {: h:ptr :}
   h OUT-BASE @ h OUT-LEN @ ;

: ERR$ ( CMD:command -- CMD:command ptr u8 n )
   CTX {: h:ptr :}
   h ERR-BASE @ h ERR-LEN @ ;

\ RUN-OUTCOME, then: a clean exit returns; any other completion writes ERR$ to
\ this process's stderr and throws code, which must be nonzero; a deadline-killed
\ run throws E-PROC-TIMEOUT (PROC-OUTCOME>RC).
: RUN ( CMD:command ptr u8 n ms n -- CMD:command )
   {: path:ptr pathu:n timeout:ms code:n :}
   path pathu timeout RUN-OUTCOME PROC-OUTCOME>RC RC>N 0= if exit then
   ERR$ {: err:ptr erru:n :}
   FD:STDERR err erru FD:WRITE-FULL code throw ;

;package
