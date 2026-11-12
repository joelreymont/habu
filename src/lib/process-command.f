\ process-command.f - scoped child-process commands under a linear token.
\ CMD:WITH-COMMAND allocates one command context, hands the body a linear
\ CMD:command naming it and releases the context with everything it grew when
\ the body returns or throws. Every member grows; no caller-visible ceiling.
\ No word writes the process-wide argv/env staging: a run spawns from the
\ context's own vectors. STORAGE CLASS. TASK-LOCAL.
require src/lib/error.f
require src/lib/memory.f
require src/lib/task.f
require src/lib/fs.f
require src/lib/fd.f
require src/lib/ffi-abi.f
require src/lib/byte-buffer.f
require src/lib/vector.f
require src/lib/heap.f
require src/lib/map.f
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

\ A task's open contexts form a chain through each context's OUTER cell, headed
\ by the task's INNER cell; RUNNING names the context whose run is in progress,
\ so the capture sinks (quotations, which carry no data) find it. Scopes nest,
\ and tasks never share a context. A halted task unwinds through each scope's
\ `finally` as a throw does.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 2 cells TASK:+USER CMD-TASK drop

: INNER ( -- ptr ptr n ) CMD-TASK 0 ptr-field ;
: RUNNING ( -- ptr ptr n ) CMD-TASK 1 ptr-field ;

STRUCTURE vectors DERIVE addr
   FIELD argv ptr VEC:vector<ptr u8>
   FIELD envp ptr VEC:vector<ptr u8>
   FIELD arg-off ptr VEC:vector<n>
   FIELD env-off ptr VEC:vector<n>
   FIELD placed ptr VEC:vector<n>
   FIELD unset-off ptr VEC:vector<n>
   FIELD env-names ptr VEC:vector<n>
;STRUCTURE

CAST: >VECTORS ( ptr n -- ptr vectors )

\ Header cells, then the PATHZ and CWDZ byte regions. A
\ pointer kept in a cell that declares none is refused.
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
\ UNSETENV's NUL-terminated names, back to back
: UNSET-BUF ( ptr n -- ptr n ) 23 cells + ;
\ the child's argv, built by PREPARE
: ARGV-VEC ( ptr n -- ptr ptr VEC:vector<ptr u8> )
   26 cells + >VECTORS VECTORS-ARGV ;
\ the child's envp, built by PREPARE
: ENVP-VEC ( ptr n -- ptr ptr VEC:vector<ptr u8> )
   26 cells + >VECTORS VECTORS-ENVP ;
\ each argument's offset in ARG-BUF
: ARG-OFF ( ptr n -- ptr ptr VEC:vector<n> )
   26 cells + >VECTORS VECTORS-ARG-OFF ;
\ each caller row's offset in ENV-BUF
: ENV-OFF ( ptr n -- ptr ptr VEC:vector<n> )
   26 cells + >VECTORS VECTORS-ENV-OFF ;
\ FD+'s child/parent pairs, back to back
: PLACED ( ptr n -- ptr ptr VEC:vector<n> )
   26 cells + >VECTORS VECTORS-PLACED ;
\ each unset name's offset in UNSET-BUF
: UNSET-OFF ( ptr n -- ptr ptr VEC:vector<n> )
   26 cells + >VECTORS VECTORS-UNSET-OFF ;
\ a MAP indexing envp rows by name during PREPARE
: ENV-NAMES ( ptr n -- ptr ptr VEC:vector<n> )
   26 cells + >VECTORS VECTORS-ENV-NAMES ;
26 VECTORS-CELLS + constant HEAD-CELLS

PROCESS:PATHZ-CAP constant PATHZ-CAP
HEAD-CELLS PATHZ-CAP 2 * 7 + 8 / + constant CTX-CELLS

: PATHZ ( ptr n -- ptr u8 ) BYTE-VIEW HEAD-CELLS cells + ;
: CWDZ ( ptr n -- ptr u8 ) PATHZ PATHZ-CAP + ;

\ Growing members start small; the first byte, row or capture grows them.
64 constant BYTES-START

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
   h ARG-OFF @ 0 VEC:RESIZE
   h ENV-OFF @ 0 VEC:RESIZE
   h PLACED @ 0 VEC:RESIZE
   h UNSET-BUF BUF:CLEAR
   h UNSET-OFF @ 0 VEC:RESIZE
   0 h CWD-LEN !
   1 h INHERIT !
   PROCESS:NO-FD h CANCEL-FD !
   h CAPTURE-RESET ;

: INIT-MEMBERS ( ptr n -- )
   {: h:ptr :}
   h ARG-BUF BYTES-START BUF:N>BLEN BUF:INIT
   h ENV-BUF BYTES-START BUF:N>BLEN BUF:INIT
   h IN-BUF BYTES-START BUF:N>BLEN BUF:INIT
   HEAP:ALLOCATOR 0 [type] ptr u8 VEC:NEW h ARGV-VEC !
   HEAP:ALLOCATOR 0 [type] ptr u8 VEC:NEW h ENVP-VEC !
   HEAP:ALLOCATOR 0 [type] n VEC:NEW h ENV-NAMES !
   HEAP:ALLOCATOR 0 [type] n VEC:NEW h ARG-OFF !
   HEAP:ALLOCATOR 0 [type] n VEC:NEW h ENV-OFF !
   HEAP:ALLOCATOR 0 [type] n VEC:NEW h PLACED !
   h UNSET-BUF BYTES-START BUF:N>BLEN BUF:INIT
   HEAP:ALLOCATOR 0 [type] n VEC:NEW h UNSET-OFF !
   h CLEAR-BUILD ;

: RELEASE-REGION ( ptr u8 n -- ) {: base:ptr cap:n :}
   cap 0= if exit then
   base cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

\ Pop the context first, so a failing disposer cannot leave a freed context on
\ the chain; a member never initialised is a dead header or a NULL-PTR vector,
\ which DISPOSE and DELETE ignore.
: CLOSE ( -- )
   INNER @ {: h:ptr :}
   h OUTER @ INNER !
   h ARG-BUF BUF:DISPOSE
   h ENV-BUF BUF:DISPOSE
   h IN-BUF BUF:DISPOSE
   h ARGV-VEC @ VEC:DELETE
   h ENVP-VEC @ VEC:DELETE
   h ENV-NAMES @ VEC:DELETE
   h ARG-OFF @ VEC:DELETE
   h ENV-OFF @ VEC:DELETE
   h PLACED @ VEC:DELETE
   h UNSET-BUF BUF:DISPOSE
   h UNSET-OFF @ VEC:DELETE
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

\ The body hands back the token it was given; any other, such as an outer
\ scope's token swapped in, is refused with CMD:E-HANDLE, and the scope still
\ frees exactly the context it opened. A throw out of the body loses the token
\ with the stack and the cleanup frees by pointer, so `catch` belongs outside.
: WITH-COMMAND ( R [ R CMD:command -- S CMD:command ] -- S )
   OPEN
   [: RUN-BODY ;] [: CLOSE ;] finally ;

\ Forget the arguments, rows, unset names, stdin, directory, placements, lent
\ descriptor and last outcome; the storage stays for reuse.
: RESET ( CMD:command -- CMD:command )
   CTX CLEAR-BUILD ;

\ Lend a pipe's read end until the next run returns or throws. Another task
\ writes or closes its writer; it never mutates a running command. PROCESS:NO-FD
\ disconnects. The owner must keep the read end open through run completion.
\ A byte or a closed writer cancels the run: it kills the child's tree, reaps
\ the child, closes its captures and throws E-PROC-CANCELLED. A completion
\ already collected wins, and PROCESS:STOP is answered first. The descriptor is
\ never read or closed, so a byte left from one request cancels the next run:
\ drain or replace the pipe before lending it again.
: CANCEL-FD! ( CMD:command fd -- CMD:command ) {: fd:fd :}
   CTX {: h:ptr :}
   fd FD>N h CANCEL-FD ! ;

private

: CSTR+ ( ptr u8 n ptr n -- )
   {: a:ptr u:n buf:ptr :}
   a u  buf u FFI:CSTR-ROOM BUF:N>BLEN BUF:GROW  FFI:CSTR drop ;

\ Offsets, not addresses: the buffer moves as it grows until PREPARE. A
\ refused entry leaves no bytes and no offset.
: ENTRY ( ptr n ptr VEC:vector<n> [ -- ] -- )
   {: buf:ptr offs:ptr w :}
   buf BUF-LEN {: at:n :}
   [: w execute  at offs VEC:PUSH ! ;] catch {: code:n :}
   code 0= if exit then
   at BUF:N>BLEN buf BUF:LEN!  code throw ;

public

: ARG+ ( CMD:command ptr u8 n -- CMD:command ) {: a:ptr u:n :}
   CTX {: h:ptr :}
   u 0 < if E-PROC-OUTPUT throw then
   h ARG-BUF h ARG-OFF @ [: a u h ARG-BUF CSTR+ ;] ENTRY ;

: ENV-ENTRY+ ( CMD:command ptr u8 n -- CMD:command ) {: a:ptr u:n :}
   CTX {: h:ptr :}
   u 0 < if E-PROC-ENV throw then
   a u >LEN PROC-ENV-CHECK-ENTRY
   h ENV-BUF h ENV-OFF @ [: a u h ENV-BUF CSTR+ ;] ENTRY ;

: ENV+ ( CMD:command ptr u8 n ptr u8 n -- CMD:command )
   {: name:ptr nameu:n val:ptr valu:n :}
   CTX {: h:ptr :}
   h ENV-BUF h ENV-OFF @
   [: name nameu >LEN val valu >LEN
      [: BUF:N>BLEN h ENV-BUF swap BUF:GROW SPAN:$ drop ;] PROC-ENV-ROW drop ;]
   ENTRY ;

\ A run inherits this process's environment and the PROC-ENV defaults unless
\ ENV-HERMETIC turns that off.
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
   h UNSET-BUF h UNSET-OFF @ [: a u h UNSET-BUF CSTR+ ;] ENTRY ;

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
   a u >LEN h CWDZ PATHZ-CAP >LEN PROCESS:ZCOPY drop
   u h CWD-LEN ! ;

: IN! ( CMD:command ptr u8 n -- CMD:command ) {: a:ptr u:n :}
   CTX {: h:ptr :}
   u 0 < if E-PROC-OUTPUT throw then
   a u BUF:N>BLEN h IN-BUF BUF:REPLACE ;


\ The child's fd child is the parent's fd parent in every run until RESET or the
\ scope's end, applied after the stdio slots as PROCESS:FD applies it
\ (src/lib/spawn.f). Each scope holds its own placements. CMD never closes a
\ placed fd: a run refuses a closed one (PROCESS:CHECK-FD: E-PROC-SPAWN, EBADF in
\ PROCESS:ERRNO) and no child runs. Either negative: E-PROC-OUTPUT.
: FD+ ( CMD:command fd fd -- CMD:command )
   {: child:fd parent:fd :}
   CTX {: h:ptr :}
   child FD>N 0 < parent FD>N 0 < or if E-PROC-OUTPUT throw then
   child FD>N h PLACED @ VEC:PUSH !
   parent FD>N h PLACED @ VEC:PUSH ! ;

private

: NAMES ( ptr n -- ptr n count ) ENV-NAMES @ 0 VEC:AT dup MAP:CAP@ ;

: ROW-NAME ( ptr u8 -- ptr u8 len ) {: z:ptr :}
   z z z ZLEN >LEN PROC-ENV-NAME-LEN ;

: NAMED-PUSH ( ptr n ptr u8 -- ) {: h:ptr z:ptr :}
   h ENVP-VEC @ VEC:COUNT h NAMES z ROW-NAME MAP:SET
   z h ENVP-VEC @ VEC:PUSH ! ;

: UNSET-NAME ( ptr n n -- ptr u8 len )
   {: h:ptr i:n :}
   h UNSET-BUF BUF-BASE h UNSET-OFF @ i VEC:AT @ + dup ZLEN >LEN ;


: UNSET? ( ptr n ptr u8 -- bool )
   {: h:ptr z:ptr :}
   0 begin dup h UNSET-OFF @ VEC:COUNT < while
      h over UNSET-NAME z z ZLEN >LEN PROC-ENV-SAME-NAME? if drop PROC-ENV-TRUE exit then
      1+
   repeat drop PROC-ENV-FALSE ;


\ Pushes the row's address, never a copy: a default is read where
\ PROC-ENV-DEFAULT+ wrote it and a parent row in this process's environment.
: INHERIT-Z ( ptr n ptr u8 -- )
   {: h:ptr z:ptr :}
   h NAMES z ROW-NAME MAP:HAS? if exit then
   h z UNSET? if exit then
   h z NAMED-PUSH ;


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

: ENV-ROWS ( ptr n -- n ) {: h:ptr :}
   h ENV-OFF @ VEC:COUNT
   h INHERIT @ 0= if exit then
   PROC-ENV-DEF-N @ COUNT>N +
   0 begin dup ENVP 0= 0= while 1 + repeat + ;

\ Twice the rows keeps the table at most half full. Keys point at the rows
\ themselves; ENV-BUF does not grow in PREPARE and inherited rows never move.
: NAMES-INIT ( ptr n -- ) {: h:ptr :}
   h ENV-ROWS 2 * 1 max >COUNT {: cap :}
   h ENV-NAMES @ cap MAP:CELL-COUNT COUNT>N VEC:ENSURE
   h ENV-NAMES @ 0 VEC:AT cap MAP:INIT ;

\ argv: the path, each argument, the NULL. envp: the caller's rows, one per name
\ with its last value, then the inherited ones, the NULL; both rebuilt every run.
: PREPARE ( ptr n ptr u8 n -- )
   {: h:ptr path:ptr pathu:n :}
   path pathu >LEN h PATHZ PATHZ-CAP >LEN PROCESS:ZCOPY {: z:ptr :}
   h ARGV-VEC @ 0 VEC:RESIZE
   z h ARGV-VEC @ VEC:PUSH !
   h ARG-OFF @ VEC:COUNT 0 ?do
      h ARG-BUF BUF-BASE  h ARG-OFF @ i VEC:AT @ +  h ARGV-VEC @ VEC:PUSH !
   loop
   NULL-PTR h ARGV-VEC @ VEC:PUSH !
   h NAMES-INIT
   h ENVP-VEC @ 0 VEC:RESIZE
   h ENV-OFF @ VEC:COUNT 0 ?do
      h ENV-BUF BUF-BASE  h ENV-OFF @ i VEC:AT @ +  {: z:ptr :}
      h NAMES z ROW-NAME MAP:GET MATCH option
         none OF h z NAMED-PUSH ENDOF
         some OF z swap h ENVP-VEC @ swap VEC:AT ! ENDOF
      ;MATCH
   loop
   h INHERIT @ 0<> if h INHERIT-DEFAULTS h INHERIT-PARENT then
   NULL-PTR h ENVP-VEC @ VEC:PUSH ! ;

: CHECK-RUN ( ptr u8 n ms -- )
   {: path:ptr pathu:n timeout:ms :}
   path pathu >LEN PROC-ARGV-CHECK-PATH
   timeout MS>N 0 < if E-PROC-TIMEOUT throw then ;


: PAIRS ( ptr n -- n ) PLACED @ VEC:COUNT 2 / ;


\ The pair at idx of PLACED: the child's fd and the parent's.
: PAIR ( ptr n n -- fd fd )
   {: h:ptr k:n :}
   h PLACED @ 2 k * VEC:AT @ >FD
   h PLACED @ 2 k * 1 + VEC:AT @ >FD ;


: CHECK-PLACED ( ptr n -- )
   dup PAIRS 0 ?do dup i PAIR nip PROCESS:CHECK-FD loop drop ;


\ The capture pipes at the child's 0, 1 and 2.
: STDIO ( PROCESS:request -- PROCESS:request )
   0 >FD PROCESS:IN-R @ >FD PROCESS:FD
   1 >FD PROCESS:OUT-W @ >FD PROCESS:FD
   2 >FD PROCESS:ERR-W @ >FD PROCESS:FD ;


: PLACE ( PROCESS:request ptr n -- PROCESS:request )
   {: h:ptr :}
   h PAIRS 0 ?do h i PAIR PROCESS:FD loop ;


\ The run's request from the context's own storage; FD+'s placements apply after
\ the stdio slots.
: REQUEST ( ptr n -- PROCESS:request )
   {: h:ptr :}
   h PATHZ dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   h ARGV-VEC @ 0 VEC:AT PROCESS:ARGV-ROWS
   h ENVP-VEC @ 0 VEC:AT PROCESS:ENV-ROWS
   h CWD-LEN @ 0 > if h CWDZ h CWD-LEN @ PROCESS:CWD then
   STDIO
   h PLACE ;


: SPAWN ( ptr n -- ) {: h:ptr :}
   [: h REQUEST PROCESS:SPAWN ;] PROCESS:CAPTURE-ADOPT-SPAWN ;


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

: OUT-ROOM ( n -- ptr u8 ) RUNNING @ {: h:ptr :} h OUT-BASE h OUT-CAP PROCESS:OUT-LEN @ ROOM ;
: ERR-ROOM ( n -- ptr u8 ) RUNNING @ {: h:ptr :} h ERR-BASE h ERR-CAP PROCESS:ERR-LEN @ ROOM ;

\ Decompose the ending sum into exited/timed-out flags plus the code cell
\ (all one cell, lossless): exit codes >= 128 stay distinct from signal deaths.
: STORE-OUTCOME ( PROCESS:ending ptr n -- )
   {: h:ptr :}
   MATCH PROCESS:ending
     exited OF h CODE ! 1 h EXITED ! 0 h TIMED-OUT ! ENDOF
     signaled OF h CODE ! 0 h EXITED ! 0 h TIMED-OUT ! ENDOF
     timeout OF 0 h CODE ! 0 h EXITED ! 1 h TIMED-OUT ! ENDOF
   ;MATCH ;

\ The command disconnects at entry; release the capture's borrowed reference
\ after its cleanup. A throw has no ending variant: OUTCOME@ and RC@ throw its
\ code again until the next run or RESET.
: RUN-DONE ( n ptr n -- )
   {: code:n h:ptr :}
   PROCESS:NO-FD PROCESS:CANCEL-FD !
   code h FAILED !
   code 0<> if code throw then ;

: START-CANCEL ( fd -- )
   FD>N PROCESS:CANCEL-FD !
   PROCESS:STOP-ASK ;

: CAPTURE ( ptr n fd ms -- ptr n fd ms )
   {: h:ptr cancel:fd timeout:ms :}
   timeout PROCESS:STDIN-CAPTURE-BEGIN
   cancel START-CANCEL
   h SPAWN
   h IN-BUF BUF-BASE h IN-BUF BUF-LEN >LEN
   [: OUT-ROOM ;] [: ERR-ROOM ;] PROCESS:RUN-STDIN-GROWING-BOTH-CAPTURE-ENDING-LOOP
   h cancel timeout ;

: OUTCOME-OF ( ptr n -- PROCESS:ending ) {: h:ptr :}
   h FAILED @ dup 0<> if throw then drop
   h TIMED-OUT @ 0<> if PROCESS-ENDING:timeout exit then
   h EXITED @ 0<> if h CODE @ PROCESS-ENDING:exited exit then
   h CODE @ PROCESS-ENDING:signaled ;

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
   PROCESS:NO-FD h CANCEL-FD !
   path pathu timeout CHECK-RUN
   h CHECK-PLACED
   h path pathu PREPARE
   h CAPTURE-RESET
   h RUNNING !
   h cancel timeout [: CAPTURE ;] catch {: code:n :} drop 2drop
   NULL-PTR RUNNING !
   PROCESS:OUT-LEN @ h OUT-LEN !
   PROCESS:ERR-LEN @ h ERR-LEN !
   code h RUN-DONE
   PROCESS:CAPTURE-FINISH-ENDING h STORE-OUTCOME 2drop ;

public

: OUTCOME@ ( CMD:command -- CMD:command PROCESS:ending )
   CTX OUTCOME-OF ;

\ Run the prepared command at path, feed it the stdin and capture both streams
\ whole; a deadline-killed run answers PROCESS-ENDING:timeout. A spawn failure, a
\ refused path, deadline or placed fd and cancellation throw their own codes; a
\ path, deadline or closed placed fd refused before the run leaves the previous
\ outcome in place.
: RUN-OUTCOME ( CMD:command ptr u8 n ms -- CMD:command PROCESS:ending )
   {: path:ptr pathu:n timeout:ms :}
   CTX {: h:ptr :}
   h path pathu timeout RUN-CAPTURE
   h OUTCOME-OF ;

\ The stored outcome's completion rc as a result<n,n>: ok = clean exit (0), err =
\ the nonzero completion code (a nonzero exit code, or 128+signal). A run its
\ deadline killed has no completion code: RC@ and RUN-RC throw E-PROC-TIMEOUT for
\ it (PROCESS:ENDING>RC), and only a caller that acts on the deadline reads it as
\ data through OUTCOME@ or RUN-OUTCOME.
: RC@ ( CMD:command -- CMD:command result<n,n> )
   OUTCOME@ PROCESS:ENDING>RC PROCESS:RC>RESULT ;

: RUN-RC ( CMD:command ptr u8 n ms -- CMD:command result<n,n> )
   RUN-OUTCOME drop RC@ ;

\ OUT$ and ERR$ are borrowed until the next run, RESET or the end of the scope.
: OUT$ ( CMD:command -- CMD:command ptr u8 n )
   CTX {: h:ptr :}
   h OUT-BASE @ h OUT-LEN @ ;

: ERR$ ( CMD:command -- CMD:command ptr u8 n )
   CTX {: h:ptr :}
   h ERR-BASE @ h ERR-LEN @ ;

\ RUN-OUTCOME, then: a clean exit returns; any other completion writes ERR$ to
\ this process's stderr and throws code, which must be nonzero; a deadline-killed
\ run throws E-PROC-TIMEOUT (PROCESS:ENDING>RC).
: RUN ( CMD:command ptr u8 n ms n -- CMD:command )
   {: path:ptr pathu:n timeout:ms code:n :}
   path pathu timeout RUN-OUTCOME PROCESS:ENDING>RC RC>N 0= if exit then
   ERR$ {: err:ptr erru:n :}
   FD:STDERR err erru FD:WRITE-FULL code throw ;

;package
