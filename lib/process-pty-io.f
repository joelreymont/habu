\ process-pty-io.f - checked process supervisor over the linear PTY handle registry.
\
\ This file REOPENS package PROCESS-PTY (the package that lib/process-pty-handle.f
\ owns). That registry publishes no public interface: RESERVE, COMMIT, TAKE,
\ TEARDOWN-DONE, the per-field readers, and the pid/fd linear coercions are all
\ PRIVATE words. A supervisor that drives the linear reservation -> handle ->
\ teardown lifecycle therefore has to live inside the same package and call those
\ words by their bare names, exactly the way lib/process-pty-handle-test.f does.
\ The supervisor exports its lifecycle words in the package's PUBLIC section, so
\ callers outside the package reach them as PROCESS-PTY:SPAWN, PROCESS-PTY:SIGNAL,
\ and so on.
\
\ I/O model: this slice connects the supervised child through ordinary pipes (the
\ `master` slot is the child's stdout read end and the `done` slot is its stdin
\ write end). The registry's slot also demands two more supervised pids and their
\ lifetime-watch descriptors, so the supervisor runs the target inside a small
\ supervised set: the target plus two lifecycle-helper processes that occupy the
\ registry's `group` (pgrp) and `sup` slots and stay alive until teardown. A
\ SPAWN-TTY is the second mode: the target's standard descriptors are the slave
\ side of a fresh pseudo-terminal, opened by PTY:OPEN, which owns the
\ per-target unlock-and-name dance for the whole tree, so a
\ child engine sees a terminal on fd 0, installs its line editor and recovers
\ from a refused definition instead of stopping at it; the handle's `master`
\ slot is the PTY master and `done` a duplicate of it for the write side. The
\ slave is opened O_NOCTTY: a session-leading supervisor without a terminal
\ (setsid, a service, a container init) would otherwise adopt it and be hung up
\ at teardown. Nobody acquires the terminal as controlling (TIOCSCTTY is an
\ unencoded request the engine's ioctl guard refuses), so job control is not
\ part of this slice. WRITE-LINE and AWAIT-BYTES drive either mode.
\
\ Spawn is gated: SPAWN forks the child but leaves it blocked before exec, opens
\ its lifetime watch descriptor while it is guaranteed alive, and only LAUNCH
\ releases it to exec. Opening the watch while the child is still alive is what
\ makes the exit observable on both backends (Linux pidfd_open and macOS
\ kqueue/NOTE_EXIT); test/proc-watch-smoke.f owns the already-dead-race behavior.

require lib/process-pty-handle.f
require lib/process-fork.f
require lib/process-argv.f
require lib/process.f
require lib/pty.f
require lib/prelude.f

package PROCESS-PTY

0 constant SIG-PROBE               \ the null signal: probe existence, deliver nothing
3 constant ESRCH#                  \ "no such process" (POSIX, identical Linux/macOS)
1 constant EPERM#                  \ "operation not permitted" (process exists, denied)
$400 constant IO-PATH-CAP          \ maximum supervised executable path length + NUL
$7F constant IO-EXEC-FAIL          \ child exit code when execve never replaces the image
0 constant F-DUPFD                 \ fcntl: duplicate to the lowest descriptor at or above the argument
10 constant IO-LF

\ Load-time scratch shared by the parent build path and by each forked child (a
\ fork copies this memory, so a child reads the same values the parent stored).
create IO-PATH-BUF IO-PATH-CAP allot   \ supervised executable path bytes (child reads)
1 TYPED-BUFFER IO-ENVP ptr u8          \ single NULL slot: the empty child environment
create IO-GOBYTE 1 allot               \ the one-byte release token written by LAUNCH
create IO-GO SLOT-CAP cells allot       \ per-slot release-gate write end, indexed by slot
create IO-PTY-NAME PTY:SLAVE-PATH-CAP allot \ slave path (NUL-terminated)
create IO-LFBYTE 1 allot                \ the line terminator WRITE-LINE appends

variable IO-PATH-LEN                    \ supervised path length (n)
variable IO-RAW                         \ raw handle number carried across the spawn catch
variable IO-TTY                         \ spawn mode: 0 pipes, -1 a pseudo-terminal

\ In-flight spawn state. One spawn runs to completion before the next, so a single
\ set of cells is enough; every cell holds -1 until the build assigns it, which
\ lets the abort path close/reap only what actually opened.
variable IO-A-PID    variable IO-T-PID    variable IO-M-PID     \ anchor, target, monitor pids
variable IO-MASTER   variable IO-DONE                            \ owner-kept child stdout/stdin ends
variable IO-ANCHOR-W variable IO-LIFE                            \ owner-kept helper keepalive write ends
variable IO-GW       variable IO-TW       variable IO-SW         \ anchor/target/monitor watch fds
variable IO-GO-W                                                 \ owner-kept release-gate write end
variable IO-SOUT-W   variable IO-SIN-R                           \ child-side stdout/stdin ends
variable IO-AH-R     variable IO-MH-R     variable IO-GO-R       \ child-side helper/gate read ends

\ ---- per-slot release-gate storage ------------------------------------------
: IO-GO-SLOT ( idx -- ptr n )
   IDX>N cells IO-GO + cell-view ;

: IO-GO@ ( idx -- fd )
   IO-GO-SLOT @ >FD ;

: IO-GO! ( fd idx -- ) {: f:fd idx:idx :}
   f FD>N idx IO-GO-SLOT ! ;

\ ---- small fd / pid helpers -------------------------------------------------
: IO-CLOSE-FD ( fd -- ) {: f:fd :}
   f FD>N 0 < if exit then
   f FD>N close ;

\ Best-effort reap used by both the abort path and teardown: neither may throw,
\ because a throw would strand the linear teardown token and leak the slot. The
\ kill result and wait status are intentionally discarded here (the process is
\ already being retired), the same shape as a deinit cleanup.
: IO-KILL-REAP ( pid -- ) {: p:pid :}
   p PID>N 0 <= if exit then
   p PID>N SIGKILL kill-errno drop
   p PID>N wait-status drop ;

: IO-GO-CLOSE ( idx -- ) {: idx:idx :}
   idx IO-GO@ IO-CLOSE-FD
   -1 >FD idx IO-GO! ;

\ ---- signal classification --------------------------------------------------
\ kill-errno reports 0 (delivered), -ESRCH (gone), or -EPERM (exists, denied). A
\ process that exists is alive whether or not we may signal it; only -ESRCH means
\ gone. Any other errno is unexpected for signal 0 and is surfaced, not hidden.
: IO-ALIVE-CLASS? ( rc -- bool ) {: r:rc :}
   r RC>N 0= if true exit then
   r RC>N ESRCH# negate = if false exit then
   r RC>N EPERM# negate = if true exit then
   E-PROC-OUTPUT throw ;

\ ---- poll one watch descriptor ----------------------------------------------
\ True only when the descriptor reports a clean readable exit; a POLLERR/POLLNVAL
\ revent means the descriptor is broken and must not masquerade as an exit.
: IO-POLL-READY? ( fd n -- bool ) {: wfd:fd ms:n :}
   ms >MS PROC-DEADLINE-AT {: deadline:n :}
   wfd POLLIN PROC-PFD!
   1 ms deadline PROC-POLL-RESTART {: rc:n :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if false exit then
   0 >IDX PROC-PFD-REVENTS {: ev:n :}
   ev POLLERR POLLNVAL or and 0 <> if E-PROC-OUTPUT throw then
   ev POLLIN and 0 <> ;

\ ---- child bodies (each ends in die; never returns) -------------------------
\ A lifecycle-helper child closes every inherited descriptor but the one keepalive
\ read end it watches, blocks until that end reports EOF (owner drop) or the child
\ is killed, then exits. It holds no other pipe end open, so it can never wedge a
\ downstream EOF.
: IO-HELPER-CHILD ( fd -- ) {: hr:fd :}
   hr hr PROC-FORK:CLOSE-EXCEPT2
   hr FD>N IO-GOBYTE 1 read drop
   s" " 0 die ;

: IO-CLOSE-HIGH ( -- )
   3 begin dup PROC-FORK:MAXFD < while
      dup close
      1+
   repeat drop ;

\ The target child blocks on the release gate, wires the pipe ends onto its
\ standard descriptors, drops every other inherited descriptor, and execs. execve
\ only returns on failure, so reaching the tail means exec failed and the child
\ exits with IO-EXEC-FAIL; the supervisor still observes the exit through the watch.
: IO-TARGET-STDIO ( -- )
   IO-SIN-R @ >FD FD>N 0 dup2 drop
   IO-SOUT-W @ >FD FD>N 1 dup2 drop
   IO-SOUT-W @ >FD FD>N 2 dup2 drop ;

\ The slave is the child's whole terminal: input, output and diagnostics.
: IO-TARGET-TTY ( -- )
   IO-SIN-R @ >FD FD>N 0 dup2 drop
   IO-SIN-R @ >FD FD>N 1 dup2 drop
   IO-SIN-R @ >FD FD>N 2 dup2 drop ;

: IO-TARGET-CHILD ( -- )
   IO-GO-R @ >FD FD>N IO-GOBYTE 1 read drop
   IO-TTY @ if IO-TARGET-TTY else IO-TARGET-STDIO then
   IO-CLOSE-HIGH
   IO-PATH-BUF IO-PATH-LEN @ >LEN PROC-ARGV-PREPARE 0 IO-ENVP execve drop
   s" " IO-EXEC-FAIL die ;

\ ---- parent-side build steps ------------------------------------------------
: IO-RESET-STX ( -- )
   -1 IO-A-PID !  -1 IO-T-PID !  -1 IO-M-PID !
   -1 IO-MASTER !  -1 IO-DONE !  -1 IO-ANCHOR-W !  -1 IO-LIFE !
   -1 IO-GW !  -1 IO-TW !  -1 IO-SW !  -1 IO-GO-W !
   -1 IO-SOUT-W !  -1 IO-SIN-R !  -1 IO-AH-R !  -1 IO-MH-R !  -1 IO-GO-R ! ;

: IO-STORE-PATH ( ptr u8 len -- ) {: path:ptr pathu:len :}
   path pathu IO-PATH-BUF IO-PATH-CAP >LEN PROC-ZCOPY drop
   pathu LEN>N IO-PATH-LEN ! ;

\ PIPE-PAIR yields ( read write ); each pipe's owner end and child end land in
\ their own cells.
: IO-MK-STDIO ( -- )
   PIPE-PAIR IO-SOUT-W ! IO-MASTER !      \ stdout: owner reads master, child writes
   PIPE-PAIR IO-DONE ! IO-SIN-R ! ;       \ stdin: owner writes done, child reads

\ ---- the pseudo-terminal pair ----------------------------------------------
: IO-OPEN-CK ( n -- n ) {: f:n :}
   f 0 < if E-PROC-OUTPUT throw then f ;

\ master -> IO-MASTER (owner reads), a duplicate of it -> IO-DONE (owner
\ writes), slave -> IO-SIN-R (the child's terminal); IO-SOUT-W stays unused.
\ The slave is opened here rather than by PTY:OPEN, and with O_NOCTTY: a
\ session-leading supervisor without a terminal would otherwise adopt it.
: IO-MK-TTY ( -- )
   IO-PTY-NAME PTY:SLAVE-PATH-CAP PTY:OPEN drop PTY:MASTER>N >FD IO-MASTER !
   IO-PTY-NAME PTY:PTY-OPEN-FLAGS 0 open IO-OPEN-CK >FD IO-SIN-R !
   IO-MASTER @ >FD FD>N F-DUPFD 3 fcntl IO-OPEN-CK >FD IO-DONE ! ;

: IO-MK-HOLDS ( -- )
   PIPE-PAIR IO-ANCHOR-W ! IO-AH-R !      \ anchor keepalive: owner write, child read
   PIPE-PAIR IO-LIFE ! IO-MH-R !          \ monitor keepalive: owner write, child read
   PIPE-PAIR IO-GO-W ! IO-GO-R ! ;        \ release gate: owner write, target read

: IO-FORK-HELPER ( fd -- pid ) {: hr:fd :}
   PROC-FORK:CHECKED dup PID>N 0= if drop hr IO-HELPER-CHILD then ;

: IO-FORK-ANCHOR ( -- )
   IO-AH-R @ >FD IO-FORK-HELPER IO-A-PID ! ;

: IO-FORK-MON ( -- )
   IO-MH-R @ >FD IO-FORK-HELPER IO-M-PID ! ;

: IO-FORK-TARGET ( -- )
   PROC-FORK:CHECKED dup PID>N 0= if drop IO-TARGET-CHILD then IO-T-PID ! ;

: IO-WATCH ( pid -- fd )
   PID>N proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then >FD ;

: IO-OPEN-WATCHES ( -- )
   IO-A-PID @ >PID IO-WATCH IO-GW !
   IO-T-PID @ >PID IO-WATCH IO-TW !
   IO-M-PID @ >PID IO-WATCH IO-SW ! ;

: IO-CLOSE-CHILD-ENDS ( -- )
   IO-SOUT-W @ >FD IO-CLOSE-FD
   IO-SIN-R @ >FD IO-CLOSE-FD
   IO-AH-R @ >FD IO-CLOSE-FD
   IO-MH-R @ >FD IO-CLOSE-FD
   IO-GO-R @ >FD IO-CLOSE-FD ;

\ Forks come before the watches so the children never inherit a lifetime-watch
\ descriptor; child-only pipe ends close in the parent right after.
: IO-BUILD ( -- )
   IO-TTY @ if IO-MK-TTY else IO-MK-STDIO then
   IO-MK-HOLDS
   IO-FORK-ANCHOR
   IO-FORK-TARGET
   IO-FORK-MON
   IO-OPEN-WATCHES
   IO-CLOSE-CHILD-ENDS ;

: IO-COMMIT ( -- process-pty-handle )
   RESERVE
   IO-M-PID @ >PID PID>SUP
   IO-A-PID @ >PID PID>PGRP
   IO-T-PID @ >PID PID>TARGET
   IO-MASTER @ >FD
   IO-LIFE @ >FD
   IO-DONE @ >FD
   IO-ANCHOR-W @ >FD
   IO-GW @ >FD FD>GROUP-WATCH
   IO-TW @ >FD FD>TARGET-WATCH
   IO-SW @ >FD FD>SUP-WATCH
   COMMIT ;

: IO-STORE-GO ( process-pty-handle -- process-pty-handle )
   HANDLE-IDX {: idx:idx :}
   IO-GO-W @ >FD idx IO-GO! ;

\ Abort path: close every descriptor still open and reap every child still live.
\ Runs only when the build threw before COMMIT handed ownership to the registry.
: IO-SPAWN-CLEAN ( -- )
   IO-A-PID @ >PID IO-KILL-REAP
   IO-T-PID @ >PID IO-KILL-REAP
   IO-M-PID @ >PID IO-KILL-REAP
   IO-MASTER @ >FD IO-CLOSE-FD
   IO-DONE @ >FD IO-CLOSE-FD
   IO-ANCHOR-W @ >FD IO-CLOSE-FD
   IO-LIFE @ >FD IO-CLOSE-FD
   IO-GW @ >FD IO-CLOSE-FD
   IO-TW @ >FD IO-CLOSE-FD
   IO-SW @ >FD IO-CLOSE-FD
   IO-GO-W @ >FD IO-CLOSE-FD
   IO-SOUT-W @ >FD IO-CLOSE-FD
   IO-SIN-R @ >FD IO-CLOSE-FD
   IO-AH-R @ >FD IO-CLOSE-FD
   IO-MH-R @ >FD IO-CLOSE-FD
   IO-GO-R @ >FD IO-CLOSE-FD ;

\ The linear handle cannot thread through `catch` (the throw path has no handle to
\ balance the success path), so the transaction converts it to a raw number inside
\ the caught quotation and the caller rebuilds it afterward.
: IO-TXN-SAVE ( -- )
   IO-BUILD
   IO-COMMIT
   IO-STORE-GO
   HANDLE>N IO-RAW ! ;

\ ---- live-handle readers (registry exposes takers, not live readers) --------
: IO-TARGET-WATCH ( process-pty-handle -- process-pty-handle target-watch )
   HANDLE-IDX TARGET-WATCH@ ;

: IO-SPAWN-MODE ( ptr u8 len bool -- process-pty-handle ) {: path:ptr pathu:len tty:bool :}
   ROOM? 0= if E-PROC-PTY-CAPACITY throw then
   IO-RESET-STX
   tty IO-TTY !
   path pathu IO-STORE-PATH
   [: IO-TXN-SAVE ;] catch dup 0 <> if
      0 IO-TTY !
      IO-SPAWN-CLEAN throw
   then
   drop
   0 IO-TTY !
   IO-RAW @ N>HANDLE ;

public

\ Spawn the supervised set and return a live handle. The target is forked but
\ blocked before exec; its lifetime watch is already open, so LAUNCH can release
\ it and the exit stays observable.
: SPAWN ( ptr u8 len -- process-pty-handle )
   false IO-SPAWN-MODE ;

\ The same lifecycle with the target's standard descriptors on the slave side
\ of a fresh pseudo-terminal; the handle's master is the terminal.
: SPAWN-TTY ( ptr u8 len -- process-pty-handle )
   true IO-SPAWN-MODE ;

\ Send one line to the target's input: the bytes then a line feed. A short write
\ is an error, never a silent truncation.
: WRITE-LINE ( process-pty-handle ptr u8 n -- process-pty-handle ) {: a:ptr u:n :}
   HANDLE-IDX DONE@ {: w:fd :}
   u 0 > if w FD>N a u write u <> if E-PROC-OUTPUT throw then then
   IO-LF IO-LFBYTE c!
   w FD>N IO-LFBYTE 1 write 1 <> if E-PROC-OUTPUT throw then ;

\ Close the target's input so a batch reader sees end of file; the slot is
\ cleared so teardown does not close it again. Under SPAWN-TTY the terminal
\ stays open (a PTY master has no end of file to give); send the child's own
\ end-of-input convention with WRITE-LINE instead.
: END-INPUT ( process-pty-handle -- process-pty-handle )
   HANDLE-IDX {: idx:idx :}
   idx DONE@ IO-CLOSE-FD
   -1 >FD idx DONE! ;

\ Wait up to ms for the target's output and read what is there into the buffer:
\ the byte count, 0 when nothing arrived in time, -1 once the target's side is
\ gone (hang-up or end of file). A broken descriptor throws, and so does an
\ empty buffer, which could otherwise only masquerade as a hang-up.
: AWAIT-BYTES ( process-pty-handle ptr u8 n n -- process-pty-handle n ) {: buf:ptr cap:n ms:n :}
   cap 0 <= if E-PROC-OUTPUT throw then
   HANDLE-MASTER@ {: m:fd :}
   ms >MS PROC-DEADLINE-AT {: deadline:n :}
   m POLLIN PROC-PFD!
   1 ms deadline PROC-POLL-RESTART {: rc:n :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if 0 exit then
   0 >IDX PROC-PFD-REVENTS {: ev:n :}
   ev POLLNVAL and 0 <> if E-PROC-OUTPUT throw then
   ev POLLIN and 0 <> if
      m FD>N buf cap read {: got:n :}
      got 0 > if got exit then
      -1 exit
   then
   -1 ;

\ Release the target to exec. Idempotent once the gate is spent.
: LAUNCH ( process-pty-handle -- process-pty-handle )
   HANDLE-IDX {: idx:idx :}
   idx IO-GO@ {: gw:fd :}
   gw FD>N 0 < if exit then
   1 IO-GOBYTE c!
   gw FD>N IO-GOBYTE 1 write drop
   idx IO-GO-CLOSE ;

\ Deliver a signal to the target; report 0 or the negated errno so the caller can
\ tell -ESRCH (gone) from -EPERM (exists, denied).
: SIGNAL ( process-pty-handle n -- process-pty-handle rc ) {: sig:n :}
   HANDLE-TARGET@ TARGET>PID PID>N sig kill-errno >RC ;

\ Probe target existence with signal 0.
: ALIVE? ( process-pty-handle -- process-pty-handle bool )
   SIG-PROBE SIGNAL IO-ALIVE-CLASS? ;

: TARGET ( process-pty-handle -- process-pty-handle pid )
   HANDLE-TARGET@ TARGET>PID ;

\ Poll the target's lifetime watch up to ms; true once its exit is signalled.
: AWAIT ( process-pty-handle n -- process-pty-handle bool ) {: ms:n :}
   IO-TARGET-WATCH TARGET-WATCH>FD ms IO-POLL-READY? ;

\ Ordered teardown: for each supervised pid close its lifetime watch BEFORE
\ reaping the pid, then close the remaining control descriptors, then hand every
\ slot back to the registry. TEARDOWN-DONE fails closed unless every descriptor
\ was taken out, so a clean return proves every handle field was erased exactly
\ once. The linear handle is consumed, so a second teardown cannot be written.
: TEARDOWN ( process-pty-handle -- )
   TAKE
   TEARDOWN-TARGET@ TARGET>PID {: tgt:pid :}
   TEARDOWN-GROUP@ PGRP>PID {: grp:pid :}
   TEARDOWN-SUP@ SUP>PID {: sup:pid :}
   TEARDOWN-TARGET-WATCH-TAKE TARGET-WATCH>FD IO-CLOSE-FD
   tgt IO-KILL-REAP
   TEARDOWN-GROUP-WATCH-TAKE GROUP-WATCH>FD IO-CLOSE-FD
   grp IO-KILL-REAP
   TEARDOWN-SUP-WATCH-TAKE SUP-WATCH>FD IO-CLOSE-FD
   sup IO-KILL-REAP
   TEARDOWN-MASTER-TAKE IO-CLOSE-FD
   TEARDOWN-LIFE-TAKE IO-CLOSE-FD
   TEARDOWN-DONE-TAKE IO-CLOSE-FD
   TEARDOWN-ANCHOR-TAKE IO-CLOSE-FD
   TEARDOWN-IDX {: idx:idx :}
   idx IO-GO-CLOSE
   TEARDOWN-DONE ;

private

: IO-INIT ( -- )
   NULL-PTR 0 IO-ENVP !
   0 begin dup SLOT-CAP < while
      -1 >FD over >IDX IO-GO!
      1+
   repeat drop ;

IO-INIT

;package
