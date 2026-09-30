\ process-tree.f - end a process and every process descended from it.
\
\ A SPAWNED CHILD LEADS A PROCESS GROUP OF ITS OWN (docs/process-pty.md), so the
\ kill that reaches a group - PROC-FORK:KILL-GROUP - ends a process and what it
\ forked, and nothing it spawned. Measured on the native gate: SIGTERM to the
\ root ended every row, and eight of the rows' own children - engine builds,
\ each leading its own group - went on for ten seconds and more, reparented to
\ init. KILL-TREE ends those too.
\
\ FREEZE, THEN LIST. A process killed while its children run leaves them to
\ init, where no link names them again, and a process listed while it runs may
\ spawn one more the moment after. So a process is stopped BEFORE its children
\ are asked for. A stopped process spawns nothing and reaps nothing, so a
\ child of a member that dies meanwhile stays a zombie under it and its number
\ is not handed to a stranger. A member whose parent is outside the tree - an
\ orphan, reached through its group - has no such pin: init reaps it when it
\ dies, and a stranger handed its pid before the walk ends is stopped and
\ killed with the tree. That takes the host's pid counter coming round inside
\ the walk, which lasts SETTLE-MS and the pass that crosses it. The tree grows
\ one pass of the process table at a time, until QUIET-PASSES passes in a row
\ add nobody and find every member settled (below); only then is each member
\ killed. SIGKILL ends a stopped process without running it again.
\
\ A STOPPED PROCESS STILL FINISHES THE SYSTEM CALL IT WAS IN, and a spawn is
\ one. Measured on macOS under load, before the walk read it: the leaf of
\ test/gate-signal-row.f showed as stopped, two passes found nothing new, and
\ a writer it had been spawning appeared after it was killed - in two runs of
\ ten. So a member counts as settled only when it is stopped and no thread of
\ it is still running in the kernel, which each host reads its own way (below),
\ and the walk ends on QUIET-PASSES such passes in a row rather than one.
\
\ WHO IS IN THE TREE. The process named; every process whose parent is in it;
\ and every process whose group is led by one in it - for what was forked and
\ then orphaned, which keeps its group and loses its parent. A zombie is a
\ member like any other: a signal does nothing to it, but the group it led
\ still names its pid, and what it forked before it exited may be in that
\ group and nowhere else (test/gate-signal-row.f's helper). Never the caller,
\ and never another child of the caller, zombie or not: a pool's reaper is a
\ child of the pool that joined its row's group, and it is the pool's own to
\ end.
\
\ WHAT IT CANNOT REACH. A process whose parent is outside the tree and whose
\ group id names no member, zombie included - a daemon that forked, called
\ setsid and let its parent exit. And on macOS, a child that did not exist
\ yet. libproc counts a task's RUNNABLE threads, so a thread inside a spawn
\ that is blocked in the kernel before the child is made - on a page-in of
\ what it copies in, an allocation, a lock such as the one the walk's own
\ libproc calls take - is not counted and its member reads as settled. The
\ SIGKILL then lets that spawn finish, and the child, leading its own group
\ and reparented to launchd, outlives the walk. Asking again until
\ QUIET-PASSES quiet passes narrows that window to a spawn blocked across the
\ last of them; it does not close it. Linux reads each thread's own state,
\ and a blocked thread is not a stopped one there.
\
\ STORAGE CLASS. PROCESS-WIDE, one walk at a time: the member table and the scan
\ buffers are this file's. Its callers are pool parents, each its own process.
\
\ HOSTS. macOS asks libproc for a member's children and group; Linux reads the
\ whole of /proc and then each member's threads. The Linux arm is written from
\ proc(5) and has run on no host here.

require lib/errors.f
require lib/string.f
require lib/adt/option.f
require lib/le.f
require lib/ffi-abi.f
require lib/fs-list.f
require lib/process.f

package PROC-TREE

private

1024 constant MEMBER-MAX
2000 constant SETTLE-MS             \ the walk's whole window; a tree that will not settle is killed as it stands
2 constant QUIET-PASSES

create MEMBERS MEMBER-MAX cells allot
variable MEMBER-N
variable SELF
variable ADDED                      \ members the pass in progress found
variable AWAKE                      \ what the pass in progress saw that is not yet settled

\ SIGKILL is 9 on every host; SIGSTOP is not one number.
: SIGSTOP ( -- n )
   HB-TARGET-MACOS? if 17 exit then
   HB-TARGET-LINUX? if 19 exit then
   HB-TARGET-LINUX-X86-64? if 19 exit then
   E-PROC-HOST throw ;

: MEMBER ( n -- n ) {: i:n :}
   i cells MEMBERS + @ ;

: MEMBER? ( n -- bool ) {: pid:n :}
   MEMBER-N @ 0 ?do
      i MEMBER pid = if true unloop exit then
   loop
   false ;

\ A member is stopped as it joins. The answer is not read: one that ended since
\ the scan saw it has nothing left to stop.
: MEMBER+ ( n -- ) {: pid:n :}
   MEMBER-N @ MEMBER-MAX >= if E-PROC-TRUNCATED throw then
   pid MEMBER-N @ cells MEMBERS + !
   MEMBER-N @ 1+ MEMBER-N !
   ADDED @ 1+ ADDED !
   pid >PID SIGSTOP PROC-KILL-RAW drop ;

\ ---- macOS: libproc ----------------------------------------------------------
\
\ sys/proc_info.h in the local SDK. proc_listpids filters the kernel's own
\ process list by parent (PROC_PPID_ONLY) or by group (PROC_PGRP_ONLY) and
\ answers the bytes it wrote, four a pid. The list holds a child from the
\ moment its parent's spawn creates it, which is long before the spawn
\ returns: that is what lets the walk see a child still being made.
\ PROC_PIDT_SHORTBSDINFO (13) fills a proc_bsdshortinfo of 64 bytes whose first
\ four fields are the u32 pid, parent pid, group id and status; SSTOP is 4.
\ PROC_PIDTASKINFO (4) fills a proc_taskinfo of 96 bytes whose pti_numrunning,
\ at 88, counts the threads that are runnable.
\
\ Measured here: a process reads SSTOP as soon as SIGSTOP is sent, whatever it
\ was doing, so the status alone does not say its system call is over. Both
\ lists hold zombies, and a zombie's group listing holds what it forked and
\ left. proc_pidinfo's third argument, 1, asks for a zombie's record as well
\ (the short flavor only: the task flavor answers ESRCH for one), and a zombie
\ answers with its own parent and group and status SZOMB; with 0 it answers
\ ESRCH, as for a pid nobody has. So a listed pid that answers no record even
\ with 1 is not yet born. SIGSTOP and SIGKILL to a zombie succeed and do
\ nothing.
\
\ A REFUSAL IS NOT AN ABSENCE. libproc answers a refused call the way it
\ answers an empty one - 0 bytes - and says which it was only in errno, which a
\ call that succeeds leaves as it found it. Measured here: a zombie or a
\ missing pid leaves ESRCH, a sandbox that denies the call EPERM, and a pid or
\ group nobody has lists nothing and leaves errno alone. So errno is cleared
\ before each call and read after a short answer, and anything but a plain
\ absence throws: a walk that took a refusal for "nobody there" would kill a
\ tree it had not seen and report it whole (lib/process-tree-test.f).

2 constant LIST-GROUP
6 constant LIST-CHILDREN
13 constant SHORT-FLAVOR
64 constant SHORT-BYTES
4 constant SHORT-PPID
12 constant SHORT-STATUS
4 constant SSTOP
4 constant TASK-FLAVOR
96 constant TASK-BYTES
88 constant TASK-RUNNING
4 constant PID-BYTES
3 constant ESRCH                    \ no such process
0 constant LIVE-ONLY                \ proc_pidinfo's third argument
1 constant ZOMBIE-TOO
1024 constant LIST-MAX              \ children, or group members, of one process
LIST-MAX PID-BYTES * constant LIST-BYTES

create LIST LIST-BYTES allot
create SHORT SHORT-BYTES allot
create TASK TASK-BYTES allot

PROCESS-SYMBOLS

FUNCTION: LIST-PIDS proc_listpids ( n n ptr u8 n -- i32 )
   2 3 WRITES-ARG
;FUNCTION

FUNCTION: PID-INFO proc_pidinfo ( n n n ptr u8 n -- i32 )
   3 4 WRITES-ARG
;FUNCTION

FUNCTION: ERRNO-CELL __error ( -- ptr u8 ) ;FUNCTION

: ERRNO-CLEAR ( -- )
   0 ERRNO-CELL LE:U32! ;

: ERRNO@ ( -- n )
   ERRNO-CELL LE:U32@ ;

\ TRUE with the record of the given flavor and size filled, FALSE when pid is
\ not there - or, asked LIVE-ONLY, is a zombie.
: RECORD? ( n n n ptr u8 n -- bool ) {: pid:n flavor:n arg:n buf:ptr size:n :}
   ERRNO-CLEAR
   pid flavor arg buf size PID-INFO size = if true exit then
   ERRNO@ ESRCH = if false exit then
   E-PROC-OUTPUT throw ;

: SHORT? ( n -- bool )
   SHORT-FLAVOR LIVE-ONLY SHORT SHORT-BYTES RECORD? ;

\ TRUE with SHORT filled for a live process or a zombie, FALSE for one not yet
\ born or already reaped.
: BORN? ( n -- bool )
   SHORT-FLAVOR ZOMBIE-TOO SHORT SHORT-BYTES RECORD? ;

\ Threads of pid that are runnable. A thread blocked in the kernel is not one,
\ which is the limit the header names. A process that ended since it was
\ asked about has none.
: RUNNING ( n -- n )
   TASK-FLAVOR LIVE-ONLY TASK TASK-BYTES RECORD? 0= if 0 exit then
   TASK TASK-RUNNING + LE:U32@ ;

\ A member is settled when it is stopped and none of its threads can run. The
\ stop is sent again until the first holds: a member that was not yet born
\ when it joined took no signal. One that answers no live record - a zombie,
\ or one reaped since - runs nothing.
: MEMBER-SETTLE ( n -- ) {: pid:n :}
   pid SHORT? 0= if exit then
   SHORT SHORT-STATUS + LE:U32@ SSTOP <> if
      pid >PID SIGSTOP PROC-KILL-RAW drop
      AWAKE @ 1+ AWAKE !
      exit
   then
   pid RUNNING 0 > if AWAKE @ 1+ AWAKE ! then ;

\ One listed pid. A child of the caller is the caller's own, zombie or not; an
\ unborn process keeps the pass from counting as quiet and is met again, born,
\ on the next.
: CANDIDATE ( n -- ) {: pid:n :}
   pid MEMBER? if exit then
   pid SELF @ = if exit then
   pid BORN? 0= if AWAKE @ 1+ AWAKE ! exit then
   SHORT SHORT-PPID + LE:U32@ SELF @ = if exit then
   pid MEMBER+ ;

\ A full buffer is a list that may have been cut: the kernel fills what fits
\ and says nothing of the rest.
: LISTED ( n n -- ) {: kind:n pid:n :}
   ERRNO-CLEAR
   kind pid LIST LIST-BYTES LIST-PIDS {: bytes:n :}
   bytes 0= ERRNO@ 0<> and if E-PROC-OUTPUT throw then
   bytes LIST-BYTES >= if E-PROC-TRUNCATED throw then
   bytes PID-BYTES / 0 ?do
      LIST i PID-BYTES * + LE:U32@ CANDIDATE
   loop ;

\ The table grows under the loop, so a member found in this pass has its own
\ children asked for in the same pass.
: SCAN-MACOS ( -- )
   0 begin dup MEMBER-N @ < while
      dup MEMBER MEMBER-SETTLE
      LIST-CHILDREN over MEMBER LISTED
      LIST-GROUP over MEMBER LISTED
      1+
   repeat drop ;

\ ---- Linux: /proc ------------------------------------------------------------
\
\ proc(5): /proc/<pid>/stat is one line, `pid (comm) state ppid pgrp ...`. comm
\ may itself hold spaces and parentheses, so the fields are read after the LAST
\ `)`. The three fields wanted sit in the first hundred bytes: comm is at most
\ 64.
\
\ That state is the main thread's alone, and any thread may spawn: lib/process.f
\ runs children from any task. /proc/<pid>/task/<tid>/stat holds each thread's,
\ in the same form. A thread shows T, stopped by a signal, or t, by a tracer,
\ only on its way back out of the kernel, so a clone it was in has returned and
\ the child it made, if any, is already listed; Z and X are a thread that has
\ exited. A member is settled when every thread of it shows one of the four.
\
\ A stat file or a task directory that will not open is taken to be a process
\ or thread that ended since it was listed. That is the one reading open
\ allows here: the engine's open answers a bare -1 with no errno
\ (src/habu/habu1.f, THE ERRNO RULE), so a refusal - /proc mounted with
\ hidepid, say - cannot be told from an absence as it is on macOS.

512 constant STAT-CAP
64 constant STAT-PATH-CAP           \ /proc/<pid>/task/<tid>/stat, each id at most seven digits
32 constant TASK-DIR-CAP            \ /proc/<pid>/task
$29 constant CLOSE-PAREN
10 constant RADIX

create STAT STAT-CAP allot
create STAT-PATH STAT-PATH-CAP allot
create TASK-DIR TASK-DIR-CAP allot
variable STAT-U
variable STAT-PATH-U
variable TASK-DIR-U
variable CUR                        \ the read position in STAT

: PATH+ ( ptr u8 n -- )
   STAT-PATH STAT-PATH-CAP STAT-PATH-U BUF-APPEND ;

: STAT-PATHZ ( ptr u8 n -- ptr u8 ) {: name:ptr nameu:n :}
   STAT-PATH-U BUF-RESET
   s" /proc/" PATH+
   name nameu PATH+
   s\" /stat\z" PATH+
   STAT-PATH ;

\ Reads the stat file pathz names into STAT: FALSE when it will not open or
\ holds nothing, as for a process or thread that ended since it was listed.
: LOAD-STAT? ( ptr u8 -- bool ) {: pathz:ptr :}
   pathz open-rd {: fd:n :}
   fd 0 < if false exit then
   fd STAT STAT-CAP read {: got:n :}
   fd close
   got 0 <= if false exit then
   got STAT-U !
   true ;

: LAST-PAREN ( -- n )
   -1
   STAT-U @ 0 ?do
      STAT i + c@ CLOSE-PAREN = if drop i then
   loop ;

: FIELD-BYTE? ( -- bool )
   CUR @ STAT-U @ >= if false exit then
   STAT CUR @ + c@ STR-SPACE <> ;

: SKIP-SPACES ( -- )
   begin
      CUR @ STAT-U @ < if STAT CUR @ + c@ STR-SPACE = else false then
   while
      CUR @ 1+ CUR !
   repeat ;

: TAKE-INT ( -- n )
   SKIP-SPACES
   CUR @ {: start:n :}
   begin FIELD-BYTE? while CUR @ 1+ CUR ! repeat
   STAT start + CUR @ start - STR>NUMBER? MATCH option
      none OF E-PROC-OUTPUT throw ENDOF
      some OF ENDOF
   ;MATCH ;

\ The state letter of the line in STAT, which leaves CUR past it.
: TAKE-STATE ( -- n )
   LAST-PAREN {: close:n :}
   close 0 < if E-PROC-OUTPUT throw then
   close 1+ CUR !
   SKIP-SPACES
   FIELD-BYTE? 0= if E-PROC-OUTPUT throw then
   STAT CUR @ + c@
   CUR @ 1+ CUR ! ;

\ One process of the /proc walk: its pid, its parent and the leader of its
\ group.
: VISIT ( n n n -- ) {: pid:n ppid:n pgid:n :}
   pid MEMBER? if exit then
   pid SELF @ = if exit then
   ppid SELF @ = if exit then
   ppid MEMBER? pgid MEMBER? or if pid MEMBER+ then ;

: PROCESS-ROW ( ptr u8 n n -- ) {: name:ptr nameu:n pid:n :}
   name nameu STAT-PATHZ LOAD-STAT? 0= if exit then
   TAKE-STATE drop
   TAKE-INT TAKE-INT {: ppid:n pgid:n :}
   pid ppid pgid VISIT ;

\ /proc holds more than processes; an entry whose name is no number is not one.
: VISIT-LINUX ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu STR>NUMBER? MATCH option
      none OF ENDOF
      some OF name nameu rot PROCESS-ROW ENDOF
   ;MATCH ;

: SETTLED-STATE? ( n -- bool ) {: c:n :}
   c [char] T = c [char] t = or c [char] Z = or c [char] X = or ;

: TASK-DIR+ ( ptr u8 n -- )
   TASK-DIR TASK-DIR-CAP TASK-DIR-U BUF-APPEND ;

\ The decimal digits of pid, most significant first.
: DIGITS+ ( n -- ) {: pid:n :}
   1 begin dup RADIX * pid <= while RADIX * repeat
   begin dup 0 > while
      pid over / RADIX mod [char] 0 + TASK-DIR TASK-DIR-CAP TASK-DIR-U BUF-APPEND-C
      RADIX /
   repeat drop ;

\ One thread of the member whose task directory is being listed.
: THREAD-VISIT ( ptr u8 n -- ) {: tid:ptr tidu:n :}
   STAT-PATH-U BUF-RESET
   TASK-DIR TASK-DIR-U BUF-LEN@ PATH+
   s" /" PATH+
   tid tidu PATH+
   s\" /stat\z" PATH+
   STAT-PATH LOAD-STAT? 0= if exit then
   TAKE-STATE SETTLED-STATE? 0= if AWAKE @ 1+ AWAKE ! then ;

: THREADS-LIST ( -- )
   TASK-DIR TASK-DIR-U BUF-LEN@ [: THREAD-VISIT ;] FS-LIST:EACH ;

\ A member that ended since it joined has no task directory left to list.
: MEMBER-SETTLE-LINUX ( n -- ) {: pid:n :}
   TASK-DIR-U BUF-RESET
   s" /proc/" TASK-DIR+
   pid DIGITS+
   s" /task" TASK-DIR+
   [: THREADS-LIST ;] catch {: code:n :}
   code 0= if exit then
   code FS-LIST:E-OPEN = if exit then
   code throw ;

\ Directory listings do not nest, so the threads are read once the walk of
\ /proc is done.
: SCAN-LINUX ( -- )
   s" /proc" [: VISIT-LINUX ;] FS-LIST:EACH
   MEMBER-N @ 0 ?do
      i MEMBER MEMBER-SETTLE-LINUX
   loop ;

\ ---- the walk ----------------------------------------------------------------

: SCAN ( -- )
   HB-TARGET-MACOS? if SCAN-MACOS exit then
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if SCAN-LINUX exit then
   E-PROC-HOST throw ;

\ TRUE when the pass added nobody and found every member settled.
: PASS ( -- bool )
   0 ADDED !
   0 AWAKE !
   SCAN
   ADDED @ 0= AWAKE @ 0= and ;

: SETTLE ( -- )
   SETTLE-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   0 begin dup QUIET-PASSES < while
      PASS if 1+ else drop 0 then
      deadline PROC-LEFT-MS MS>N 0= if drop exit then
   repeat drop ;

\ The answer is not read: a member that ended since it joined has nothing left
\ to kill.
: END-MEMBERS ( -- )
   MEMBER-N @ 0 ?do
      i MEMBER >PID SIGKILL PROC-KILL-RAW drop
   loop ;

public

\ SIGKILL pid and every process descended from it. A pid at or below 1 names
\ init or a group rather than a process, and the caller cannot be its own tree.
\ Whatever the walk throws - a process table it could not read, a tree past
\ MEMBER-MAX - every member found by then is still killed, so none is left
\ stopped. A SIGKILL of the caller runs no finally: what the walk had stopped
\ is left stopped (docs/gate.md).
: KILL-TREE ( pid -- ) {: pid:pid :}
   pid PID>N 1 <= if E-PROC-OUTPUT throw then
   getpid SELF !
   pid PID>N SELF @ = if E-PROC-OUTPUT throw then
   0 MEMBER-N !
   pid PID>N MEMBER+
   [: SETTLE ;] [: END-MEMBERS ;] finally ;

;package
