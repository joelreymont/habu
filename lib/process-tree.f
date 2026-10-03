\ process-tree.f - end a process and every process descended from it, or read
\ the CPU time they have run.
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
\ WHETHER A ROOT CAN BE ASKED FIRST. CATCHES? reads whether a process runs a
\ handler for a signal. A pool asks a row's root to end itself with SIGTERM
\ only when it does: the default action would end the root at once, and what
\ it spawned, each leading its own group, would be left to init before the
\ walk could list it.
\
\ STORAGE CLASS. PROCESS-WIDE, one walk or CPU reading at a time: the member
\ table and the scan buffers are this file's. lib/process.f ends a capture's
\ child from whichever task ran the capture, so KILL-TREE, CATCHES? and CPU-NS
\ hold WALKING while they use them, and a task that finds it held waits its
\ turn.
\
\ BENEATH lib/process.f, which requires this file to end a capture's child. A
\ walk signals through the kill primitive and times itself with mono-ns, so it
\ needs nothing of lib/process.f.
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
require lib/task.f                  \ TASK:SLEEP, while another task walks

package PROC-TREE

private

1024 constant MEMBER-MAX
2000 constant SETTLE-MS             \ the walk's whole window; a tree that will not settle is killed as it stands
1000000 constant NS-PER-MS
2 constant QUIET-PASSES

create MEMBERS MEMBER-MAX cells allot
variable MEMBER-N
variable SELF
variable ADDED                      \ members the pass in progress found
variable AWAKE                      \ what the pass in progress saw that is not yet settled
variable WALKING                    \ 1 while a walk, a CATCHES? or a CPU-NS holds the storage above and below

\ SIGKILL is 9 on every host; SIGSTOP is not one number.
9 constant SIGKILL
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

: JOIN ( n -- ) {: pid:n :}
   MEMBER-N @ MEMBER-MAX >= if E-PROC-TRUNCATED throw then
   pid MEMBER-N @ cells MEMBERS + !
   MEMBER-N @ 1+ MEMBER-N !
   ADDED @ 1+ ADDED ! ;

\ A member is stopped as it joins. The answer is not read: one that ended since
\ the scan saw it has nothing left to stop.
: MEMBER+ ( n -- ) {: pid:n :}
   pid JOIN
   pid SIGSTOP kill drop ;

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
      pid SIGSTOP kill drop
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

\ The pids of pid's children or group, in LIST; answers how many. A full buffer
\ is a list that may have been cut: the kernel fills what fits and says nothing
\ of the rest.
: LIST! ( n n -- n ) {: kind:n pid:n :}
   ERRNO-CLEAR
   kind pid LIST LIST-BYTES LIST-PIDS {: bytes:n :}
   bytes 0= ERRNO@ 0<> and if E-PROC-OUTPUT throw then
   bytes LIST-BYTES >= if E-PROC-TRUNCATED throw then
   bytes PID-BYTES / ;

: LISTED@ ( n -- n ) {: i:n :}
   LIST i PID-BYTES * + LE:U32@ ;

: LISTED ( n n -- )
   LIST! 0 ?do i LISTED@ CANDIDATE loop ;

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
\ `)`. The fields wanted, the walk's three and CPU-NS's 14 to 17, end within
\ the first 400 bytes: comm is at most 64 and no number is longer than 20
\ digits.
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
   mono-ns SETTLE-MS NS-PER-MS * + {: deadline:n :}
   0 begin dup QUIET-PASSES < while
      PASS if 1+ else drop 0 then
      mono-ns deadline >= if drop exit then
   repeat drop ;

\ The answer is not read: a member that ended since it joined has nothing left
\ to kill.
: END-MEMBERS ( -- )
   MEMBER-N @ 0 ?do
      i MEMBER SIGKILL kill drop
   loop ;

\ One walk or query at a time. A task that finds WALKING held sleeps a
\ millisecond a turn rather than spin through a walk of up to SETTLE-MS.
: WALK-GET ( -- )
   begin 0 1 WALKING atomic-cas 0<> while 1 >MS TASK:SLEEP repeat ;

: WALK-RELEASE ( -- )
   0 WALKING atomic! ;

: WALK ( pid -- ) {: pid:pid :}
   getpid SELF !
   pid PID>N SELF @ = if E-PROC-OUTPUT throw then
   0 MEMBER-N !
   pid PID>N MEMBER+
   [: SETTLE ;] [: END-MEMBERS ;] finally ;

\ ---- a process that catches a signal ------------------------------------------
\
\ A signal a process catches runs its handler; any other takes the default
\ action or is ignored. Each host keeps the caught set as one bit per signal,
\ bit n-1 for signal n.
\
\ macOS: sysctl {CTL_KERN, KERN_PROC, KERN_PROC_PID, pid} fills a struct
\ kinfo_proc of 648 bytes whose kp_proc.p_sigcatch, a u32, sits at 236
\ (sys/sysctl.h and sys/proc.h in the local SDK; size and offset measured with
\ its clang). Measured here: an engine that ran SIGNAL:CATCH on SIGTERM reads
\ $4698, one that did not $698. A pid nobody has fills nothing and still
\ answers 0.
\
\ Linux: /proc/<pid>/status holds the set as the line `SigCgt:` and sixteen hex
\ digits (proc(5)). A status file that will not open is a process that ended.
\ Written from proc(5); it has run on no host here.

1 constant CTL-KERN
14 constant KERN-PROC
1 constant KERN-PROC-PID
4 constant MIB-N
648 constant KINFO-BYTES
236 constant KINFO-SIGCATCH
4096 constant STATUS-CAP

create MIB MIB-N 4 * allot
create KINFO KINFO-BYTES allot
create STATUS STATUS-CAP allot
variable KINFO-U                    \ sysctl's size_t in and out
variable STATUS-U

FUNCTION: SYSCTL sysctl ( ptr u8 n ptr u8 ptr u8 ptr u8 n -- i32 )
   2 KINFO-BYTES WRITES-BYTES
   3 8 WRITES-BYTES
;FUNCTION

: CAUGHT-BIT? ( n n -- bool ) {: set:n sig:n :}
   set 1 sig 1- lshift and 0<> ;

: CATCHES-MACOS? ( n n -- bool ) {: pid:n sig:n :}
   CTL-KERN MIB LE:U32!
   KERN-PROC MIB 4 + LE:U32!
   KERN-PROC-PID MIB 8 + LE:U32!
   pid MIB 12 + LE:U32!
   KINFO-BYTES KINFO-U !
   MIB MIB-N KINFO KINFO-U BYTE-VIEW NULL-PTR 0 SYSCTL 0 <> if E-PROC-OUTPUT throw then
   KINFO-U @ KINFO-BYTES <> if false exit then
   KINFO KINFO-SIGCATCH + LE:U32@ sig CAUGHT-BIT? ;

: HEX-DIGIT ( n -- n ) {: c:n :}
   c [char] 0 >= c [char] 9 <= and if c [char] 0 - exit then
   c [char] a >= c [char] f <= and if c [char] a - 10 + exit then
   -1 ;

\ The offset just past `SigCgt:` at the start of a line of STATUS.
: SIGCGT-AT ( -- n )
   s" SigCgt:" {: key:ptr keyu:n :}
   STATUS-U @ 0 ?do
      i 0= if true else STATUS i 1- + c@ 10 = then
      if
         STATUS i + STATUS-U @ i - key keyu STARTS-WITH? if i keyu + unloop exit then
      then
   loop
   E-PROC-OUTPUT throw ;

: BLANK? ( n -- bool ) {: c:n :}
   c 9 = c STR-SPACE = or ;

\ The hex number at offset at in STATUS, past the tab or spaces before it.
: HEX-AT ( n -- n ) {: at:n :}
   at begin
      dup STATUS-U @ < if STATUS over + c@ BLANK? else false then
   while 1+ repeat
   0 swap begin
      dup STATUS-U @ < if STATUS over + c@ HEX-DIGIT 0 >= else false then
   while
      STATUS over + c@ HEX-DIGIT rot 4 lshift or swap 1+
   repeat drop ;

: CATCHES-LINUX? ( n n -- bool ) {: pid:n sig:n :}
   TASK-DIR-U BUF-RESET
   s" /proc/" TASK-DIR+
   pid DIGITS+
   s\" /status\z" TASK-DIR+
   TASK-DIR open-rd {: fd:n :}
   fd 0 < if false exit then
   fd STATUS STATUS-CAP read {: got:n :}
   fd close
   got 0 <= if false exit then
   got STATUS-U !
   SIGCGT-AT HEX-AT sig CAUGHT-BIT? ;

: CATCHES-HOST? ( n n -- bool ) {: pid:n sig:n :}
   HB-TARGET-MACOS? if pid sig CATCHES-MACOS? exit then
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if pid sig CATCHES-LINUX? exit then
   E-PROC-HOST throw ;

\ ---- the CPU time a tree has run ----------------------------------------------
\
\ CPU-NS adds up the CPU time, user and system, that a process and every live
\ process descended from it have run, each with the time of the children it
\ reaped: a child's time passes to its parent when the parent waits for it, so
\ the tree's work is counted whole while its processes come and go. Nothing is
\ stopped; a reading is a moment of a running tree.
\
\ THROUGH PARENTS ONLY. A member's children join it; the groups KILL-TREE also
\ follows do not. A process whose parent exited before it - an orphan - has
\ left the tree, and its time with it.
\
\ EACH MEMBER IS READ BEFORE ITS CHILDREN ARE LISTED. A child its parent reaps
\ between the two is in neither count rather than in both, so a reading may
\ fall short of the tree's time and never exceeds it.
\
\ macOS: proc_pid_rusage with RUSAGE_INFO_V2 (2) fills a struct rusage_info_v2
\ of 160 bytes (sys/resource.h in the local SDK): ri_user_time and
\ ri_system_time at 16 and 24 are the process's own, ri_child_user_time and
\ ri_child_system_time at 96 and 104 what it reaped. They count mach absolute
\ time units, which mach_timebase_info's numer/denom scales to nanoseconds:
\ 125/3 on the Apple silicon host here, where a child's time read this way
\ matched the parent's getrusage of it once reaped. A zombie answers with its
\ final times, a pid already reaped with ESRCH, another user's process with
\ EPERM (measured).
\
\ Linux: fields 14 to 17 of /proc/<pid>/stat are utime, stime, cutime and
\ cstime, in clock ticks of USER_HZ, 100 on every architecture this engine
\ targets (proc(5)). A child may be listed before its parent, so /proc is read
\ again until a pass adds nobody. Written from proc(5); it has run on no host
\ here.

2 constant RUSAGE-FLAVOR
160 constant RUSAGE-BYTES
16 constant RU-USER
24 constant RU-SYSTEM
96 constant RU-CHILD-USER
104 constant RU-CHILD-SYSTEM
8 constant TIMEBASE-BYTES           \ u32 numer, then u32 denom
100 constant USER-HZ
1000000000 constant NS-PER-S

create RUSAGE RUSAGE-BYTES allot
create TIMEBASE TIMEBASE-BYTES allot
variable TICKS                      \ the reading in progress

FUNCTION: PID-RUSAGE proc_pid_rusage ( n n ptr u8 -- i32 )
   2 RUSAGE-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: TIMEBASE-INFO mach_timebase_info ( ptr u8 -- i32 )
   0 TIMEBASE-BYTES WRITES-BYTES
;FUNCTION

: RU@ ( n -- n ) {: off:n :}
   RUSAGE off + LE:U64@ ;

\ The time units pid ran and reaped; 0 for one already reaped.
: UNITS-MACOS ( n -- n ) {: pid:n :}
   ERRNO-CLEAR
   pid RUSAGE-FLAVOR RUSAGE PID-RUSAGE 0= if
      RU-USER RU@ RU-SYSTEM RU@ + RU-CHILD-USER RU@ + RU-CHILD-SYSTEM RU@ + exit
   then
   ERRNO@ ESRCH = if 0 exit then
   E-PROC-OUTPUT throw ;

: UNITS>NS ( n -- n ) {: units:n :}
   TIMEBASE TIMEBASE-INFO 0<> if E-PROC-OUTPUT throw then
   units TIMEBASE LE:U32@ * TIMEBASE 4 + LE:U32@ / ;

\ The table grows under the loop, so a member's children are read in the same
\ pass that lists them.
: CPU-MACOS ( -- n )
   0 TICKS !
   0 begin dup MEMBER-N @ < while
      dup MEMBER UNITS-MACOS TICKS @ + TICKS !
      LIST-CHILDREN over MEMBER LIST! 0 ?do
         i LISTED@ dup MEMBER? if drop else JOIN then
      loop
      1+
   repeat drop
   TICKS @ UNITS>NS ;

\ Fields 14 to 17 of the line in STAT, with CUR past field 4, the parent.
: TAKE-TIMES ( -- n )
   9 0 ?do TAKE-INT drop loop
   TAKE-INT TAKE-INT + TAKE-INT + TAKE-INT + ;

\ One process of a pass of /proc: it joins, with its time, once its parent is
\ a member.
: CPU-ROW ( ptr u8 n n -- ) {: name:ptr nameu:n pid:n :}
   pid MEMBER? if exit then
   name nameu STAT-PATHZ LOAD-STAT? 0= if exit then
   TAKE-STATE drop
   TAKE-INT MEMBER? 0= if exit then
   pid JOIN
   TAKE-TIMES TICKS @ + TICKS ! ;

: CPU-VISIT-LINUX ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu STR>NUMBER? MATCH option
      none OF ENDOF
      some OF name nameu rot CPU-ROW ENDOF
   ;MATCH ;

\ The root's own line; 0 once it is reaped.
: ROOT-TICKS-LINUX ( n -- n ) {: pid:n :}
   TASK-DIR-U BUF-RESET
   s" /proc/" TASK-DIR+
   pid DIGITS+
   s\" /stat\z" TASK-DIR+
   TASK-DIR LOAD-STAT? 0= if 0 exit then
   TAKE-STATE drop
   TAKE-INT drop
   TAKE-TIMES ;

: CPU-LINUX ( -- n )
   0 MEMBER ROOT-TICKS-LINUX TICKS !
   begin
      0 ADDED !
      s" /proc" [: CPU-VISIT-LINUX ;] FS-LIST:EACH
      ADDED @ 0=
   until
   TICKS @ NS-PER-S USER-HZ / * ;

\ CPU-NS's reading, run with WALKING held.
: CPU-READ ( n -- n ) {: pid:n :}
   0 MEMBER-N !
   pid JOIN
   HB-TARGET-MACOS? if CPU-MACOS exit then
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if CPU-LINUX exit then
   E-PROC-HOST throw ;

public

\ SIGKILL pid and every process descended from it. A pid at or below 1 names
\ init or a group rather than a process, and the caller cannot be its own tree.
\ Whatever the walk throws - a process table it could not read, a tree past
\ MEMBER-MAX - every member found by then is still killed, so none is left
\ stopped. A SIGKILL of the caller runs no finally: what the walk had stopped
\ is left stopped (docs/gate.md).
: KILL-TREE ( pid -- ) {: pid:pid :}
   pid PID>N 1 <= if E-PROC-OUTPUT throw then
   WALK-GET
   pid [: WALK ;] [: WALK-RELEASE ;] finally ;

\ TRUE when pid runs a handler for signal sig; FALSE when the default action
\ or SIG_IGN takes it, or when nobody has pid. A caller that would SIGKILL a
\ tree asks this first to know whether its root can be asked to end itself
\ (test/gate-pool.f GT-POOL-ASK-END).
: CATCHES? ( pid n -- bool ) {: pid:pid sig:n :}
   WALK-GET
   pid PID>N sig [: CATCHES-HOST? ;] [: WALK-RELEASE ;] finally ;

\ The CPU time, in nanoseconds, pid and every live process descended from it
\ have run, user and system, with what each of them reaped; 0 once pid is
\ reaped. Nothing is stopped. A refusal throws, as in a walk; an absence is
\ none. A pool holds a row to a CPU budget with it (test/gate-pool.f
\ GT-POOL-CPU-BUDGET!).
: CPU-NS ( pid -- n ) {: pid:pid :}
   pid PID>N 1 <= if E-PROC-OUTPUT throw then
   WALK-GET
   pid PID>N [: CPU-READ ;] [: WALK-RELEASE ;] finally ;

;package
