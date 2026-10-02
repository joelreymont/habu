\ process-fork-test.f - a forked child's cleanup table, and reapers whose forks
\ fail.
\
\ A forked child starts with an EMPTY fs cleanup table: the entries it would
\ otherwise inherit name paths the live parent still owns, so running them
\ deletes the parent's files. Every failed reaper fork is refused:
\ PROC-FORK:FORK-REAPER throws when either of its forks fails, and a capture
\ whose co-located reaper (PROC-FORK:SPAWN-REAPER) cannot fork kills its child
\ and throws. The pid contract of PROC-FORK:CHECKED and RAW is proved where it
\ is used (lib/process-test.f's fork cases, SUBJECT:RUN, the gate pool), and so
\ is 0 0 PROC-FORK:SET-PGID (every pool worker).
\ Run: bin/hb --load lib/errors.f lib/prelude.f lib/string.f lib/test.f \
\      lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f \
\      lib/process-fork.f lib/process-fork-test.f

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-fork.f

package PROC-FORK-TEST

: FORK-EXIT ( n -- )
   s" " rot die ;

\ Reap the forked child by pid: a clean (0) exit lands on the ok arm; anything
\ else is a test failure.
: EXPECT-CLEAN-CHILD ( n -- )
   >PID PROC-WAIT-RC MATCH result
     ok  OF 0 T= ENDOF
     err OF drop -1 0 T= ENDOF
   ;MATCH ;

\ Fork-inherited cleanup registrations, straight through the wrapper instead of
\ through a pool: whoever the caller is, the child arm of PROC-FORK:RAW empties
\ lib/fs-mutate.f's cleanup table before the caller's child code runs.
\
\ The parent registers a tree, forks, and the child exits with the cleanup depth
\ it inherited as its exit code - so the reap alone decides the invariant, since
\ an inherited entry comes back as a nonzero code. The table caps at
\ FS-MUT-CLEANUP-MAX, so the depth always fits in an exit code. The child also
\ registers and runs its OWN cleanup, which lets the parent see afterwards that
\ the child's tree is gone (the machinery really ran) while the parent's tree is
\ untouched. The fixture root itself is never registered, so no cleanup run can
\ erase the evidence; the parent removes it at the end.
create FIX-ROOT FS-PATH-CAP allot        \ fixture root; never registered
create KEEP FS-PATH-CAP allot            \ registered by the parent, before the fork
create KEEP-FILE FS-PATH-CAP allot
create OWN FS-PATH-CAP allot             \ registered by the child, after the fork
variable FIX-ROOT-U
variable KEEP-U
variable KEEP-FILE-U
variable OWN-U

: FIX-ROOT$ ( -- ptr u8 n )
   FIX-ROOT FIX-ROOT-U @ ;

: KEEP$ ( -- ptr u8 n )
   KEEP KEEP-U @ ;

: KEEP-FILE$ ( -- ptr u8 n )
   KEEP-FILE KEEP-FILE-U @ ;

: OWN$ ( -- ptr u8 n )
   OWN OWN-U @ ;

: FIX-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a FIX-ROOT u BYTE-COPY
   u FIX-ROOT-U ! ;

: SUB! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: base:ptr baseu:n name:ptr nameu:n dst:ptr up:ptr :}
   base baseu name nameu dst JOIN-PATH up ! ;

: PATHS! ( -- )
   s" hb-fork-cleanup" HB-TMP-MKDIR FIX-ROOT!
   FIX-ROOT$ s" keep" KEEP KEEP-U SUB!
   KEEP$ s" file" KEEP-FILE KEEP-FILE-U SUB!
   FIX-ROOT$ s" own" OWN OWN-U SUB! ;

\ Child body: read the inherited depth BEFORE registering anything of its own
\ (its own registration would add to it, and CLEANUP-RUN clears it), then clean
\ up what it registered and carry the depth out as the exit code.
: CHILD-CLEANUP ( -- )
   FS-MUT-CLEANUP-N @ {: depth:n :}
   OWN$ MAKE-DIRS
   OWN$ CLEANUP-TREE+
   CLEANUP-RUN
   depth FORK-EXIT ;

: CHECK-CHILD-CLEANUP ( -- )
   PATHS!
   KEEP$ MAKE-DIRS
   KEEP-FILE$ s" keep" WRITE-ALL
   KEEP$ CLEANUP-TREE+
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if CHILD-CLEANUP then
   s" fork child inherits an empty cleanup table" T-LABEL
   pid EXPECT-CLEAN-CHILD
   s" fork child ran its own cleanup" T-LABEL
   OWN$ EXISTS? TFALSE
   s" the parent's registration survives the child" T-LABEL
   KEEP-FILE$ FILE? TTRUE
   CLEANUP-RUN
   s" the parent's own run removes what it registered" T-LABEL
   KEEP$ EXISTS? TFALSE
   FIX-ROOT$ REMOVE-TREE ;

\ ---- reapers whose forks fail -------------------------------------------------
\ A refused fork answers a negative pid: -1 from macOS's libc fork, -EAGAIN from
\ Linux's raw clone. That pid must reach neither a wait, where wait4(-1) blocks
\ for any child of the caller and reaps it, nor a caller that takes it for an
\ armed reaper.
\
\ The kernel refuses a fork at the user's process limit: RLIMIT_NPROC at 1
\ refuses every fork of a user other than root, while a sleeper forked before
\ it, which the caller still owns, runs on. That limit counts every process of
\ the user, so it cannot refuse one fork of a pair without racing the rest of
\ the host, and it never refuses root: the cases it cannot make refuse their
\ fork through PROC-FORK:FORK-CALL.
PROCESS-SYMBOLS
FUNCTION: GET-LIMIT getrlimit ( n ptr u8 -- i32 )
   1 16 WRITES-BYTES
;FUNCTION
FUNCTION: SET-LIMIT setrlimit ( n ptr u8 -- i32 ) ;FUNCTION

create SAVED-NPROC 16 allot
create NPROC-NOW 16 allot
create NO-FDS 8 allot                    \ an empty pollfd set: poll only waits
300 constant SLEEPER-MS                  \ outlasts the reaper call, bounds a wrong wait
7 constant SLEEPER-RC
variable REAPER-PD
variable REAPER-PW
variable REAPER-WA
variable REAPER-WW
variable OWN-PID
variable FORKS-MADE

: NPROC ( -- n ) HB-TARGET-MACOS? if 7 else 6 then ;

: NPROC-ONE ( -- )
   NPROC SAVED-NPROC GET-LIMIT 0 T=
   1 NPROC-NOW !
   SAVED-NPROC cell+ @ NPROC-NOW cell+ !
   NPROC NPROC-NOW SET-LIMIT 0 T= ;

\ The soft limit goes back under the hard limit the kernel holds now: macOS
\ clamps a non-root caller's hard limit to kern.maxprocperuid on every
\ setrlimit and refuses to raise it again.
: NPROC-RESTORE ( -- )
   NPROC NPROC-NOW GET-LIMIT 0 T=
   SAVED-NPROC @ NPROC-NOW !
   NPROC NPROC-NOW SET-LIMIT 0 T= ;

: SLEEPER ( -- )
   NO-FDS 0 SLEEPER-MS poll drop
   SLEEPER-RC FORK-EXIT ;

\ Whether the limit refuses a fork here. A root user's probe child leaves at
\ once and is reaped, and the reaper's fork is then refused through FORK-CALL:
\ armed for real, its watch would signal this process's group when the death
\ pipe closes.
: FORK-REFUSED? ( -- bool )
   PROC-FORK:RAW PID>N {: pid:n :}
   pid 0= if 0 FORK-EXIT then
   pid 0 < if true exit then
   pid EXPECT-CLEAN-CHILD
   false ;

\ The refusals FORK-CALL takes: every fork, or every fork but this process's
\ own, which refuses only the forks of the processes this one makes. They
\ answer Linux's -EAGAIN, not macOS's -1: a refused pid that leaked into a kill
\ as -1 would signal every process this user owns.
-11 constant REFUSED-PID

: REFUSE-EVERY ( -- n )
   REFUSED-PID ;

: REFUSE-BELOW ( -- n )
   getpid OWN-PID @ = if
      1 FORKS-MADE +!
      fork exit
   then
   REFUSED-PID ;

: REAPER-PIPES ( -- )
   PROC-FORK:DEATH-PIPE {: pd:fd pw:fd :}
   PROC-FORK:DEATH-PIPE {: wa:fd ww:fd :}
   pd FD>N REAPER-PD !
   pw FD>N REAPER-PW !
   wa FD>N REAPER-WA !
   ww FD>N REAPER-WW ! ;

: REAPER-PIPES-CLOSE ( -- )
   REAPER-PD @ close
   REAPER-PW @ close
   REAPER-WA @ close
   REAPER-WW @ close ;

: ARM-FORK-REAPER ( -- )
   REAPER-PD @ >FD REAPER-WA @ >FD PROC-FORK:FORK-REAPER ;

\ The kernel's limit refuses the reaper's first fork where it can; elsewhere
\ the case says so and FORK-CALL refuses it.
: REFUSE-FIRST-FORK ( -- )
   NPROC-ONE
   FORK-REFUSED? if exit then
   s" process-fork-test: RLIMIT_NPROC refuses no fork here (root is exempt); PROC-FORK:FORK-CALL refuses the reaper's" type cr
   [: REFUSE-EVERY ;] is PROC-FORK:FORK-CALL ;

: CHECK-REAPER-FORK-FAILS ( -- )
   REAPER-PIPES
   PROC-FORK:CHECKED PID>N {: kid:n :}
   kid 0= if SLEEPER then
   REFUSE-FIRST-FORK
   s" a reaper whose fork fails throws E-PROC-SPAWN" T-LABEL
   [: ARM-FORK-REAPER ;] E-PROC-SPAWN TTHROWSQ
   PROC-FORK:FORK-CALL-DEFAULT
   NPROC-RESTORE
   s" the caller's own child is still the caller's to reap" T-LABEL
   kid >PID PROC-WAIT-STATUS-RAW {: status:n :}
   status 0 < TFALSE
   status PROC-STATUS>RC RC>N SLEEPER-RC T=
   REAPER-PIPES-CLOSE ;

\ FORK-REAPER's intermediate makes the second fork, the reaper's own, and the
\ worker only waits on the intermediate. Refusing every fork but this
\ process's own refuses that fork alone; FORKS-MADE counts the worker's.
: CHECK-INTERMEDIATE-FORK-FAILS ( -- )
   REAPER-PIPES
   getpid OWN-PID !
   0 FORKS-MADE !
   [: REFUSE-BELOW ;] is PROC-FORK:FORK-CALL
   s" a reaper whose intermediate cannot fork throws E-PROC-SPAWN" T-LABEL
   [: ARM-FORK-REAPER ;] E-PROC-SPAWN TTHROWSQ
   PROC-FORK:FORK-CALL-DEFAULT
   s" the worker forked the intermediate whose fork was refused" T-LABEL
   FORKS-MADE @ 1 T=
   REAPER-PIPES-CLOSE ;

\ ---- a capture whose reaper cannot fork -------------------------------------
\ While a death-watch fd is published, every capture arms a co-located reaper
\ for its child (PROC-REAP-ARM, then PROC-FORK:SPAWN-REAPER). FORK-CALL refuses
\ that fork alone: the capture's own spawn is the spawn primitive. The capture
\ is refused with it: its child is killed and reaped and its descriptors closed
\ before E-PROC-SPAWN goes on, so no capture child runs unwatched. The child
\ would sleep past the capture's deadline, so a status of SIGKILL with no
\ timeout is the capture's own kill.
256 constant CAP
1000 constant CAPTURE-MS                 \ bounds the capture if the refusal is missed
create OUT-BUF CAP allot
create ERR-BUF CAP allot
variable SAVED-WATCH

: CAPTURE-SLEEPER ( -- )
   PROC-ARGV-RESET
   s" 30" >LEN PROC-ARGV+
   s" /bin/sleep" >LEN OUT-BUF CAP >LEN ERR-BUF CAP >LEN CAPTURE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME MATCH outcome
     exited   OF drop ENDOF
     signaled OF drop ENDOF
     timeout  OF ENDOF
   ;MATCH 2drop ;

: CHECK-CAPTURE-REAPER-FAILS ( -- )
   PROC-FORK:DEATH-PIPE {: wa:fd ww:fd :}
   PROC-FORK:REAP-WATCH-FD @ SAVED-WATCH !
   wa FD>N PROC-FORK:REAP-WATCH-FD !
   [: REFUSE-EVERY ;] is PROC-FORK:FORK-CALL
   s" a capture whose reaper cannot fork throws E-PROC-SPAWN" T-LABEL
   [: CAPTURE-SLEEPER ;] E-PROC-SPAWN TTHROWSQ
   PROC-FORK:FORK-CALL-DEFAULT
   SAVED-WATCH @ PROC-FORK:REAP-WATCH-FD !
   s" the refused capture killed and reaped its child" T-LABEL
   PROC-TIMED-OUT @ 0 T=
   PROC-STATUS @ PROC-STATUS>RC RC>N 128 SIGKILL + T=
   s" the refused capture closed its descriptors" T-LABEL
   PROC-OUT-R @ PROC-NO-FD T=
   PROC-ERR-R @ PROC-NO-FD T=
   wa FD>N close
   ww FD>N close ;

: RUN ( -- )
   T-RESET
   CHECK-CHILD-CLEANUP
   CHECK-REAPER-FORK-FAILS
   CHECK-INTERMEDIATE-FORK-FAILS
   CHECK-CAPTURE-REAPER-FAILS
   T-REPORT
   s" process-fork-test: ok" type cr ;

RUN

;package
