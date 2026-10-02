\ process-fork-test.f - a forked child's cleanup table, and a reaper whose fork
\ fails.
\
\ A forked child starts with an EMPTY fs cleanup table: the entries it would
\ otherwise inherit name paths the live parent still owns, so running them
\ deletes the parent's files. PROC-FORK:FORK-REAPER refuses a failed fork
\ instead of waiting on its pid. The pid contract of PROC-FORK:CHECKED and RAW
\ is proved where it is used (lib/process-test.f's fork cases, SUBJECT:RUN, the
\ gate pool), and so is 0 0 PROC-FORK:SET-PGID (every pool worker).
\ Run: bin/hb --load lib/errors.f lib/prelude.f lib/string.f lib/test.f \
\      lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-fork.f \
\      lib/process-fork-test.f

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
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

\ ---- a reaper whose fork fails -----------------------------------------------
\ FORK-REAPER's first fork fails at the user's process limit, and its pid is
\ then negative: -1 from macOS's libc fork. That pid must not reach the wait,
\ where wait4(-1) blocks for any child of the caller and reaps it. The case
\ holds RLIMIT_NPROC at 1, which refuses every fork of a user other than root,
\ while a sleeper forked before it, which the caller still owns, runs on.
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
variable REAPER-WA

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
\ once and is reaped, and the reaper is then not asked: armed for real, its
\ watch would signal this process's group when the death pipe closes.
: FORK-REFUSED? ( -- bool )
   PROC-FORK:RAW PID>N {: pid:n :}
   pid 0= if 0 FORK-EXIT then
   pid 0 < if true exit then
   pid EXPECT-CLEAN-CHILD
   false ;

: REAPER-UNDER-LIMIT ( -- )
   REAPER-PD @ >FD REAPER-WA @ >FD PROC-FORK:FORK-REAPER ;

: CHECK-REAPER-FORK-FAILS ( -- )
   PROC-FORK:DEATH-PIPE {: pd:fd pw:fd :}
   PROC-FORK:DEATH-PIPE {: wa:fd ww:fd :}
   pd FD>N REAPER-PD !
   wa FD>N REAPER-WA !
   PROC-FORK:CHECKED PID>N {: kid:n :}
   kid 0= if SLEEPER then
   NPROC-ONE
   s" RLIMIT_NPROC at 1 refuses this user's fork (root is exempt)" T-LABEL
   FORK-REFUSED? dup TTRUE
   if
      s" a reaper whose fork fails throws E-PROC-SPAWN" T-LABEL
      [: REAPER-UNDER-LIMIT ;] E-PROC-SPAWN TTHROWSQ
   then
   NPROC-RESTORE
   s" the caller's own child is still the caller's to reap" T-LABEL
   kid >PID PROC-WAIT-STATUS-RAW {: status:n :}
   status 0 < TFALSE
   status PROC-STATUS>RC RC>N SLEEPER-RC T=
   pd FD>N close
   pw FD>N close
   wa FD>N close
   ww FD>N close ;

: RUN ( -- )
   T-RESET
   CHECK-CHILD-CLEANUP
   CHECK-REAPER-FORK-FAILS
   T-REPORT
   s" process-fork-test: ok" type cr ;

RUN

;package
