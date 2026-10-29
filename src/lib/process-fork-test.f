\ process-fork-test.f - a forked child's cleanup table, a child run as its own
\ process root that defines code of its own and keeps the code it was forked
\ with while the parent recompiles over it, a task's child that runs a word
\ main defined after the task's PREPARE, and reapers whose forks fail.
\ A forked child starts with an EMPTY fs cleanup table: inherited entries name
\ paths the live parent still owns. The pid contract of PROC-FORK:CHECKED and
\ RAW and 0 0 PROC-FORK:SET-PGID are proved where they are used
\ (src/lib/process-test.f's fork cases, SUBJECT:RUN, the gate pool).

require src/lib/errors.f
require src/lib/prelude.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-fork.f
require src/lib/task.f

\ The private FS words this test reads, forwarded into its own package.
package FS
: PROC-FORK-TEST:CLEANUP-COUNT ( -- n ) CLEANUP-N @ ;
;package

\ A global and a used public of one name: CHECK-CHILD-REFUSAL's source, which
\ names it bare under `using`, is refused with E-USING-SHADOW-GLOBAL. Packages
\ do not nest, so the pair is declared here, outside PROC-FORK-TEST.
package PF-SHADOW public
   : PF-SHADOW-W ( -- n ) 1 ;
;package
: PF-SHADOW-W ( -- n ) 2 ;

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

\ The child arm of PROC-FORK:RAW empties src/lib/fs-mutate.f's cleanup table
\ before the caller's child code runs. The child exits 1 when it inherited any
\ cleanup entry; the table has no fixed size, so its depth need not fit an exit
\ code. The child runs its own cleanup, so the parent sees the child's tree gone
\ and its own untouched. The fixture root is never registered, so no cleanup run
\ erases the evidence; the parent removes it at the end.
create FIX-ROOT PATH-CAP allot
create KEEP PATH-CAP allot            \ registered by the parent, before the fork
create KEEP-FILE PATH-CAP allot
create OWN PATH-CAP allot             \ registered by the child, after the fork
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
   u PATH-CAP > if E-FS-PATH throw then
   a FIX-ROOT u BYTE-COPY
   u FIX-ROOT-U ! ;

: SUB! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: base:ptr baseu:n name:ptr nameu:n dst:ptr up:ptr :}
   base baseu name nameu dst FS:JOIN-PATH up ! ;

: PATHS! ( -- )
   s" hb-fork-cleanup" FS:MKTEMP-DIR FIX-ROOT!
   FIX-ROOT$ s" keep" KEEP KEEP-U SUB!
   KEEP$ s" file" KEEP-FILE KEEP-FILE-U SUB!
   FIX-ROOT$ s" own" OWN OWN-U SUB! ;

\ The inherited depth is read before the child registers anything: its own
\ registration adds to it, and FS:CLEANUP-RUN clears it.
: CHILD-CLEANUP ( -- )
   CLEANUP-COUNT {: depth:n :}
   OWN$ FS:MAKE-DIRS
   OWN$ FS:CLEANUP-TREE+
   FS:CLEANUP-RUN
   depth 0<> if 1 else 0 then FORK-EXIT ;

: CHECK-CHILD-CLEANUP ( -- )
   PATHS!
   KEEP$ FS:MAKE-DIRS
   KEEP-FILE$ s" keep" FS:WRITE-ALL
   KEEP$ FS:CLEANUP-TREE+
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if CHILD-CLEANUP then
   s" fork child inherits an empty cleanup table" T-LABEL
   pid EXPECT-CLEAN-CHILD
   s" fork child ran its own cleanup" T-LABEL
   OWN$ FS:EXISTS? TFALSE
   s" the parent's registration survives the child" T-LABEL
   KEEP-FILE$ FS:FILE? TTRUE
   FS:CLEANUP-RUN
   s" the parent's own run removes what it registered" T-LABEL
   KEEP$ FS:EXISTS? TFALSE
   FIX-ROOT$ FS:REMOVE-TREE ;

\ PROC-FORK:CHILD-MAIN runs a quotation as the child's process root: it exits 0
\ when the quotation returns, and a throw out of it ends the child as an uncaught
\ throw ends a fresh process - the engine's line on fd 2 and UNCAUGHT-RC - with
\ no catch frame or code of the parent's resumed in the child. Each child's fd 2
\ is a fresh file, and ESCAPE-CHECK, the parent's code after the fork, writes
\ ESCAPED there and exits 0 if a child ever reaches it, as a child that left
\ CHILD-MAIN or ran a parent's handler would. CHILD-CODE lies outside 1-255,
\ so the engine reports it rather than exiting with it. Under --load the REPL
\ route is already empty, so the catch-frame and refusal clearing are what is
\ observable here.
-4242 constant CHILD-CODE
3 constant DUP2-FAILED-RC
create MAIN-ERR PATH-CAP allot
variable MAIN-ERR-U
variable PARENT-PID
variable MAIN-PID
256 constant CHILD-ERR-CAP
create CHILD-ERR CHILD-ERR-CAP allot

: MAIN-ERR$ ( -- ptr u8 n )
   MAIN-ERR MAIN-ERR-U @ ;

: ESCAPED$ ( -- ptr u8 n )
   s" ESCAPED" ;

: UNCAUGHT-LINE$ ( -- ptr u8 n )
   s\" hb: uncaught throw code -4242\n" ;

: ERR-TO-FILE ( -- )
   MAIN-ERR$ FS:OPEN-APPEND-FD {: fd :}
   fd 2 dup2 0 < if DUP2-FAILED-RC FORK-EXIT then
   fd close ;

\ Only a child that escaped CHILD-MAIN runs this with a pid not the parent's.
: ESCAPE-CHECK ( -- )
   getpid PARENT-PID @ = if exit then
   2 ESCAPED$ write drop
   0 FORK-EXIT ;

: FORK-MAIN ( [ -- ] -- ) {: q :}
   getpid PARENT-PID !
   MAIN-ERR$ s" " FS:WRITE-ALL
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if ERR-TO-FILE q PROC-FORK:CHILD-MAIN then
   ESCAPE-CHECK
   pid MAIN-PID ! ;

\ The reaped child's exit code, and its fd 2 in CHILD-ERR.
: MAIN-RC ( -- n n )
   MAIN-PID @ >PID PROC-WAIT-STATUS-RAW {: status:n :}
   status 0 < TFALSE
   status PROC-STATUS>RC RC>N
   MAIN-ERR$ CHILD-ERR CHILD-ERR-CAP FS:READ-ALL ;

: THROWER ( -- )
   CHILD-CODE throw ;

: CHECK-CHILD-RETURNS ( -- )
   [: ;] FORK-MAIN
   MAIN-RC {: rc errn :}
   s" a child whose quotation returns exits 0" T-LABEL
   rc 0 T=
   s" and writes nothing to fd 2" T-LABEL
   errn 0 T= ;

\ The parent forks inside a catch: a child that resumed it would leave the
\ catch with CHILD-CODE and reach ESCAPE-CHECK after it.
: CHECK-CHILD-THROWS ( -- )
   [: [: THROWER ;] FORK-MAIN ;] catch
   ESCAPE-CHECK
   s" the parent's own catch sees no throw" T-LABEL
   0 T=
   MAIN-RC {: rc errn :}
   s" a child whose quotation throws exits UNCAUGHT-RC" T-LABEL
   rc UNCAUGHT-RC T=
   s" its fd 2 has the engine's report" T-LABEL
   CHILD-ERR errn UNCAUGHT-LINE$ STR= TTRUE ;

\ The parent catches a refusal the checker named, which leaves its code in
\ REFUSAL-ABI:CODE-CELL; a child that kept the cell would exit RC-REJECT (70)
\ for a throw of that code. The refusal's diagnostic goes to the parent's fd 2.
variable REFUSAL-CODE

: REFUSE ( -- )
   s" using PF-SHADOW : PF-SHADOW-R ( -- n ) PF-SHADOW-W ; ;using" evaluate-closed ;

: REFUSAL-THROWER ( -- )
   REFUSAL-CODE @ throw ;

: CHECK-CHILD-REFUSAL ( -- )
   ['] REFUSE catch REFUSAL-CODE !
   s" the parent caught a refusal the checker named" T-LABEL
   REFUSAL-CODE @ 0<>
   data-base REFUSAL-ABI:CODE-CELL + @ REFUSAL-CODE @ = and TTRUE
   [: REFUSAL-THROWER ;] FORK-MAIN
   MAIN-RC drop {: rc :}
   s" a child throwing the parent's caught refusal exits UNCAUGHT-RC" T-LABEL
   rc UNCAUGHT-RC T= ;

\ Parent and child each define a word after the fork, at the same code address,
\ and each runs its own and finds the other's absent. The child defines first
\ and the parent second, so a region the two shared would hand the child the
\ parent's code, and a child left without its own region alias would fault on
\ its first code store. The child's release is the parent closing its end of
\ TO-CHILD, which cannot raise SIGPIPE in a parent whose child has died.
variable OWN-RESULT
variable TO-PARENT-R
variable TO-PARENT-W
variable TO-CHILD-R
variable TO-CHILD-W
create SYNC-BYTE 1 allot

: AWAITED? ( n -- bool )
   SYNC-BYTE 1 read 1 = ;

: FOUND? ( ptr u8 n -- bool )
   get-current search-wl 0<> ;

\ "<who> own|other absent|sees" on fd: whether the word just run answered
\ want, and whether the other process's word is in this dictionary.
: OWN-LINE ( n ptr u8 n n ptr u8 n -- ) {: fd who:ptr whon want other:ptr othern :}
   fd who whon write drop
   OWN-RESULT @ want = if fd s"  own" write drop else fd s"  other" write drop then
   other othern FOUND? if fd s"  sees" write drop else fd s"  absent" write drop then
   fd s\" \n" write drop ;

: CHILD-DEFINES ( -- )
   TO-PARENT-R @ close
   TO-CHILD-W @ close
   s" : CHILD-W ( -- n ) 22 ;" evaluate-closed
   TO-PARENT-W @ s" x" write drop
   TO-CHILD-R @ AWAITED? drop
   s" CHILD-W OWN-RESULT !" evaluate-closed
   2 s" child" 22 s" PARENT-W" OWN-LINE ;

: CHECK-CHILD-OWN-CODE ( -- )
   PIPE-PAIR {: pr:fd pw:fd :}
   PIPE-PAIR {: cr:fd cw:fd :}
   pr FD>N TO-PARENT-R !
   pw FD>N TO-PARENT-W !
   cr FD>N TO-CHILD-R !
   cw FD>N TO-CHILD-W !
   [: CHILD-DEFINES ;] FORK-MAIN
   pw FD>N close
   cr FD>N close
   s" the child defined its word after the fork" T-LABEL
   pr FD>N AWAITED? TTRUE
   s" : PARENT-W ( -- n ) 11 ;" evaluate-closed
   s" PARENT-W OWN-RESULT !" evaluate-closed
   1 s" parent" 11 s" CHILD-W" OWN-LINE
   s" the parent runs its own word" T-LABEL
   OWN-RESULT @ 11 T=
   s" and finds the child's absent" T-LABEL
   s" CHILD-W" FOUND? TFALSE
   cw FD>N close
   MAIN-RC {: rc errn :}
   CHILD-ERR errn type
   s" the child ran its own word and found the parent's absent" T-LABEL
   CHILD-ERR errn s\" child own absent\n" STR= TTRUE
   s" and exits 0" T-LABEL
   rc 0 T=
   pr FD>N close ;

\ The parent defines a word, forks, rolls its dictionary and code back to
\ before that word and compiles a different one at the same record slot and
\ code address, and only then releases the child, which runs the pre-fork word
\ by name. Its answer is the pre-fork body's: a child whose views were the
\ parent's live region would run the parent's new bytes, or find the record
\ gone. The marks after each definition prove the two words stood at one place.
: CHILD-RUNS-PRE-WORD ( -- )
   TO-CHILD-W @ close
   TO-CHILD-R @ AWAITED? drop
   s" PRE-W OWN-RESULT !" evaluate-closed
   2 s" child" 33 s" POST-W" OWN-LINE ;

: CHECK-CHILD-PRE-FORK-CODE ( -- )
   PIPE-PAIR {: cr:fd cw:fd :}
   cr FD>N TO-CHILD-R !
   cw FD>N TO-CHILD-W !
   ndict@ cp@ {: nmark:n cmark:n :}
   s" : PRE-W ( -- n ) 33 ;" evaluate-closed
   ndict@ cp@ {: npre:n cpre:n :}
   [: CHILD-RUNS-PRE-WORD ;] FORK-MAIN
   cr FD>N close
   nmark ndict!
   cmark cp!
   s" : POST-W ( -- n ) 44 ;" evaluate-closed
   s" the parent's new word stands where the pre-fork word stood" T-LABEL
   ndict@ npre T=
   cp@ cpre T=
   s" PRE-W" FOUND? TFALSE
   cw FD>N s" x" write drop
   cw FD>N close
   MAIN-RC {: rc errn :}
   CHILD-ERR errn type
   s" the child ran the pre-fork word's own body and found the new word absent" T-LABEL
   CHILD-ERR errn s\" child own absent\n" STR= TTRUE
   s" and exits 0" T-LABEL
   rc 0 T= ;

: CHECK-CHILD-MAIN ( -- )
   s" hb-fork-main" FS:MKTEMP-DIR FIX-ROOT!
   FIX-ROOT$ s" err" MAIN-ERR MAIN-ERR-U SUB!
   CHECK-CHILD-RETURNS
   CHECK-CHILD-THROWS
   CHECK-CHILD-REFUSAL
   CHECK-CHILD-OWN-CODE
   CHECK-CHILD-PRE-FORK-CODE
   FIX-ROOT$ FS:REMOVE-TREE ;

\ Main prepares a task, then defines a word that forks, and activates the task
\ with it. The child returns from `fork` into that word, so its region has to
\ hold the code main compiled between TASK:PREPARE and TASK:ACTIVATE. Only Linux
\ can fail here: its child gets a copy bounded by the task's dictionary snapshot
\ (LIBC-OS:FORK), and one taken at PREPARE leaves the word as zero bytes the
\ child dies on. A macOS child has a copy-on-write image of the whole region and
\ passes either way.
TASK:MIN-STACK TASK:TASK FORK-TASK
variable TASK-PIPE-W
create TASK-LINE CHILD-ERR-CAP allot

: TASK-CHILD-LINE$ ( -- ptr u8 n )
   s\" task child ran\n" ;

: TASK-CHILD-WRITES ( -- )
   TASK-PIPE-W @ TASK-CHILD-LINE$ write drop
   0 FORK-EXIT ;

: CHECK-TASK-CHILD-LATE-WORD ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   w FD>N TASK-PIPE-W !
   FORK-TASK TASK:PREPARE
   s" : TASK-FORKS ( -- ) PROC-FORK:CHECKED PID>N {: pid:n :} pid 0= if TASK-CHILD-WRITES then pid TASK:RETURN ;"
      evaluate-closed
   s" ' TASK-FORKS FORK-TASK TASK:ACTIVATE" evaluate-closed
   FORK-TASK TASK:JOIN MATCH result
     ok  OF ENDOF
     err OF drop -1 ENDOF
   ;MATCH {: pid:n :}
   w FD>N close
   s" the task forked a child" T-LABEL
   pid 0 > TTRUE
   pid 0 > if pid >PID PROC-WAIT-STATUS-RAW else -1 then {: status:n :}
   r FD>N TASK-LINE CHILD-ERR-CAP read {: n :}
   r FD>N close
   TASK-LINE n type
   s" the task's child ran the word main defined after the task's PREPARE" T-LABEL
   TASK-LINE n TASK-CHILD-LINE$ STR= TTRUE
   s" and exits 0" T-LABEL
   status PROC-STATUS>RC RC>N 0 T= ;

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
   CHECK-CHILD-MAIN
   CHECK-TASK-CHILD-LATE-WORD
   CHECK-REAPER-FORK-FAILS
   CHECK-INTERMEDIATE-FORK-FAILS
   CHECK-CAPTURE-REAPER-FAILS
   T-REPORT
   s" process-fork-test: ok" type cr ;

RUN

;package
