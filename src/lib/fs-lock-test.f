split-str
\ fs-lock-test.f - advisory directory ownership across processes.
\ Run: bin/hb --load src/lib/fs-lock-test.f
\ Failure modes: a second process can take a live hold; close or process death
\ leaves a hold behind; an exec child prolongs the parent's hold; an invalid
\ descriptor is mistaken for contention; a regular file is opened as a directory;
\ WAIT returns while another process still holds the lock, or without taking it;
\ RELEASE leaves the lock held while its descriptor stays open.

require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-fork.f
require src/lib/task.f
require src/lib/fs-lock.f

package FS-LOCK-TEST

1 constant F-GETFD
create ROOT PATH-CAP allot
create FILE-PATH PATH-CAP allot
create MARK-PATH PATH-CAP allot
create SIGNAL 1 allot
variable ROOT-U
variable MARK-U
variable HELD-FD
\ The holder's lead over the parent's WAIT. WAIT-BLOCKS proves its order by the
\ marker the holder writes before releasing, never by this length: a slow host
\ that reaches WAIT late still passes, only with less of a wait in between.
200 constant HOLD-MS

: ROOT$ ( -- ^u8 n ) ROOT ROOT-U @ ;

: SETUP ( -- )
   s" hb-fs-lock" FS:MKTEMP-DIR {: path:^ u:n :}
   path ROOT u BYTE-COPY
   u ROOT-U ! ;

: CHILD-EXIT ( n -- ) s" " rot die ;

: EXPECT-CLEAN ( pid -- )
   PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T= ;

: CHILD-BUSY ( -- )
   HELD-FD @ close
   ROOT$ FS-LOCK:OPEN-DIR {: fd:fd :}
   fd FS-LOCK:TRY if 1 else 0 then {: result:n :}
   fd FD>N close
   result CHILD-EXIT ;

: BUSY-AND-CLOSE ( -- )
   ROOT$ FS-LOCK:OPEN-DIR {: fd:fd :}
   fd FD>N HELD-FD !
   fd FS-LOCK:TRY TTRUE
   PROC-FORK:CHECKED {: child:pid :}
   child PID>N 0= if CHILD-BUSY then
   child EXPECT-CLEAN
   fd FD>N close
   ROOT$ FS-LOCK:OPEN-DIR {: again:fd :}
   again FS-LOCK:TRY TTRUE
   again FD>N close ;

: CHILD-DIES ( -- )
   ROOT$ FS-LOCK:OPEN-DIR {: fd:fd :}
   fd FS-LOCK:TRY if 0 else 1 then CHILD-EXIT ;

: DEATH-RELEASES ( -- )
   PROC-FORK:CHECKED {: child:pid :}
   child PID>N 0= if CHILD-DIES then
   child EXPECT-CLEAN
   ROOT$ FS-LOCK:OPEN-DIR {: fd:fd :}
   fd FS-LOCK:TRY TTRUE
   fd FD>N close ;

\ The holder takes the lock, tells the parent through the pipe, and HOLD-MS
\ later writes the marker and then releases the lock.
: CHILD-HOLDS ( fd -- )
   {: w:fd :}
   ROOT$ FS-LOCK:OPEN-DIR {: fd:fd :}
   fd FS-LOCK:TRY 0= if 1 CHILD-EXIT then
   w FD>N SIGNAL 1 write 1 <> if 2 CHILD-EXIT then
   HOLD-MS >MS TASK:SLEEP
   MARK-PATH MARK-U @ s" released" FS:WRITE-ALL
   fd FD>N close
   0 CHILD-EXIT ;


\ The parent's WAIT starts while the holder holds the lock, so it returns only
\ after the release, when the marker exists; then the parent holds the lock
\ and a further holder's TRY answers false.
: WAIT-BLOCKS ( -- )
   ROOT$ s" released" MARK-PATH FS:JOIN-PATH MARK-U !
   FD:PIPE-PAIR {: r:fd w:fd :}
   PROC-FORK:CHECKED {: holder:pid :}
   holder PID>N 0= if w CHILD-HOLDS then
   w FD>N close
   r FD>N SIGNAL 1 read 1 T=
   r FD>N close
   ROOT$ FS-LOCK:OPEN-DIR {: fd:fd :}
   fd FS-LOCK:WAIT
   MARK-PATH MARK-U @ FS:EXISTS? TTRUE
   fd FD>N HELD-FD !
   PROC-FORK:CHECKED {: child:pid :}
   child PID>N 0= if CHILD-BUSY then
   child EXPECT-CLEAN
   holder EXPECT-CLEAN
   fd FD>N close ;

\ A spawned process stays alive after the parent closes its hold. It must not
\ keep the directory locked through exec. The child is cat reading a pipe whose
\ only write end the parent holds, so it lives until the parent closes that end.
: EXEC-DOES-NOT-INHERIT ( -- )
   ROOT$ FS-LOCK:OPEN-DIR {: fd:fd :}
   fd FD>N F-GETFD 0 fcntl FD:FD-CLOEXEC and FD:FD-CLOEXEC T=
   fd FS-LOCK:TRY TTRUE
   FD:PIPE-PAIR {: r:fd w:fd :}
   r FD:FD-CLOEXEC!  w FD:FD-CLOEXEC!
   PROC-ARGV-RESET
   s" /bin/cat" >LEN r -1 >FD -1 >FD PROC-SPAWN-ARGV-IO {: child:pid :}
   r FD>N close
   fd FD>N close
   ROOT$ FS-LOCK:OPEN-DIR {: again:fd :}
   again FS-LOCK:TRY TTRUE
   again FD>N close
   w FD>N close
   child EXPECT-CLEAN ;

\ RELEASE frees the lock while its descriptor stays open: a second open file
\ description, blocked before, takes it.
: RELEASE-FREES ( -- )
   ROOT$ FS-LOCK:OPEN-DIR {: first:fd :}
   ROOT$ FS-LOCK:OPEN-DIR {: second:fd :}
   first FS-LOCK:TRY TTRUE
   second FS-LOCK:TRY TFALSE
   first FS-LOCK:RELEASE
   second FS-LOCK:TRY TTRUE
   second FD>N close
   first FD>N close ;

: BAD-FD ( -- ) -1 >FD FS-LOCK:TRY drop ;
: BAD-WAIT ( -- ) -1 >FD FS-LOCK:WAIT ;
: BAD-RELEASE ( -- ) -1 >FD FS-LOCK:RELEASE ;
: BAD-DIR ( -- )
   ROOT$ s" file" FILE-PATH FS:JOIN-PATH {: u:n :}
   FILE-PATH u s" x" FS:WRITE-ALL
   FILE-PATH u FS-LOCK:OPEN-DIR FD>N close ;

\ The quotation, run on a closed descriptor, throws E-FS-IO with EBADF in
\ FS:ERRNO; the cause is cleared first, so a stale one cannot pass.
: BAD-EBADF ( [ -- ] -- )
   FS:CAUSE-CLEAR
   catch FS:ERRNO {: code:n cause:n :}
   code E-FS-IO T=
   cause FS:EBADF T= ;

: ERRORS ( -- )
   [: BAD-FD ;] BAD-EBADF
   [: BAD-WAIT ;] BAD-EBADF
   [: BAD-RELEASE ;] BAD-EBADF
   [: BAD-DIR ;] E-FS-OPEN TTHROWSQ ;

: RUN ( -- )
   T-RESET SETUP
   [: BUSY-AND-CLOSE DEATH-RELEASES WAIT-BLOCKS EXEC-DOES-NOT-INHERIT RELEASE-FREES
      ERRORS ;] catch {: code:n :}
   ROOT$ FS:REMOVE-TREE
   code 0<> if code throw then
   T-REPORT ;

RUN
;package
