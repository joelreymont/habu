\ fs-lock-test.f - advisory directory ownership across processes.
\ Run: bin/hb --load lib/fs-lock-test.f
\
\ Failure modes: a second process can take a live hold; close or process death
\ leaves a hold behind; an exec child prolongs the parent's hold; an invalid
\ descriptor is mistaken for contention; a regular file is opened as a directory.

require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-fork.f
require lib/fs-lock.f

package FS-LOCK-TEST

1 constant F-GETFD
create ROOT FS-PATH-CAP allot
create FILE-PATH FS-PATH-CAP allot
variable ROOT-U
variable HELD-FD

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: SETUP ( -- )
   s" hb-fs-lock" HB-TMP-MKDIR {: path:ptr u:n :}
   path ROOT u BYTE-COPY
   u ROOT-U ! ;

: CHILD-EXIT ( n -- ) s" " rot die ;

: EXPECT-CLEAN ( pid -- )
   PROC-WAIT-RC MATCH result
      ok OF 0 T= ENDOF
      err OF drop 1 0 T= ENDOF
   ;MATCH ;

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

\ A spawned process stays alive after the parent closes its hold. It must not
\ keep the directory locked through exec.
: EXEC-DOES-NOT-INHERIT ( -- )
   ROOT$ FS-LOCK:OPEN-DIR {: fd:fd :}
   fd FD>N F-GETFD 0 fcntl FD-CLOEXEC and FD-CLOEXEC T=
   fd FS-LOCK:TRY TTRUE
   PROC-ARGV-RESET
   s" 1" >LEN PROC-ARGV+
   s" /bin/sleep" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO {: child:pid :}
   fd FD>N close
   ROOT$ FS-LOCK:OPEN-DIR {: again:fd :}
   again FS-LOCK:TRY TTRUE
   again FD>N close
   child EXPECT-CLEAN ;

: BAD-FD ( -- ) -1 >FD FS-LOCK:TRY drop ;
: BAD-DIR ( -- )
   ROOT$ s" file" FILE-PATH JOIN-PATH {: u:n :}
   FILE-PATH u s" x" WRITE-ALL
   FILE-PATH u FS-LOCK:OPEN-DIR FD>N close ;

: ERRORS ( -- )
   [: BAD-FD ;] E-FS-IO TTHROWSQ
   [: BAD-DIR ;] E-FS-OPEN TTHROWSQ ;

: RUN ( -- )
   T-RESET SETUP
   [: BUSY-AND-CLOSE DEATH-RELEASES EXEC-DOES-NOT-INHERIT ERRORS ;] catch {: code:n :}
   ROOT$ REMOVE-TREE
   code 0<> if code throw then
   T-REPORT ;

RUN
;package
