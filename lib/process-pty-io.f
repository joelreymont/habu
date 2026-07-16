\ process-pty-io.f - checked PTY supervisor and linear lifecycle API.

require lib/process-pty-handle.f
require lib/process-fork.f
require lib/memory.f

package PROCESS-PTY

$1000 constant PATH-CAP
$80 constant PTY-PATH-CAP
$20000 constant O-NOCTTY
2 constant O-RDWR
2 constant F-SETFD
1 constant FD-CLOEXEC-FLAG
1 constant POLLIN
8 constant POLLERR
$10 constant POLLHUP
$20 constant POLLNVAL
9 constant SIGKILL
0 constant SIGNAL-PROBE
$1388 constant KILL-WAIT-MS

1 constant CLEAN-LIFE-W
2 constant CLEAN-SUP
4 constant CLEAN-MASTER
8 constant CLEAN-SLAVE
$10 constant CLEAN-LIFE-R
$20 constant CLEAN-DONE-R
$40 constant CLEAN-DONE-W
$80 constant CLEAN-CANCEL
$100 constant CLEAN-GUARD
$200 constant CLEAN-ANCHOR-R
$400 constant CLEAN-ANCHOR-W
$800 constant CLEAN-GROUP
$1000 constant CLEAN-GROUP-WAIT
$2000 constant CLEAN-SUP-GUARD
$4000 constant CLEAN-ARM-R
$8000 constant CLEAN-ARM-W
$10000 constant CLEAN-ACK-R
$20000 constant CLEAN-ACK-W
$40000 constant CLEAN-TARGET-ACK-R
$80000 constant CLEAN-TARGET-ACK-W

1 constant SUP-CLEAN-KILL
2 constant SUP-CLEAN-WAIT
4 constant SUP-CLEAN-GATE-R
8 constant SUP-CLEAN-GATE-W
$10 constant SUP-CLEAN-EXEC-R
$20 constant SUP-CLEAN-EXEC-W
$40 constant SUP-CLEAN-PROC
$80 constant SUP-CLEAN-OWNER
$100 constant SUP-CLEAN-MASTER
$200 constant SUP-CLEAN-SLAVE
$400 constant SUP-CLEAN-LIFE-R
$800 constant SUP-CLEAN-LIFE-W
$1000 constant SUP-CLEAN-DONE-R
$2000 constant SUP-CLEAN-PROTOCOL
$4000 constant SUP-CLEAN-ANCHOR
$8000 constant SUP-CLEAN-GROUP
$10000 constant SUP-CLEAN-ANCHOR-R
$20000 constant SUP-CLEAN-ANCHOR-W
$40000 constant SUP-CLEAN-READY-R
$80000 constant SUP-CLEAN-READY-W
$100000 constant SUP-CLEAN-ACK-R
SUP-CLEAN-KILL SUP-CLEAN-WAIT or SUP-CLEAN-ANCHOR or SUP-CLEAN-GROUP or
constant SUP-CLEAN-AUTH

$20007454 constant DARWIN-TIOCPTYGRANT
$40807453 constant DARWIN-TIOCPTYGNAME
$20007452 constant DARWIN-TIOCPTYUNLK
$20007461 constant DARWIN-TIOCSCTTY
$80047476 constant DARWIN-TIOCSPGRP
$40045431 constant LINUX-TIOCSPTLCK
$80045430 constant LINUX-TIOCGPTN
$540E constant LINUX-TIOCSCTTY
$5410 constant LINUX-TIOCSPGRP
1 constant EPERM#
3 constant ESRCH#

create EXE-Z PATH-CAP allot
create MASTER-Z PTY-PATH-CAP allot
create SLAVE-Z PTY-PATH-CAP allot
create ARGV 2 cells allot
create ENVP 1 cells allot
create FRAME 1 cells allot
create SIGNAL-BYTE 1 allot
create POLL-FDS 3 cells allot

ENUM watch-event target-exit owner-exit cancel ;ENUM

variable BUSY
variable PATH-A
variable PATH-U
variable PTY-U
variable PTY-N

variable TX-MASTER
variable TX-SLAVE
variable TX-LIFE-R
variable TX-LIFE-W
variable TX-DONE-R
variable TX-DONE-W
variable TX-ANCHOR-R
variable TX-ANCHOR-W
variable TX-ARM-R
variable TX-ARM-W
variable TX-ACK-R
variable TX-ACK-W
variable TX-TARGET-ACK-R
variable TX-TARGET-ACK-W
variable TX-GUARD
variable TX-TARGET-GUARD
variable TX-SUP-GUARD
variable TX-PGRP
variable TX-TARGET
variable TX-GUARD-DEAD
variable TX-GROUP-SEEN
variable TX-SUP
variable TX-OWNER

variable SUP-GATE-R
variable SUP-GATE-W
variable SUP-EXEC-R
variable SUP-EXEC-W
variable SUP-READY-R
variable SUP-READY-W
variable SUP-PROC
variable SUP-OWNER-PROC
variable SUP-TARGET
variable SUP-ANCHOR
variable SUP-PGRP
variable SUP-GROUP
variable SUP-STATUS

variable OP-SUP
variable OP-TARGET
variable OP-GROUP
variable OP-MASTER
variable OP-LIFE
variable OP-DONE
variable OP-ANCHOR
variable OP-GUARD
variable OP-TARGET-GUARD
variable OP-SUP-GUARD
variable OP-ERR
variable OP-STATUS
variable OP-STRUCTURED
variable OP-PROTOCOL
variable OP-GUARD-DEAD
variable OP-EOF
variable FRAME-OFF
variable CLEAN-MASK
variable SUP-CLEAN-MASK
variable SUP-FAILED
variable START-RAW

: PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: PATH-A@ ( -- ptr u8 )
   PATH-A PTR-U8-FIELD @ ;

: PATH-A! ( ptr u8 -- )
   PATH-A PTR-U8-FIELD ! ;

: CELL-FD@ ( ptr a -- fd )
   @ >FD ;

: CELL-FD! ( fd ptr a -- )
   swap FD>N swap ! ;

: CELL-PID@ ( ptr a -- pid )
   @ >PID ;

: CELL-PID! ( pid ptr a -- )
   swap PID>N swap ! ;

: LOCK ( -- )
   begin
      0 1 BUSY atomic-cas 0= if exit then
   again ;

: UNLOCK ( -- )
   0 BUSY atomic! ;

: COPY-Z ( ptr u8 n ptr u8 n -- ptr u8 )
   {: src:ptr u:n dst:ptr cap:n :}
   u 0 <= if E-PROC-PATH throw then
   u cap >= if E-PROC-PATH throw then
   src dst u BYTE-COPY
   0 dst u + c!
   dst ;

: INPUTS! ( -- )
   PATH-A@ PATH-U @ EXE-Z PATH-CAP COPY-Z drop
   EXE-Z ARGV !
   0 ARGV cell+ !
   0 ENVP !
   s" /dev/ptmx" MASTER-Z PTY-PATH-CAP COPY-Z drop ;

: FD-CLOSE-BAD? ( fd -- bool ) {: fd:fd :}
   fd FD>N 0 < if false exit then
   fd FD>N close-rc 0 <> ;

: FD-CLOSE ( fd -- )
   FD-CLOSE-BAD? if E-PROC-OUTPUT throw then ;

: SET-CLOEXEC ( fd -- ) {: fd:fd :}
   fd FD>N F-SETFD FD-CLOEXEC-FLAG fcntl 0 <> if E-PROC-OUTPUT throw then ;

: PIPE-CLOEXEC ( -- fd fd )
   pipe {: r:n w:n rc:n :}
   rc 0 <> if E-PROC-OUTPUT throw then
   r >FD {: rf:fd :}
   w >FD {: wf:fd :}
   rf SET-CLOEXEC
   wf SET-CLOEXEC
   rf wf ;

: WRITE-EXACT ( fd ptr u8 n -- )
   {: fd:fd src:ptr u:n :}
   u 0= if exit then
   fd FD>N src u write {: wrote:n :}
   wrote 0 <= if E-PROC-OUTPUT throw then
   wrote u > if E-PROC-OUTPUT throw then
   fd src wrote + u wrote - recurse ;

: WRITE-EXACT? ( fd ptr u8 n -- bool )
   {: fd:fd src:ptr u:n :}
   u 0= if true exit then
   fd FD>N src u write {: wrote:n :}
   wrote 0 <= if false exit then
   wrote u > if false exit then
   fd src wrote + u wrote - recurse ;

: READ-EXACT ( fd ptr u8 n -- )
   {: fd:fd dst:ptr u:n :}
   u 0= if exit then
   fd FD>N dst u read {: got:n :}
   got 0 <= if E-PROC-OUTPUT throw then
   got u > if E-PROC-OUTPUT throw then
   fd dst got + u got - recurse ;

: READ-EOF? ( fd -- bool ) {: fd:fd :}
   fd FD>N SIGNAL-BYTE 1 read dup 0 < if drop E-PROC-OUTPUT throw then
   0= ;

: FRAME-WRITE ( fd n -- ) {: fd:fd value:n :}
   value FRAME !
   fd FRAME 1 cells WRITE-EXACT ;

: FRAME-WRITE-BAD? ( fd n -- bool )
   FRAME !
   FRAME 1 cells WRITE-EXACT? 0= ;

: FRAME-READ ( fd -- n ) {: fd:fd :}
   fd FRAME 1 cells READ-EXACT
   FRAME @ ;

: FRAME-READ? ( fd -- n bool ) {: fd:fd :}
   0 FRAME-OFF !
   begin FRAME-OFF @ 1 cells < while
      fd FD>N FRAME FRAME-OFF @ + 1 cells FRAME-OFF @ - read {: got:n :}
      got 0 <= if 0 false exit then
      FRAME-OFF @ got + FRAME-OFF !
   repeat
   FRAME @ true ;

: EXEC-RESULT ( fd -- n bool ) {: fd:fd :}
   0 FRAME-OFF !
   begin FRAME-OFF @ 1 cells < while
      fd FD>N FRAME FRAME-OFF @ + 1 cells FRAME-OFF @ - read {: got:n :}
      got 0= if
         FRAME-OFF @ 0= if 0 true exit then
         E-PROC-OUTPUT false exit
      then
      got 0 < if E-PROC-OUTPUT false exit then
      FRAME-OFF @ got + FRAME-OFF !
   repeat
   FRAME @ false ;

: PFD! ( fd n idx -- ) {: fd:fd events:n idx:idx :}
   events 32 lshift fd FD>N $FFFFFFFF and or
   idx IDX>N cells POLL-FDS + ! ;

: PFD-REVENTS ( idx -- n )
   IDX>N cells POLL-FDS + @ 48 rshift $FFFF and ;

: PFD-EVENT? ( idx -- bool )
   PFD-REVENTS POLLIN POLLERR or POLLHUP or POLLNVAL or and 0 <> ;

: POLL-ONE ( fd ms -- n ) {: fd:fd ms:ms :}
   fd POLLIN 0 >IDX PFD!
   POLL-FDS 1 ms MS>N poll ;

: PROC-WATCH-EXIT? ( fd -- bool ) {: fd:fd :}
   fd 0 >MS POLL-ONE {: rc:n :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if false exit then
   0 >IDX PFD-REVENTS {: events:n :}
   events POLLERR POLLNVAL or and 0 <> if E-PROC-OUTPUT throw then
   events POLLIN and 0= if E-PROC-OUTPUT throw then
   true ;

: FRAME-READ-GUARDED? ( fd fd -- n bool ) {: fd:fd guard:fd :}
   0 FRAME-OFF !
   begin FRAME-OFF @ 1 cells < while
      fd POLLIN 0 >IDX PFD!
      guard POLLIN 1 >IDX PFD!
      POLL-FDS 2 -1 poll {: rc:n :}
      rc 0 < if E-PROC-OUTPUT false exit then
      0 >IDX PFD-EVENT? if
         fd FD>N FRAME FRAME-OFF @ + 1 cells FRAME-OFF @ - read {: got:n :}
         got 0 <= if E-PROC-OUTPUT false exit then
         FRAME-OFF @ got + FRAME-OFF !
      else
         E-PROC-OUTPUT false exit
      then
   repeat
   FRAME @ true ;

: WATCH-NEXT ( fd fd fd -- watch-event ) {: procfd:fd ownerfd:fd lifefd:fd :}
   begin
      procfd POLLIN 0 >IDX PFD!
      ownerfd POLLIN 1 >IDX PFD!
      lifefd POLLIN 2 >IDX PFD!
      POLL-FDS 3 -1 poll {: rc:n :}
      rc 0 < if E-PROC-OUTPUT throw then
      rc 0 > if
         1 >IDX PFD-EVENT? if construct watch-event owner-exit exit then
         2 >IDX PFD-EVENT? if construct watch-event cancel exit then
         0 >IDX PFD-EVENT? if construct watch-event target-exit exit then
      then
   again ;

: POSITIVE-PID ( pid -- pid )
   dup PID>N 0 <= if drop E-PROC-PTY-HANDLE throw then ;

: KILL-CLEAN? ( n -- bool )
   dup 0= if drop true exit then
   ESRCH# negate = ;

: RETIRE-KILL? ( n -- bool )
   dup KILL-CLEAN? swap EPERM# negate = or ;

: GROUP-KILL-RC ( pid -- n )
   POSITIVE-PID PID>N negate SIGKILL kill-errno ;

: GROUP-PROBE-RC ( pid -- n )
   POSITIVE-PID PID>N negate SIGNAL-PROBE kill-errno ;

: GROUP-EMPTY? ( pid -- bool )
   GROUP-PROBE-RC
   dup ESRCH# negate = if drop true exit then
   dup 0= swap EPERM# negate = or if false exit then
   E-PROC-OUTPUT throw ;

: GROUP-PAUSE ( -- )
   POLL-FDS 0 1 poll 0 < if E-PROC-OUTPUT throw then ;

: GROUP-WAIT-EMPTY ( pid -- ) {: pgrp:pid :}
   mono-ns KILL-WAIT-MS PROC-NS-PER-MS * + {: deadline:n :}
   begin
      pgrp GROUP-EMPTY? if exit then
      mono-ns deadline >= if E-PROC-TIMEOUT throw then
      GROUP-PAUSE
   again ;

: PID-WAIT-STATUS ( pid -- n )
   POSITIVE-PID PID>N wait-status dup 0 < if drop E-PROC-WAIT throw then ;

: TIOCSCTTY# ( -- n )
   HB-TARGET-LINUX? if LINUX-TIOCSCTTY exit then
   HB-TARGET-MACOS? if DARWIN-TIOCSCTTY exit then
   E-PROC-PTY-HANDLE throw ;

: TIOCSPGRP# ( -- n )
   HB-TARGET-LINUX? if LINUX-TIOCSPGRP exit then
   HB-TARGET-MACOS? if DARWIN-TIOCSPGRP exit then
   E-PROC-PTY-HANDLE throw ;

: PTY-C ( n -- ) {: c:n :}
   c SLAVE-Z PTY-U @ + c!
   PTY-U @ 1+ PTY-U ! ;

: PTY-$+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ PTY-C
      1+
   repeat drop ;

: PTY-U+ ( n -- ) {: n:n :}
   n 10 >= if n 10 / recurse then
   n 10 mod 48 + PTY-C ;

: LINUX-SLAVE-Z ( -- )
   0 PTY-U !
   s" /dev/pts/" PTY-$+
   PTY-N @ PTY-U+
   0 PTY-C ;

: TX-RESET ( -- )
   0 SUP-CLEAN-MASK !
   false SUP-FAILED !
   false TX-GROUP-SEEN !
   -1 >FD TX-MASTER CELL-FD!
   -1 >FD TX-SLAVE CELL-FD!
   -1 >FD TX-LIFE-R CELL-FD!
   -1 >FD TX-LIFE-W CELL-FD!
   -1 >FD TX-DONE-R CELL-FD!
   -1 >FD TX-DONE-W CELL-FD!
   -1 >FD TX-ANCHOR-R CELL-FD!
   -1 >FD TX-ANCHOR-W CELL-FD!
   -1 >FD TX-ARM-R CELL-FD!
   -1 >FD TX-ARM-W CELL-FD!
   -1 >FD TX-ACK-R CELL-FD!
   -1 >FD TX-ACK-W CELL-FD!
   -1 >FD TX-TARGET-ACK-R CELL-FD!
   -1 >FD TX-TARGET-ACK-W CELL-FD!
   -1 >FD TX-GUARD CELL-FD!
   -1 >FD TX-TARGET-GUARD CELL-FD!
   -1 >FD TX-SUP-GUARD CELL-FD!
   -1 >PID TX-PGRP CELL-PID!
   -1 >PID TX-TARGET CELL-PID!
   -1 >PID TX-SUP CELL-PID!
   -1 >PID TX-OWNER CELL-PID! ;

: TX-CLOSE ( ptr a -- bool ) {: slot:ptr :}
   slot CELL-FD@ {: fd:fd :}
   -1 >FD slot CELL-FD!
   fd FD-CLOSE-BAD? ;

: TX-WAIT-SUP? ( -- bool )
   TX-SUP CELL-PID@ {: sup:pid :}
   -1 >PID TX-SUP CELL-PID!
   sup PID>N 0 <= if false exit then
   sup PID>N wait-status {: status:n :}
   status 0 < if true exit then
   SUP-FAILED @ if false exit then
   status 0 <> ;

: CANCEL-BAD? ( fd -- bool ) {: fd:fd :}
   fd FD>N 0 < if false exit then
   1 SIGNAL-BYTE c!
   fd SIGNAL-BYTE 1 WRITE-EXACT? 0= ;

: CLEAN+ ( n -- ) {: bit:n :}
   CLEAN-MASK @ bit or CLEAN-MASK ! ;

: ABORT-CLOSE ( ptr a n -- ) {: slot:ptr bit:n :}
   slot TX-CLOSE if bit CLEAN+ then ;

: ABORT-WAIT ( -- )
   TX-WAIT-SUP? if CLEAN-SUP CLEAN+ then ;

: TX-RESULT-BAD ( -- )
   true SUP-FAILED !
   CLEAN-SUP CLEAN+ ;

: TX-READ-RESULT ( -- )
   SUP-FAILED @ if exit then
   TX-SUP CELL-PID@ PID>N 0 <= if exit then
   TX-SUP-GUARD CELL-FD@ FD>N 0 < if TX-RESULT-BAD exit then
   TX-DONE-R CELL-FD@ TX-SUP-GUARD CELL-FD@ FRAME-READ-GUARDED?
   {: code:n code-ok:bool :}
   code-ok 0= if TX-RESULT-BAD exit then
   TX-DONE-R CELL-FD@ TX-SUP-GUARD CELL-FD@ FRAME-READ-GUARDED?
   {: mask:n mask-ok:bool :}
   mask-ok 0= if TX-RESULT-BAD exit then
   mask 0 <> if TX-RESULT-BAD then
   code 0 < if true SUP-FAILED ! then ;

: TX-GUARD-CHECK ( -- )
   TX-GUARD CELL-FD@ dup FD>N 0 < if drop false TX-GUARD-DEAD ! exit then
   PROC-WATCH-EXIT? TX-GUARD-DEAD ! ;

: TX-KILL-GROUP ( -- )
   TX-PGRP CELL-PID@ GROUP-KILL-RC RETIRE-KILL? 0= if
      CLEAN-GROUP CLEAN+
   then ;

: TX-WAIT-GROUP-RUN ( -- )
   TX-PGRP CELL-PID@ GROUP-WAIT-EMPTY ;

: TX-WAIT-GROUP ( -- )
   [: TX-WAIT-GROUP-RUN ;] catch dup 0= if drop exit then
   drop CLEAN-GROUP-WAIT CLEAN+ ;

: TX-WAIT-GROUP-KNOWN ( -- )
   TX-PGRP CELL-PID@ PID>N 0 > if TX-WAIT-GROUP then ;

: TX-FAIL-SIGNAL ( -- )
   TX-PGRP CELL-PID@ PID>N 0 <= if exit then
   false TX-GUARD-DEAD !
   [: TX-GUARD-CHECK ;] catch dup 0 <> if
      drop CLEAN-GUARD CLEAN+
   else
      drop TX-GUARD-DEAD @ 0= if TX-KILL-GROUP then
   then ;

: TX-ABORT ( -- )
   0 CLEAN-MASK !
   SUP-FAILED @ 0= if
      TX-LIFE-W CELL-FD@ CANCEL-BAD? if CLEAN-CANCEL CLEAN+ then
   then
   TX-ARM-W CLEAN-ARM-W ABORT-CLOSE
   TX-ARM-R CLEAN-ARM-R ABORT-CLOSE
   TX-ACK-W CLEAN-ACK-W ABORT-CLOSE
   TX-ACK-R CLEAN-ACK-R ABORT-CLOSE
   TX-TARGET-ACK-W CLEAN-TARGET-ACK-W ABORT-CLOSE
   TX-TARGET-ACK-R CLEAN-TARGET-ACK-R ABORT-CLOSE
   TX-FAIL-SIGNAL
   TX-GROUP-SEEN @ if TX-READ-RESULT then
   TX-ANCHOR-W CLEAN-ANCHOR-W ABORT-CLOSE
   TX-MASTER CLEAN-MASTER ABORT-CLOSE
   ABORT-WAIT
   TX-WAIT-GROUP-KNOWN
   TX-LIFE-W CLEAN-LIFE-W ABORT-CLOSE
   TX-SLAVE CLEAN-SLAVE ABORT-CLOSE
   TX-LIFE-R CLEAN-LIFE-R ABORT-CLOSE
   TX-DONE-R CLEAN-DONE-R ABORT-CLOSE
   TX-DONE-W CLEAN-DONE-W ABORT-CLOSE
   TX-ANCHOR-R CLEAN-ANCHOR-R ABORT-CLOSE
   TX-GUARD CLEAN-GUARD ABORT-CLOSE
   TX-TARGET-GUARD CLEAN-GUARD ABORT-CLOSE
   TX-SUP-GUARD CLEAN-SUP-GUARD ABORT-CLOSE ;

: START-THROW ( n -- ) {: code:n :}
   TX-ABORT
   UNLOCK
   code throw ;

: TX-CLOSE! ( ptr a -- )
   TX-CLOSE if E-PROC-OUTPUT throw then ;

: START-CLOEXEC ( ptr a -- )
   CELL-FD@ {: fd:fd :}
   fd FD>N F-SETFD FD-CLOEXEC-FLAG fcntl 0 <> if E-PROC-OUTPUT throw then ;

: START-PIPE ( ptr a ptr a -- ) {: rd:ptr wr:ptr :}
   pipe {: r:n w:n rc:n :}
   rc 0 <> if E-PROC-OUTPUT throw then
   r >FD rd CELL-FD!
   w >FD wr CELL-FD!
   rd START-CLOEXEC
   wr START-CLOEXEC ;

: START-PIPES ( -- )
   TX-LIFE-R TX-LIFE-W START-PIPE
   TX-DONE-R TX-DONE-W START-PIPE
   TX-ANCHOR-R TX-ANCHOR-W START-PIPE
   TX-ARM-R TX-ARM-W START-PIPE
   TX-ACK-R TX-ACK-W START-PIPE
   TX-TARGET-ACK-R TX-TARGET-ACK-W START-PIPE
   TX-LIFE-W CELL-FD@ FD-NOSIGPIPE!
   TX-ANCHOR-W CELL-FD@ FD-NOSIGPIPE!
   TX-ARM-W CELL-FD@ FD-NOSIGPIPE!
   TX-ACK-W CELL-FD@ FD-NOSIGPIPE! ;
   TX-TARGET-ACK-W CELL-FD@ FD-NOSIGPIPE! ;

: START-OPEN-MASTER ( -- )
   MASTER-Z O-RDWR O-NOCTTY or 0 open dup 0 < if drop E-PROC-OUTPUT throw then
   >FD TX-MASTER CELL-FD!
   TX-MASTER START-CLOEXEC ;

: START-OPEN-SLAVE ( -- )
   SLAVE-Z O-RDWR O-NOCTTY or 0 open dup 0 < if drop E-PROC-OUTPUT throw then
   >FD TX-SLAVE CELL-FD!
   TX-SLAVE START-CLOEXEC ;

: START-OPEN-DARWIN ( -- )
   START-OPEN-MASTER
   TX-MASTER CELL-FD@ FD>N DARWIN-TIOCPTYGRANT NULL$ drop ioctl 0 <> if E-PROC-OUTPUT throw then
   TX-MASTER CELL-FD@ FD>N DARWIN-TIOCPTYUNLK NULL$ drop ioctl 0 <> if E-PROC-OUTPUT throw then
   TX-MASTER CELL-FD@ FD>N DARWIN-TIOCPTYGNAME SLAVE-Z ioctl 0 <> if E-PROC-OUTPUT throw then
   START-OPEN-SLAVE ;

: START-OPEN-LINUX ( -- )
   START-OPEN-MASTER
   0 PTY-N !
   TX-MASTER CELL-FD@ FD>N LINUX-TIOCSPTLCK PTY-N ioctl 0 <> if E-PROC-OUTPUT throw then
   TX-MASTER CELL-FD@ FD>N LINUX-TIOCGPTN PTY-N ioctl 0 <> if E-PROC-OUTPUT throw then
   LINUX-SLAVE-Z
   START-OPEN-SLAVE ;

: START-OPEN-PTY ( -- )
   HB-TARGET-LINUX? if START-OPEN-LINUX exit then
   HB-TARGET-MACOS? if START-OPEN-DARWIN exit then
   E-PROC-PTY-HANDLE throw ;

: SUP-RESET ( -- )
   -1 >FD SUP-GATE-R CELL-FD!
   -1 >FD SUP-GATE-W CELL-FD!
   -1 >FD SUP-EXEC-R CELL-FD!
   -1 >FD SUP-EXEC-W CELL-FD!
   -1 >FD SUP-READY-R CELL-FD!
   -1 >FD SUP-READY-W CELL-FD!
   -1 >FD SUP-PROC CELL-FD!
   -1 >FD SUP-OWNER-PROC CELL-FD!
   -1 >PID SUP-TARGET CELL-PID!
   -1 >PID SUP-ANCHOR CELL-PID!
   -1 >PID SUP-PGRP CELL-PID!
   false SUP-GROUP !
   0 SUP-STATUS ! ;

: SUP-CLOSE-OWN ( ptr a -- bool ) {: slot:ptr :}
   slot CELL-FD@ {: fd:fd :}
   -1 >FD slot CELL-FD!
   fd FD-CLOSE-BAD? ;

: TARGET-DUP ( fd n -- ) {: fd:fd dst:n :}
   fd FD>N dst dup2 dup 0 < if drop E-PROC-OUTPUT throw then
   drop ;

: TARGET-CLOSE ( ptr a -- )
   SUP-CLOSE-OWN if E-PROC-OUTPUT throw then ;

: ANCHOR-RUN ( -- )
   TX-ACK-R TARGET-CLOSE
   SUP-READY-W TARGET-CLOSE
   SUP-READY-R CELL-FD@ SIGNAL-BYTE 1 READ-EXACT
   SUP-READY-R TARGET-CLOSE
   SUP-GATE-R TARGET-CLOSE
   SUP-GATE-W TARGET-CLOSE
   SUP-EXEC-R TARGET-CLOSE
   SUP-EXEC-W TARGET-CLOSE
   TX-DONE-W TARGET-CLOSE
   TX-SLAVE TARGET-CLOSE
   TX-LIFE-R TARGET-CLOSE
   TX-ANCHOR-W TARGET-CLOSE
   TX-ANCHOR-R CELL-FD@ READ-EOF? 0= if E-PROC-OUTPUT throw then
   TX-ANCHOR-R TARGET-CLOSE
   getpid >PID GROUP-KILL-RC KILL-CLEAN? 0= if E-PROC-OUTPUT throw then
   s" " 1 die ;

: ANCHOR-MAIN ( -- )
   [: ANCHOR-RUN ;] catch 0= if s" " 0 die then
   s" " 1 die ;

: TARGET-RUN ( -- )
   SUP-GATE-W TARGET-CLOSE
   SUP-EXEC-R TARGET-CLOSE
   TX-LIFE-R TARGET-CLOSE
   TX-DONE-W TARGET-CLOSE
   TX-ANCHOR-W TARGET-CLOSE
   SUP-GATE-R CELL-FD@ SIGNAL-BYTE 1 READ-EXACT
   SUP-GATE-R TARGET-CLOSE
   TX-SLAVE CELL-FD@ 0 TARGET-DUP
   TX-SLAVE CELL-FD@ 1 TARGET-DUP
   TX-SLAVE CELL-FD@ 2 TARGET-DUP
   TX-SLAVE CELL-FD@ FD>N 2 > if TX-SLAVE TARGET-CLOSE then
   EXE-Z ARGV ENVP execve drop
   E-PROC-SPAWN throw ;

: TARGET-FAIL ( n -- ) {: code:n :}
   SUP-EXEC-W CELL-FD@ code FRAME-WRITE-BAD?
   if s" " 1 die then
   s" " $7F die ;

: TARGET-MAIN ( -- )
   [: TARGET-RUN ;] catch
   dup 0= if drop E-PROC-SPAWN then
   TARGET-FAIL ;

: SUP-PIPES ( -- )
   PIPE-CLOEXEC SUP-GATE-W CELL-FD! SUP-GATE-R CELL-FD!
   PIPE-CLOEXEC SUP-EXEC-W CELL-FD! SUP-EXEC-R CELL-FD!
   PIPE-CLOEXEC SUP-READY-W CELL-FD! SUP-READY-R CELL-FD! ;

: SUP-NOSIGPIPE ( -- )
   SUP-GATE-W CELL-FD@ FD-NOSIGPIPE!
   SUP-EXEC-W CELL-FD@ FD-NOSIGPIPE!
   SUP-READY-W CELL-FD@ FD-NOSIGPIPE!
   TX-DONE-W CELL-FD@ FD-NOSIGPIPE! ;

: SUP-SESSION ( -- )
   setsid dup 0 <= if drop E-PROC-SPAWN throw then drop
   TX-SLAVE CELL-FD@ FD>N TIOCSCTTY# NULL$ drop ioctl 0 <> if E-PROC-OUTPUT throw then ;

: SUP-PUBLISH-PID ( pid -- )
   PID>N TX-DONE-W CELL-FD@ swap FRAME-WRITE ;

defer BEFORE-ANCHOR-JOIN ( -- )

: ANCHOR-JOIN-NOW ( -- ) ;

: ANCHOR-JOIN-DEFAULT ( -- )
   [: ANCHOR-JOIN-NOW ;] is BEFORE-ANCHOR-JOIN ;

ANCHOR-JOIN-DEFAULT

defer BEFORE-ANCHOR-RELEASE ( -- )

: ANCHOR-RELEASE-NOW ( -- ) ;

: ANCHOR-RELEASE-DEFAULT ( -- )
   [: ANCHOR-RELEASE-NOW ;] is BEFORE-ANCHOR-RELEASE ;

ANCHOR-RELEASE-DEFAULT

defer BEFORE-TARGET-JOIN ( -- )

: TARGET-JOIN-NOW ( -- ) ;

: TARGET-JOIN-DEFAULT ( -- )
   [: TARGET-JOIN-NOW ;] is BEFORE-TARGET-JOIN ;

TARGET-JOIN-DEFAULT

defer BEFORE-TARGET-PUBLISH ( -- )

: TARGET-PUBLISH-NOW ( -- ) ;

: TARGET-PUBLISH-DEFAULT ( -- )
   [: TARGET-PUBLISH-NOW ;] is BEFORE-TARGET-PUBLISH ;

TARGET-PUBLISH-DEFAULT

: SUP-SPAWN-ANCHOR ( -- )
   PROC-FORK {: pid:pid :}
   pid PID>N 0= if ANCHOR-MAIN then
   pid SUP-ANCHOR CELL-PID!
   pid SUP-PGRP CELL-PID!
   SUP-READY-R SUP-CLOSE-OWN if E-PROC-OUTPUT throw then
   pid SUP-PUBLISH-PID
   TX-ACK-R CELL-FD@ SIGNAL-BYTE 1 READ-EXACT
   TX-ACK-R TX-CLOSE!
   BEFORE-ANCHOR-JOIN
   pid PID>N pid PID>N setpgid 0 <> if E-PROC-OUTPUT throw then
   true SUP-GROUP !
   TX-ANCHOR-R SUP-CLOSE-OWN if E-PROC-OUTPUT throw then
   BEFORE-ANCHOR-RELEASE
   1 SIGNAL-BYTE c!
   SUP-READY-W CELL-FD@ SIGNAL-BYTE 1 WRITE-EXACT
   SUP-READY-W SUP-CLOSE-OWN if E-PROC-OUTPUT throw then ;

: SUP-SPAWN-TARGET ( -- )
   PROC-FORK {: pid:pid :}
   pid PID>N 0= if TARGET-MAIN then
   pid SUP-TARGET CELL-PID!
   SUP-GATE-R SUP-CLOSE-OWN if E-PROC-OUTPUT throw then
   SUP-EXEC-W SUP-CLOSE-OWN if E-PROC-OUTPUT throw then
   BEFORE-TARGET-JOIN
   pid PID>N SUP-PGRP CELL-PID@ PID>N setpgid 0 <> if E-PROC-OUTPUT throw then
   SUP-PGRP CELL-PID@ PID>N FRAME !
   TX-SLAVE CELL-FD@ FD>N TIOCSPGRP# FRAME ioctl 0 <> if E-PROC-OUTPUT throw then
   1 SIGNAL-BYTE c!
   SUP-GATE-W CELL-FD@ SIGNAL-BYTE 1 WRITE-EXACT
   SUP-GATE-W SUP-CLOSE-OWN if E-PROC-OUTPUT throw then ;

: SUP-OPEN-TARGET-WATCH ( -- )
   SUP-TARGET CELL-PID@ POSITIVE-PID PID>N proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then
   >FD SUP-PROC CELL-FD! ;

: SUP-OPEN-OWNER-WATCH ( -- )
   TX-OWNER CELL-PID@ POSITIVE-PID PID>N proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then
   >FD SUP-OWNER-PROC CELL-FD! ;

: SUP-OPEN-WATCHES ( -- )
   SUP-OPEN-TARGET-WATCH
   SUP-OPEN-OWNER-WATCH ;

: SUP-SETUP-LIVE? ( -- bool )
   TX-LIFE-R CELL-FD@ 0 >MS POLL-ONE {: rc:n :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= ;

: SUP-PUBLISH-TARGET ( -- )
   SUP-TARGET CELL-PID@ SUP-PUBLISH-PID ;

: SUP-WAIT-TARGET ( -- n )
   SUP-TARGET CELL-PID@ PID-WAIT-STATUS
   -1 >PID SUP-TARGET CELL-PID! ;

: SUP-WAIT-ANCHOR ( -- n )
   SUP-ANCHOR CELL-PID@ PID-WAIT-STATUS
   -1 >PID SUP-ANCHOR CELL-PID! ;

: SUP-GROUP-CLEAR ( -- )
   false SUP-GROUP !
   -1 >PID SUP-PGRP CELL-PID! ;

: SUP-RETIRE-GROUP ( -- n )
   SUP-PGRP CELL-PID@ {: pgrp:pid :}
   pgrp GROUP-KILL-RC {: first:n :}
   SUP-WAIT-TARGET {: status:n :}
   SUP-WAIT-ANCHOR drop
   first RETIRE-KILL? 0= if E-PROC-OUTPUT throw then
   pgrp GROUP-WAIT-EMPTY
   SUP-GROUP-CLEAR
   status ;

: SUP-CHECK-EXEC ( -- )
   SUP-EXEC-R CELL-FD@ EXEC-RESULT {: code:n ok:bool :}
   SUP-EXEC-R SUP-CLOSE-OWN if E-PROC-OUTPUT throw then
   ok if exit then
   SUP-RETIRE-GROUP drop
   code throw ;

: SUP-OWNER-DIED ( -- n )
   SUP-RETIRE-GROUP ;

: SUP-TARGET-DIED ( -- n )
   SUP-RETIRE-GROUP ;

: SUP-WATCH ( -- n )
   SUP-PROC CELL-FD@ SUP-OWNER-PROC CELL-FD@ TX-LIFE-R CELL-FD@ WATCH-NEXT
   MATCH watch-event
     target-exit OF SUP-TARGET-DIED ENDOF
     owner-exit OF SUP-OWNER-DIED ENDOF
     cancel OF SUP-OWNER-DIED ENDOF
   ;MATCH ;

: SUP-RUN ( -- )
   SUP-RESET
   TX-MASTER TX-CLOSE! 
   TX-LIFE-W TX-CLOSE!
   TX-DONE-R TX-CLOSE!
   TX-ACK-W TX-CLOSE!
   SUP-SESSION
   SUP-PIPES
   SUP-NOSIGPIPE
   SUP-SPAWN-ANCHOR
   SUP-SPAWN-TARGET
   SUP-CHECK-EXEC
   SUP-OPEN-WATCHES
   SUP-SETUP-LIVE? 0= if E-PROC-OUTPUT throw then
   TX-SLAVE TX-CLOSE!
   BEFORE-TARGET-PUBLISH
   SUP-PUBLISH-TARGET
   SUP-WATCH SUP-STATUS ! ;

: SUP-KILL-PID? ( ptr a -- bool )
   CELL-PID@
   dup PID>N 0 <= if drop false exit then
   PID>N SIGKILL kill-errno KILL-CLEAN? 0= ;

: SUP-KILL-GROUP? ( -- bool )
   SUP-GROUP @ 0= if false exit then
   SUP-PGRP CELL-PID@ GROUP-KILL-RC RETIRE-KILL? 0= ;

: SUP-WAIT-TARGET? ( -- bool )
   SUP-TARGET CELL-PID@ dup PID>N 0 <= if drop false exit then
   -1 >PID SUP-TARGET CELL-PID!
   PID>N wait-status 0 < ;

: SUP-WAIT-ANCHOR? ( -- bool )
   SUP-ANCHOR CELL-PID@ dup PID>N 0 <= if drop false exit then
   -1 >PID SUP-ANCHOR CELL-PID!
   PID>N wait-status 0 < ;

: SUP-WAIT-GROUP-RUN ( -- )
   SUP-PGRP CELL-PID@ GROUP-WAIT-EMPTY ;

: SUP-WAIT-GROUP? ( -- bool )
   SUP-GROUP @ 0= if false exit then
   [: SUP-WAIT-GROUP-RUN ;] catch dup 0= if
      drop SUP-GROUP-CLEAR false exit
   then
   drop true ;

: SUP-CLEAN+ ( n -- ) {: bit:n :}
   SUP-CLEAN-MASK @ bit or SUP-CLEAN-MASK ! ;

: SUP-CLOSE-MARK ( ptr a n -- ) {: slot:ptr bit:n :}
   slot SUP-CLOSE-OWN if bit SUP-CLEAN+ then ;

: SUP-CLOSE-ALL ( -- )
   SUP-GATE-R SUP-CLEAN-GATE-R SUP-CLOSE-MARK
   SUP-GATE-W SUP-CLEAN-GATE-W SUP-CLOSE-MARK
   SUP-EXEC-R SUP-CLEAN-EXEC-R SUP-CLOSE-MARK
   SUP-EXEC-W SUP-CLEAN-EXEC-W SUP-CLOSE-MARK
   SUP-READY-R SUP-CLEAN-READY-R SUP-CLOSE-MARK
   SUP-READY-W SUP-CLEAN-READY-W SUP-CLOSE-MARK
   TX-ACK-R SUP-CLEAN-ACK-R SUP-CLOSE-MARK
   SUP-PROC SUP-CLEAN-PROC SUP-CLOSE-MARK
   SUP-OWNER-PROC SUP-CLEAN-OWNER SUP-CLOSE-MARK
   TX-MASTER SUP-CLEAN-MASTER SUP-CLOSE-MARK
   TX-SLAVE SUP-CLEAN-SLAVE SUP-CLOSE-MARK
   TX-LIFE-R SUP-CLEAN-LIFE-R SUP-CLOSE-MARK
   TX-LIFE-W SUP-CLEAN-LIFE-W SUP-CLOSE-MARK
   TX-DONE-R SUP-CLEAN-DONE-R SUP-CLOSE-MARK
   TX-ANCHOR-R SUP-CLEAN-ANCHOR-R SUP-CLOSE-MARK
   TX-ANCHOR-W SUP-CLEAN-ANCHOR-W SUP-CLOSE-MARK ;

: SUP-RESULT-WRITE ( n -- ) {: code:n :}
   TX-DONE-W CELL-FD@ code FRAME-WRITE-BAD?
   if s" " 1 die then
   TX-DONE-W CELL-FD@ SUP-CLEAN-MASK @ FRAME-WRITE-BAD?
   if s" " 1 die then
   code 0 < if s" " 1 die then
   s" " 0 die ;

: SUP-FAIL ( n -- ) {: code:n :}
   0 SUP-CLEAN-MASK !
   SUP-KILL-GROUP? if SUP-CLEAN-KILL SUP-CLEAN+ then
   SUP-TARGET SUP-KILL-PID? if SUP-CLEAN-KILL SUP-CLEAN+ then
   SUP-ANCHOR SUP-KILL-PID? if SUP-CLEAN-KILL SUP-CLEAN+ then
   SUP-WAIT-TARGET? if SUP-CLEAN-WAIT SUP-CLEAN+ then
   SUP-WAIT-ANCHOR? if SUP-CLEAN-ANCHOR SUP-CLEAN+ then
   SUP-WAIT-GROUP? if SUP-CLEAN-GROUP SUP-CLEAN+ then
   SUP-CLOSE-ALL
   code SUP-RESULT-WRITE ;

defer BEFORE-SUP-CLEAN ( -- )

: SUP-CLEAN-NOW ( -- ) ;

: SUP-CLEAN-DEFAULT ( -- )
   [: SUP-CLEAN-NOW ;] is BEFORE-SUP-CLEAN ;

SUP-CLEAN-DEFAULT

: SUP-TXN ( -- )
   SUP-RUN
   BEFORE-SUP-CLEAN ;

: SUP-SUCCEED ( -- )
   0 SUP-CLEAN-MASK !
   SUP-CLOSE-ALL
   SUP-CLEAN-MASK @ 0 <> if E-PROC-OUTPUT SUP-RESULT-WRITE then
   SUP-STATUS @ SUP-RESULT-WRITE ;

: SUP-MAIN ( -- )
   [: SUP-TXN ;] catch
   dup 0= if drop SUP-SUCCEED then
   SUP-FAIL ;

: SUP-ARM-WAIT ( -- )
   TX-ARM-W TX-CLOSE!
   TX-ARM-R CELL-FD@ SIGNAL-BYTE 1 READ-EXACT
   TX-ARM-R TX-CLOSE! ;

: SUP-START ( -- )
   [: SUP-ARM-WAIT ;] catch dup 0= if drop SUP-MAIN then
   drop s" " 1 die ;

: START-FORK ( -- )
   fork dup 0 < if drop E-PROC-SPAWN throw then >PID {: pid:pid :}
   pid PID>N 0= if SUP-START then
   pid TX-SUP CELL-PID! ;

defer OPEN-SUP-GUARD ( pid -- fd )

: OPEN-SUP-GUARD-NOW ( pid -- fd )
   PID>N proc-watch-open >FD ;

: OPEN-SUP-GUARD-DEFAULT ( -- )
   [: OPEN-SUP-GUARD-NOW ;] is OPEN-SUP-GUARD ;

OPEN-SUP-GUARD-DEFAULT

: START-SUP-GUARD ( -- )
   TX-ARM-R TX-CLOSE!
   TX-SUP CELL-PID@ OPEN-SUP-GUARD {: guard:fd :}
   guard FD>N 0 < if
      TX-ARM-W TX-CLOSE!
      E-PROC-OUTPUT throw
   then
   guard TX-SUP-GUARD CELL-FD!
   TX-SUP-GUARD CELL-FD@ SET-CLOEXEC ;

: START-SUP-RELEASE ( -- )
   1 SIGNAL-BYTE c!
   TX-ARM-W CELL-FD@ SIGNAL-BYTE 1 WRITE-EXACT
   TX-ARM-W TX-CLOSE! ;

: START-ACK-PARENT ( -- )
   TX-ACK-R TX-CLOSE! ;

: START-GROUP-ACK ( -- )
   1 SIGNAL-BYTE c!
   TX-ACK-W CELL-FD@ SIGNAL-BYTE 1 WRITE-EXACT
   TX-ACK-W TX-CLOSE! ;

defer BEFORE-GROUP-ACK ( -- )

: GROUP-ACK-NOW ( -- ) ;

: GROUP-ACK-DEFAULT ( -- )
   [: GROUP-ACK-NOW ;] is BEFORE-GROUP-ACK ;

GROUP-ACK-DEFAULT

defer BEFORE-SUP-RELEASE ( -- )

: SUP-RELEASE-NOW ( -- ) ;

: SUP-RELEASE-DEFAULT ( -- )
   [: SUP-RELEASE-NOW ;] is BEFORE-SUP-RELEASE ;

SUP-RELEASE-DEFAULT

: TX-MOVED ( -- )
   -1 >FD TX-MASTER CELL-FD!
   -1 >FD TX-LIFE-W CELL-FD!
   -1 >FD TX-DONE-R CELL-FD!
   -1 >FD TX-ANCHOR-W CELL-FD!
   -1 >FD TX-ARM-R CELL-FD!
   -1 >FD TX-ARM-W CELL-FD!
   -1 >FD TX-ACK-R CELL-FD!
   -1 >FD TX-ACK-W CELL-FD!
   -1 >FD TX-GUARD CELL-FD!
   -1 >FD TX-SUP-GUARD CELL-FD!
   -1 >PID TX-PGRP CELL-PID!
   -1 >PID TX-SUP CELL-PID!
   -1 >PID TX-OWNER CELL-PID! ;

defer BEFORE-COMMIT ( -- )

: COMMIT-READY ( -- ) ;

: COMMIT-DEFAULT ( -- )
   [: COMMIT-READY ;] is BEFORE-COMMIT ;

COMMIT-DEFAULT

: START-COMMIT ( -- process-pty-handle )
   BEFORE-COMMIT
   RESERVE TX-SUP CELL-PID@ TX-PGRP CELL-PID@
   TX-MASTER CELL-FD@ TX-LIFE-W CELL-FD@ TX-DONE-R CELL-FD@
   TX-ANCHOR-W CELL-FD@ TX-GUARD CELL-FD@ TX-SUP-GUARD CELL-FD@
   COMMIT ;

: START-TARGET ( n -- pid )
   dup 0= if drop E-PROC-OUTPUT throw then
   dup 0 < if
      >r
      TX-DONE-R CELL-FD@ TX-SUP-GUARD CELL-FD@ FRAME-READ-GUARDED?
      if SUP-CLEAN-MASK ! else drop SUP-CLEAN-PROTOCOL SUP-CLEAN-MASK ! then
      true SUP-FAILED !
      r> throw
   then
   >PID ;

: START-READ-PID ( -- pid )
   TX-DONE-R CELL-FD@ TX-SUP-GUARD CELL-FD@ FRAME-READ-GUARDED?
   0= if drop E-PROC-OUTPUT throw then
   START-TARGET ;

: START-GROUP ( -- )
   START-READ-PID {: pgrp:pid :}
   pgrp TX-PGRP CELL-PID!
   true TX-GROUP-SEEN !
   pgrp PID>N proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then
   >FD TX-GUARD CELL-FD!
   TX-GUARD START-CLOEXEC ;

defer BEFORE-READY ( -- )

: READY-NOW ( -- ) ;

: READY-DEFAULT ( -- )
   [: READY-NOW ;] is BEFORE-READY ;

READY-DEFAULT

: START-TXN ( -- process-pty-handle )
   TX-RESET
   getpid dup 0 <= if drop E-PROC-SPAWN throw then >PID TX-OWNER CELL-PID!
   INPUTS!
   ROOM? 0= if E-PROC-PTY-CAPACITY throw then
   START-OPEN-PTY
   START-PIPES
   START-FORK
   START-SUP-GUARD
   BEFORE-SUP-RELEASE
   START-SUP-RELEASE
   START-ACK-PARENT
   TX-SLAVE TX-CLOSE if E-PROC-OUTPUT throw then
   TX-LIFE-R TX-CLOSE if E-PROC-OUTPUT throw then
   TX-DONE-W TX-CLOSE if E-PROC-OUTPUT throw then
   TX-ANCHOR-R TX-CLOSE if E-PROC-OUTPUT throw then
   BEFORE-READY
   START-GROUP
   BEFORE-GROUP-ACK
   START-GROUP-ACK
   START-READ-PID drop
   START-COMMIT ;

: START-SAVE ( -- )
   START-TXN HANDLE>N START-RAW !
   TX-MOVED ;

: START-GUARD ( -- process-pty-handle )
   [: START-SAVE ;] catch
   dup 0 <> if START-THROW then
   drop
   START-RAW @ N>HANDLE ;

: MASTER-FD ( process-pty-handle -- process-pty-handle fd )
   VIEW >r >r >r >r >r >r 2drop
   r> r> drop r> drop r> drop r> drop r> drop ;

: VALID-HANDLE ( process-pty-handle -- process-pty-handle )
   VIEW >r >r >r >r >r >r 2drop
   r> drop r> drop r> drop r> drop r> drop r> drop ;

: OP-RESET ( -- )
   0 OP-ERR !
   0 OP-STATUS !
   false OP-STRUCTURED !
   false OP-PROTOCOL !
   0 SUP-CLEAN-MASK ! ;

: OP-SAVE ( process-pty-teardown pid pid fd fd fd fd fd fd -- process-pty-teardown )
   OP-SUP-GUARD CELL-FD!
   OP-GUARD CELL-FD!
   OP-ANCHOR CELL-FD!
   OP-DONE CELL-FD!
   OP-LIFE CELL-FD!
   OP-MASTER CELL-FD!
   OP-TARGET CELL-PID!
   OP-SUP CELL-PID! ;

: OP-ERROR ( n -- ) {: code:n :}
   OP-ERR @ 0= if code OP-ERR ! then ;

: OP-CLOSE ( ptr a -- ) {: slot:ptr :}
   slot CELL-FD@ {: fd:fd :}
   -1 >FD slot CELL-FD!
   fd FD-CLOSE-BAD? if E-PROC-OUTPUT OP-ERROR then ;

: OP-READ-STATUS ( -- )
   OP-DONE CELL-FD@ OP-SUP-GUARD CELL-FD@ FRAME-READ-GUARDED?
   {: value:n value-ok:bool :}
   value-ok 0= if
      true OP-PROTOCOL !
      SUP-CLEAN-PROTOCOL SUP-CLEAN-MASK !
      E-PROC-OUTPUT OP-ERROR
      exit
   then
   OP-DONE CELL-FD@ OP-SUP-GUARD CELL-FD@ FRAME-READ-GUARDED?
   {: mask:n mask-ok:bool :}
   mask-ok 0= if
      true OP-PROTOCOL !
      SUP-CLEAN-PROTOCOL SUP-CLEAN-MASK !
      value 0 < if value else E-PROC-OUTPUT then OP-ERROR
      exit
   then
   true OP-STRUCTURED !
   mask SUP-CLEAN-MASK !
   mask SUP-CLEAN-AUTH and 0 <> if true OP-PROTOCOL ! then
   value 0 < if value OP-ERROR exit then
   mask 0 <> if E-PROC-OUTPUT OP-ERROR exit then
   value OP-STATUS ! ;

: OP-WAIT-SUP ( -- )
   OP-SUP CELL-PID@ dup PID>N 0 <= if
      drop true OP-PROTOCOL ! E-PROC-PTY-HANDLE OP-ERROR exit
   then
   PID>N wait-status {: status:n :}
   status 0 < if
      true OP-PROTOCOL ! E-PROC-WAIT OP-ERROR exit
   then
   status 0 <> if
      OP-STRUCTURED @ OP-ERR @ 0 <> and 0= if true OP-PROTOCOL ! then
      E-PROC-WAIT OP-ERROR
   then ;

: OP-GUARD-CHECK ( -- )
   OP-GUARD CELL-FD@ PROC-WATCH-EXIT? OP-GUARD-DEAD ! ;

: OP-WAIT-GROUP-RUN ( -- )
   OP-TARGET CELL-PID@ GROUP-WAIT-EMPTY ;

: OP-WAIT-GROUP ( -- )
   [: OP-WAIT-GROUP-RUN ;] catch dup 0= if drop exit then
   drop
   SUP-CLEAN-GROUP SUP-CLEAN+
   E-PROC-OUTPUT OP-ERROR ;

: OP-FAIL-SIGNAL ( -- )
   OP-PROTOCOL @ 0= if exit then
   false OP-GUARD-DEAD !
   [: OP-GUARD-CHECK ;] catch dup 0 <> if
      drop
      SUP-CLEAN-PROTOCOL SUP-CLEAN+
      E-PROC-OUTPUT OP-ERROR
   else
      drop
   then
   OP-GUARD-DEAD @ 0= if
      OP-TARGET CELL-PID@ GROUP-KILL-RC RETIRE-KILL? 0= if
         SUP-CLEAN-KILL SUP-CLEAN+
         E-PROC-OUTPUT OP-ERROR
      then
   then ;

: OP-FAIL-WAIT ( -- )
   OP-PROTOCOL @ if OP-WAIT-GROUP then ;

: OP-READ-EOF ( -- )
   OP-DONE CELL-FD@ READ-EOF? OP-EOF ! ;

: OP-VERIFY-EOF ( -- )
   false OP-EOF !
   [: OP-READ-EOF ;] catch dup 0 <> if
      drop
      true OP-PROTOCOL !
      SUP-CLEAN-PROTOCOL SUP-CLEAN+
      E-PROC-OUTPUT OP-ERROR
      exit
   then
   drop
   OP-EOF @ 0= if
      true OP-PROTOCOL !
      SUP-CLEAN-PROTOCOL SUP-CLEAN+
      E-PROC-OUTPUT OP-ERROR
   then ;

: OP-CLEAN ( -- )
   OP-FAIL-SIGNAL
   OP-MASTER OP-CLOSE
   OP-WAIT-SUP
   OP-VERIFY-EOF
   OP-FAIL-SIGNAL
   OP-FAIL-WAIT
   OP-LIFE OP-CLOSE
   OP-DONE OP-CLOSE
   OP-ANCHOR OP-CLOSE
   OP-GUARD OP-CLOSE
   OP-SUP-GUARD OP-CLOSE ;

: OP-CANCEL ( -- )
   OP-LIFE CELL-FD@ CANCEL-BAD? if E-PROC-OUTPUT OP-ERROR then ;

: OP-DONE-READY? ( -- bool )
   OP-DONE CELL-FD@ POLLIN 0 >IDX PFD!
   OP-SUP-GUARD CELL-FD@ POLLIN 1 >IDX PFD!
   POLL-FDS 2 KILL-WAIT-MS >MS MS>N poll {: rc:n :}
   rc 0 < if E-PROC-OUTPUT OP-ERROR true OP-PROTOCOL ! false exit then
   rc 0= if E-PROC-TIMEOUT OP-ERROR true OP-PROTOCOL ! false exit then
   0 >IDX PFD-EVENT? if true exit then
   E-PROC-OUTPUT OP-ERROR true OP-PROTOCOL ! false ;

: IO-FAIL ( process-pty-handle n -- )
   >r
   TAKE TEARDOWN-VIEW OP-SAVE
   OP-RESET
   r> OP-ERROR
   OP-CANCEL
   OP-DONE-READY? if OP-READ-STATUS then
   OP-CLEAN
   TEARDOWN-DONE
   UNLOCK
   OP-ERR @ throw ;

: OP-FINISH ( process-pty-teardown -- outcome )
   TEARDOWN-DONE
   UNLOCK
   OP-ERR @ dup 0 <> if throw then drop
   OP-STATUS @ PROC-STATUS>OUTCOME ;

: OP-KILL ( -- )
   OP-TARGET CELL-PID@ dup PID>N 0 <= if drop E-PROC-PTY-HANDLE OP-ERROR exit then
   drop
   OP-CANCEL ;

public

: START ( ptr u8 len -- process-pty-handle ) {: a:ptr u:len :}
   u LEN>N 0 <= if E-PROC-PATH throw then
   u LEN>N PATH-CAP >= if E-PROC-PATH throw then
   LOCK
   a PATH-A!
   u LEN>N PATH-U !
   START-GUARD
   UNLOCK ;

: WRITE ( process-pty-handle ptr u8 n -- process-pty-handle )
   >r >r MASTER-FD r> r>
   {: fd:fd src:ptr u:n :}
   LOCK
   fd src u WRITE-EXACT? if UNLOCK exit then
   E-PROC-OUTPUT IO-FAIL ;

: READ ( process-pty-handle ptr u8 n -- process-pty-handle n )
   >r >r MASTER-FD r> r>
   {: fd:fd dst:ptr u:n :}
   LOCK
   fd FD>N dst u read {: got:n :}
   got 0 < if E-PROC-OUTPUT IO-FAIL then
   UNLOCK
   got ;

: POLL-IN ( process-pty-handle ms -- process-pty-handle count )
   >r MASTER-FD r>
   {: fd:fd ms:ms :}
   LOCK
   fd ms POLL-ONE {: rc:n :}
   rc 0 < if E-PROC-OUTPUT IO-FAIL then
   UNLOCK
   rc >COUNT ;

: WAIT ( process-pty-handle -- outcome )
   VALID-HANDLE
   LOCK
   TAKE TEARDOWN-VIEW OP-SAVE
   OP-RESET
   OP-READ-STATUS
   OP-CLEAN
   OP-FINISH ;

: KILL ( process-pty-handle -- outcome )
   VALID-HANDLE
   LOCK
   TAKE TEARDOWN-VIEW OP-SAVE
   OP-RESET
   OP-KILL
   OP-DONE-READY? if OP-READ-STATUS then
   OP-CLEAN
   OP-FINISH ;

;package
