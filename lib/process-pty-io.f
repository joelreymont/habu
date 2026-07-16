\ process-pty-io.f - framed PTY supervisor protocol and stable exec inputs.

require lib/process-pty-handle.f
require lib/memory.f

package PROCESS-PTY

$1000 constant PATH-CAP
$100 constant VEC-CAP
$20000 constant O-NOCTTY
2 constant O-RDWR
2 constant F-SETFD
1 constant FD-CLOEXEC
1 constant POLLIN
8 constant POLLERR
16 constant POLLHUP
32 constant POLLNVAL
9 constant SIGKILL

create EXE-Z PATH-CAP allot
create SLAVE-Z PATH-CAP allot
create ARGV VEC-CAP cells allot
create ENVP VEC-CAP cells allot
create FRAME 1 cells allot
create SIGNAL-BYTE 1 allot
create POLL-FDS 2 cells allot

variable BUSY

: LOCK ( -- )
   0 1 BUSY atomic-cas 0 <> if E-PROC-PTY-CAPACITY throw then ;

: UNLOCK ( -- )
   0 BUSY atomic! ;

: COPY-Z ( ptr u8 n ptr u8 -- ptr u8 ) {: src:ptr u:n dst:ptr :}
   u 0 < if E-PROC-PATH throw then
   u PATH-CAP >= if E-PROC-PATH throw then
   src dst u BYTE-COPY
   0 dst u + c!
   dst ;

: VEC-COPY ( ptr a ptr a -- ptr a ) {: src:ptr dst:ptr :}
   0 begin dup VEC-CAP < while
      dup cells src + @ dup over cells dst + !
      0= if drop dst exit then
      1+
   repeat drop
   E-PROC-ENV throw ;

: INPUTS! ( ptr u8 n ptr u8 n ptr a ptr a -- )
   {: exe:ptr exeu:n slave:ptr slaveu:n argv:ptr envp:ptr :}
   exe exeu EXE-Z COPY-Z drop
   slave slaveu SLAVE-Z COPY-Z drop
   argv ARGV VEC-COPY drop
   envp ENVP VEC-COPY drop ;

: FD-CLOSE ( fd -- ) {: fd:fd :}
   fd FD>N 0 >= if fd FD>N close then ;

: FD-CLOEXEC? ( fd -- bool ) {: fd:fd :}
   fd FD>N F-SETFD FD-CLOEXEC fcntl 0= ;

: PIPE-CLOSE ( fd fd -- )
   FD-CLOSE FD-CLOSE ;

: PIPE-PAIR-CLOEXEC ( -- fd fd )
   pipe {: r:n w:n rc:n :}
   rc 0 <> if E-PROC-OUTPUT throw then
   r >FD {: rf:fd :}
   w >FD {: wf:fd :}
   rf FD-CLOEXEC? 0= if rf wf PIPE-CLOSE E-PROC-OUTPUT throw then
   wf FD-CLOEXEC? 0= if rf wf PIPE-CLOSE E-PROC-OUTPUT throw then
   rf wf ;

: WRITE-EXACT ( fd ptr u8 n -- bool ) {: fd:fd src:ptr u:n :}
   u 0= if true exit then
   fd FD>N src u write {: wrote:n :}
   wrote 0 <= if false exit then
   wrote u > if false exit then
   fd src wrote + u wrote - recurse ;

: READ-EXACT ( fd ptr u8 n -- bool ) {: fd:fd dst:ptr u:n :}
   u 0= if true exit then
   fd FD>N dst u read {: got:n :}
   got 0 <= if false exit then
   got u > if false exit then
   fd dst got + u got - recurse ;

: FRAME-WRITE ( fd n -- bool ) {: fd:fd value:n :}
   value FRAME !
   fd FRAME 1 cells WRITE-EXACT ;

: FRAME-READ ( fd -- n bool ) {: fd:fd :}
   fd FRAME 1 cells READ-EXACT dup if FRAME @ swap else 0 swap then ;

: SIGNAL-WRITE ( fd -- bool ) {: fd:fd :}
   1 SIGNAL-BYTE c!
   fd SIGNAL-BYTE 1 WRITE-EXACT ;

: SIGNAL-READ ( fd -- bool ) {: fd:fd :}
   fd SIGNAL-BYTE 1 READ-EXACT ;

: EXEC-SUCCEEDED? ( fd -- bool ) {: fd:fd :}
   fd FD>N SIGNAL-BYTE 1 read 0= ;

: PFD! ( fd idx -- ) {: fd:fd idx:idx :}
   POLLIN 32 lshift fd FD>N $FFFFFFFF and or
   idx IDX>N cells POLL-FDS + ! ;

: FD-EVENT? ( fd -- bool ) {: fd:fd :}
   fd 0 >IDX PFD!
   POLL-FDS 1 0 poll 0 <> ;

: WATCH-EVENT ( fd fd -- ) {: procfd:fd lifefd:fd :}
   procfd 0 >IDX PFD!
   lifefd 1 >IDX PFD!
   POLL-FDS 2 -1 poll drop ;

: PID-KILL-GROUP ( pid -- ) {: pid:pid :}
   pid PID>N negate SIGKILL kill drop ;

: PID-KILL ( pid -- ) {: pid:pid :}
   pid PID>N SIGKILL kill drop ;

: PID-WAIT-RAW ( pid -- n ) {: pid:pid :}
   pid PID>N wait-status ;

: CHILD-EXIT ( n -- ) {: rc:n :}
   s" " drop 0 rc die ;

;package
