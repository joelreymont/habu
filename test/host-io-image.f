\ Serial and UDP workers in a saved image, three native generations in fresh
\ processes: the pty master and the UDP peer are tasks in this row. The first
\ generation is saved from the keyed saver host (test/app-image-engine.f).
require src/lib/test.f
require src/lib/test/subject.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/pty.f
require src/lib/net/udp4.f
require test/app-image-engine.f
require test/suite-budget.f
require test/pcap-result.f

package HOST-IO-IMAGE
private

CAST: BLEN>N ( NUM:byte-len -- n )

$10000 constant CAP
2 constant ROUNDS                  \ RUN, PREPARE, PREPARE, RUN: two exchanges per process
100 constant POLL-MS               \ one peer wait; STOP is read between waits
$7F000001 constant LOOPBACK
$A5 constant SERIAL-REQ
$5A constant SERIAL-REPLY
$19 constant UDP-REQ
$E6 constant UDP-REPLY

create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create FIRST-BUF FS-PATH-CAP allot
create SECOND-BUF FS-PATH-CAP allot
create SLAVE PTY:SLAVE-PATH-CAP allot
create SCRIPT SB-CAP allot          \ one string-builder text (SB$)
create SERIAL-IN 1 allot
create SERIAL-OUT 1 allot
create UDP-IN 2 allot
create UDP-OUT 1 allot

variable ROOT-U
variable FIRST-U
variable SECOND-U
variable SLAVE-U
variable SCRIPT-U
variable MASTER
variable SLAVE-FD
variable SOCK
variable PEER-PORT
variable LIVE                      \ the peer tasks are activated and not yet released
variable STOP
variable SERIAL-DONE
variable UDP-DONE
variable SERIAL-BAD
variable UDP-BAD
TASK:MIN-STACK TASK:TASK SERIAL-PEER
TASK:MIN-STACK TASK:TASK UDP-PEER

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: FIRST$ ( -- ptr u8 n ) FIRST-BUF FIRST-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;
: SCRIPT$ ( -- ptr u8 n ) SCRIPT SCRIPT-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" host-io-image-test" FS:MKTEMP-DIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" first" FIRST-BUF JOIN-PATH FIRST-U !
   ROOT$ s" second" SECOND-BUF JOIN-PATH SECOND-U ! ;

\ The pair is the device: this process holds a slave descriptor for the whole
\ run, so the subject's CLOSE between rounds does not hang the master up;
\ neither end may become a controlling terminal.
: OPEN-PEERS ( -- )
   SLAVE PTY:SLAVE-PATH-CAP PTY:OPEN {: m:PTY:master len:n :}
   m PTY:MASTER>N MASTER !
   len SLAVE-U !
   MASTER @ >FD FD-CLOEXEC!
   SLAVE PTY:PTY-OPEN-FLAGS 0 open {: sfd:n :}
   sfd 0 >= TTRUE
   sfd SLAVE-FD !
   sfd >FD FD-CLOEXEC!
   LOOPBACK UDP4:ADDRESS 0 UDP4:PORT UDP4:BIND MATCH UDP4:open-result
      opened OF UDP4:SOCKET>N SOCK ! ENDOF
      failed OF UDP4:ERRNO>N throw ENDOF
   ;MATCH
   SOCK @ UDP4:>SOCKET UDP4:LOCAL MATCH UDP4:endpoint-result
      endpoint OF UDP4:PORT>N PEER-PORT ! UDP4:ADDRESS>N drop ENDOF
      failed OF UDP4:ERRNO>N throw ENDOF
   ;MATCH ;

: CLOSE-PEERS ( -- )
   SOCK @ UDP4:>SOCKET UDP4:CLOSE MATCH UDP4:status
      ok OF ENDOF
      failed OF UDP4:ERRNO>N throw ENDOF
   ;MATCH
   SLAVE-FD @ close
   MASTER @ PTY:>MASTER PTY:CLOSE ;

\ One serial exchange per round: exactly $A5 in, $5A out.
: SERIAL-SERVE ( -- )
   begin STOP atomic@ 0= SERIAL-DONE atomic@ ROUNDS < and while
      MASTER @ PTY:>MASTER SERIAL-IN 1 POLL-MS >MS PTY:READ {: got:n :}
      got 0 > if
         SERIAL-IN c@ SERIAL-REQ <> if SERIAL-IN c@ SERIAL-BAD atomic! exit then
         MASTER @ PTY:>MASTER SERIAL-OUT 1 PTY:WRITE
         1 SERIAL-DONE atomic-add drop
      then
   repeat ;

\ One datagram per round: exactly one byte $19, answered with $E6 to its sender.
: UDP-SERVE ( -- )
   begin STOP atomic@ 0= UDP-DONE atomic@ ROUNDS < and while
      SOCK @ UDP4:>SOCKET UDP-IN 2 UDP4:PAYLOAD-BYTES POLL-MS >MS UDP4:RECEIVE
      MATCH UDP4:receive-result
         packet OF {: size:NUM:byte-len address:UDP4:address port:UDP4:port :}
            size BLEN>N 1 <> UDP-IN c@ UDP-REQ <> or if UDP-IN c@ UDP-BAD atomic! exit then
            SOCK @ UDP4:>SOCKET address port UDP-OUT 1 UDP4:PAYLOAD-BYTES UDP4:SEND
            MATCH UDP4:status
               ok OF ENDOF
               failed OF UDP4:ERRNO>N UDP-BAD atomic! exit ENDOF
            ;MATCH
            1 UDP-DONE atomic-add drop
         ENDOF
         truncated OF drop drop BLEN>N UDP-BAD atomic! exit ENDOF
         timeout OF ENDOF
         failed OF UDP4:ERRNO>N UDP-BAD atomic! exit ENDOF
      ;MATCH
   repeat ;

: PEERS-START ( -- )
   0 STOP atomic!
   0 SERIAL-DONE atomic!
   0 UDP-DONE atomic!
   -1 SERIAL-BAD atomic!
   -1 UDP-BAD atomic!
   SERIAL-REPLY SERIAL-OUT c!
   UDP-REPLY UDP-OUT c!
   ['] SERIAL-SERVE SERIAL-PEER TASK:ACTIVATE
   ['] UDP-SERVE UDP-PEER TASK:ACTIVATE
   1 LIVE ! ;

: WAIT-DONE ( ptr n -- ) {: worker:ptr :}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat ;

\ Each peer ends on its own within one POLL-MS of STOP, with no wait in flight
\ on the loop, and is then released. A released task never reads as done, so
\ LIVE is what makes a second call, CLEANUP's after a generation's, a no-op.
: PEERS-STOP ( -- )
   LIVE @ 0= if exit then
   0 LIVE !
   1 STOP atomic!
   SERIAL-PEER WAIT-DONE SERIAL-PEER TASK:KILL
   UDP-PEER WAIT-DONE UDP-PEER TASK:KILL ;

: PEERS-CHECK ( -- )
   s" the serial peer saw $A5 and answered every round" T-LABEL
   SERIAL-DONE atomic@ ROUNDS T=
   SERIAL-BAD atomic@ -1 T=
   s" the UDP peer saw one byte $19 and answered every round" T-LABEL
   UDP-DONE atomic@ ROUNDS T=
   UDP-BAD atomic@ -1 T= ;

\ The stdin program: the exchange, an empty second PREPARE (must be harmless;
\ RUN then registers both owners again before capture's own PREPARE), the
\ exchange again, and a SAVE to argv 0 when the generation is captured.
: RUN-LINE ( -- )
   S\" s\q " SB-APPEND
   SLAVE SLAVE-U @ SB-APPEND
   S\" \q " SB-APPEND
   PEER-PORT @ FMT:SB-U
   S\"  UDP4:PORT HOST-IO-IMAGE-SUBJECT:RUN\n" SB-APPEND ;

: SCRIPT! ( bool bool -- )
   {: first:bool save:bool :}
   SB-RESET
   first if S\" 1 set-tier\nrequire test/host-io-image-subject.f\n" SB-APPEND then
   RUN-LINE
   S\" IMAGE-LIFECYCLE:PREPARE\nIMAGE-LIFECYCLE:PREPARE\n" SB-APPEND
   RUN-LINE
   save if S\" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" SB-APPEND then
   SB$ {: a:ptr u:n :}
   a SCRIPT u BYTE-COPY
   u SCRIPT-U ! ;

\ A generation whose own transfer met its deadline exits PROC-TIMEOUT-RC: that is
\ the row's deadline, printed with the program and capture and thrown, not
\ asserted.
: CHECK-RUN ( n n n -- )
   {: outu:n erru:n rc:n :}
   SCRIPT$ OUT outu ERR erru rc SUBJECT:RC-TIMED-OUT
   rc 0<> erru 0<> or if
      s" host-io-image child failed: " type rc . cr
      OUT outu type ERR erru type
   then
   s" the generation exits 0 with nothing on stderr" T-LABEL
   rc 0 T= erru 0 T=
   s" both exchanges report io=ok" T-LABEL
   OUT outu S\" io=ok\nio=ok\n" T$= ;

: SAVED ( ptr u8 n -- )
   2dup FILE? TTRUE EXECUTABLE? TTRUE ;

\ Use a small explicit environment for every child.
: ENVIRONMENT ( -- )
   s" PATH" >LEN s" /usr/bin:/bin" >LEN PROC-ENV+
   s" LANG" >LEN s" C" >LEN PROC-ENV+ ;

\ Generation 0: the saver host compiles the subject in the source tree.
: GEN-FIRST ( -- )
   APP-IMAGE-ENGINE:PATH$ {: host:ptr hostu:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   FIRST$ >LEN PROC-ARGV+
   ENVIRONMENT
   true true SCRIPT!
   PEERS-START
   host hostu >LEN SCRIPT$ >LEN OUT CAP >LEN ERR CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE PCAP-RESULT:>N
   PEERS-STOP CHECK-RUN PEERS-CHECK
   s" the first generation saved an executable" T-LABEL
   FIRST$ SAVED ;

\ A restored image runs outside the source tree, cwd the scratch root.
: GEN-RESTORED ( ptr u8 n bool -- )
   {: exe:ptr exeu:n save:bool :}
   PROC-ARGV-ENV-RESET
   save if
      s" --" >LEN PROC-ARGV+
      SECOND$ >LEN PROC-ARGV+
   then
   ENVIRONMENT
   false save SCRIPT!
   PEERS-START
   exe exeu >LEN ROOT$ >LEN SCRIPT$ >LEN OUT CAP >LEN ERR CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE PCAP-RESULT:>N
   PEERS-STOP CHECK-RUN PEERS-CHECK ;

: GENERATIONS ( -- )
   GEN-FIRST
   FIRST$ true GEN-RESTORED
   s" the restored first generation saved the second" T-LABEL
   SECOND$ SAVED
   SECOND$ false GEN-RESTORED ;

: CLEANUP ( -- )
   PEERS-STOP CLOSE-PEERS AIO:STOP CLEANUP-RUN ;

public

: RUN ( -- )
   T-RESET PREPARE
   AIO:START OPEN-PEERS
   [: GENERATIONS ;] [: CLEANUP ;] finally
   T-REPORT ;

;package
HOST-IO-IMAGE:RUN
