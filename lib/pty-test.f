\ pty-test.f - a pseudoterminal pair carries bytes both ways.
\
\ PTY:READ waits on the AIO loop, so RUN starts the loop after the last
\ definition (a live task forbids compilation) and stops it at the end.
\
\ Run: bin/hb --load lib/pty-test.f
require lib/errors.f
require lib/test.f
require lib/string.f
require lib/fs.f
require lib/aio.f
require lib/pty.f

package PTY-TEST

create SLAVE PTY:SLAVE-PATH-CAP allot
create SPARE PTY:SLAVE-PATH-CAP allot
create BUF 32 allot
variable SPARE-MASTER

\ The slave end, opened the way a device driver under test opens a serial port.
: SLAVE-FD ( n -- n ) {: u:n :}
   SLAVE u FS-PATHZ PTY:PTY-OPEN-FLAGS 0 open ;

: PAIR ( PTY:master n -- ) {: m:PTY:master u:n :}
   s" the slave is a pts device" T-LABEL
   u 9 > TTRUE
   SLAVE 9 s" /dev/pts/" STR= TTRUE
   u SLAVE-FD {: s:n :}
   s 0 >= TTRUE
   s" a signal storm does not shorten a wait the loop owns" T-LABEL
   1000 prof-rate 1000000 prof-on
   mono-ns {: started:n :}
   m BUF 32 100 >MS PTY:READ {: quiet:n :}
   mono-ns started - {: elapsed:n :}
   prof-off
   quiet 0 T=
   elapsed 90000000 >= TTRUE
   elapsed 1000000000 < TTRUE
   s" bytes written at the slave arrive at the master" T-LABEL
   s s" pong" write 4 T=
   m BUF 32 2000 >MS PTY:READ 4 T=
   BUF 4 s" pong" STR= TTRUE
   s" and a line written at the master arrives at the slave" T-LABEL
   m S\" ping\n" PTY:WRITE
   s BUF 32 read 5 T=
   BUF 5 S\" ping\n" STR= TTRUE
   s" and the master hears the terminal's echo of it, newline as CR LF" T-LABEL
   m BUF 32 2000 >MS PTY:READ 6 T=
   BUF 4 s" ping" STR= TTRUE
   BUF 4 + c@ 13 T=  BUF 5 + c@ 10 T=
   s close
   m PTY:CLOSE ;

\ The loop is the program's to start, and no library starts one: a read without
\ it says so by name instead of falling back to a thread parked in poll(2).
: STOPPED-READ ( -- )
   SPARE-MASTER @ PTY:>MASTER BUF 32 100 >MS PTY:READ drop ;

: NO-LOOP ( -- )
   s" a read with the loop stopped is refused by name" T-LABEL
   SPARE PTY:SLAVE-PATH-CAP PTY:OPEN drop PTY:MASTER>N SPARE-MASTER !
   AIO:STOP
   [: STOPPED-READ ;] E-AIO-STATE TTHROWSQ
   AIO:START
   SPARE-MASTER @ PTY:>MASTER PTY:CLOSE ;

: RUN ( -- )
   T-RESET
   AIO:START
   SLAVE PTY:SLAVE-PATH-CAP PTY:OPEN PAIR
   NO-LOOP
   AIO:STOP
   T-REPORT
   s" pty-test: ok" type cr ;

RUN

;package
