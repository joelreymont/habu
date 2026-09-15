\ pty-test.f - a pseudoterminal pair carries bytes both ways.
\
\ Run: bin/hb --load lib/pty-test.f
require lib/test.f
require lib/string.f
require lib/fs.f
require lib/pty.f

package PTY-TEST

create SLAVE PTY:SLAVE-PATH-CAP allot
create BUF 32 allot

\ The slave end, opened the way a device driver under test opens a serial port.
: SLAVE-FD ( n -- n ) {: u:n :}
   SLAVE u FS-PATHZ PTY:PTY-OPEN-FLAGS 0 open ;

: PAIR ( PTY:master n -- ) {: m:PTY:master u:n :}
   s" the slave is a pts device" T-LABEL
   u 9 > TTRUE
   SLAVE 9 s" /dev/pts/" STR= TTRUE
   u SLAVE-FD {: s:n :}
   s 0 >= TTRUE
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

: RUN ( -- )
   T-RESET
   SLAVE PTY:SLAVE-PATH-CAP PTY:OPEN PAIR
   T-REPORT
   s" pty-test: ok" type cr ;

RUN

;package
