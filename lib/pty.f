\ pty.f - Linux pseudoterminal pairs for test peers.
\
\ A test opens a pair, hands the slave path to the device driver under test
\ (lib/serial.f opens it like any serial device), and drives the master side
\ as the peer. The sequence is the one the process fixtures already use: open
\ /dev/ptmx, unlock it with TIOCSPTLCK, read its number with TIOCGPTN, and name
\ the slave /dev/pts/<n>. Only the raw open, ioctl, read, write, poll and close
\ primitives are involved; no libc symbol is borrowed.
\
\ Load after lib/errors.f and lib/fs.f.
require lib/errors.f
require lib/fs.f

package PTY
public

DEFTYPE MASTER

E-PTY-OPEN constant E-OPEN
E-PTY-IOCTL constant E-IOCTL
E-PTY-IO constant E-IO
E-PTY-CAPACITY constant E-CAPACITY

$40045431 constant LINUX-TIOCSPTLCK
$80045430 constant LINUX-TIOCGPTN
$102 constant PTY-OPEN-FLAGS            \ O_RDWR | O_NOCTTY
16 constant SLAVE-PATH-CAP              \ /dev/pts/ plus up to six digits and a NUL

private

1 constant POLLIN
create LOCK 1 cells allot
create NUMBER 1 cells allot
create POLLFD 8 allot
variable WRITTEN

: W32! ( n ptr u8 -- ) {: value:n dst :}
   value $FF and dst c! value 8 rshift $FF and dst 1 + c!
   value 16 rshift $FF and dst 2 + c! value 24 rshift $FF and dst 3 + c! ;

: DIGIT-COUNT ( n -- n )
   1 swap begin 10 / dup 0 > while swap 1+ swap repeat drop ;

: DIGITS! ( n ptr u8 -- n ) {: value:n dst :}
   value DIGIT-COUNT {: count:n :}
   value count 0 do 10 /mod swap [char] 0 + dst count 1- i - + c! loop drop
   count ;

\ Writes /dev/pts/<number> into the caller's buffer and returns its length.
: SLAVE-PATH! ( n ptr u8 -- n ) {: number:n path :}
   s" /dev/pts/" {: prefix plen:n :}
   plen 0 do prefix i + c@ path i + c! loop
   number path plen + DIGITS! plen + ;

public

\ The caller's buffer of at least SLAVE-PATH-CAP bytes receives the slave path.
: OPEN ( ptr u8 n -- master n ) {: path cap:n :}
   cap SLAVE-PATH-CAP < if E-CAPACITY throw then
   s" /dev/ptmx" FS-PATHZ PTY-OPEN-FLAGS 0 open {: m:n :}
   m 0 < if E-OPEN throw then
   0 LOCK !
   m LINUX-TIOCSPTLCK LOCK ioctl 0 <> if m close E-IOCTL throw then
   0 NUMBER !
   m LINUX-TIOCGPTN NUMBER ioctl 0 <> if m close E-IOCTL throw then
   m >MASTER NUMBER @ $FFFFFFFF and path SLAVE-PATH! ;


\ One chunk after at most ms of waiting; zero means nothing arrived in time.
: READ ( master ptr u8 n ms -- n ) {: m:master bytes cap:n timeout:ms :}
   m MASTER>N POLLFD W32! POLLIN POLLFD 4 + W32!
   POLLFD 1 timeout MS>N poll {: ready:n :}
   ready 0 < if E-IO throw then
   ready 0= if 0 exit then
   m MASTER>N bytes cap read {: got:n :}
   got 0 < if E-IO throw then got ;


: WRITE ( master ptr u8 n -- ) {: m:master bytes size:n :}
   0 WRITTEN !
   begin WRITTEN @ size < while
      m MASTER>N bytes WRITTEN @ + size WRITTEN @ - write {: wrote:n :}
      wrote 0 <= if E-IO throw then
      wrote WRITTEN +!
   repeat ;


: CLOSE ( master -- )
   MASTER>N close ;

;package
