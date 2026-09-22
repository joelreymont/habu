\ pty.f - pseudoterminal pairs, and the one place the pair is opened.
\
\ A test opens a pair, hands the slave path to the device driver under test
\ (lib/serial.f opens it like any serial device), and drives the master side
\ as the peer. lib/pty-harness.f, lib/process-pty-io.f and the suites that need
\ a terminal open their pairs here too and then open the slave themselves.
\
\ OPEN carries both targets: Linux unlocks /dev/ptmx with TIOCSPTLCK, reads its
\ number with TIOCGPTN and names the slave /dev/pts/<n>; Darwin grants and
\ unlocks the same device with TIOCPTYGRANT/TIOCPTYUNLK and asks TIOCPTYGNAME
\ for the name. A host that is neither is E-PROC-HOST. The request numbers are
\ private: a caller that needs a pair calls OPEN instead of repeating them.
\ Only the raw open, ioctl, read, write and close primitives are involved; no
\ libc symbol is borrowed.
\
\ READ waits on the AIO loop (docs/aio.md): one POLL-ADD for the window it was
\ given and one AWAIT, so no thread parks in poll(2). A program calls
\ AIO:LOOP-START before its first READ; a wait with no loop is E-AIO-STATE.
\
\ Load after lib/errors.f and lib/fs.f.
require lib/errors.f
require lib/type/deftype.f
require lib/fs.f
require lib/aio.f

package PTY
public

DEFTYPE MASTER

E-PTY-OPEN constant E-OPEN
E-PTY-IOCTL constant E-IOCTL
E-PTY-IO constant E-IO
E-PTY-CAPACITY constant E-CAPACITY

\ The engine's open takes Darwin-spelled flags on both targets and translates
\ them (src/os/linux/sys.f OS-OPEN-FLAGS), so O_NOCTTY is this bit, not the raw
\ Linux one: neither end of a pair may become a controlling terminal here.
$20002 constant PTY-OPEN-FLAGS          \ O_RDWR | O_NOCTTY
$80 constant SLAVE-PATH-CAP             \ Darwin's TIOCPTYGNAME answer, NUL included

private

$40045431 constant LINUX-TIOCSPTLCK
$80045430 constant LINUX-TIOCGPTN
$20007454 constant MACOS-TIOCPTYGRANT
$20007452 constant MACOS-TIOCPTYUNLK
$40807453 constant MACOS-TIOCPTYGNAME
create LOCK 1 cells allot
create NUMBER 1 cells allot
variable WRITTEN

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

\ A failed ioctl leaves no master behind: the pair is not open until the name is.
: IOCTL-CK ( n n -- ) {: m:n rc:n :}
   rc 0 <> if m close E-IOCTL throw then ;


: OPEN-MASTER ( -- n )
   s" /dev/ptmx" FS-PATHZ PTY-OPEN-FLAGS 0 open {: m:n :}
   m 0 < if E-OPEN throw then
   m ;


: NAME-LINUX ( n ptr u8 -- n ) {: m:n path :}
   0 LOCK !
   m  m LINUX-TIOCSPTLCK LOCK ioctl  IOCTL-CK
   0 NUMBER !
   m  m LINUX-TIOCGPTN NUMBER ioctl  IOCTL-CK
   NUMBER @ $FFFFFFFF and path SLAVE-PATH! ;


\ Darwin writes the name itself, NUL-terminated, so its length is the distance
\ to that NUL rather than something this side built.
: NAME-LEN ( ptr u8 -- n ) {: path :}
   0 begin dup SLAVE-PATH-CAP < while
      dup path + c@ 0= if exit then
      1+
   repeat
   drop E-CAPACITY throw ;


: NAME-MACOS ( n ptr u8 -- n ) {: m:n path :}
   m  m MACOS-TIOCPTYGRANT NULL-PTR ioctl  IOCTL-CK
   m  m MACOS-TIOCPTYUNLK NULL-PTR ioctl  IOCTL-CK
   m  m MACOS-TIOCPTYGNAME path ioctl  IOCTL-CK
   path NAME-LEN ;


: NAME-SLAVE ( n ptr u8 -- n ) {: m:n path :}
   HB-TARGET-LINUX? if m path NAME-LINUX exit then
   HB-TARGET-MACOS? if m path NAME-MACOS exit then
   m close E-PROC-HOST throw ;


\ What the master had to give once the loop answered that it was readable.
: READ-READY ( master ptr u8 n -- n ) {: m:master bytes cap:n :}
   m MASTER>N bytes cap read {: got:n :}
   got 0 < if E-IO throw then got ;

public

\ Open a pseudoterminal pair on either target: the master descriptor, and the
\ slave's path written NUL-terminated into the caller's buffer of at least
\ SLAVE-PATH-CAP bytes with its length. The slave is the caller's to open, with
\ PTY-OPEN-FLAGS, at the moment it wants one.
: OPEN ( ptr u8 n -- master n ) {: path cap:n :}
   cap SLAVE-PATH-CAP < if E-CAPACITY throw then
   OPEN-MASTER {: m:n :}
   m path NAME-SLAVE {: len:n :}
   0 path len + c!
   m >MASTER len ;


\ One chunk after at most ms of waiting; zero means nothing arrived in time.
\ The window is the poll's own linked timeout, so a signal no longer cuts it
\ short and there is nothing to restart. `cancelled` cannot arrive - nothing
\ here cancels, the ticket never leaves this word, and the cleanup AIO registers
\ on a submitting task runs only after that task has ended - so it is a broken
\ foreign result.
: READ ( master ptr u8 n ms -- n ) {: m:master bytes cap:n timeout:ms :}
   m MASTER>N >FD AIO:READABLE timeout AIO:POLL-ADD AIO:AWAIT
   MATCH AIO:outcome
      ready OF drop m bytes cap READ-READY ENDOF
      timed-out OF 0 ENDOF
      cancelled OF E-IO throw ENDOF
      refused OF drop E-IO throw ENDOF
   ;MATCH ;


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
