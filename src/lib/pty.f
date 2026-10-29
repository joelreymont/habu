\ pty.f - pseudoterminal pairs, and the one place a pair is opened: tests,
\ src/lib/pty-harness.f and src/lib/process-pty-io.f open pairs here and open
\ the slave themselves (src/lib/serial.f opens it like any serial device). Only
\ raw open, ioctl, read, write and close are used; no libc symbol.
\ STORAGE CLASS. PROCESS-WIDE: OPEN stages its ioctl arguments in this file's
\ cells, so one task opens a pair at a time.

require src/lib/errors.f
require src/lib/type/deftype.f
require src/lib/fs.f
require src/lib/aio.f

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

: SLAVE-PATH! ( n ptr u8 -- n ) {: number:n path :}
   s" /dev/pts/" {: prefix plen:n :}
   plen 0 do prefix i + c@ path i + c! loop
   number path plen + DIGITS! plen + ;

\ A failed ioctl leaves no master behind: the pair is not open until the name is.
: IOCTL-CK ( n n -- ) {: m:n rc:n :}
   rc 0 <> if m close E-IOCTL throw then ;


: OPEN-MASTER ( -- n )
   s" /dev/ptmx" FS:>PATHZ PTY-OPEN-FLAGS 0 open {: m:n :}
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
   HB-TARGET-LINUX-KERNEL? if m path NAME-LINUX exit then
   HB-TARGET-MACOS? if m path NAME-MACOS exit then
   m close E-PROC-HOST throw ;


: READ-READY ( master ptr u8 n -- n ) {: m:master bytes cap:n :}
   m MASTER>N bytes cap read {: got:n :}
   got 0 < if E-IO throw then got ;

public

\ The master descriptor, and the slave's path written NUL-terminated into a
\ caller buffer of at least SLAVE-PATH-CAP bytes, with its length; the caller
\ opens the slave with PTY-OPEN-FLAGS. Linux unlocks /dev/ptmx with TIOCSPTLCK
\ and names /dev/pts/<TIOCGPTN> (needs a mounted /dev/pts); Darwin grants and
\ unlocks with TIOCPTYGRANT/TIOCPTYUNLK and asks TIOCPTYGNAME. Any other host
\ is E-PROC-HOST. The request numbers stay private to this file.
: OPEN ( ptr u8 n -- master n ) {: path cap:n :}
   cap SLAVE-PATH-CAP < if E-CAPACITY throw then
   OPEN-MASTER {: m:n :}
   m path NAME-SLAVE {: len:n :}
   0 path len + c!
   m >MASTER len ;


\ One chunk after at most ms; zero means nothing arrived in time. Waits on the
\ AIO loop (src/lib/aio.f), one POLL with its own linked timeout and one AWAIT,
\ so no thread parks in poll(2) and a signal does not cut the window short.
\ Call AIO:START before the first READ; with no loop it is E-AIO-STATE.
\ `cancelled` cannot arrive (nothing cancels, the ticket stays here, AIO's
\ cleanup runs only after the submitting task ends): it is a broken foreign
\ result.
: READ ( master ptr u8 n ms -- n ) {: m:master bytes cap:n timeout:ms :}
   m MASTER>N >FD AIO:READABLE timeout AIO:POLL AIO:AWAIT
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
