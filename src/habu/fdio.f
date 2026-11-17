\ fdio.f - write a whole span to a file descriptor, or die naming the failure.
\ Its own file so a booted engine can require it: driver-io.f reaches MBUF and
\ the image writer. A short write is retried; a write returning zero or an
\ error stops the process, unretried and unlogged.

require src/core/exit-status.f

package FDIO

variable WR                \ bytes the last write accepted
variable OFF               \ bytes handed over so far
variable FD
\ The span's address is a pointer, so its cell is declared storage rather than a
\ plain cell.
PTR-VARIABLE A
variable U

: A@ ( -- ^u8 ) A @ ;
: A! ( ^u8 -- ) A ! ;

: LEFT ( -- n ) U @ OFF @ - ;
: DST ( -- ^u8 ) A@ OFF @ + ;
: MORE? ( -- bool ) OFF @ U @ < ;

: STEP ( -- )
   FD @ DST LEFT write WR !
   WR @ 0 <= if s" fdio: write failed" EXIT-STATUS:IOERR die then
   OFF @ WR @ + OFF ! ;

public

: WALL ( n ^u8 n -- )
   U !
   A!
   FD !
   0 OFF !
   begin MORE? while STEP repeat ;

;package
