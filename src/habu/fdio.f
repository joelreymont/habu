\ fdio.f — write a whole span to a file descriptor, or die naming the failure.
\
\ ONE CONCERN, AND IT IS SPLIT OUT BECAUSE IT HAS THREE CONSUMERS IN TWO KINDS OF
\ PROCESS. `write` may write fewer bytes than it was given, so every producer of a
\ file in this tree needs the same loop. src/habu/driver-io.f carried it as
\ DRV-WALL alongside two unrelated concerns - image emission with its size map,
\ and uncaught-throw reporting - and those two drag in the whole image writer
\ (MBUF, BUILD-IMAGE, CODESIG2, the target's Mach-O or ELF half). That is why a
\ BOOTED engine cannot `require src/habu/driver-io.f`: it stops at E-UNDEFINED:
\ MBUF, and reaching it needs six more files including the target-specific image
\ writer. A capture running inside bin/hb has to write its artifact and has no
\ business compiling a Mach-O writer to do it, so the loop lives here, on its own,
\ where a booted engine can require it with nothing behind it.
\
\ WHO LOADS IT. Every builder, immediately before driver-io.f (tools/bootstrap.sh
\ SRC_COMMON, tools/build-fixpoint.f BF-APPEND-COMMON and BF-APPEND-SNAP-BUILD,
\ tools/srclist.f), and tools/object-image.f loads it ahead of driver-io.f the
\ same way. A booted engine requires it directly.
\
\ FAIL-CLOSED. A write that returns zero or an error is not retried and not
\ logged: the caller asked for bytes on disk and did not get them, so the process
\ stops. A partial write IS retried, because a short write is the kernel doing
\ what it is allowed to do rather than a failure.

package FDIO

74 constant FAIL-RC        \ the internal-driver I/O exit status (driver-io.f's own)

variable WR                \ bytes the last write accepted
variable OFF               \ bytes handed over so far
variable FD
variable A
variable U

\ The span's address is a pointer, so it travels through a pointer field rather
\ than a plain cell (docs/forth.md § ptr locals and cell access).
: A-FIELD ( -- ptr ptr u8 ) A 0 ptr-field ;
: A@ ( -- ptr u8 ) A-FIELD @ ;
: A! ( ptr u8 -- ) A-FIELD ! ;

: LEFT ( -- n ) U @ OFF @ - ;
: DST ( -- ptr u8 ) A@ OFF @ + ;
: MORE? ( -- bool ) OFF @ U @ < ;

: STEP ( -- )
   FD @ DST LEFT write WR !
   WR @ 0 <= if s" fdio: write failed" FAIL-RC die then
   OFF @ WR @ + OFF ! ;

public

\ Hand every byte of [a, a+u) to the descriptor, however many writes that takes.
: WALL ( n ptr u8 n -- )
   U !
   A!
   FD !
   0 OFF !
   begin MORE? while STEP repeat ;

;package
