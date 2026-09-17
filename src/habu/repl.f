\ repl.f — the interactive REPL for the stdin engine (bin/hb). Baked as the
\ engine's LSRC; EMIT-SOURCE runs it only when fd 0 is a tty (a pipe gets the
\ classic batch read-all and never sees these words run). INSTALL points
\ REPLH-CELL at RD-LINE; the engine's LEXIT path then prints " ok", saves the
\ line-start compile state (CP/NDICT/DP/SP), and BLRs RD-LINE for the next
\ line. Undefined words print `E-UNDEFINED: name`, uncaught THROWs print "?",
\ roll the line back, and re-read instead of exiting.
\ The editor runs the tty RAW (per line; canonical is restored while the line
\ executes): insert/backspace at a cursor, left/right arrows, ^A/^E home/end,
\ ^K kill-to-end, ^C cancel line, ^D on an empty line = EOF, and a 16-line
\ history ring on up/down.

: DATAB ( -- ptr a )
   data-base ;

create TIOB0 80 allot           \ original (canonical) termios, saved at INSTALL
create TIOB 80 allot            \ working termios
create KB 8 allot               \ 1-byte key buffer
create LBUF 256 allot           \ line under edit
create HIST 4096 allot          \ history ring: 16 slots x 256 ([len][bytes])
variable LLEN  variable LPOS    \ line length, cursor
variable HN  variable HV        \ history count, browse index
PTR-VARIABLE HS                 \ history slot scratch: it holds a ring address
variable DONE                   \ 0 editing, 1 accepted, 2 eof

-1 constant KEY-EOF             \ KEY1's no-key answer; no byte collides with it

defer REPL-READ ( -- ptr u8 n )

: HS@ ( -- ptr u8 )
   HS @ ;

: HS! ( ptr u8 -- )
   HS ! ;

: TIO32@ ( ptr u8 -- n ) {: a:ptr :}
   a c@  a 1 + c@ 8 lshift or
   a 2 + c@ 16 lshift or
   a 3 + c@ 24 lshift or ;

: TIO32! ( n ptr u8 -- ) {: x a:ptr :}
   x $FF and a c!
   x 8 rshift $FF and a 1 + c!
   x 16 rshift $FF and a 2 + c!
   x 24 rshift $FF and a 3 + c! ;

: TIO-LFLAG-U8 ( -- ptr u8 )
   TIOB HBR-LFLAG-OFF + ;

: TIO-LFLAG-CELL ( -- ptr n )
   TIOB HBR-LFLAG-OFF + ;

: TIO-LFLAG@ ( -- n )
   HBR-LFLAG-32? IF TIO-LFLAG-U8 TIO32@ ELSE TIO-LFLAG-CELL @ THEN ;

: TIO-LFLAG! ( n -- ) {: x :}
   HBR-LFLAG-32? IF x TIO-LFLAG-U8 TIO32! exit THEN
   x TIO-LFLAG-CELL ! ;

: TTY? ( -- bool )  0 HBR-TIO-GET TIOB ioctl 0 = ;

: RAW-ON ( -- )
   0 HBR-TIO-GET TIOB ioctl drop
   TIO-LFLAG@  HBR-RAWMASK invert and  TIO-LFLAG!
   1 TIOB HBR-VMIN-OFF + c!  0 TIOB HBR-VTIME-OFF + c!
   0 HBR-TIO-SET TIOB ioctl drop ;

: RAW-OFF ( -- )  0 HBR-TIO-SET TIOB0 ioctl drop ;

\ The editor's echo goes out through `emit` and `type` rather than a write to
\ descriptor 1, so it follows the task's current output device (docs/genio.md):
\ a line edited over a connection is echoed back over that connection. KEY1 and
\ RD-LINE below stay on descriptor 0 because together they ARE the terminal
\ device's key and accept; another device supplies its own pair, and lib/genio.f
\ picks between them at REPL-READ.
\ A one-byte read answers 1, or 0 once the terminal has hung up — a pty slave
\ whose master closed reads 0 — and -1 when the read failed. Neither left a byte
\ in KB, so both are end of input rather than the key before them. EINTR is not
\ among them: the one asynchronous handler this engine installs (the profiler's
\ SIGALRM) sets SA_RESTART and the crash handlers exit, and the syscall wrappers
\ in src/habu/habu1.f collapse a failed read to -1 without errno, so a handler
\ that returns without SA_RESTART would end the line here
\ (habu-keep-key1-s-cfc8c6ad).
: KEY1 ( -- n )
   0 KB 1 read 1 < IF KEY-EOF exit THEN
   KB c@ ;

\ full-line redraw: CR, clear-to-eol, prompt, line, cursor back to LPOS
: REDRAW ( -- )
   13 emit  27 emit  91 emit  75 emit
   s" habu> " type
   LBUF LLEN @ type
   LLEN @ LPOS @ - 0 ?do 8 emit loop ;

: CLEARLN ( -- )  0 LLEN !  0 LPOS ! ;

: INSCH ( n -- ) {: c :}
   LLEN @ 255 < IF
      LLEN @ begin dup LPOS @ > while
         dup 1 - LBUF + c@  over LBUF + c!  1 - repeat drop
      c LPOS @ LBUF + c!
      LLEN @ 1 + LLEN !  LPOS @ 1 + LPOS ! THEN ;

: DELCH ( -- )
   LPOS @ 0 > IF
      LPOS @ begin dup LLEN @ < while
         dup LBUF + c@  over 1 - LBUF + c!  1 + repeat drop
      LLEN @ 1 - LLEN !  LPOS @ 1 - LPOS ! THEN ;

\ ---- history ring ----
: HSLOT ( n -- ptr u8 )  15 and 256 * HIST + ;

: HSAVE ( -- )
   LLEN @ 0 > IF
      HN @ HSLOT HS!
      LLEN @ HS@ c!                      \ len byte (INSCH caps LLEN at 255)
      LLEN @ 0 ?do LBUF i + c@  HS@ 1 + i + c! loop
      HN @ 1 + HN ! THEN ;

: HLOAD ( n -- )
   HSLOT HS!
   HS@ c@ LLEN !
   LLEN @ 0 ?do HS@ 1 + i + c@  LBUF i + c! loop
   LLEN @ LPOS ! ;

: HLO ( -- n )  HN @ 16 -  dup 0 < IF drop 0 THEN ;

: HUP ( -- )
   HLO HV @ < IF  HV @ 1 - HV !  HV @ HLOAD  REDRAW THEN ;

: HDOWN ( -- )
   HV @ HN @ < IF
      HV @ 1 + HV !
      HV @ HN @ = IF CLEARLN ELSE HV @ HLOAD THEN
      REDRAW THEN ;

\ ---- key dispatch ----
: ESCKEY ( n -- ) {: k :}
   k 68 = IF LPOS @ 0 > IF LPOS @ 1 - LPOS ! REDRAW THEN exit THEN
   k 67 = IF LPOS @ LLEN @ < IF LPOS @ 1 + LPOS ! REDRAW THEN exit THEN
   k 65 = IF HUP exit THEN
   k 66 = IF HDOWN THEN ;

: DOKEY ( n -- ) {: c :}
   c KEY-EOF = IF 2 DONE ! exit THEN
   c 13 =  c 10 = or IF 13 emit 10 emit  1 DONE !  exit THEN
   c 4 = IF LLEN @ 0 = IF 2 DONE ! THEN exit THEN
   c 3 = IF CLEARLN REDRAW exit THEN
   c 1 = IF 0 LPOS ! REDRAW exit THEN
   c 5 = IF LLEN @ LPOS ! REDRAW exit THEN
   c 11 = IF LPOS @ LLEN ! REDRAW exit THEN
   c 127 =  c 8 = or IF DELCH REDRAW exit THEN
   c 27 = IF KEY1 91 = IF KEY1 ESCKEY THEN exit THEN
   c 31 >  c 127 < and IF c INSCH REDRAW THEN ;

: RD-LINE ( -- ptr u8 n )
   TTY? 0= IF NULL$ exit THEN
   0 HBR-TIO-GET TIOB0 ioctl drop
   RAW-ON  CLEARLN  HN @ HV !  0 DONE !  REDRAW
   begin KEY1 DOKEY DONE @ 0 = 0= until
   RAW-OFF
   DONE @ 2 = IF NULL$ ELSE HSAVE  LBUF LLEN @ THEN ;

: REPLH-PTR ( -- ptr a )
   DATAB REPLH-CELL + ;

: REPLH! ( [ -- ptr u8 n ] -- )
   REPLH-PTR xt! ;

: REPL-ENABLE ( -- )
   [: REPL-READ ;] REPLH! ;

: INSTALL ( -- )
   [: RD-LINE ;] is REPL-READ
   REPL-ENABLE ;
INSTALL
