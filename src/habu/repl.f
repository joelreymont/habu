\ repl.f — the interactive REPL for the stdin engine (bin/hb). Baked as the
\ engine's LSRC; EMIT-SOURCE runs it only when fd 0 is a tty (a pipe gets the
\ classic batch read-all and never sees these words run). INSTALL points
\ REPLH-CELL at RD-LINE; the engine's LEXIT path then prints " ok", saves the
\ line-start compile state (CP/NDICT/DP/SP), and BLRs RD-LINE for the next
\ line. Undefined words and uncaught THROWs print "?", roll the line back, and
\ re-read instead of exiting.
\ The editor runs the tty RAW (per line; canonical is restored while the line
\ executes): insert/backspace at a cursor, left/right arrows, ^A/^E home/end,
\ ^K kill-to-end, ^C cancel line, ^D on an empty line = EOF, and a 16-line
\ history ring on up/down.

data-base constant DATAB        \ the DATA region's fixed VA
$3640 constant REPLH-CELL       \ engine: REPL line-reader xt
$40487413 constant TIOCGETA
$80487414 constant TIOCSETA
$188 constant RAWMASK           \ ICANON ($100) | ISIG ($80) | ECHO ($8)

create TIOB0 80 allot           \ original (canonical) termios, saved at INSTALL
create TIOB 80 allot            \ working termios
create KB 8 allot               \ 1-byte key buffer
create LBUF 256 allot           \ line under edit
create HIST 4096 allot          \ history ring: 16 slots x 256 ([len][bytes])
variable LLEN  variable LPOS    \ line length, cursor
variable HN  variable HV        \ history count, browse index
variable HS                     \ history slot scratch
variable DONE                   \ 0 editing, 1 accepted, 2 eof

: TTY? ( -- n )  0 TIOCGETA TIOB ioctl 0 = ;

: RAW-ON ( -- )
   0 TIOCGETA TIOB ioctl drop
   TIOB 24 + @  RAWMASK invert and  TIOB 24 + !
   1 TIOB 48 + c!  0 TIOB 49 + c!       \ cc[VMIN]=1 cc[VTIME]=0
   0 TIOCSETA TIOB ioctl drop ;

: RAW-OFF ( -- )  0 TIOCSETA TIOB0 ioctl drop ;

: EMITS {: a u :}  1 a u write drop ;

: EMIT1 ( c -- )  KB c!  1 KB 1 write drop ;

: KEY1 ( -- c )  0 KB 1 read drop  KB c@ ;

\ full-line redraw: CR, clear-to-eol, prompt, line, cursor back to LPOS
: REDRAW ( -- )
   13 EMIT1  27 EMIT1  91 EMIT1  75 EMIT1
   s" habu> " EMITS
   LBUF LLEN @ EMITS
   LLEN @ LPOS @ - 0 ?do 8 EMIT1 loop ;

: CLEARLN ( -- )  0 LLEN !  0 LPOS ! ;

: INSCH {: c :}
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
: HSLOT ( n -- a )  15 and 256 * HIST + ;

: HSAVE ( -- )
   LLEN @ 0 > IF
      HN @ HSLOT HS !
      LLEN @ HS @ c!                     \ len byte (INSCH caps LLEN at 255)
      LLEN @ 0 ?do LBUF i + c@  HS @ 1 + i + c! loop
      HN @ 1 + HN ! THEN ;

: HLOAD ( n -- )
   HSLOT HS !
   HS @ c@ LLEN !
   LLEN @ 0 ?do HS @ 1 + i + c@  LBUF i + c! loop
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
: ESCKEY {: k :}
   k 68 = IF LPOS @ 0 > IF LPOS @ 1 - LPOS ! REDRAW THEN exit THEN
   k 67 = IF LPOS @ LLEN @ < IF LPOS @ 1 + LPOS ! REDRAW THEN exit THEN
   k 65 = IF HUP exit THEN
   k 66 = IF HDOWN THEN ;

: DOKEY {: c :}
   c 13 =  c 10 = or IF 13 EMIT1 10 EMIT1  1 DONE !  exit THEN
   c 4 = IF LLEN @ 0 = IF 2 DONE ! THEN exit THEN
   c 3 = IF CLEARLN REDRAW exit THEN
   c 1 = IF 0 LPOS ! REDRAW exit THEN
   c 5 = IF LLEN @ LPOS ! REDRAW exit THEN
   c 11 = IF LPOS @ LLEN ! REDRAW exit THEN
   c 127 =  c 8 = or IF DELCH REDRAW exit THEN
   c 27 = IF KEY1 91 = IF KEY1 ESCKEY THEN exit THEN
   c 31 >  c 127 < and IF c INSCH REDRAW THEN ;

: RD-LINE ( -- n n )
   RAW-ON  CLEARLN  HN @ HV !  0 DONE !  REDRAW
   begin KEY1 DOKEY DONE @ 0 = 0= until
   RAW-OFF
   DONE @ 2 = IF 0 0 ELSE HSAVE  LBUF LLEN @ THEN ;

: INSTALL ( -- )
   TTY? IF
      0 TIOCGETA TIOB0 ioctl drop
      ['] RD-LINE DATAB REPLH-CELL + ! THEN ;
INSTALL
