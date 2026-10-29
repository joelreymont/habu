\ repl.f - the terminal line reader for OUTER:REPL (src/habu/interpret.f).
\ INSTALL makes RD-LINE the default REPL-READ defer and puts its xt in REPLH-CELL;
\ genio may replace the defer, so OUTER:REPL calls it again before each line. The
\ session owns prompts, pending definitions and recovery; this file owns editing,
\ with the tty raw per line and canonical while the line executes. A line longer
\ than LLINE-MAX is refused by name and read again, never truncated.

create TIOB0 80 allot           \ original (canonical) termios, saved at INSTALL
create TIOB 80 allot            \ working termios
create KB 8 allot               \ 1-byte key buffer
create LBUF 256 allot           \ line under edit
create HIST 4096 allot          \ history ring: 16 slots x 256 ([len][bytes])
variable LLEN  variable LPOS    \ line length, cursor
variable LOVER                  \ bytes this line asked for past LLINE-MAX
create LDEC 24 allot            \ the refusal's decimal scratch, filled from its end
variable LDEC-I
create LDEC-LF 10 c,            \ the refusal's newline: an escaped literal whose
                                \ payload opens with a backslash breaks the shared
                                \ lint tokenizer (tools/lint/token.f)
variable HN  variable HV        \ history count, browse index
PTR-VARIABLE HS                 \ history slot scratch: it holds a ring address
variable DONE                   \ 0 editing, 1 accepted, 2 eof

-1 constant KEY-EOF             \ KEY1's no-key answer; no byte collides with it

\ LBUF holds 256 bytes and a history slot spends its first byte on the length,
\ so 255 is what one line can be and be recalled as. A longer line is refused by
\ name (LINE-FULL): dropping keys silently lost a pasted definition's `;`.
255 constant LLINE-MAX

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

\ The echo goes through `emit` and `type`, so it follows the task's output device
\ (src/lib/genio-core.f). KEY1 and RD-LINE stay on descriptor 0 as the terminal
\ device's key and accept; src/lib/genio.f picks a device's pair at REPL-READ.
\ A one-byte read answers 1, 0 once the terminal hung up (a pty whose master
\ closed), -1 on failure; 0 and -1 leave no byte in KB and are end of input.
\ EINTR cannot arrive: the one asynchronous handler (the profiler's SIGALRM) sets
\ SA_RESTART, the crash handlers exit, and habu1.f's syscall wrappers collapse a
\ failed read to -1 without errno, so a handler without SA_RESTART would end the
\ line here.
: KEY1 ( -- n )
   0 KB 1 read 1 < IF KEY-EOF exit THEN
   KB c@ ;

: REDRAW ( -- )
   13 emit  27 emit  91 emit  75 emit
   s" habu> " type
   LBUF LLEN @ type
   LLEN @ LPOS @ - 0 ?do 8 emit loop ;

: CLEARLN ( -- )  0 LLEN !  0 LPOS !  0 LOVER ! ;

\ A key past the ceiling is counted, not dropped or inserted, so RD-LINE refuses
\ with the length the line asked for. A refusal mid-line would leave the pasted
\ tail to run as the next line.
: INSCH ( n -- ) {: c :}
   LLEN @ LLINE-MAX < 0= IF LOVER @ 1 + LOVER ! exit THEN
   LLEN @ begin dup LPOS @ > while
      dup 1 - LBUF + c@  over LBUF + c!  1 - repeat drop
   c LPOS @ LBUF + c!
   LLEN @ 1 + LLEN !  LPOS @ 1 + LPOS ! ;

: DELCH ( -- )
   LPOS @ 0 > IF
      LPOS @ begin dup LLEN @ < while
         dup LBUF + c@  over 1 - LBUF + c!  1 + repeat drop
      LLEN @ 1 - LLEN !  LPOS @ 1 - LPOS ! THEN ;

: HSLOT ( n -- ptr u8 )  15 and 256 * HIST + ;

: HSAVE ( -- )
   LLEN @ 0 > IF
      HN @ HSLOT HS!
      LLEN @ HS@ c!                      \ len byte (INSCH holds LLEN at LLINE-MAX)
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

\ The count is written from the end of LDEC so the digits come out in one write
\ and in order; 24 bytes hold any i64 (a paste can ask for a great many bytes).
: LDEC-C! ( n -- ) {: c :}
   LDEC-I @ 1 - LDEC-I !
   c LDEC LDEC-I @ + c! ;

: LDEC! ( n -- ) {: v :}
   24 LDEC-I !
   v 0 = IF 48 LDEC-C! exit THEN
   v begin dup 0 > while
      dup 10 mod 48 + LDEC-C!
      10 /
   repeat drop ;

: LDEC$ ( -- ptr u8 n )
   LDEC LDEC-I @ +  24 LDEC-I @ - ;

\ One line on descriptor 2: what filled up, the ceiling and the length the line
\ asked for. Diagnostics do not leave through the editor's echo (src/lib/genio-core.f),
\ and no REPL-baked word may `die` or `bye` (tools/repl-lint-core.f), so the
\ session states the refusal and reads the next line.
: LINE-FULL ( -- )
   2 s" hb: repl line over " write drop
   LLINE-MAX LDEC!  2 LDEC$ write drop
   2 s"  bytes: " write drop
   LLEN @ LOVER @ + LDEC!  2 LDEC$ write drop
   2 s"  typed" write drop
   2 LDEC-LF 1 write drop ;

\ A refused line stays out of the history ring; the outer session sees only
\ whole lines.
: RD-LINE ( -- ptr u8 n )
   TTY? 0= IF NULL$ exit THEN
   0 HBR-TIO-GET TIOB0 ioctl drop
   begin
      RAW-ON  CLEARLN  HN @ HV !  0 DONE !  REDRAW
      begin KEY1 DOKEY DONE @ 0 = 0= until
      RAW-OFF
      DONE @ 1 =  LOVER @ 0 > and
   while LINE-FULL repeat
   DONE @ 2 = IF NULL$ ELSE HSAVE  LBUF LLEN @ THEN ;

\ REPLH-CELL is an image-ABI offset (layout.f names it, habu2.f reads it), so it
\ cannot become a TYPED-VARIABLE; `xt!` declares what it holds (docs/forth.md
\ "An undeclared cell holds no execution token"). Nothing reads it back as code,
\ so no accessor hands its address out.
: REPL-ENABLE ( -- )
   [: REPL-READ ;] data-base REPLH-CELL + xt! ;

: INSTALL ( -- )
   [: RD-LINE ;] is REPL-READ
   REPL-ENABLE ;
INSTALL
