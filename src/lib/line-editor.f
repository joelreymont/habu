split-str
\ line-editor.f - the terminal line editor: READ-LINE answers one edited line,
\ with the tty raw per line and canonical while the line executes, and a
\ history ring. The session owns prompts, pending definitions and recovery;
\ this file owns editing. A line longer than LLINE-MAX is rejected by name and
\ read again, never truncated.
\ STORAGE CLASS. PROCESS-WIDE: the termios copies, the line and the history
\ ring (one terminal per process).

package LINE-EDITOR
private

create TIOB0 80 SCRATCH-ALLOT   \ original (canonical) termios, saved at READ-LINE
create TIOB 80 SCRATCH-ALLOT    \ working termios
create KB 8 SCRATCH-ALLOT       \ 1-byte key buffer
create LBUF 256 SCRATCH-ALLOT   \ line under edit
create HIST 4096 SCRATCH-ALLOT  \ history ring: 16 slots x 256 ([len][bytes])
SCRATCH-VARIABLE LLEN  SCRATCH-VARIABLE LPOS \ line length, cursor
SCRATCH-VARIABLE LOVER          \ bytes this line asked for past LLINE-MAX
create LDEC 24 SCRATCH-ALLOT    \ the rejection's decimal scratch, filled from its end
SCRATCH-VARIABLE LDEC-I
create LDEC-LF 10 c,            \ the rejection's newline
SCRATCH-VARIABLE HN  SCRATCH-VARIABLE HV \ history count, browse index
PTR-VARIABLE HS                 \ history slot scratch: it holds a ring address
SCRATCH-VARIABLE DONE           \ 0 editing, 1 accepted, 2 eof

-1 constant KEY-EOF             \ KEY's no-key answer; no byte collides with it

\ LBUF holds 256 bytes and a history slot spends its first on the length.
255 constant LLINE-MAX

\ Linux's termios is one struct on every architecture, so one Linux value
\ serves aarch64 and x86-64.
: TIO-GET ( -- n ) HB-TARGET-MACOS? if $40487413 else $5401 then ;
: TIO-SET ( -- n ) HB-TARGET-MACOS? if $80487414 else $5402 then ;
: RAW-MASK ( -- n ) HB-TARGET-MACOS? if $188 else $B then ;
: LFLAG-OFF ( -- n ) HB-TARGET-MACOS? if 24 else 12 then ;
: VMIN-OFF ( -- n ) HB-TARGET-MACOS? if 48 else 23 then ;
: VTIME-OFF ( -- n ) HB-TARGET-MACOS? if 49 else 22 then ;
: LFLAG-32? ( -- bool ) HB-TARGET-MACOS? if false else true then ;

: HS@ ( -- ^u8 )
   HS @ ;

: HS! ( ^u8 -- )
   HS ! ;

: TIO32@ ( ^u8 -- n ) {: a:^ :}
   a c@  a 1 + c@ 8 lshift or
   a 2 + c@ 16 lshift or
   a 3 + c@ 24 lshift or ;

: TIO32! ( n ^u8 -- ) {: x a:^ :}
   x $FF and a c!
   x 8 rshift $FF and a 1 + c!
   x 16 rshift $FF and a 2 + c!
   x 24 rshift $FF and a 3 + c! ;

: TIO-LFLAG-U8 ( -- ^u8 )
   TIOB LFLAG-OFF + ;

: TIO-LFLAG-CELL ( -- ^n )
   TIOB LFLAG-OFF + ;

: TIO-LFLAG@ ( -- n )
   LFLAG-32? IF TIO-LFLAG-U8 TIO32@ ELSE TIO-LFLAG-CELL @ THEN ;

: TIO-LFLAG! ( n -- ) {: x :}
   LFLAG-32? IF x TIO-LFLAG-U8 TIO32! exit THEN
   x TIO-LFLAG-CELL ! ;

: RAW-ON ( -- )
   0 TIO-GET TIOB ioctl drop
   TIO-LFLAG@  RAW-MASK invert and  TIO-LFLAG!
   1 TIOB VMIN-OFF + c!  0 TIOB VTIME-OFF + c!
   0 TIO-SET TIOB ioctl drop ;

: RAW-OFF ( -- )  0 TIO-SET TIOB0 ioctl drop ;

public

: TTY? ( -- bool )  0 TIO-GET TIOB ioctl 0 = ;

\ Reads descriptor 0 whatever the task's output device. A read answering 0
\ (hangup) or -1 (any failure, EINTR included) leaves no byte in KB: end of input.
: KEY ( -- n )
   0 KB 1 read 1 < IF KEY-EOF exit THEN
   KB c@ ;

private

: REDRAW ( -- )
   13 emit  27 emit  91 emit  75 emit
   s" habu> " type
   LBUF LLEN @ type
   LLEN @ LPOS @ - 0 ?do 8 emit loop ;

: CLEARLN ( -- )  0 LLEN !  0 LPOS !  0 LOVER ! ;

\ A key past the ceiling is counted, not dropped or inserted, so READ-LINE refuses
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

: HSLOT ( n -- ^u8 )  15 and 256 * HIST + ;

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
   c 27 = IF KEY 91 = IF KEY ESCKEY THEN exit THEN
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

: LDEC$ ( -- str )
   LDEC LDEC-I @ +  24 LDEC-I @ - ;

\ One line on descriptor 2: what filled up, the ceiling and the length the line
\ asked for. Diagnostics do not leave through the editor's echo (src/lib/genio-core.f),
\ and no REPL-baked word may `die` or `bye`, so the session states the refusal
\ and reads the next line.
: LINE-FULL ( -- )
   2 s" hb: repl line over " write drop
   LLINE-MAX LDEC!  2 LDEC$ write drop
   2 s"  bytes: " write drop
   LLEN @ LOVER @ + LDEC!  2 LDEC$ write drop
   2 s"  typed" write drop
   2 LDEC-LF 1 write drop ;

public

\ A refused line stays out of the history ring; the outer session sees only
\ whole lines.
: READ-LINE ( -- str )
   TTY? 0= IF NULL$ exit THEN
   0 TIO-GET TIOB0 ioctl drop
   begin
      RAW-ON  CLEARLN  HN @ HV !  0 DONE !  REDRAW
      begin KEY DOKEY DONE @ 0 = 0= until
      RAW-OFF
      DONE @ 1 =  LOVER @ 0 > and
   while LINE-FULL repeat
   DONE @ 2 = IF NULL$ ELSE HSAVE  LBUF LLEN @ THEN ;

;package
