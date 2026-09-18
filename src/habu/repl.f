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
\ history ring on up/down. A line longer than LLINE-MAX is refused by name and
\ read again instead of being truncated.

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

\ The line's own ceiling. LBUF holds 256 bytes and a history slot spends its
\ first byte on the length ([len][bytes] in 256), so 255 is what one line can be
\ and what one line can be recalled as. A line that asks for more is REFUSED by
\ name at the prompt (LINE-FULL below) rather than truncated: the editor used to
\ drop every key past 255 silently, so a pasted 257-byte definition lost its `;`
\ and left the session compiling a word that was never defined
\ (dot habu-refuse-a-repl-a58c0eba).
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

: CLEARLN ( -- )  0 LLEN !  0 LPOS !  0 LOVER ! ;

\ A key past the ceiling is COUNTED, not dropped and not inserted: the buffer
\ stays within its 255 and RD-LINE has the length the line really asked for when
\ it refuses it. Counting instead of refusing here is deliberate — a refusal
\ mid-line would leave the untyped tail of a pasted line to be read as the NEXT
\ line and executed.
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

\ ---- history ring ----
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

\ ---- the over-long line's refusal ----
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
\ asked for. Diagnostics do not leave through the editor's echo (docs/genio.md),
\ and no REPL-baked word may `die` or `bye` (tools/repl-lint-core.f), so the
\ session states the refusal and reads the next line.
: LINE-FULL ( -- )
   2 s" hb: repl line over " write drop
   LLINE-MAX LDEC!  2 LDEC$ write drop
   2 s"  bytes: " write drop
   LLEN @ LOVER @ + LDEC!  2 LDEC$ write drop
   2 s"  typed" write drop
   2 LDEC-LF 1 write drop ;

\ A line that asked for more than LLINE-MAX is NOT accepted: it is refused by
\ name, left out of the history ring and read again from an empty buffer. The
\ engine's LEXIT path only ever sees whole lines.
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

\ The cell holds the line reader itself, so the accessor says so: a bare
\ `( -- ptr a )` over a DATA offset let every caller pick the element type.
\ REPLH-CELL is an image-ABI offset (layout.f names it and habu2.f reads it), so
\ the cell cannot become a TYPED-VARIABLE, and since the executable-value fence
\ (dot habu-refuse-an-executable-e8834546) no CHECKED word may give a DATA
\ address a quotation pointee -- a `data-base <off> +` accessor declared
\ `( -- ptr [ ... ] )` is the launder that fence refuses. The boundary is this
\ one address computation, and it is exactly the declaration `xt!` makes below;
\ it retires when the checker gains a quotation type kind
\ (habu-campaign-c2-mem-c3d7662b), after which the accessor is checked again.
TRUSTED: REPLH-PTR ( -- ptr [ -- ptr u8 n ] )
   data-base REPLH-CELL + ;

: REPLH! ( [ -- ptr u8 n ] -- )
   REPLH-PTR xt! ;

: REPL-ENABLE ( -- )
   [: REPL-READ ;] REPLH! ;

: INSTALL ( -- )
   [: RD-LINE ;] is REPL-READ
   REPL-ENABLE ;
INSTALL
