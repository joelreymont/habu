\ repl.f — the interactive line REPL for the stdin engine (bin/hbi). Baked as
\ the engine's LSRC; EMIT-SOURCE runs it only when fd 0 is a tty (a pipe gets
\ the classic batch read-all and never sees these words run). INSTALL points
\ REPLH-CELL at RD-LINE; the engine's LEXIT path then prints " ok", saves the
\ line-start compile state (CP/NDICT/DP), and BLRs RD-LINE for the next line.
\ An undefined word prints "?", rolls the line back, and re-reads instead of
\ exit(70). v1 is canonical-mode (the tty line discipline edits the line);
\ raw-mode editing + history are dot caf-bc8b1f72's open half.

$340000000 constant DATAB       \ the DATA region's fixed VA
$3640 constant REPLH-CELL       \ engine: REPL line-reader xt
$40487413 constant TIOCGETA

create TIOB 80 allot            \ termios scratch (darwin: 72 B)
create LBUF 1024 allot          \ line buffer

: TTY? ( -- n )  0 TIOCGETA TIOB ioctl 0 = ;

: EMITS {: a u :}  1 a u write drop ;

: PROMPT ( -- )  s" habu> " EMITS ;

: RD-LINE ( -- n n )
   PROMPT  0 LBUF 1024 read
   dup 0 > IF LBUF swap ELSE drop 0 0 THEN ;

: INSTALL ( -- )  TTY? IF ['] RD-LINE DATAB REPLH-CELL + ! THEN ;
INSTALL
