\ tui.fs — full-screen TUI REPL for caf, in Forth (patterns from ~/Work/pz: raw
\ termios via stty, ANSI render, as-you-type feedback). Live: as you edit a
\ `: NAME ( eff ) body ;` line, the status row shows the inferred effect (✓) or
\ the caf diagnostic (✗) — WITHOUT defining (CHECK-DRY). Enter commits the def.
\ The parser + live-status core is plain Forth and unit-tested; the raw key loop
\ is thin glue. Launch:  gforth caf-tui.fs -e RUN-TUI

require caf.fs
CHECKING-ON? off          \ TUI infrastructure is not itself checked caf

\ --- ANSI ---
: ESC ( -- )  27 emit ;
: CSI ( -- )  ESC [char] [ emit ;
: +G CSI ." 32m" ;  : +R CSI ." 31m" ;  : +C CSI ." 36m" ;  : +D CSI ." 2m" ;  : -X CSI ." 0m" ;
: AT {: r c -- :}  CSI r 0 .r [char] ; emit c 0 .r [char] H emit ;   \ cursor to row;col
: CLR ( -- )       CSI ." 2J" ;
: CLR-EOL ( -- )   CSI ." K" ;
: ALT-ON ( -- )    CSI ." ?1049h" ;   : ALT-OFF ( -- )  CSI ." ?1049l" ;
: CUR-HIDE CSI ." ?25l" ;  : CUR-SHOW CSI ." ?25h" ;

\ --- def-line parser:  ": NAME ( eff ) body ;"  ->  na nu sa su ba bu  true ----
variable >P   variable >E
: P-INIT ( a u -- )  over + >E !  >P ! ;
: P-MORE ( -- f )  >P @ >E @ < ;
: P-SKIP ( -- )  begin P-MORE >P @ c@ bl = and while 1 >P +! repeat ;
: P-TOK ( -- a u )  P-SKIP  >P @  begin P-MORE >P @ c@ bl <> and while 1 >P +! repeat  >P @ over - ;
: LASTCH ( a u -- c )  + 1- c@ ;               \ char at a[u-1] (assumes u>0)
: -TRAIL ( a u -- a u' )                       \ trim trailing blanks
   begin dup 0> if 2dup LASTCH bl = else false then while 1- repeat ;
: -SEMI ( a u -- a u' )                       \ drop a trailing ';' + spaces
   -TRAIL dup 0> if 2dup + 1- c@ [char] ; = if 1- then then  -TRAIL ;

: PARSE-DEF ( a u -- na nu sa su ba bu true | false )
   P-INIT
   P-TOK s" :" CI= 0= if false exit then
   P-TOK 2dup nip 0= if 2drop false exit then  {: na nu :}
   P-TOK s" (" CI= 0= if false exit then
   P-SKIP >P @  begin P-MORE >P @ c@ [char] ) <> and while 1 >P +! repeat
   >P @ over -  P-MORE 0= if 2drop false exit then  1 >P +!   {: sa su :}
   P-SKIP >P @  >E @ over -                                   {: ra ru :}  \ raw body
   ra ru s" ;" search nip nip 0= if false exit then          \ require a closing ';'
   ra ru -SEMI                                                {: ba bu :}  \ strip the ';'
   na nu sa su ba bu true ;

\ --- live status for the current buffer (prints into the status row) ----------
: LIVE-STATUS ( a u -- )
   PARSE-DEF 0= if +D ." …" -X exit then
   {: na nu sa su ba bu :}
   na nu sa su ba bu ['] CHECK-DRY catch ?dup if         \ check, do NOT chart
      DIAG-CODE!  +R ." ✗ " FORMAT-DIAG type -X
   else
      +G ." ✓ " na nu type -X  +D ."   ( " sa su -TRAIL type ."  )" -X
   then ;

\ --- raw-mode interactive loop (thin glue over the tested core above) ---------
\ Single-line live editor: `caf> <buffer>    <live status>`, cursor inside the
\ buffer. Horizontal positioning only — robust across terminals. Enter commits
\ (EVALUATE → real definition); the line scrolls up as history.
512 constant LMAX
create LBUF LMAX allot   variable LLEN   variable LCUR
5 constant PROMPT-W                              \ width of "caf> "
: L-RESET ( -- )  0 LLEN !  0 LCUR ! ;
: CSI-G ( col -- )  CSI 0 .r [char] G emit ;     \ absolute column (1-based)

: L-INS ( c -- )                                 \ insert at cursor
   LLEN @ LMAX >= if drop exit then
   LBUF LCUR @ +  dup 1+  LLEN @ LCUR @ -  move   \ shift right
   LBUF LCUR @ + c!  1 LLEN +!  1 LCUR +! ;
: L-BS ( -- )                                    \ delete before cursor
   LCUR @ 0= if exit then
   LBUF LCUR @ +  dup 1-  swap  LLEN @ LCUR @ -  move
   -1 LLEN +!  -1 LCUR +! ;
: L-LEFT  ( -- )  LCUR @ 0>        if -1 LCUR +! then ;
: L-RIGHT ( -- )  LCUR @ LLEN @ <  if  1 LCUR +! then ;

: RENDER ( -- )
   CR CLR-EOL  +C ." caf> " -X  LBUF LLEN @ type
   ."     "  LBUF LLEN @ LIVE-STATUS              \ live status trails the buffer
   CR  PROMPT-W LCUR @ + 1+ CSI-G ;               \ cursor back into the buffer

: COMMIT ( -- )                                  \ Enter: define for real, scroll up
   cr  LBUF LLEN @ ['] evaluate catch ?dup if +R ."  ✗ " . -X then  cr
   L-RESET ;

: TTY? ( -- f )  s" test -t 0 2>/dev/null" system $? 0= ;

: KEY-ESC ( -- )                                 \ ESC: arrows ESC [ A/B/C/D
   key 91 <> if exit then  key
   dup 67 = if drop L-RIGHT exit then            \ →
   dup 68 = if drop L-LEFT  exit then            \ ←
   drop ;                                         \ ↑/↓ (history) — ignored for now

: RUN-TUI ( -- )
   TTY? 0= if cr ." caf-tui needs a terminal (try: gforth caf-tui.fs -e RUN-TUI)" cr exit then
   s" stty raw -echo" system  L-RESET
   cr +C ." caf TUI" -X ."  — edit a checked def; effect shows live. Ctrl-D quits." cr
   begin
      RENDER  key
      dup 4 =  over 3 = or if drop true                       \ Ctrl-D / Ctrl-C
      else
         dup 13 = over 10 = or if drop COMMIT false
         else dup 127 = over 8 = or if drop L-BS false
         else dup 27 = if drop KEY-ESC false
         else dup 32 >= over 126 <= and if L-INS false
         else drop false then then then then
      then
   until
   s" stty sane" system  cr +D ." bye" -X cr ;

CHECKING-ON? on            \ committed defs (COMMIT's EVALUATE) are checked caf
