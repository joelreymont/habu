\ lib.f — string/file library for the self-hosted linters (shadow/parity/clobber),
\ run by bin/hb. Replaces the regex/text plumbing the Python tools used. Tooling
\ tier: uncheckable metaprogramming, so checking is OFF for the whole library.
0 set-check

\ ---- whole-file read -------------------------------------------------------
\ open/read/close are engine prims: open ( path flags mode -- fd ), read ( fd
\ buf n -- n ), close ( fd -- ). O_RDONLY = 0. Path must be NUL-terminated.
create PATHBUF 1024 allot
: PATHZ  {: a u :}   \ PATHBUF := a/u + trailing NUL (open wants a C string)
   0 begin dup u < while  dup a + c@  over PATHBUF + c!  1+  repeat  drop
   0  u PATHBUF + c! ;
variable RFD  variable RGOT  variable RLEN
: READ-FILE  {: a u buf cap :}   \ ( -- fa fu )  slurp file a/u into buf (≤cap), return buf + length
   a u PATHZ
   PATHBUF 0 0 open  RFD !
   RFD @ 0 < IF s" lint: cannot open " type a u type cr  1 die THEN
   0 RLEN !
   begin  RFD @  buf RLEN @ +  cap RLEN @ -  read  dup RGOT !  0 >
   while  RLEN @ RGOT @ + RLEN !  repeat
   RFD @ close
   buf RLEN @ ;

\ ---- string ops ------------------------------------------------------------
: STR=  {: a u b v :}   \ ( -- f )  byte-wise string equality
   u v <> IF 0 exit THEN
   0 begin dup u < while  dup a + c@  over b + c@  <> IF drop 0 exit THEN  1+  repeat  drop  -1 ;
: PREFIX?  {: a u b v :}   \ ( -- f )  does a/u start with b/v ?
   u v < IF 0 exit THEN  a v b v STR= ;
\ FIND-SUB ( a u sa su -- idx | -1 ) : index of first occurrence of sa/su in a/u
: FIND-SUB  {: a u sa su :}   \ ( -- idx )
   su 0= IF 0 exit THEN
   0 begin dup u su - <= while
      dup a +  su  sa su STR= IF exit THEN  1+
   repeat  drop  -1 ;
: CONTAINS?  ( a u sa su -- f )  FIND-SUB 0< 0= ;

\ ---- tokenizer: whitespace tokens, stripping \ line comments (and ( .. ) stack
\ comments when PARENS? is set). Parallel arrays: TOFF/TLEN, plus TBOL = was the
\ token at column 0 (so a real `: NAME` def is a ':' token with TBOL set, not a
\ ':' inside a string or {: locals :}). ----
$6000 constant TMAX
create TOFF TMAX cells allot   create TLEN TMAX cells allot   create TBOL TMAX cells allot
variable TN#  variable PARENS?  variable TI  variable TS  variable BOL
: SP?  ( c -- f )  dup 32 = over 9 = or swap 10 = or ;
: TOKENIZE  {: a u :}   \ fill TOFF/TLEN/TBOL/TN# from a/u (PARENS? gates ( .. ) stripping)
   0 TN# !  0 TI !  -1 BOL !
   begin TI @ u < while
      TI @ a + c@ SP? IF
         TI @ a + c@ 10 = IF -1 BOL ! THEN  TI @ 1+ TI !
      ELSE TI @ a + c@ 92 = IF
         begin TI @ u < TI @ a + c@ 10 <> and while TI @ 1+ TI ! repeat
      ELSE PARENS? @  TI @ a + c@ 40 = and  TI @ 1+ u < and  TI @ 1+ a + c@ 32 = and IF
         begin TI @ u < TI @ a + c@ 41 <> and while TI @ 1+ TI ! repeat
         TI @ u < IF TI @ 1+ TI ! THEN
      ELSE
         TI @ TS !  BOL @ TBOL TN# @ cells + !  0 BOL !
         begin TI @ u < TI @ a + c@ SP? 0= and while TI @ 1+ TI ! repeat
         a TS @ + TOFF TN# @ cells + !  TI @ TS @ - TLEN TN# @ cells + !  TN# @ 1+ TN# !
      THEN THEN THEN
   repeat ;
\ stack-based (no locals) so a caller can hold its own locals frame without deep
\ nesting — habu locals do not nest more than ~2 deep reliably.
: TOK   ( k -- a u )   dup cells TOFF + @   swap cells TLEN + @ ;
: TOK0? ( k -- f )     cells TBOL + @ ;
: TOK=  ( k a u -- f )  >R >R TOK R> R> STR= ;

\ ---- more string ops for the linters ----
: BMOVE  {: a dst u :}  ( -- )  \ copy u bytes a -> dst
   0 begin dup u < while  dup a + c@  over dst + c!  1+  repeat  drop ;
: FOLD  ( c -- c )  dup 64 > over 91 < and IF 32 + THEN ;   \ ASCII A-Z -> a-z
: STR=CI  {: a u b v :}  ( -- f )   \ case-insensitive byte equality
   u v <> IF 0 exit THEN
   0 begin dup u < while  dup a + c@ FOLD  over b + c@ FOLD  <> IF drop 0 exit THEN  1+  repeat  drop  -1 ;

\ ---- token helpers for the def-walkers ----
: TEOL?  ( k -- f )   \ is token k the last on its source line? (next is BOL, or end)
   1+ dup TN# @ >= IF drop -1 ELSE TOK0? THEN ;
: FOLD-TO  {: a u dst :}  ( -- )  \ copy a/u to dst, ASCII-folded to lower-case
   0 begin dup u < while  dup a + c@ FOLD  over dst + c!  1+  repeat  drop ;
