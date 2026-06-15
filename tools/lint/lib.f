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

\ ---- bounded string helpers for source tools ------------------------------
13 constant CR
34 constant DQUOTE
46 constant DOT
47 constant SLASH
96 constant BEFORE-A
123 constant AFTER-Z

: ASCII-UP  ( c -- c )  dup BEFORE-A > over AFTER-Z < and IF 32 - THEN ;
: COPY-LOWER  {: a u dst :}  ( -- )  a u dst FOLD-TO ;
: COPY-UPPER  {: a u dst :}  ( -- )
   0 begin dup u < while  dup a + c@ ASCII-UP  over dst + c!  1+  repeat  drop ;
: WS?  ( c -- f )  dup SP? swap CR = or ;
: LTRIM  {: a u :}  ( -- a' u' )
   0 begin dup u < while
      dup a + c@ WS? 0= IF dup a +  u rot -  exit THEN  1+
   repeat  drop  a 0 ;
: RTRIM  {: a u :}  ( -- a u' )
   u TI !
   begin TI @ 0 > while
      a TI @ 1- + c@ WS? IF TI @ 1- TI ! ELSE a TI @ exit THEN
   repeat  a 0 ;
: TRIM  ( a u -- a' u' )  LTRIM RTRIM ;
: STARTS-WITH?  ( a u b v -- f )  PREFIX? ;
: SUFFIX?  {: a u b v :}  ( -- f )
   u v < IF 0 exit THEN  a u v - +  v  b v STR= ;
: ENDS-WITH?  ( a u b v -- f )  SUFFIX? ;
: INDEX-OF  {: a u c :}  ( -- idx|-1 )
   0 begin dup u < while  dup a + c@ c = IF exit THEN  1+  repeat  drop -1 ;
: COUNT-CHAR  {: a u c :}  ( -- n )
   0  0 begin dup u < while
      dup a + c@ c = IF swap 1+ swap THEN  1+
   repeat  drop ;

\ Shared split result arrays. Entries point into the original source string.
$400 constant SMAX
create SOFF SMAX cells allot   create SLEN SMAX cells allot   variable SN#
: SPLIT-CLEAR  ( -- )  0 SN# ! ;
: SPLIT+  {: a u :}  ( -- )
   SN# @ SMAX >= IF s" lint: split result overflow" type cr 1 die THEN
   a SOFF SN# @ cells + !  u SLEN SN# @ cells + !  SN# @ 1+ SN# ! ;
: S@  ( k -- a u )  dup cells SOFF + @  swap cells SLEN + @ ;
: ADD-LINE  {: a u :}  ( -- )
   u 0 >  a u 1- + c@ CR = and IF a u 1- SPLIT+ ELSE a u SPLIT+ THEN ;
: SPLIT-LINES  {: a u :}  ( -- )
   SPLIT-CLEAR  0 TS !  0 TI !
   begin TI @ u < while
      a TI @ + c@ 10 = IF
         a TS @ +  TI @ TS @ -  ADD-LINE
         TI @ 1+ TS !
      THEN
      TI @ 1+ TI !
   repeat
   TS @ u < IF a TS @ +  u TS @ -  ADD-LINE THEN ;
: SPLIT-WHITESPACE  {: a u :}  ( -- )
   SPLIT-CLEAR  0 TI !
   begin TI @ u < while
      begin TI @ u <  a TI @ + c@ WS? and while TI @ 1+ TI ! repeat
      TI @ u < IF
         TI @ TS !
         begin TI @ u <  a TI @ + c@ WS? 0= and while TI @ 1+ TI ! repeat
         a TS @ +  TI @ TS @ -  SPLIT+
      THEN
   repeat ;
: BUF-APPEND  {: a u dst cap lp :}  ( -- )
   lp @ u + cap > IF s" lint: string buffer overflow" type cr 1 die THEN
   a dst lp @ + u BMOVE  lp @ u + lp ! ;
: BUF-APPEND-C  {: c dst cap lp :}  ( -- )
   lp @ 1+ cap > IF s" lint: string buffer overflow" type cr 1 die THEN
   c dst lp @ + c!  lp @ 1+ lp ! ;
: JOIN-SPLIT  {: sep su dst cap lp :}  ( -- )
   0 lp !
   0 begin dup SN# @ < while
      dup 0 > IF sep su dst cap lp BUF-APPEND THEN
      dup S@ dst cap lp BUF-APPEND  1+
   repeat  drop ;
: HAS-EXT?  ( a u ea eu -- f )  SUFFIX? ;
: PATHISH?  {: a u :}  ( -- f )
   a u SLASH INDEX-OF 0 >= IF -1 exit THEN
   a u s" .md" HAS-EXT? IF -1 exit THEN
   a u s" .sh" HAS-EXT? IF -1 exit THEN
   a u s" .py" HAS-EXT? IF -1 exit THEN
   a u s" .f" HAS-EXT? IF -1 exit THEN
   a u s" .fs" HAS-EXT? IF -1 exit THEN
   a u s" .tsv" HAS-EXT? ;

\ ---- named scanners replacing the current Python regex use-cases ----------
variable PSA  variable PSU  variable PX  variable PSTART
variable P1A  variable P1U  variable P2A  variable P2U
: PAT-RESET  {: a u :}  ( -- )  a PSA !  u PSU !  0 PX !  0 P1U !  0 P2U ! ;
: PAT-END?  ( -- f )  PX @ PSU @ >= ;
: PAT-C@  ( -- c )  PSA @ PX @ + c@ ;
: PAT-WS?  ( -- f )  PAT-END? 0= IF PAT-C@ WS? ELSE 0 THEN ;
: PAT-SKIP-WS  ( -- )  begin PAT-WS? while PX @ 1+ PX ! repeat ;
: PAT-WORD-END?  ( -- f )  PAT-END? IF -1 ELSE PAT-C@ WS? THEN ;
: PAT-SQ?  ( -- f )
   PX @ 1+ PSU @ >= IF 0 exit THEN
   PSA @ PX @ + c@ FOLD 115 =  PSA @ PX @ 1+ + c@ DQUOTE = and ;
: PAT-CAP-1  ( -- f )
   PAT-SQ? 0= IF 0 exit THEN
   PX @ 2 + PX !  PAT-WS? 0= IF 0 exit THEN
   PAT-SKIP-WS  PSA @ PX @ + P1A !
   begin PAT-END? 0= PAT-C@ DQUOTE <> and while PX @ 1+ PX ! repeat
   PAT-END? IF 0 exit THEN
   PSA @ PX @ + P1A @ - P1U !  PX @ 1+ PX !  -1 ;
: PAT-CAP-2  ( -- f )
   PAT-SQ? 0= IF 0 exit THEN
   PX @ 2 + PX !  PAT-WS? 0= IF 0 exit THEN
   PAT-SKIP-WS  PSA @ PX @ + P2A !
   begin PAT-END? 0= PAT-C@ DQUOTE <> and while PX @ 1+ PX ! repeat
   PAT-END? IF 0 exit THEN
   PSA @ PX @ + P2A @ - P2U !  PX @ 1+ PX !  -1 ;
: PAT-MATCH-WORD  {: a u :}  ( -- f )
   PSU @ PX @ - u < IF 0 exit THEN
   PSA @ PX @ + u  a u STR=CI 0= IF 0 exit THEN
   PX @ u + PX !  PAT-WORD-END? ;
: TRUST-SITE?  {: a u :}  ( -- f )  \ s" name" s" effect" TRUST
   a u PAT-RESET
   begin PAT-END? 0= while
      PX @ PSTART !
      PAT-CAP-1 IF
         PAT-SKIP-WS
         PAT-CAP-2 IF
            PAT-SKIP-WS  s" TRUST" PAT-MATCH-WORD IF -1 exit THEN
         THEN
      THEN
      PSTART @ 1+ PX !
   repeat  0 ;
: SRC-PATH-REF?  {: a u :}  ( -- f )  \ token ending -SRC, then s" src/*.f"
   a u PAT-RESET
   begin PAT-END? 0= while
      begin PAT-END? 0= PAT-C@ WS? and while PX @ 1+ PX ! repeat
      PX @ PSTART !
      begin PAT-END? 0= PAT-C@ WS? 0= and while PX @ 1+ PX ! repeat
      PSA @ PSTART @ +  PX @ PSTART @ -  s" -SRC" SUFFIX? IF
         PAT-SKIP-WS
         PAT-CAP-1 IF
            P1A @ P1U @ s" src/" STARTS-WITH?  P1A @ P1U @ s" .f" HAS-EXT? and IF -1 exit THEN
         THEN
      THEN
   repeat  0 ;
: BACKTICK-PATH?  {: a u :}  ( -- f )
   a u PAT-RESET
   begin PAT-END? 0= while
      PAT-C@ 96 = IF
         PX @ 1+ PX !  PSA @ PX @ + P1A !
         begin PAT-END? 0= PAT-C@ 96 <> and while PX @ 1+ PX ! repeat
         PAT-END? IF 0 exit THEN
         PSA @ PX @ + P1A @ - P1U !
         P1A @ P1U @ PATHISH? IF -1 exit THEN
      THEN
      PX @ 1+ PX !
   repeat  0 ;

\ Signature comment classification used by strict source tools.
0 constant SIG-MISSING
1 constant SIG-TYPED
2 constant SIG-OPTOUT
: SIG-OPTOUT?  {: a u :}  ( -- f )
   a u SPLIT-WHITESPACE
   SN# @ 1 = IF
      0 S@ s" infer" STR=CI IF -1 exit THEN
      0 S@ s" private" STR=CI IF -1 exit THEN
   THEN
   SN# @ 2 = IF
      0 S@ s" infer" STR=CI  1 S@ s" private" STR=CI and IF -1 exit THEN
      0 S@ s" private" STR=CI  1 S@ s" infer" STR=CI and IF -1 exit THEN
   THEN  0 ;
: SIG-KIND  {: a u :}  ( -- kind )
   a u s" --" CONTAINS? IF SIG-TYPED exit THEN
   a u SIG-OPTOUT? IF SIG-OPTOUT exit THEN
   SIG-MISSING ;
