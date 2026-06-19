\ lib.f — string/file library for the self-hosted linters (shadow/parity/clobber),
\ run by bin/hb. Replaces old host-side text plumbing.
0 set-check

\ ---- whole-file read -------------------------------------------------------
\ open/read/close are engine prims: open ( path flags mode -- fd ), read ( fd
\ buf n -- n ), close ( fd -- ). O_RDONLY = 0. Path must be NUL-terminated.
create PATHBUF 1024 allot
variable RFD  variable RGOT  variable RLEN

: LINT-CHECK-HOOK ( -- )
   CHECK! ;
' LINT-CHECK-HOOK set-check

: LINT-TRUE ( -- bool )
   0 0= ;

: LINT-FALSE ( -- bool )
   0 1 = ;

: LINT-NOT ( bool -- bool )
   IF LINT-FALSE ELSE LINT-TRUE THEN ;

: PATHZ ( ptr u8 n -- ) {: a:ptr u :}
   u 1+ 1024 > IF s" lint: path too long" 1 die THEN
   0 begin dup u < while
      dup a + c@ over PATHBUF + c!
      1+
   repeat drop
   0 u PATHBUF + c! ;

: READ-FILE ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u buf:ptr cap :}
   a u PATHZ
   PATHBUF 0 0 open  RFD !
   RFD @ 0 < IF s" lint: cannot open file" 1 die THEN
   0 RLEN !
   begin  RFD @  buf RLEN @ +  cap RLEN @ -  read  dup RGOT !  0 >
   while  RLEN @ RGOT @ + RLEN !  repeat
   RFD @ close
   buf RLEN @ ;

\ ---- string ops ------------------------------------------------------------
: STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> IF LINT-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ over b + c@ <> IF drop LINT-FALSE exit THEN
      1+
   repeat drop LINT-TRUE ;
: PREFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < IF LINT-FALSE exit THEN
   a v b v STR= ;
\ FIND-SUB ( a u sa su -- idx | -1 ) : index of first occurrence of sa/su in a/u
: FIND-SUB ( ptr u8 n ptr u8 n -- n ) {: a:ptr u sa:ptr su :}
   su 0= IF 0 exit THEN
   0 begin dup u su - <= while
      dup a +  su  sa su STR= IF exit THEN  1+
   repeat  drop  -1 ;
: CONTAINS? ( ptr u8 n ptr u8 n -- bool )
   FIND-SUB 0< LINT-NOT ;

\ ---- tokenizer: whitespace tokens, stripping \ line comments (and ( .. ) stack
\ comments when PARENS? is set). Parallel arrays: TOFF/TLEN, plus TBOL = was the
\ token at column 0 (so a real `: NAME` def is a ':' token with TBOL set, not a
\ ':' inside a string or {: locals :}). ----
0 set-check
\ Explicit unchecked boundary: token arrays store source pointers in ordinary
\ cells and expose them through TOK/TOK0?/TOK= to legacy linters.
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

\ Back to checked helpers after the tokenizer's pointer-cell boundary.
' LINT-CHECK-HOOK set-check

\ ---- more string ops for the linters ----
: BMOVE ( ptr u8 ptr u8 n -- ) {: a:ptr dst:ptr u :}
   0 begin dup u < while  dup a + c@  over dst + c!  1+  repeat  drop ;
: FOLD ( n -- n )
   dup 64 > over 91 < and IF 32 + THEN ;
: STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> IF LINT-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ FOLD over b + c@ FOLD <> IF drop LINT-FALSE exit THEN
      1+
   repeat drop LINT-TRUE ;

\ ---- token helpers for the def-walkers ----
: TEOL? ( n -- bool )
   1+ dup TN# @ >= IF drop LINT-TRUE ELSE TOK0? THEN ;
: FOLD-TO ( ptr u8 n ptr u8 -- ) {: a:ptr u dst:ptr :}
   0 begin dup u < while  dup a + c@ FOLD  over dst + c!  1+  repeat  drop ;

\ ---- bounded string-keyed intern/set --------------------------------------
76 constant E-LINT-INTERN-CAP
$200 constant INTERN-MAX
$2000 constant INTERN-BUF-CAP
$100 constant INTERN-FOLD-CAP

create INTERN-BUF INTERN-BUF-CAP allot
create INTERN-OFF INTERN-MAX cells allot
create INTERN-LEN INTERN-MAX cells allot
create INTERN-FOLD-BUF INTERN-FOLD-CAP allot
variable INTERN-N
variable INTERN-END

: INTERN#  ( -- n )  INTERN-N @ ;
: INTERN-RESET  ( -- )  0 INTERN-N !  0 INTERN-END ! ;
: INTERN-INIT  ( -- )  INTERN-RESET ;
: INTERN$ ( n -- ptr u8 n ) {: id :}
   id 0 <  id INTERN-N @ >= or IF E-LINT-INTERN-CAP throw THEN
   id cells INTERN-OFF + @  id cells INTERN-LEN + @ ;
: INTERN-FIND ( ptr u8 n -- n ) {: a:ptr u :}
   0 begin dup INTERN-N @ < while
      dup cells INTERN-OFF + @  over cells INTERN-LEN + @  a u STR= IF exit THEN
      1+
   repeat  drop -1 ;
: INTERN ( ptr u8 n -- n ) {: a:ptr u :}
   a u INTERN-FIND dup 0 >= IF exit THEN drop
   INTERN-N @ INTERN-MAX >= IF E-LINT-INTERN-CAP throw THEN
   INTERN-END @ u + INTERN-BUF-CAP > IF E-LINT-INTERN-CAP throw THEN
   a INTERN-BUF INTERN-END @ + u BMOVE
   INTERN-BUF INTERN-END @ + INTERN-OFF INTERN-N @ cells + !
   u INTERN-LEN INTERN-N @ cells + !
   INTERN-END @ u + INTERN-END !
   INTERN-N @ dup 1+ INTERN-N ! ;
: INTERN? ( ptr u8 n -- bool )
   INTERN-FIND 0 >= ;
: INTERN-FOLD ( ptr u8 n -- n ) {: a:ptr u :}
   u INTERN-FOLD-CAP > IF E-LINT-INTERN-CAP throw THEN
   a u INTERN-FOLD-BUF FOLD-TO
   INTERN-FOLD-BUF u INTERN ;
: INTERN-FOLD? ( ptr u8 n -- bool ) {: a:ptr u :}
   u INTERN-FOLD-CAP > IF E-LINT-INTERN-CAP throw THEN
   a u INTERN-FOLD-BUF FOLD-TO
   INTERN-FOLD-BUF u INTERN? ;

\ ---- bounded string helpers for source tools ------------------------------
13 constant CR
34 constant DQUOTE
46 constant DOT
47 constant SLASH
96 constant BEFORE-A
123 constant AFTER-Z

: ASCII-UP ( n -- n )  dup BEFORE-A > over AFTER-Z < and IF 32 - THEN ;
: COPY-LOWER ( ptr u8 n ptr u8 -- ) {: a:ptr u dst:ptr :}  a u dst FOLD-TO ;
: COPY-UPPER ( ptr u8 n ptr u8 -- ) {: a:ptr u dst:ptr :}
   0 begin dup u < while  dup a + c@ ASCII-UP  over dst + c!  1+  repeat  drop ;
: WS? ( n -- bool )  dup SP? swap CR = or ;
: LTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ WS? 0= IF dup a +  u rot -  exit THEN  1+
   repeat  drop  a 0 ;
: RTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u TI !
   begin TI @ 0 > while
      a TI @ 1- + c@ WS? IF TI @ 1- TI ! ELSE a TI @ exit THEN
   repeat  a 0 ;
: TRIM ( ptr u8 n -- ptr u8 n )  LTRIM RTRIM ;
: STARTS-WITH? ( ptr u8 n ptr u8 n -- bool )  PREFIX? ;
: SUFFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < IF LINT-FALSE exit THEN
   a u v - + v b v STR= ;
: ENDS-WITH? ( ptr u8 n ptr u8 n -- bool )  SUFFIX? ;
: INDEX-OF ( ptr u8 n n -- n ) {: a:ptr u c :}
   0 begin dup u < while  dup a + c@ c = IF exit THEN  1+  repeat  drop -1 ;
: COUNT-CHAR ( ptr u8 n n -- n ) {: a:ptr u c :}
   0  0 begin dup u < while
      dup a + c@ c = IF swap 1+ swap THEN  1+
   repeat  drop ;

\ Shared split result arrays. Entries point into the original source string.
$400 constant SMAX
create SOFF SMAX cells allot   create SLEN SMAX cells allot   variable SN#
: SPLIT-CLEAR  ( -- )  0 SN# ! ;
: SPLIT+ ( ptr u8 n -- ) {: a:ptr u :}
   SN# @ SMAX >= IF s" lint: split result overflow" 1 die THEN
   a SOFF SN# @ cells + !  u SLEN SN# @ cells + !  SN# @ 1+ SN# ! ;
: S@ ( n -- ptr u8 n )  dup cells SOFF + @  swap cells SLEN + @ ;
: ADD-LINE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 >  a u 1- + c@ CR = and IF a u 1- SPLIT+ ELSE a u SPLIT+ THEN ;
: SPLIT-LINES ( ptr u8 n -- ) {: a:ptr u :}
   SPLIT-CLEAR  0 TS !  0 TI !
   begin TI @ u < while
      a TI @ + c@ 10 = IF
         a TS @ +  TI @ TS @ -  ADD-LINE
         TI @ 1+ TS !
      THEN
      TI @ 1+ TI !
   repeat
   TS @ u < IF a TS @ +  u TS @ -  ADD-LINE THEN ;
: SPLIT-WHITESPACE ( ptr u8 n -- ) {: a:ptr u :}
   SPLIT-CLEAR  0 TI !
   begin TI @ u < while
      begin TI @ u <  a TI @ + c@ WS? and while TI @ 1+ TI ! repeat
      TI @ u < IF
         TI @ TS !
         begin TI @ u <  a TI @ + c@ WS? 0= and while TI @ 1+ TI ! repeat
         a TS @ +  TI @ TS @ -  SPLIT+
      THEN
   repeat ;
: BUF-APPEND ( ptr u8 n ptr u8 n ptr n -- ) {: a:ptr u dst:ptr cap lp:ptr :}
   lp @ u + cap > IF s" lint: string buffer overflow" 1 die THEN
   a dst lp @ + u BMOVE  lp @ u + lp ! ;
: BUF-APPEND-C ( n ptr u8 n ptr n -- ) {: c dst:ptr cap lp:ptr :}
   lp @ 1+ cap > IF s" lint: string buffer overflow" 1 die THEN
   c dst lp @ + c!  lp @ 1+ lp ! ;
: JOIN-SPLIT ( ptr u8 n ptr u8 n ptr n -- ) {: sep:ptr su dst:ptr cap lp:ptr :}
   0 lp !
   0 begin dup SN# @ < while
      dup 0 > IF sep su dst cap lp BUF-APPEND THEN
      dup S@ dst cap lp BUF-APPEND  1+
   repeat  drop ;
: HAS-EXT? ( ptr u8 n ptr u8 n -- bool )  SUFFIX? ;
: PATHISH? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u SLASH INDEX-OF 0 >= IF LINT-TRUE exit THEN
   a u s" .md" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .sh" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .f" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .fs" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .tsv" HAS-EXT? ;

\ ---- named scanners replacing the former regex use-cases ------------------
0 set-check
\ Explicit unchecked boundary: PAT-* scanners keep current parse state and
\ captured source pointers in global cells shared with legacy lints.
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
: PAT-CAP-WORD-1  ( -- f )
   PAT-SKIP-WS
   PAT-END? IF 0 exit THEN
   PSA @ PX @ + P1A !
   begin PAT-END? 0= PAT-C@ WS? 0= and while PX @ 1+ PX ! repeat
   PSA @ PX @ + P1A @ - P1U !
   P1U @ 0 > ;
: PAT-CAP-PARENS-2  ( -- f )
   PAT-SKIP-WS
   PAT-END? IF 0 exit THEN
   PAT-C@ 40 <> IF 0 exit THEN
   PX @ 1+ PX !
   PAT-SKIP-WS  PSA @ PX @ + P2A !
   begin PAT-END? 0= PAT-C@ 41 <> and while PX @ 1+ PX ! repeat
   PAT-END? IF 0 exit THEN
   PSA @ PX @ + P2A @ - P2U !
   P2A @ P2U @ TRIM P2U ! P2A !
   PX @ 1+ PX !  -1 ;
: PAT-MATCH-WORD  {: a u :}  ( -- f )
   PSU @ PX @ - u < IF 0 exit THEN
   PSA @ PX @ + u  a u STR=CI 0= IF 0 exit THEN
   PX @ u + PX !  PAT-WORD-END? ;
: TRUST-LITERAL-SITE?  {: a u :}  ( -- f )  \ s" name" s" effect" TRUST
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
: TRUSTED-DEF-SITE?  {: a u :}  ( -- f )  \ TRUSTED: name ( effect )
   a u PAT-RESET
   begin PAT-END? 0= while
      PX @ PSTART !
      s" TRUSTED:" PAT-MATCH-WORD IF
         PAT-CAP-WORD-1 IF
            PAT-CAP-PARENS-2 IF -1 exit THEN
         THEN
      THEN
      PSTART @ 1+ PX !
   repeat  0 ;
: TRUST-SITE?  {: a u :}  ( -- f )
   a u TRUST-LITERAL-SITE? IF -1 exit THEN
   a u TRUSTED-DEF-SITE? ;
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
' LINT-CHECK-HOOK set-check

0 constant SIG-MISSING
1 constant SIG-TYPED
2 constant SIG-OPTOUT
: SIG-OPTOUT? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u SPLIT-WHITESPACE
   SN# @ 1 = IF
      0 S@ s" infer" STR=CI IF LINT-TRUE exit THEN
      0 S@ s" private" STR=CI IF LINT-TRUE exit THEN
   THEN
   SN# @ 2 = IF
      0 S@ s" infer" STR=CI  1 S@ s" private" STR=CI and IF LINT-TRUE exit THEN
      0 S@ s" private" STR=CI  1 S@ s" infer" STR=CI and IF LINT-TRUE exit THEN
   THEN LINT-FALSE ;
: SIG-KIND ( ptr u8 n -- n ) {: a:ptr u :}
   a u s" --" CONTAINS? IF SIG-TYPED exit THEN
   a u SIG-OPTOUT? IF SIG-OPTOUT exit THEN
   SIG-MISSING ;

0 set-check
\ End-of-library handoff: downstream legacy scanner modules choose their own
\ checked boundary instead of inheriting this file's checked helper hook.
