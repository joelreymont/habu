\ lib.f — tokenizer, intern, and scanner foundation for native lint tools.
\ Load after tools/lint/text.f.
0 set-check

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

\ ---- token helpers for the def-walkers ----
: TEOL? ( n -- bool )
   1+ dup TN# @ >= IF drop LINT-TRUE ELSE TOK0? THEN ;

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

\ ---- named scanners replacing the former regex use-cases ------------------
0 set-check
\ Explicit unchecked boundary: PAT-* scanners keep current parse state and
\ captured source pointers in global cells shared with legacy lints.
variable PSA  variable PSU  variable PX  variable PSTART
variable P1A  variable P1U  variable P2A  variable P2U
variable PTA  variable PTU
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
: PAT-TOK$  ( -- a u )
   PTA @ PTU @ ;
: PAT-TOK=  {: a u :}  ( -- f )
   PAT-TOK$ a u STR=CI ;
: PAT-TOK-SQ?  ( -- f )
   PTU @ 2 <> IF 0 exit THEN
   PTA @ c@ FOLD 115 <> IF 0 exit THEN
   PTA @ 1+ c@ DQUOTE = ;
: PAT-TOK-STRING?  ( -- f )
   PTU @ 2 <> IF 0 exit THEN
   PTA @ 1+ c@ DQUOTE <> IF 0 exit THEN
   PTA @ c@ FOLD 115 = IF -1 exit THEN
   PTA @ c@ DOT = IF -1 exit THEN
   PTA @ c@ FOLD 99 = ;
: PAT-SKIP-LINE-COMMENT  ( -- )
   begin PAT-END? 0= PAT-C@ 10 <> and while PX @ 1+ PX ! repeat ;
: PAT-SKIP-PAREN-COMMENT  ( -- )
   PX @ 1+ PX !
   begin PAT-END? 0= PAT-C@ 41 <> and while PX @ 1+ PX ! repeat
   PAT-END? 0= IF PX @ 1+ PX ! THEN ;
: PAT-SKIP-IGNORED  ( -- )
   begin
      PAT-SKIP-WS
      PAT-END? IF exit THEN
      PAT-C@ 92 = IF PAT-SKIP-LINE-COMMENT ELSE
      PAT-C@ 40 = IF PAT-SKIP-PAREN-COMMENT ELSE exit THEN THEN
   again ;
: PAT-SKIP-STRING-BODY  ( -- )
   begin PAT-END? 0= while
      PAT-C@ DQUOTE = IF PX @ 1+ PX ! exit THEN
      PX @ 1+ PX !
   repeat ;
: PAT-READ-TOKEN  ( -- f )
   PAT-SKIP-IGNORED
   PAT-END? IF 0 exit THEN
   PSA @ PX @ + PTA !
   begin PAT-END? 0= PAT-C@ WS? 0= and while PX @ 1+ PX ! repeat
   PSA @ PX @ + PTA @ - PTU !
   -1 ;
: PAT-CAP-STRING-1  ( -- f )
   PAT-TOK-SQ? 0= IF 0 exit THEN
   PAT-END? IF 0 exit THEN
   PAT-C@ WS? 0= IF 0 exit THEN
   PAT-SKIP-WS
   PSA @ PX @ + P1A !
   begin PAT-END? 0= PAT-C@ DQUOTE <> and while PX @ 1+ PX ! repeat
   PAT-END? IF 0 exit THEN
   PSA @ PX @ + P1A @ - P1U !
   PX @ 1+ PX !
   -1 ;
: PAT-CAP-STRING-2  ( -- f )
   PAT-TOK-SQ? 0= IF 0 exit THEN
   PAT-END? IF 0 exit THEN
   PAT-C@ WS? 0= IF 0 exit THEN
   PAT-SKIP-WS
   PSA @ PX @ + P2A !
   begin PAT-END? 0= PAT-C@ DQUOTE <> and while PX @ 1+ PX ! repeat
   PAT-END? IF 0 exit THEN
   PSA @ PX @ + P2A @ - P2U !
   PX @ 1+ PX !
   -1 ;
: PAT-CAP-TOKEN-1  ( -- f )
   PAT-READ-TOKEN 0= IF 0 exit THEN
   PTA @ P1A !  PTU @ P1U !
   P1U @ 0 > ;
: PAT-CAP-EFFECT-2  ( -- f )
   PAT-SKIP-WS
   PAT-END? IF 0 exit THEN
   PAT-C@ 40 <> IF 0 exit THEN
   PX @ 1+ PX !
   PAT-SKIP-WS
   PSA @ PX @ + P2A !
   begin PAT-END? 0= PAT-C@ 41 <> and while PX @ 1+ PX ! repeat
   PAT-END? IF 0 exit THEN
   PSA @ PX @ + P2A @ - P2U !
   P2A @ P2U @ TRIM P2U ! P2A !
   PX @ 1+ PX !
   -1 ;
: TRUST-LITERAL-SITE?  {: a u :}  ( -- f )  \ s" name" s" effect" TRUST
   a u PAT-RESET
   begin PAT-READ-TOKEN while
      PAT-TOK-SQ? IF
         PAT-CAP-STRING-1 IF
            PAT-READ-TOKEN IF
               PAT-CAP-STRING-2 IF
                  PAT-READ-TOKEN IF s" TRUST" PAT-TOK= IF -1 exit THEN THEN
               THEN
            THEN
         THEN
      ELSE
         PAT-TOK-STRING? IF PAT-SKIP-STRING-BODY THEN
      THEN
   repeat  0 ;
: TRUSTED-DEF-SITE?  {: a u :}  ( -- f )  \ TRUSTED: name ( effect )
   a u PAT-RESET
   begin PAT-READ-TOKEN while
      s" TRUSTED:" PAT-TOK= IF
         PAT-CAP-TOKEN-1 IF PAT-CAP-EFFECT-2 IF -1 exit THEN THEN
      ELSE
         PAT-TOK-STRING? IF PAT-SKIP-STRING-BODY THEN
      THEN
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

\ Downstream legacy scanner modules that need unchecked mode must declare their
\ own `0 set-check` boundary instead of inheriting one from this shared library.
