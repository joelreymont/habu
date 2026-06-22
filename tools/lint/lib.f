\ lib.f - intern and scanner foundation for native lint tools.
\ Load after tools/lint/text.f and tools/lint/token.f.

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
\ Scanner captures remain in shared cells for legacy callers, but scanner logic
\ is checked and returns typed bools.
variable PSA  variable PSU  variable PX  variable PSTART
variable P1A  variable P1U  variable P2A  variable P2U
variable PTA  variable PTU

: PSA-FIELD ( -- ptr ptr u8 )  PSA 0 ptr-field ;
: P1A-FIELD ( -- ptr ptr u8 )  P1A 0 ptr-field ;
: P2A-FIELD ( -- ptr ptr u8 )  P2A 0 ptr-field ;
: PTA-FIELD ( -- ptr ptr u8 )  PTA 0 ptr-field ;

: PSA@ ( -- ptr u8 )  PSA-FIELD @ ;
: P1A@ ( -- ptr u8 )  P1A-FIELD @ ;
: P2A@ ( -- ptr u8 )  P2A-FIELD @ ;
: PTA@ ( -- ptr u8 )  PTA-FIELD @ ;

: PSA! ( ptr u8 -- )  PSA-FIELD ! ;
: P1A! ( ptr u8 -- )  P1A-FIELD ! ;
: P2A! ( ptr u8 -- )  P2A-FIELD ! ;
: PTA! ( ptr u8 -- )  PTA-FIELD ! ;

: PAT-RESET ( ptr u8 n -- )
   {: a:ptr u :}
   a PSA!  u PSU !  0 PX !  0 P1U !  0 P2U ! ;

: PAT-END? ( -- bool )
   PX @ PSU @ >= ;

: PAT-C@ ( -- n )
   PSA@ PX @ + c@ ;

: PAT-WS? ( -- bool )
   PAT-END? IF LINT-FALSE ELSE PAT-C@ WS? THEN ;

: PAT-ADV ( -- )
   PX @ 1+ PX ! ;

: PAT-SKIP-WS ( -- )
   begin PAT-WS? while PAT-ADV repeat ;

: PAT-WORD-END? ( -- bool )
   PAT-END? IF LINT-TRUE ELSE PAT-C@ WS? THEN ;

: PAT-TOK$ ( -- ptr u8 n )
   PTA@ PTU @ ;

: PAT-TOK= ( ptr u8 n -- bool )
   {: a:ptr u :}
   PAT-TOK$ a u STR=CI ;

: PAT-TOK-SQ? ( -- bool )
   PTU @ 2 <> IF LINT-FALSE exit THEN
   PTA@ c@ FOLD 115 <> IF LINT-FALSE exit THEN
   PTA@ 1+ c@ DQUOTE = ;

: PAT-TOK-STRING? ( -- bool )
   PTU @ 2 <> IF LINT-FALSE exit THEN
   PTA@ 1+ c@ DQUOTE <> IF LINT-FALSE exit THEN
   PTA@ c@ FOLD 115 = IF LINT-TRUE exit THEN
   PTA@ c@ DOT = IF LINT-TRUE exit THEN
   PTA@ c@ FOLD 99 = ;

: PAT-SKIP-LINE-COMMENT ( -- )
   begin PAT-END? 0= PAT-C@ 10 <> and while PAT-ADV repeat ;

: PAT-SKIP-PAREN-COMMENT ( -- )
   PAT-ADV
   begin PAT-END? 0= PAT-C@ 41 <> and while PAT-ADV repeat
   PAT-END? 0= IF PAT-ADV THEN ;

: PAT-SKIP-IGNORED ( -- )
   begin
      PAT-SKIP-WS
      PAT-END? IF exit THEN
      PAT-C@ 92 = IF PAT-SKIP-LINE-COMMENT ELSE
      PAT-C@ 40 = IF PAT-SKIP-PAREN-COMMENT ELSE exit THEN THEN
   again ;

: PAT-SKIP-STRING-BODY ( -- )
   begin PAT-END? 0= while
      PAT-C@ DQUOTE = IF PAT-ADV exit THEN
      PAT-ADV
   repeat ;

: PAT-READ-TOKEN ( -- bool )
   PAT-SKIP-IGNORED
   PAT-END? IF LINT-FALSE exit THEN
   PSA@ PX @ + PTA!
   begin PAT-END? 0= PAT-C@ WS? 0= and while PAT-ADV repeat
   PSA@ PX @ + PTA@ - PTU !
   LINT-TRUE ;

: PAT-CAP-STRING-1 ( -- bool )
   PAT-TOK-SQ? 0= IF LINT-FALSE exit THEN
   PAT-END? IF LINT-FALSE exit THEN
   PAT-C@ WS? 0= IF LINT-FALSE exit THEN
   PAT-SKIP-WS
   PSA@ PX @ + P1A!
   begin PAT-END? 0= PAT-C@ DQUOTE <> and while PAT-ADV repeat
   PAT-END? IF LINT-FALSE exit THEN
   PSA@ PX @ + P1A@ - P1U !
   PAT-ADV
   LINT-TRUE ;

: PAT-CAP-STRING-2 ( -- bool )
   PAT-TOK-SQ? 0= IF LINT-FALSE exit THEN
   PAT-END? IF LINT-FALSE exit THEN
   PAT-C@ WS? 0= IF LINT-FALSE exit THEN
   PAT-SKIP-WS
   PSA@ PX @ + P2A!
   begin PAT-END? 0= PAT-C@ DQUOTE <> and while PAT-ADV repeat
   PAT-END? IF LINT-FALSE exit THEN
   PSA@ PX @ + P2A@ - P2U !
   PAT-ADV
   LINT-TRUE ;

: PAT-CAP-TOKEN-1 ( -- bool )
   PAT-READ-TOKEN 0= IF LINT-FALSE exit THEN
   PTA@ P1A!  PTU @ P1U !
   P1U @ 0 > ;

: PAT-CAP-EFFECT-2 ( -- bool )
   PAT-SKIP-WS
   PAT-END? IF LINT-FALSE exit THEN
   PAT-C@ 40 <> IF LINT-FALSE exit THEN
   PAT-ADV
   PAT-SKIP-WS
   PSA@ PX @ + P2A!
   begin PAT-END? 0= PAT-C@ 41 <> and while PAT-ADV repeat
   PAT-END? IF LINT-FALSE exit THEN
   PSA@ PX @ + P2A@ - P2U !
   P2A@ P2U @ TRIM P2U ! P2A!
   PAT-ADV
   LINT-TRUE ;

: TRUST-LITERAL-SITE? ( ptr u8 n -- bool )
   \ s" name" s" effect" TRUST
   {: a:ptr u :}
   a u PAT-RESET
   begin PAT-READ-TOKEN while
      PAT-TOK-SQ? IF
         PAT-CAP-STRING-1 IF
            PAT-READ-TOKEN IF
               PAT-CAP-STRING-2 IF
                  PAT-READ-TOKEN IF s" TRUST" PAT-TOK= IF LINT-TRUE exit THEN THEN
               THEN
            THEN
         THEN
      ELSE
         PAT-TOK-STRING? IF PAT-SKIP-STRING-BODY THEN
      THEN
   repeat  LINT-FALSE ;

: TRUSTED-DEF-SITE? ( ptr u8 n -- bool )
   \ TRUSTED: name ( effect )
   {: a:ptr u :}
   a u PAT-RESET
   begin PAT-READ-TOKEN while
      s" TRUSTED:" PAT-TOK= IF
         PAT-CAP-TOKEN-1 IF PAT-CAP-EFFECT-2 IF LINT-TRUE exit THEN THEN
      ELSE
         PAT-TOK-STRING? IF PAT-SKIP-STRING-BODY THEN
      THEN
   repeat  LINT-FALSE ;

: TRUST-SITE? ( ptr u8 n -- bool )
   {: a:ptr u :}
   a u TRUST-LITERAL-SITE? IF LINT-TRUE exit THEN
   a u TRUSTED-DEF-SITE? ;

: SRC-PATH-REF? ( ptr u8 n -- bool )
   \ token ending -SRC, then s" src/*.f"
   {: a:ptr u :}
   a u PAT-RESET
   begin PAT-READ-TOKEN while
      PAT-TOK$ s" -SRC" SUFFIX? IF
         PAT-READ-TOKEN IF
            PAT-CAP-STRING-1 IF
               P1A@ P1U @ s" src/" STARTS-WITH?
               P1A@ P1U @ s" .f" HAS-EXT? and IF LINT-TRUE exit THEN
            THEN
         THEN
      THEN
   repeat  LINT-FALSE ;

: BACKTICK-PATH? ( ptr u8 n -- bool )
   {: a:ptr u :}
   a u PAT-RESET
   begin PAT-END? 0= while
      PAT-C@ 96 = IF
         PAT-ADV  PSA@ PX @ + P1A!
         begin PAT-END? 0= PAT-C@ 96 <> and while PAT-ADV repeat
         PAT-END? IF LINT-FALSE exit THEN
         PSA@ PX @ + P1A@ - P1U !
         P1A@ P1U @ PATHISH? IF LINT-TRUE exit THEN
      THEN
      PAT-ADV
   repeat  LINT-FALSE ;

\ Signature comment classification used by strict source tools.

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
