\ error-code-lint-core.f - E- throw-code collision lint: error codes are one
\ global throw namespace. Walks .f/.fs sources under src/, tools/ and test/ and
\ flags a code claimed by two different E- names. bootstrap/ is not walked: the
\ recovery seed is a pinned corpus in its own process space.
\ LINT-LEX (src/lib/source-lex.f) decides which bytes are code; a lexer defect
\ stops the scan. STRICT is the gate entry; LEDGER prints without throwing.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/adt/option.f
require src/lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require src/lib/source-lex.f

package ERROR-CODE-LINT
using LINT-INTERN
public

\ ---- source-defect codes ----------------------------------------------------
\ A lexer defect hides every later claim in its source, so the scan refuses
\ rather than certify a partial ledger. -4808..-4810 sit in the lint-tool block
\ that starts at E-SHADOW-UNTERM (-4800).
-4808 constant E-QUOTE   \ a string literal ran past end of input
-4809 constant E-ROW     \ a primitive-axiom row lacked a header or its closer
-4810 constant E-LEX     \ a lexer diagnostic or token kind this lint was never taught

private

\ Exit statuses are not claims: an uncaught throw of 0..EXIT-MAX exits with that
\ status (src/habu/driver-io.f DRV-FAIL), so E-USAGE 64 means the same in every
\ tool by design. A larger positive value is a throw identity, held to the same
\ uniqueness and reserved ranges as a negative one.
255 constant EXIT-MAX

2048 constant MAX-CLAIMS  \ the tree passed 1024 live claims
1024 constant MAX-RES
48 constant ZERO-C
36 constant DOLLAR-C
45 constant MINUS-C

create SRC-SLAB LINT-SLAB:CELLS cells allot
create PATH PATH-CAP allot
create DIGITS 32 allot

NEWTYPE intern-id
NEWTYPE file-id
NEWTYPE stem-id

CAST: N>NAME ( n -- intern-id )
CAST: NAME>N ( intern-id -- n )
CAST: N>FILE ( n -- file-id )
CAST: FILE>N ( file-id -- n )
CAST: N>STEM ( n -- stem-id )
CAST: STEM>N ( stem-id -- n )

: NAME= ( intern-id intern-id -- bool ) NAME>N swap NAME>N = ;
: FILE= ( file-id file-id -- bool ) FILE>N swap FILE>N = ;
: STEM= ( stem-id stem-id -- bool ) STEM>N swap STEM>N = ;

STRUCTURE claim DERIVE addr
   FIELD code n FIELD name intern-id FIELD owner file-id
;STRUCTURE
ENUM reservation-state
   VARIANT none ;VARIANT
   VARIANT first-only FIELD code n ;VARIANT
   VARIANT last-only FIELD code n ;VARIANT
   VARIANT complete FIELD first n FIELD last n ;VARIANT
;ENUM
STRUCTURE reservation DERIVE addr
   FIELD stem stem-id FIELD owner file-id FIELD state reservation-state
;STRUCTURE

MAX-CLAIMS LAYOUT-BUFFER CLAIM-ROWS claim
MAX-RES LAYOUT-BUFFER RES-ROWS reservation
variable RES#

variable PATH-U
variable CLAIM#
variable BAD
variable FILE#
variable SHOW?
variable ND#
variable NV
variable IX
variable JX

: NL ( -- ) 10 emit ;

: SHOW! ( bool -- )  SHOW? ! ;
: SHOW-ON  ( -- )  LINT-TRUE  SHOW! ;
: SHOW-OFF ( -- )  LINT-FALSE SHOW! ;

: EMIT-U ( n -- )
   0 ND# !
   dup 0= if drop ZERO-C emit exit then
   begin dup 0 > while
      dup 10 mod ZERO-C + DIGITS ND# @ + c!
      10 / ND# @ 1+ ND# !
   repeat drop
   begin ND# @ 0 > while
      ND# @ 1- ND# !
      DIGITS ND# @ + c@ emit
   repeat ;

: EMIT-N ( n -- )
   dup 0 < if MINUS-C emit 0 swap - then
   EMIT-U ;

\ ---- numeric-literal parse --------------------------------------------------
: DEC? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   u 0= if 0 LINT-FALSE exit then
   0 NV !
   0 begin dup u < while
      dup a + c@
      dup 48 < over 57 > or if 2drop 0 LINT-FALSE exit then
      NV @ 10 * + 48 - NV !
      1+
   repeat drop
   NV @ LINT-TRUE ;

: HEXDIG ( n -- n )   \ -1 when not a hex digit
   dup 48 >= over 57 <= and if 48 - exit then
   dup 65 >= over 70 <= and if 55 - exit then
   dup 97 >= over 102 <= and if 87 - exit then
   drop -1 ;

: HEX? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   u 0= if 0 LINT-FALSE exit then
   0 NV !
   0 begin dup u < while
      dup a + c@ HEXDIG
      dup 0 < if 2drop 0 LINT-FALSE exit then
      NV @ 16 * + NV !
      1+
   repeat drop
   NV @ LINT-TRUE ;

: MAG? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   u 0= if 0 LINT-FALSE exit then
   a c@ DOLLAR-C = if a 1 + u 1- HEX? exit then
   a u DEC? ;

: NEG? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   u 2 < if 0 LINT-FALSE exit then
   a c@ MINUS-C <> if 0 LINT-FALSE exit then
   a 1 + u 1- MAG? {: v:n ok:bool :}
   ok 0= if 0 LINT-FALSE exit then
   0 v - LINT-TRUE ;

\ A code is the literal's signed-cell value: `$FFFFFFFFFFFFDCD7 constant E-X`
\ claims -9001.
: CODE? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   a u NEG? {: code:n negative:bool :}
   negative if code LINT-TRUE exit then
   a u MAG? ;

: THROW-CODE? ( n -- bool ) {: code:n :}
   code 0 <  code EXIT-MAX > or ;

\ ---- claim table ------------------------------------------------------------
: SENTINEL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" -FIRST" LINT-ENDS-WITH?  a u s" -LAST" LINT-ENDS-WITH? or ;

: CODE@  ( n -- n )         CLAIM-ROWS CLAIM-CODE @ ;
: NAME@  ( n -- intern-id ) CLAIM-ROWS CLAIM-NAME @ ;
: OWNER@ ( n -- file-id )   CLAIM-ROWS CLAIM-OWNER @ ;

: NAME$ ( intern-id -- ptr u8 n ) NAME>N INTERN$ ;
: FILE$ ( file-id -- ptr u8 n ) FILE>N INTERN$ ;
: STEM-NAME$ ( stem-id -- ptr u8 n ) STEM>N INTERN$ ;

\ Identical (code, name) re-registrations are one identity reachable through two
\ files (re-export shims; src/habu/stack-abi.f E-STACK-UNGUARDED for emitters
\ that compile before src/lib/), allowed inside another file's range too. Every
\ copy is recorded with its file: COLLIDE? ignores same-name pairs and FOREIGN?
\ asks whether the range's owner registers the pair itself.
: CLAIM+ ( n intern-id file-id -- ) {: code:n name:intern-id file:file-id :}
   CLAIM# @ MAX-CLAIMS >= if s" error-code-lint: claim table full" 1 die then
   code name file CLAIM-MAKE CLAIM# @ CLAIM-ROWS !
   CLAIM# @ 1+ CLAIM# ! ;

: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if s" error-code-lint: path too long" 1 die then
   a PATH u LINT-BMOVE  u PATH-U ! ;

: PATH$ ( -- ptr u8 n )  PATH PATH-U @ ;

\ ---- reservation table (E-*-FIRST/E-*-LAST range blocks) --------------------
\ E-*-FIRST / E-*-LAST are range sentinels aliasing their block's boundary codes,
\ not new identities. A pair, keyed by shared stem (E-FS-FIRST/E-FS-LAST -> E-FS)
\ and declaring file, reserves the inclusive [FIRST,LAST] range for that file; an
\ E- code another file claims inside it is flagged even before the owner mints it.
: RES-STEM@  ( n -- stem-id ) RES-ROWS RESERVATION-STEM @ ;
: RES-OWNER@ ( n -- file-id ) RES-ROWS RESERVATION-OWNER @ ;
: RES-STATE@ ( n -- reservation-state ) RES-ROWS RESERVATION-STATE @ ;

\ the caller guarantees SENTINEL?
: STEM$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u s" -FIRST" LINT-ENDS-WITH? if a u 6 - exit then
   a u 5 - ;

: FIRST-TOK? ( ptr u8 n -- bool )  s" -FIRST" LINT-ENDS-WITH? ;

: RES-FIND ( stem-id file-id -- option<n> ) {: stem:stem-id file:file-id :}
   RES# @ 0 ?do
      i RES-STEM@ stem STEM= i RES-OWNER@ file FILE= and if
         i OPTION:SOME unloop exit
      then
   loop OPTION:NONE ;

: WITH-FIRST ( reservation-state n -- reservation-state ) {: code:n :}
   \ A spelled -0 clears this bound.
   code 0= if
      MATCH reservation-state
         none OF construct reservation-state none ENDOF
         first-only OF drop construct reservation-state none ENDOF
         last-only OF construct reservation-state last-only ENDOF
         complete OF swap drop construct reservation-state last-only ENDOF
      ;MATCH exit
   then
   MATCH reservation-state
      none OF code construct reservation-state first-only ENDOF
      first-only OF drop code construct reservation-state first-only ENDOF
      last-only OF code swap construct reservation-state complete ENDOF
      complete OF swap drop code swap construct reservation-state complete ENDOF
   ;MATCH ;

: WITH-LAST ( reservation-state n -- reservation-state ) {: code:n :}
   code 0= if
      MATCH reservation-state
         none OF construct reservation-state none ENDOF
         first-only OF construct reservation-state first-only ENDOF
         last-only OF drop construct reservation-state none ENDOF
         complete OF drop construct reservation-state first-only ENDOF
      ;MATCH exit
   then
   MATCH reservation-state
      none OF code construct reservation-state last-only ENDOF
      first-only OF code construct reservation-state complete ENDOF
      last-only OF drop code construct reservation-state last-only ENDOF
      complete OF drop code construct reservation-state complete ENDOF
   ;MATCH ;

: WITH-BOUND ( reservation-state n bool -- reservation-state )
   if WITH-FIRST else WITH-LAST then ;

\ A new row is published whole after capacity checks; an existing row changes
\ only after the exhaustive transition has produced its replacement state.
: RES+ ( n ptr u8 n -- ) {: code:n a:ptr u:n :}
   a u STEM$ INTERN N>STEM {: stem:stem-id :}
   PATH$ INTERN N>FILE {: file:file-id :}
   a u FIRST-TOK? {: first:bool :}
   stem file RES-FIND MATCH option
      some OF
         {: k:n :}
         k RES-STATE@ code first WITH-BOUND k RES-ROWS RESERVATION-STATE !
      ENDOF
      none OF
         RES# @ MAX-RES >= if s" error-code-lint: reservation table full" 1 die then
         construct reservation-state none code first WITH-BOUND {: state:reservation-state :}
         stem file state RESERVATION-MAKE RES# @ RES-ROWS !
         RES# @ 1+ RES# !
      ENDOF
   ;MATCH ;

\ ---- token walk -------------------------------------------------------------
: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

\ A comment and a complete primitive-axiom row declare no constant. Any other
\ non-WORD kind is one this lint was never taught: refused, never skipped.
: KNOWN-KIND? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ {: kind:n :}
   kind LINT-LEX:WORD = if LINT-TRUE exit then
   kind LINT-LEX:COMMENT = if LINT-TRUE exit then
   kind LINT-LEX:REGISTRY = ;

\ A comment between words is not code: `-9001 constant ( note ) E-XA` is one claim.
: NEXT-WORD ( n -- n ) {: k:n :}
   k begin dup LINT-LEX:COUNT < while
      dup WORD? if exit then
      1+
   repeat ;

\ `E-OWNER constant E-LOCAL` is an alias, not a claim: only a numeric literal
\ claims. A file reads another's code this way when the owner is loaded
\ (src/arch/arm32/asm.f E-OPERAND) and re-registers the identical pair when not.
: SCAN-CLAIM ( n -- ) {: k:n :}
   k 1+ NEXT-WORD {: ki:n :}
   ki LINT-LEX:COUNT >= if exit then
   ki LINT-LEX:TOKEN s" constant" LINT-STR=CI 0= if exit then
   ki 1+ NEXT-WORD {: ni:n :}
   ni LINT-LEX:COUNT >= if exit then
   ni LINT-LEX:TOKEN {: na:ptr nu:n :}
   na nu s" E-" LINT-PREFIX? 0= if exit then
   k LINT-LEX:TOKEN CODE? {: code:n numeric:bool :}
   numeric 0= if exit then
   na nu SENTINEL? if
      \ A sentinel bounds a range where a claim could stand.
      code THROW-CODE? if code na nu RES+ exit then
      code 0= if
         k LINT-LEX:TOKEN NEG? if na nu RES+ exit then drop
      then
   then
   code THROW-CODE? if
      code na nu INTERN N>NAME PATH$ INTERN N>FILE CLAIM+
   then ;

: UNKNOWN-KIND ( n -- ) {: k:n :}
   s" error-code-lint: " type PATH$ type
   s"  token " type k EMIT-U
   s" : unknown lexer token kind " type k LINT-LEX:KIND@ EMIT-U NL
   E-LEX throw ;

: SCAN-TOKENS ( -- )
   0 begin dup LINT-LEX:COUNT < while
      dup KNOWN-KIND? 0= if dup UNKNOWN-KIND then
      dup WORD? if dup SCAN-CLAIM then
      1+
   repeat drop ;

: DEFECT-SITE ( -- )
   s" error-code-lint: " type PATH$ type
   s" :" type LINT-LEX:ERROR-LINE@ EMIT-U
   s" :" type LINT-LEX:ERROR-COL@ EMIT-U
   s" : " type ;

: LEX-DEFECT ( -- )
   DEFECT-SITE
   LINT-LEX:ERROR-KIND@ {: kind:n :}
   kind LINT-LEX:UNTERMINATED-QUOTE = if
      s" unterminated string literal" type NL  E-QUOTE throw
   then
   kind LINT-LEX:MALFORMED-REGISTRY = if
      s" malformed primitive-axiom row" type NL  E-ROW throw
   then
   s" unknown lexer diagnostic" type NL  E-LEX throw ;

: SCAN-TEXT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LINT-LEX:SOURCE
   LINT-LEX:ERROR? if LEX-DEFECT then
   SCAN-TOKENS ;

\ ---- findings ---------------------------------------------------------------
: HIT ( n n -- ) {: i:n j:n :}
   SHOW? @ if
      s" ERROR-CODE " type i CODE@ EMIT-N
      s"  claimed by '" type i NAME@ NAME$ type
      s" ' (" type i OWNER@ FILE$ type
      s" ) and '" type j NAME@ NAME$ type
      s" ' (" type j OWNER@ FILE$ type
      s" )" type NL
   then
   BAD @ 1+ BAD ! ;

: COLLIDE? ( n n -- bool ) {: i:n j:n :}
   i CODE@ j CODE@ =
   i NAME@ j NAME@ NAME= 0= and ;

: FINDINGS ( -- )
   0 IX !
   begin IX @ CLAIM# @ < while
      IX @ 1+ JX !
      begin JX @ CLAIM# @ < while
         IX @ JX @ COLLIDE? if IX @ JX @ HIT then
         JX @ 1+ JX !
      repeat
      IX @ 1+ IX !
   repeat ;

\ ---- foreign-range findings -------------------------------------------------
: ORDER ( n n -- n n )
   2dup > if swap then ;

: IN-RANGE? ( n n n -- bool ) {: code:n first:n last:n :}
   first last ORDER {: lo:n hi:n :}
   code lo >= code hi <= and ;

: OWNER-REGISTERED? ( n n -- bool ) {: ci:n ri:n :}
   CLAIM# @ 0 ?do
      i OWNER@ ri RES-OWNER@ FILE=
      i CODE@ ci CODE@ = and
      i NAME@ ci NAME@ NAME= and if LINT-TRUE unloop exit then
   loop LINT-FALSE ;

: FOREIGN? ( n n -- bool ) {: ci:n ri:n :}
   ri RES-STATE@ MATCH reservation-state
      none OF LINT-FALSE ENDOF
      first-only OF drop LINT-FALSE ENDOF
      last-only OF drop LINT-FALSE ENDOF
      complete OF
         {: first:n last:n :}
         ci CODE@ first last IN-RANGE?
         ci OWNER@ ri RES-OWNER@ FILE= 0= and
         if ci ri OWNER-REGISTERED? 0= else LINT-FALSE then
      ENDOF
   ;MATCH ;

: RES-HIT ( n n -- ) {: ci:n ri:n :}
   SHOW? @ if
      s" ERROR-CODE " type ci CODE@ EMIT-N
      s"  claimed by '" type ci NAME@ NAME$ type
      s" ' (" type ci OWNER@ FILE$ type
      s" ) inside reserved range " type ri RES-STEM@ STEM-NAME$ type
      s" -FIRST..-LAST owned by (" type ri RES-OWNER@ FILE$ type
      s" )" type NL
   then
   BAD @ 1+ BAD ! ;

: RES-FINDINGS ( -- )
   0 IX !
   begin IX @ CLAIM# @ < while
      0 JX !
      begin JX @ RES# @ < while
         IX @ JX @ FOREIGN? if IX @ JX @ RES-HIT then
         JX @ 1+ JX !
      repeat
      IX @ 1+ IX !
   repeat ;

: RESET-LEDGER ( -- )
   0 BAD !  0 CLAIM# !  0 RES# ! ;

\ ---- file walk --------------------------------------------------------------
: FORTH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT?  a u s" .fs" HAS-EXT? or ;

: SCAN-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FORTH? 0= if exit then
   a u PATH!
   FILE# @ 1+ FILE# !
   a u SRC-SLAB LINT-SLAB:LOAD
   SRC-SLAB LINT-SLAB:TEXT SCAN-TEXT ;

: ROOT ( ptr u8 n -- )
   [: SCAN-FILE ;] FS:WALK-FILES ;

: WALK ( -- )
   RESET-LEDGER  0 FILE# !
   s" src/" ROOT
   s" tools/" ROOT
   s" test/" ROOT
   FINDINGS RES-FINDINGS ;

: SUMMARY ( -- )
   s" error-code-lint: " type
   FILE# @ EMIT-U s"  file(s), " type
   CLAIM# @ EMIT-U s"  claim(s), " type
   RES#   @ EMIT-U s"  reservation(s), " type
   BAD    @ EMIT-U s"  finding(s)" type NL ;

public

: COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   SHOW? @ {: show:bool :}
   SHOW-OFF
   RESET-LEDGER
   s" <test>" PATH!
   a u SCAN-TEXT
   FINDINGS RES-FINDINGS
   show SHOW!
   BAD @ ;

: COUNT2 ( ptr u8 n ptr u8 n -- n ) {: ao:ptr auo:n af:ptr auf:n :}
   SHOW? @ {: show:bool :}
   SHOW-OFF
   RESET-LEDGER
   s" owner.f" PATH!    ao auo SCAN-TEXT
   s" foreign.f" PATH!  af auf SCAN-TEXT
   FINDINGS RES-FINDINGS
   show SHOW!
   BAD @ ;

\ Live rows whose bounds are exactly first/last. This public query retains 0
\ for an absent bound; the stored state and range checks never use a sentinel.
: RESERVATIONS ( n n -- n ) {: first:n last:n :}
   0 RES# @ 0 ?do
      i RES-STATE@ MATCH reservation-state
         none OF first 0= last 0= and if 1+ then ENDOF
         first-only OF first = last 0= and if 1+ then ENDOF
         last-only OF last = first 0= and if 1+ then ENDOF
         complete OF
            {: lo:n hi:n :}
            lo first = hi last = and if 1+ then
         ENDOF
      ;MATCH
   loop ;

: LEDGER ( -- )
   SHOW-ON  WALK  SUMMARY ;

: STRICT ( -- )
   LEDGER
   BAD @ 0 > if 1 throw then ;

;package
