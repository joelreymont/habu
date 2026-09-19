\ error-code-lint-core.f - global E- throw-code uniqueness lint.
\
\ Error codes are one global throw namespace: a thrown negative code is
\ ambiguous the moment two different E- names claim it. Three live collisions
\ motivated this lint (E-CUDA/E-FUSE at -5002, E-PTX-READBACK/E-MK-EVAL at
\ -5003, E-LMV-NOOUT+E-LMV-REG/E-ABL-NOSUB+E-ABL-CAP at -5210/-5211). The scan
\ walks tracked .f/.fs sources under src/, lib/, tools/, and test/ for
\ `-NNNN constant E-*` claims and flags any numeric code owned by two
\ different E- names.
\
\ Which bytes are code is decided by the one shared source lexer, package
\ LINT-LEX in tools/lint/source-lex.f. It consumes `\` line comments, `( ... )`
\ and `.( ... )` comment bodies, and every string literal body - the plain
\ `s" c" ."` openers and the escaped `s\" c\" .\"` openers alike - so none of
\ that text ever reaches this scan as a token. This lint used to decide string
\ membership itself, by counting `"` bytes per token and toggling an in-string
\ flag on an odd count. That is a value heuristic standing in for a structural
\ fact, and it failed in both directions: one bare `[char] "` blinded the rest
\ of a file, a `.( ... )` body was read as code, and a `\` written INSIDE a
\ string body made the old tokenizer strip that string's closing quote, which
\ inverted the flag for the remainder of the file. The gate then skipped every
\ later claim in that source while still printing `0 finding(s)`.
\
\ Scope and allowances (each deliberate):
\ - Negative codes only: positive `NN constant E-*` values are sysexits-style
\   process exit codes (64/70/74/76...), shared across tools by design.
\ - `E-*-FIRST` / `E-*-LAST` names are range sentinels (lib/errors.f blocks):
\   they alias their block's boundary member codes, not new throw identities.
\   Each FIRST/LAST pair (matched by shared stem, e.g. E-FS-FIRST/E-FS-LAST) also
\   reserves the inclusive [FIRST,LAST] code range for the file that declares it.
\   A negative E- code claimed INSIDE another file's reserved range is a foreign
\   claim and is flagged, even before the owning block mints that exact member.
\ - Identical (code, name) re-registrations are allowed (re-export shims; the
\   same constant reachable through two entry files), inside another file's
\   reserved range too: the owner holds the identity and the copy adds no
\   second name (src/habu/stack-abi.f E-STACK-UNGUARDED for the engine
\   emitters that compile before lib/ exists).
\ - bootstrap/ is not walked: the frozen recovery seed is a pinned corpus in
\   its own process space; renumbering it would break the audited seed.
\
\ LEDGER prints the ledger without throwing; STRICT throws on any finding and is
\ the gate entrypoint. SCAN, RESERVATIONS and CLAIMS-IN expose the live ledger to
\ tools/error-code-region-test.f without letting a caller reach the tables.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/adt/option.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/source-lex.f

package ERROR-CODE-LINT
using LINT-INTERN
public

\ ---- source-defect codes ----------------------------------------------------
\ A lexer diagnostic truncates the token table at the defect, so every later
\ claim in that source is invisible. Continuing would certify a ledger built
\ from a partial file: exactly the blindness this lint exists to prevent. Each
\ defect gets its own name, the way tools/lint/shadow-lint.f splits
\ E-SHADOW-UNTERM from E-SHADOW-REGISTRY. They are public so a caller (and the
\ fixture that pins the refusal) can name which defect stopped a scan instead of
\ matching a bare number. Negative, so this lint keeps them globally unique.
\ -4808..-4810 continue the unclaimed lint-tool gap that holds E-SHADOW-UNTERM
\ (-4800) through E-PKGDIFF-NONAME (-4807); the gap ends before lib/errors.f's
\ reserved E-REPORT block at -4900.
-4808 constant E-QUOTE   \ a string literal ran past end of input
-4809 constant E-ROW     \ a `PRIM:`/`PPRIM:` axiom row lacked a header or its closer
\ The residual arm: a diagnostic or token kind added to LINT-LEX after this
\ consumer was written. It must reach a named refusal rather than borrow one of
\ the two labels above, and it must never pass in silence.
-4810 constant E-LEX     \ a lexer diagnostic or token kind this lint was never taught

private

512 constant PATH-CAP
2048 constant MAX-CLAIMS  \ the tree passed 1024 live claims; the table is sized above what it holds, and a full one dies rather than certifying a partial ledger
1024 constant MAX-RES
48 constant ZERO-C
36 constant DOLLAR-C
45 constant MINUS-C

\ One file at a time, in a slab sized from the file. It was a fixed arena, and an
\ arena doubled once already for src/core/checker.f is the shape tools/lint/text.f
\ LINT-SLAB was written to end: the next source to cross the constant stops the
\ whole ledger with a message about a buffer rather than about a code.
create SRC-SLAB LINT-SLAB:CELLS cells allot
create PATH PATH-CAP allot
create DIGITS 32 allot

NEWTYPE intern-id 0
NEWTYPE file-id 0
NEWTYPE stem-id 0

CAST: N>NAME ( n -- intern-id )
CAST: NAME>N ( intern-id -- n )
CAST: N>FILE ( n -- file-id )
CAST: FILE>N ( file-id -- n )
CAST: N>STEM ( n -- stem-id )
CAST: STEM>N ( stem-id -- n )

: NAME= ( intern-id intern-id -- bool ) NAME>N swap NAME>N = ;
: FILE= ( file-id file-id -- bool ) FILE>N swap FILE>N = ;
: STEM= ( stem-id stem-id -- bool ) STEM>N swap STEM>N = ;

STRUCTURE claim 0 DERIVE addr
   FIELD code n FIELD name intern-id FIELD owner file-id
;STRUCTURE
ENUM reservation-state 0
   VARIANT none ;VARIANT
   VARIANT first-only FIELD code n ;VARIANT
   VARIANT last-only FIELD code n ;VARIANT
   VARIANT complete FIELD first n FIELD last n ;VARIANT
;ENUM
STRUCTURE reservation 0 DERIVE addr
   FIELD stem stem-id FIELD owner file-id FIELD state reservation-state
;STRUCTURE

MAX-CLAIMS LAYOUT-BUFFER CLAIM-ROWS claim
MAX-RES LAYOUT-BUFFER RES-ROWS reservation
variable RES#

variable PATH-U
variable CLAIM#
variable BAD                            \ collision findings (claim pairs)
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

\ unsigned decimal (digit-buffer print, as maki-dep/namespace lint)
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

\ signed decimal (codes are negative)
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

: MAG? ( ptr u8 n -- n bool ) {: a:ptr u:n :}   \ decimal or $hex magnitude
   u 0= if 0 LINT-FALSE exit then
   a c@ DOLLAR-C = if a 1 + u 1- HEX? exit then
   a u DEC? ;

\ negative numeric literal (-NNNN or -$HH) -> its value
: NEG? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   u 2 < if 0 LINT-FALSE exit then
   a c@ MINUS-C <> if 0 LINT-FALSE exit then
   a 1 + u 1- MAG? {: v:n ok:bool :}
   ok 0= if 0 LINT-FALSE exit then
   0 v - LINT-TRUE ;

\ ---- claim table ------------------------------------------------------------
: SENTINEL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" -FIRST" LINT-ENDS-WITH?  a u s" -LAST" LINT-ENDS-WITH? or ;

: CODE@  ( n -- n )         CLAIM-ROWS CLAIM-CODE @ ;
: NAME@  ( n -- intern-id ) CLAIM-ROWS CLAIM-NAME @ ;
: OWNER@ ( n -- file-id )   CLAIM-ROWS CLAIM-OWNER @ ;

: NAME$ ( intern-id -- ptr u8 n ) NAME>N INTERN$ ;
: FILE$ ( file-id -- ptr u8 n ) FILE>N INTERN$ ;
: STEM-NAME$ ( stem-id -- ptr u8 n ) STEM>N INTERN$ ;

\ Every claim is recorded with its file, an identical (code, name) pair from a
\ second file included: the pair is one identity reachable through two files,
\ so COLLIDE? ignores same-name pairs and FOREIGN? asks whether the range's
\ owner registers the pair itself. Dropping the second copy here used to lose
\ which file was the owner, and the range check then flagged the re-export.
: CLAIM+ ( n intern-id file-id -- ) {: code:n name:intern-id file:file-id :}
   CLAIM# @ MAX-CLAIMS >= if s" error-code-lint: claim table full" 1 die then
   code name file CLAIM-MAKE CLAIM# @ CLAIM-ROWS !
   CLAIM# @ 1+ CLAIM# ! ;

: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if s" error-code-lint: path too long" 1 die then
   a PATH u LINT-BMOVE  u PATH-U ! ;

: PATH$ ( -- ptr u8 n )  PATH PATH-U @ ;

\ ---- reservation table (E-*-FIRST/E-*-LAST range blocks) --------------------
\ A FIRST/LAST pair reserves the inclusive numeric range between its two member
\ codes for the file that declares it (lib/errors.f owns every stdlib block).
\ Pairs are keyed by shared stem (E-FS-FIRST/E-FS-LAST -> E-FS) and declaring
\ file, so two files can each own a same-named block.
: RES-STEM@  ( n -- stem-id ) RES-ROWS RESERVATION-STEM @ ;
: RES-OWNER@ ( n -- file-id ) RES-ROWS RESERVATION-OWNER @ ;
: RES-STATE@ ( n -- reservation-state ) RES-ROWS RESERVATION-STATE @ ;

\ name minus its -FIRST / -LAST suffix (caller guarantees SENTINEL?)
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
   \ The scanner accepts -0. Preserve its old meaning: clear this bound.
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

\ Kinds this scan understands. A WORD is code. A `( ... )` or `.( ... )` comment
\ and a complete `PRIM:`/`PPRIM:` axiom row are inert spans that declare no
\ constant, so they are stepped over whole. Any other kind is one this lint was
\ never taught, and skipping it in silence is how a scanner goes blind: the token
\ would span source the scan never reads while the ledger still reports zero
\ findings.
: KNOWN-KIND? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ {: kind:n :}
   kind LINT-LEX:WORD = if LINT-TRUE exit then
   kind LINT-LEX:COMMENT = if LINT-TRUE exit then
   kind LINT-LEX:REGISTRY = ;

\ Index of the next WORD token at or after k, or the token count when the source
\ has no word left. A comment sits between words without being code, so
\ `-9001 constant ( note ) E-XA` is still one claim.
: NEXT-WORD ( n -- n ) {: k:n :}
   k begin dup LINT-LEX:COUNT < while
      dup WORD? if exit then
      1+
   repeat ;

\ token k as a `<negative-number> constant E-NAME` claim
: SCAN-CLAIM ( n -- ) {: k:n :}
   k 1+ NEXT-WORD {: ki:n :}
   ki LINT-LEX:COUNT >= if exit then
   ki LINT-LEX:TOKEN s" constant" LINT-STR=CI 0= if exit then
   ki 1+ NEXT-WORD {: ni:n :}
   ni LINT-LEX:COUNT >= if exit then
   ni LINT-LEX:TOKEN {: na:ptr nu:n :}
   na nu s" E-" LINT-PREFIX? 0= if exit then
   k LINT-LEX:TOKEN NEG? {: code:n ok:bool :}
   ok 0= if exit then
   na nu SENTINEL? if code na nu RES+ exit then
   code na nu INTERN N>NAME PATH$ INTERN N>FILE CLAIM+ ;

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

\ Fail-closed: a lexer diagnostic stops the scan at the defect, so every claim
\ after it in that source is unreadable. Name the file, the site and the defect,
\ then throw a catchable code rather than build the ledger from a partial file.
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

\ one finding per colliding claim pair
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
: ORDER ( n n -- n n )   \ order two bounds ascending
   2dup > if swap then ;

: IN-RANGE? ( n n n -- bool ) {: code:n first:n last:n :}
   first last ORDER {: lo:n hi:n :}
   code lo >= code hi <= and ;

\ the reservation's owner claims the same (code, name) itself: claim ci is a
\ re-registration of that identity, not a foreign one
: OWNER-REGISTERED? ( n n -- bool ) {: ci:n ri:n :}
   CLAIM# @ 0 ?do
      i OWNER@ ri RES-OWNER@ FILE=
      i CODE@ ci CODE@ = and
      i NAME@ ci NAME@ NAME= and if LINT-TRUE unloop exit then
   loop LINT-FALSE ;

\ claim ci falls inside a COMPLETE reservation ri owned by another file, under
\ a name the owner does not itself register for that code
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

\ one finding per (claim, foreign reservation) pair
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
   [: SCAN-FILE ;] WALK-FILES ;

: WALK ( -- )
   RESET-LEDGER  0 FILE# !
   s" src/" ROOT
   s" lib/" ROOT
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

\ findings from scanning one string in isolation (reset -> scan -> pair count)
: COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   SHOW? @ {: show:bool :}
   SHOW-OFF
   RESET-LEDGER
   s" <test>" PATH!
   a u SCAN-TEXT
   FINDINGS RES-FINDINGS
   show SHOW!
   BAD @ ;

\ two-file finding count: OWNER source declares the block, FOREIGN source mints
\ its claims under a different path, so a cross-file range claim can be tested.
: COUNT2 ( ptr u8 n ptr u8 n -- n ) {: ao:ptr auo:n af:ptr auf:n :}
   SHOW? @ {: show:bool :}
   SHOW-OFF
   RESET-LEDGER
   s" owner.f" PATH!    ao auo SCAN-TEXT
   s" foreign.f" PATH!  af auf SCAN-TEXT
   FINDINGS RES-FINDINGS
   show SHOW!
   BAD @ ;

\ build the live ledger for the whole tree without printing anything
: SCAN ( -- )
   SHOW? @ {: show:bool :}
   SHOW-OFF  WALK  show SHOW! ;

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

\ live claims whose code falls inside the inclusive range
: CLAIMS-IN ( n n -- n ) {: first:n last:n :}
   0  0 begin dup CLAIM# @ < while
      dup CODE@ first last IN-RANGE? if swap 1+ swap then
      1+
   repeat drop ;

\ report view: prints the ledger without throwing
: LEDGER ( -- )
   SHOW-ON  WALK  SUMMARY ;

\ gate entry (enforcing): any code claimed by two different E- names fails
: STRICT ( -- )
   LEDGER
   BAD @ 0 > if 1 throw then ;

;package
