\ error-code-lint-core.f - global E- throw-code uniqueness lint.
\
\ Error codes are one global throw namespace: a thrown negative code is
\ ambiguous the moment two different E- names claim it. Three live collisions
\ motivated this lint (E-CUDA/E-FUSE at -5002, E-PTX-READBACK/E-MK-EVAL at
\ -5003, E-LMV-NOOUT+E-LMV-REG/E-ABL-NOSUB+E-ABL-CAP at -5210/-5211). The scan
\ walks tracked .f/.fs sources under src/, lib/, tools/, test/, and maki/ for
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
\   same constant reachable through two entry files).
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
require lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/source-lex.f

package ERROR-CODE-LINT
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

$80000 constant SRC-CAP   \ >= largest scanned source (checker.f grew past $40000)
512 constant PATH-CAP
2048 constant MAX-CLAIMS  \ the tree passed 1024 live claims; the table is sized above what it holds, and a full one dies rather than certifying a partial ledger
1024 constant MAX-RES
48 constant ZERO-C
36 constant DOLLAR-C
45 constant MINUS-C

create BUF  SRC-CAP allot
create PATH PATH-CAP allot
create DIGITS 32 allot

create CODES  MAX-CLAIMS cells allot   \ claimed negative codes
create NAMES  MAX-CLAIMS cells allot   \ claimant name intern ids
create OWNERS MAX-CLAIMS cells allot   \ claimant file intern ids

create RES-STEM  MAX-RES cells allot   \ block stem intern id (E-FS)
create RES-OWNER MAX-RES cells allot   \ declaring/owning file intern id
create RES-LO    MAX-RES cells allot   \ FIRST value (0 = not yet seen)
create RES-HI    MAX-RES cells allot   \ LAST value  (0 = not yet seen)
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

: CODE@  ( n -- n )  cells CODES + @ ;
: NAME@  ( n -- n )  cells NAMES + @ ;
: OWNER@ ( n -- n )  cells OWNERS + @ ;

\ exact (code, name) already recorded -> re-registration, not a new claim
: CLAIM-DUP? ( n n -- bool ) {: code:n name:n :}
   0 begin dup CLAIM# @ < while
      dup CODE@ code =
      over NAME@ name = and if drop LINT-TRUE exit then
      1+
   repeat drop
   LINT-FALSE ;

: CLAIM+ ( n n n -- ) {: code:n name:n file:n :}
   code name CLAIM-DUP? if exit then
   CLAIM# @ MAX-CLAIMS >= if s" error-code-lint: claim table full" 1 die then
   code CLAIM# @ cells CODES + !
   name CLAIM# @ cells NAMES + !
   file CLAIM# @ cells OWNERS + !
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
: RES-STEM@  ( n -- n )  cells RES-STEM + @ ;
: RES-OWNER@ ( n -- n )  cells RES-OWNER + @ ;
: RES-LO@    ( n -- n )  cells RES-LO + @ ;
: RES-HI@    ( n -- n )  cells RES-HI + @ ;

\ name minus its -FIRST / -LAST suffix (caller guarantees SENTINEL?)
: STEM$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u s" -FIRST" LINT-ENDS-WITH? if a u 6 - exit then
   a u 5 - ;

: FIRST-TOK? ( ptr u8 n -- bool )  s" -FIRST" LINT-ENDS-WITH? ;

: RES-FIND ( n n -- n ) {: stem:n file:n :}   \ row for (stem,file) or -1
   0 begin dup RES# @ < while
      dup RES-STEM@ stem =
      over RES-OWNER@ file = and if exit then
      1+
   repeat drop -1 ;

: RES-NEW ( n n -- n ) {: stem:n file:n :}    \ append an empty row, return idx
   RES# @ MAX-RES >= if s" error-code-lint: reservation table full" 1 die then
   RES# @ {: k:n :}
   stem k cells RES-STEM + !
   file k cells RES-OWNER + !
   0 k cells RES-LO + !
   0 k cells RES-HI + !
   k 1+ RES# !
   k ;

: RES-ROW ( n n -- n ) {: stem:n file:n :}    \ find-or-create (stem,file) row
   stem file RES-FIND dup 0 >= if exit then drop
   stem file RES-NEW ;

\ record one FIRST/LAST sentinel into its (stem,file) reservation row
: RES+ ( n ptr u8 n -- ) {: code:n a:ptr u:n :}
   a u STEM$ INTERN {: stem:n :}
   PATH$ INTERN {: file:n :}
   stem file RES-ROW {: k:n :}
   a u FIRST-TOK? if code k cells RES-LO + !
                  else code k cells RES-HI + ! then ;

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
: CLAIM-AT ( n -- ) {: k:n :}
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
   code  na nu INTERN  PATH$ INTERN  CLAIM+ ;

: UNKNOWN-KIND ( n -- ) {: k:n :}
   s" error-code-lint: " type PATH$ type
   s"  token " type k EMIT-U
   s" : unknown lexer token kind " type k LINT-LEX:KIND@ EMIT-U NL
   E-LEX throw ;

: SCAN-TOKENS ( -- )
   0 begin dup LINT-LEX:COUNT < while
      dup KNOWN-KIND? 0= if dup UNKNOWN-KIND then
      dup WORD? if dup CLAIM-AT then
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
      s"  claimed by '" type i NAME@ INTERN$ type
      s" ' (" type i OWNER@ INTERN$ type
      s" ) and '" type j NAME@ INTERN$ type
      s" ' (" type j OWNER@ INTERN$ type
      s" )" type NL
   then
   BAD @ 1+ BAD ! ;

: COLLIDE? ( n n -- bool ) {: i:n j:n :}
   i CODE@ j CODE@ =
   i NAME@ j NAME@ <> and ;

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

\ claim ci falls inside a COMPLETE reservation ri owned by another file
: FOREIGN? ( n n -- bool ) {: ci:n ri:n :}
   ri RES-LO@ {: first:n :}
   ri RES-HI@ {: last:n :}
   first 0= last 0= or if LINT-FALSE exit then
   ci CODE@ first last IN-RANGE? 0= if LINT-FALSE exit then
   ci OWNER@ ri RES-OWNER@ <> ;

: RES-HIT ( n n -- ) {: ci:n ri:n :}
   SHOW? @ if
      s" ERROR-CODE " type ci CODE@ EMIT-N
      s"  claimed by '" type ci NAME@ INTERN$ type
      s" ' (" type ci OWNER@ INTERN$ type
      s" ) inside reserved range " type ri RES-STEM@ INTERN$ type
      s" -FIRST..-LAST owned by (" type ri RES-OWNER@ INTERN$ type
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
   a u BUF SRC-CAP READ-FILE SCAN-TEXT ;

: ROOT ( ptr u8 n -- )
   [: SCAN-FILE ;] WALK-FILES ;

: WALK ( -- )
   RESET-LEDGER  0 FILE# !
   s" src/" ROOT
   s" lib/" ROOT
   s" tools/" ROOT
   s" test/" ROOT
   s" maki/" ROOT
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

\ live reservation rows whose two bounds are exactly first/last
: RESERVATIONS ( n n -- n ) {: first:n last:n :}
   0  0 begin dup RES# @ < while
      dup RES-LO@ first =
      over RES-HI@ last = and if swap 1+ swap then
      1+
   repeat drop ;

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
