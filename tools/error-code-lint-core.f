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
\ Scope and allowances (each deliberate):
\ - Negative codes only: positive `NN constant E-*` values are sysexits-style
\   process exit codes (64/70/74/76...), shared across tools by design.
\ - `E-*-FIRST` / `E-*-LAST` names are range sentinels (lib/errors.f blocks):
\   they alias their block's boundary member codes, not new throw identities.
\ - Identical (code, name) re-registrations are allowed (re-export shims; the
\   same constant reachable through two entry files).
\ - bootstrap/ is not walked: the frozen recovery seed is a pinned corpus in
\   its own process space; renumbering it would break the audited seed.
\
\ ERROR-CODE-LINT prints the ledger without throwing; ERROR-CODE-LINT-STRICT
\ throws on any finding and is the gate entrypoint.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/intern.f, and tools/lint/token.f.

$40000 constant ECL-CAP
512 constant ECL-PCAP
1024 constant ECL-MAX-CLAIMS
48 constant ECL-ZERO
36 constant ECL-DOLLAR
45 constant ECL-MINUS

create ECL-BUF  ECL-CAP allot
create ECL-PATH ECL-PCAP allot
create ECL-NBUF 32 allot

create ECL-CODES ECL-MAX-CLAIMS cells allot   \ claimed negative codes
create ECL-NAMES ECL-MAX-CLAIMS cells allot   \ claimant name intern ids
create ECL-FILEIDS ECL-MAX-CLAIMS cells allot \ claimant file intern ids

variable ECL-PATHU
variable ECL-INSTR                      \ inside an s" ... " string literal body
variable ECL-CLAIM#
variable ECL-BAD                        \ collision findings (claim pairs)
variable ECL-FILES#
variable ECL-REPORT?
variable ECL-ND#
variable ECL-QI
variable ECL-NV
variable ECL-I
variable ECL-J

: ECL-NL ( -- ) 10 emit ;

: ECL-REPORT! ( bool -- )  ECL-REPORT? ! ;
: ECL-REPORT-ON  ( -- )  LINT-TRUE  ECL-REPORT! ;
: ECL-REPORT-OFF ( -- )  LINT-FALSE ECL-REPORT! ;

\ unsigned decimal (digit-buffer print, as maki-dep/namespace lint)
: ECL-U. ( n -- )
   0 ECL-ND# !
   dup 0= if drop ECL-ZERO emit exit then
   begin dup 0 > while
      dup 10 mod ECL-ZERO + ECL-NBUF ECL-ND# @ + c!
      10 / ECL-ND# @ 1+ ECL-ND# !
   repeat drop
   begin ECL-ND# @ 0 > while
      ECL-ND# @ 1- ECL-ND# !
      ECL-NBUF ECL-ND# @ + c@ emit
   repeat ;

\ signed decimal (codes are negative)
: ECL-N. ( n -- )
   dup 0 < if ECL-MINUS emit 0 swap - then
   ECL-U. ;

\ odd number of `"` bytes -> the token flips the in-string state
: ECL-QUOTES-ODD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0  0 ECL-QI !
   begin ECL-QI @ u < while
      a ECL-QI @ BYTE@ 34 = if 1+ then
      ECL-QI @ 1+ ECL-QI !
   repeat
   1 and 0= 0= ;

\ ---- numeric-literal parse --------------------------------------------------
: ECL-DEC? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   u 0= if 0 LINT-FALSE exit then
   0 ECL-NV !
   0 begin dup u < while
      dup a + c@
      dup 48 < over 57 > or if 2drop 0 LINT-FALSE exit then
      ECL-NV @ 10 * + 48 - ECL-NV !
      1+
   repeat drop
   ECL-NV @ LINT-TRUE ;

: ECL-HEXDIG ( n -- n )   \ -1 when not a hex digit
   dup 48 >= over 57 <= and if 48 - exit then
   dup 65 >= over 70 <= and if 55 - exit then
   dup 97 >= over 102 <= and if 87 - exit then
   drop -1 ;

: ECL-HEX? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   u 0= if 0 LINT-FALSE exit then
   0 ECL-NV !
   0 begin dup u < while
      dup a + c@ ECL-HEXDIG
      dup 0 < if 2drop 0 LINT-FALSE exit then
      ECL-NV @ 16 * + ECL-NV !
      1+
   repeat drop
   ECL-NV @ LINT-TRUE ;

: ECL-MAG? ( ptr u8 n -- n bool ) {: a:ptr u:n :}   \ decimal or $hex magnitude
   u 0= if 0 LINT-FALSE exit then
   a c@ ECL-DOLLAR = if a 1 + u 1- ECL-HEX? exit then
   a u ECL-DEC? ;

\ negative numeric literal (-NNNN or -$HH) -> its value
: ECL-NEG? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   u 2 < if 0 LINT-FALSE exit then
   a c@ ECL-MINUS <> if 0 LINT-FALSE exit then
   a 1 + u 1- ECL-MAG? {: v:n ok:bool :}
   ok 0= if 0 LINT-FALSE exit then
   0 v - LINT-TRUE ;

\ ---- claim table ------------------------------------------------------------
: ECL-SENTINEL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" -FIRST" LINT-ENDS-WITH?  a u s" -LAST" LINT-ENDS-WITH? or ;

: ECL-CODE@ ( n -- n )  cells ECL-CODES + @ ;
: ECL-NAME@ ( n -- n )  cells ECL-NAMES + @ ;
: ECL-FILE@ ( n -- n )  cells ECL-FILEIDS + @ ;

\ exact (code, name) already recorded -> re-registration, not a new claim
: ECL-CLAIM-DUP? ( n n -- bool ) {: code:n name:n :}
   0 begin dup ECL-CLAIM# @ < while
      dup ECL-CODE@ code =
      over ECL-NAME@ name = and if drop LINT-TRUE exit then
      1+
   repeat drop
   LINT-FALSE ;

: ECL-CLAIM+ ( n n n -- ) {: code:n name:n file:n :}
   code name ECL-CLAIM-DUP? if exit then
   ECL-CLAIM# @ ECL-MAX-CLAIMS >= if s" error-code-lint: claim table full" 1 die then
   code ECL-CLAIM# @ cells ECL-CODES + !
   name ECL-CLAIM# @ cells ECL-NAMES + !
   file ECL-CLAIM# @ cells ECL-FILEIDS + !
   ECL-CLAIM# @ 1+ ECL-CLAIM# ! ;

: ECL-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u ECL-PCAP > if s" error-code-lint: path too long" 1 die then
   a ECL-PATH u LINT-BMOVE  u ECL-PATHU ! ;

\ ---- token walk -------------------------------------------------------------
\ token i as `<negative-number> constant E-NAME` claim (outside strings)
: ECL-TOK-CLAIM ( n -- ) {: i:n :}
   i 2 + TN# @ >= if exit then
   i 1+ TOK s" constant" LINT-STR=CI 0= if exit then
   i 2 + TOK {: nptr:ptr nu:n :}
   nptr nu s" E-" LINT-PREFIX? 0= if exit then
   nptr nu ECL-SENTINEL? if exit then
   i TOK ECL-NEG? {: code:n ok:bool :}
   ok 0= if exit then
   code  nptr nu INTERN  ECL-PATH ECL-PATHU @ INTERN  ECL-CLAIM+ ;

: ECL-STEP ( n -- n ) {: i:n :}
   i TOK {: tp:ptr tu:n :}
   ECL-INSTR @ if                                    \ skip string bodies wholesale
      tp tu ECL-QUOTES-ODD? if ECL-INSTR @ 0= ECL-INSTR ! then
      i 1+ exit then
   i ECL-TOK-CLAIM
   tp tu ECL-QUOTES-ODD? if ECL-INSTR @ 0= ECL-INSTR ! then
   i 1+ ;

: ECL-SCAN-TOKENS ( -- )
   0 ECL-INSTR !
   0 begin dup TN# @ < while ECL-STEP repeat drop ;

: ECL-SCAN-STR ( ptr u8 n -- ) {: a:ptr u:n :}
   LINT-TRUE PARENS? !
   a u TOKENIZE
   ECL-SCAN-TOKENS ;

\ ---- findings ---------------------------------------------------------------
: ECL-HIT ( n n -- ) {: i:n j:n :}
   ECL-REPORT? @ if
      s" ERROR-CODE " type i ECL-CODE@ ECL-N.
      s"  claimed by '" type i ECL-NAME@ INTERN$ type
      s" ' (" type i ECL-FILE@ INTERN$ type
      s" ) and '" type j ECL-NAME@ INTERN$ type
      s" ' (" type j ECL-FILE@ INTERN$ type
      s" )" type ECL-NL
   then
   ECL-BAD @ 1+ ECL-BAD ! ;

: ECL-COLLIDE? ( n n -- bool ) {: i:n j:n :}
   i ECL-CODE@ j ECL-CODE@ =
   i ECL-NAME@ j ECL-NAME@ <> and ;

\ one finding per colliding claim pair
: ECL-FINDINGS ( -- )
   0 ECL-I !
   begin ECL-I @ ECL-CLAIM# @ < while
      ECL-I @ 1+ ECL-J !
      begin ECL-J @ ECL-CLAIM# @ < while
         ECL-I @ ECL-J @ ECL-COLLIDE? if ECL-I @ ECL-J @ ECL-HIT then
         ECL-J @ 1+ ECL-J !
      repeat
      ECL-I @ 1+ ECL-I !
   repeat ;

\ findings from scanning one string in isolation (reset -> scan -> pair count)
: ECL-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   ECL-REPORT? @ {: report:bool :}
   ECL-REPORT-OFF
   0 ECL-BAD !  0 ECL-CLAIM# !
   s" <test>" ECL-PATH!
   a u ECL-SCAN-STR
   ECL-FINDINGS
   report ECL-REPORT!
   ECL-BAD @ ;

\ ---- file walk --------------------------------------------------------------
: ECL-SRC? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT?  a u s" .fs" HAS-EXT? or ;

: ECL-SCAN-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u ECL-SRC? 0= if exit then
   a u ECL-PATH!
   ECL-FILES# @ 1+ ECL-FILES# !
   a u ECL-BUF ECL-CAP READ-FILE ECL-SCAN-STR ;

: ECL-ROOT ( ptr u8 n -- )
   [: ECL-SCAN-FILE ;] WALK-FILES ;

: ECL-RUN ( -- )
   0 ECL-BAD !  0 ECL-FILES# !  0 ECL-CLAIM# !
   s" src/" ECL-ROOT
   s" lib/" ECL-ROOT
   s" tools/" ECL-ROOT
   s" test/" ECL-ROOT
   s" maki/" ECL-ROOT
   ECL-FINDINGS ;

: ECL-SUMMARY ( -- )
   s" error-code-lint: " type
   ECL-FILES# @ ECL-U. s"  file(s), " type
   ECL-CLAIM# @ ECL-U. s"  claim(s), " type
   ECL-BAD    @ ECL-U. s"  collision(s)" type ECL-NL ;

\ report view: prints the ledger without throwing
: ERROR-CODE-LINT ( -- )
   ECL-REPORT-ON  ECL-RUN  ECL-SUMMARY ;

\ gate entry (enforcing): any code claimed by two different E- names fails
: ERROR-CODE-LINT-STRICT ( -- )
   ERROR-CODE-LINT
   ECL-BAD @ 0 > if 1 throw then ;
