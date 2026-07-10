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
\   Each FIRST/LAST pair (matched by shared stem, e.g. E-FS-FIRST/E-FS-LAST) also
\   reserves the inclusive [FIRST,LAST] code range for the file that declares it.
\   A negative E- code claimed INSIDE another file's reserved range is a foreign
\   claim and is flagged, even before the owning block mints that exact member.
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

$80000 constant ECL-CAP   \ >= largest scanned source (checker.f grew past $40000)
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

1024 constant ECL-MAX-RES
create ECL-RES-STEM  ECL-MAX-RES cells allot   \ block stem intern id (E-FS)
create ECL-RES-FILE  ECL-MAX-RES cells allot   \ declaring/owning file intern id
create ECL-RES-FIRST ECL-MAX-RES cells allot   \ FIRST value (0 = not yet seen)
create ECL-RES-LAST  ECL-MAX-RES cells allot   \ LAST value  (0 = not yet seen)
variable ECL-RES#

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

\ ---- reservation table (E-*-FIRST/E-*-LAST range blocks) --------------------
\ A FIRST/LAST pair reserves the inclusive numeric range between its two member
\ codes for the file that declares it (lib/errors.f owns every stdlib block).
\ Pairs are keyed by shared stem (E-FS-FIRST/E-FS-LAST -> E-FS) and declaring
\ file, so two files can each own a same-named block.
: ECL-RES-STEM@  ( n -- n )  cells ECL-RES-STEM + @ ;
: ECL-RES-FILE@  ( n -- n )  cells ECL-RES-FILE + @ ;
: ECL-RES-FIRST@ ( n -- n )  cells ECL-RES-FIRST + @ ;
: ECL-RES-LAST@  ( n -- n )  cells ECL-RES-LAST + @ ;

\ name minus its -FIRST / -LAST suffix (caller guarantees ECL-SENTINEL?)
: ECL-STEM$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u s" -FIRST" LINT-ENDS-WITH? if a u 6 - exit then
   a u 5 - ;

: ECL-RES-FIRST-TOK? ( ptr u8 n -- bool )  s" -FIRST" LINT-ENDS-WITH? ;

: ECL-RES-FIND ( n n -- n ) {: stem:n file:n :}   \ row for (stem,file) or -1
   0 begin dup ECL-RES# @ < while
      dup ECL-RES-STEM@ stem =
      over ECL-RES-FILE@ file = and if exit then
      1+
   repeat drop -1 ;

: ECL-RES-NEW ( n n -- n ) {: stem:n file:n :}    \ append an empty row, return idx
   ECL-RES# @ ECL-MAX-RES >= if s" error-code-lint: reservation table full" 1 die then
   ECL-RES# @ {: k:n :}
   stem k cells ECL-RES-STEM + !
   file k cells ECL-RES-FILE + !
   0 k cells ECL-RES-FIRST + !
   0 k cells ECL-RES-LAST + !
   k 1+ ECL-RES# !
   k ;

: ECL-RES-ROW ( n n -- n ) {: stem:n file:n :}    \ find-or-create (stem,file) row
   stem file ECL-RES-FIND dup 0 >= if exit then drop
   stem file ECL-RES-NEW ;

\ record one FIRST/LAST sentinel into its (stem,file) reservation row
: ECL-RES+ ( n ptr u8 n -- ) {: code:n a:ptr u:n :}
   a u ECL-STEM$ INTERN {: stem:n :}
   ECL-PATH ECL-PATHU @ INTERN {: file:n :}
   stem file ECL-RES-ROW {: k:n :}
   a u ECL-RES-FIRST-TOK? if code k cells ECL-RES-FIRST + !
                          else code k cells ECL-RES-LAST + ! then ;

\ ---- token walk -------------------------------------------------------------
\ token i as `<negative-number> constant E-NAME` claim (outside strings)
: ECL-TOK-CLAIM ( n -- ) {: i:n :}
   i 2 + TN# @ >= if exit then
   i 1+ TOK s" constant" LINT-STR=CI 0= if exit then
   i 2 + TOK {: nptr:ptr nu:n :}
   nptr nu s" E-" LINT-PREFIX? 0= if exit then
   i TOK ECL-NEG? {: code:n ok:bool :}
   ok 0= if exit then
   nptr nu ECL-SENTINEL? if code nptr nu ECL-RES+ exit then
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

\ ---- foreign-range findings -------------------------------------------------
: ECL-MINMAX ( n n -- n n )   \ order two bounds ascending
   2dup > if swap then ;

: ECL-INRANGE? ( n n n -- bool ) {: code:n first:n last:n :}
   first last ECL-MINMAX {: lo:n hi:n :}
   code lo >= code hi <= and ;

\ claim ci falls inside a COMPLETE reservation ri owned by another file
: ECL-CLAIM-FOREIGN? ( n n -- bool ) {: ci:n ri:n :}
   ri ECL-RES-FIRST@ {: first:n :}
   ri ECL-RES-LAST@ {: last:n :}
   first 0= last 0= or if LINT-FALSE exit then
   ci ECL-CODE@ first last ECL-INRANGE? 0= if LINT-FALSE exit then
   ci ECL-FILE@ ri ECL-RES-FILE@ <> ;

: ECL-RES-HIT ( n n -- ) {: ci:n ri:n :}
   ECL-REPORT? @ if
      s" ERROR-CODE " type ci ECL-CODE@ ECL-N.
      s"  claimed by '" type ci ECL-NAME@ INTERN$ type
      s" ' (" type ci ECL-FILE@ INTERN$ type
      s" ) inside reserved range " type ri ECL-RES-STEM@ INTERN$ type
      s" -FIRST..-LAST owned by (" type ri ECL-RES-FILE@ INTERN$ type
      s" )" type ECL-NL
   then
   ECL-BAD @ 1+ ECL-BAD ! ;

\ one finding per (claim, foreign reservation) pair
: ECL-RES-FINDINGS ( -- )
   0 ECL-I !
   begin ECL-I @ ECL-CLAIM# @ < while
      0 ECL-J !
      begin ECL-J @ ECL-RES# @ < while
         ECL-I @ ECL-J @ ECL-CLAIM-FOREIGN? if ECL-I @ ECL-J @ ECL-RES-HIT then
         ECL-J @ 1+ ECL-J !
      repeat
      ECL-I @ 1+ ECL-I !
   repeat ;

\ findings from scanning one string in isolation (reset -> scan -> pair count)
: ECL-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   ECL-REPORT? @ {: report:bool :}
   ECL-REPORT-OFF
   0 ECL-BAD !  0 ECL-CLAIM# !  0 ECL-RES# !
   s" <test>" ECL-PATH!
   a u ECL-SCAN-STR
   ECL-FINDINGS ECL-RES-FINDINGS
   report ECL-REPORT!
   ECL-BAD @ ;

\ two-file finding count: OWNER source declares the block, FOREIGN source mints
\ its claims under a different path, so a cross-file range claim can be tested.
: ECL-COUNT2 ( ptr u8 n ptr u8 n -- n ) {: ao:ptr auo:n af:ptr auf:n :}
   ECL-REPORT? @ {: report:bool :}
   ECL-REPORT-OFF
   0 ECL-BAD !  0 ECL-CLAIM# !  0 ECL-RES# !
   s" owner.f" ECL-PATH!    ao auo ECL-SCAN-STR
   s" foreign.f" ECL-PATH!  af auf ECL-SCAN-STR
   ECL-FINDINGS ECL-RES-FINDINGS
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
   0 ECL-BAD !  0 ECL-FILES# !  0 ECL-CLAIM# !  0 ECL-RES# !
   s" src/" ECL-ROOT
   s" lib/" ECL-ROOT
   s" tools/" ECL-ROOT
   s" test/" ECL-ROOT
   s" maki/" ECL-ROOT
   ECL-FINDINGS ECL-RES-FINDINGS ;

: ECL-SUMMARY ( -- )
   s" error-code-lint: " type
   ECL-FILES# @ ECL-U. s"  file(s), " type
   ECL-CLAIM# @ ECL-U. s"  claim(s), " type
   ECL-RES#   @ ECL-U. s"  reservation(s), " type
   ECL-BAD    @ ECL-U. s"  finding(s)" type ECL-NL ;

\ report view: prints the ledger without throwing
: ERROR-CODE-LINT ( -- )
   ECL-REPORT-ON  ECL-RUN  ECL-SUMMARY ;

\ gate entry (enforcing): any code claimed by two different E- names fails
: ERROR-CODE-LINT-STRICT ( -- )
   ERROR-CODE-LINT
   ECL-BAD @ 0 > if 1 throw then ;
