\ verify-source.f - pre-compile checked source verifier.
\
\ Load after checker/render/hook support. This scanner verifies colon
\ definitions with CHECK! and records top-level defining words that the checker
\ needs before those definitions are compiled by the native compiler.

package VERIFY

variable SOURCE-A
variable SOURCE-U
variable SCAN-I
variable SKIP-STRINGS
variable FOUND
variable TOKEN-START
variable TOKEN-A
variable TOKEN-U
variable BODY-U
variable LINE-N
variable LINE-START
variable TOKEN-LINE
variable TOKEN-COL
variable TOKEN-BYTE
variable BASE-LINE
variable BASE-COL
variable BASE-BYTE
variable STR-PREV-A
variable STR-PREV-U
variable STR-LAST-A
variable STR-LAST-U
variable TOP-PREV-A
variable TOP-PREV-U
variable TOP-CUR-A
variable TOP-CUR-U

create BODY-BUF BODYBUF-CAP allot

: SOURCE@ ( -- ptr u8 )
   SOURCE-A 0 ptr-field @ ;

: BASE-RESET ( -- )
   1 BASE-LINE !
   1 BASE-COL !
   0 BASE-BYTE ! ;

: SOURCE! ( ptr u8 n -- )
   BASE-RESET
   SOURCE-U !
   SOURCE-A ! ;

: SOURCE-AT! ( ptr u8 n n n n -- ) {: a:ptr u:n line:n col:n byte:n :}
   a u SOURCE!
   line BASE-LINE !
   col BASE-COL !
   byte BASE-BYTE ! ;

: SCAN-RESET ( -- )
   0 SCAN-I !
   1 LINE-N !
   0 LINE-START ! ;

: SCAN-C@ ( -- n )
   SOURCE@ SCAN-I @ + c@ ;

: SCAN-C+ ( -- n )
   SCAN-C@ {: c:n :}
   SCAN-I @ 1 + SCAN-I !
   c 10 = if
      LINE-N @ 1 + LINE-N !
      SCAN-I @ LINE-START !
   then
   c ;

: TOKEN-START! ( -- )
   SCAN-I @ TOKEN-START !
   BASE-LINE @ LINE-N @ + 1 - TOKEN-LINE !
   SCAN-I @ LINE-START @ - 1 + {: col:n :}
   LINE-N @ 1 = if BASE-COL @ col + 1 - else col then TOKEN-COL !
   BASE-BYTE @ SCAN-I @ + TOKEN-BYTE ! ;

: TOKEN-ORIGIN! ( -- )
   TOKEN-LINE @ TOKEN-COL @ TOKEN-BYTE @ DIAG-ORIGIN! ;

: SKIP-WS ( -- )
   begin SCAN-I @ SOURCE-U @ < if SCAN-C@ 33 < else 0 0= 0= then while
      SCAN-C+ drop
   repeat ;

: SKIP-PAST ( n -- ) {: ch:n :}
   0 FOUND !
   begin SCAN-I @ SOURCE-U @ < while
      SCAN-C+ ch = if -1 FOUND ! exit then
   repeat ;

: NEXT-RAW ( -- ptr u8 n )
   SKIP-WS
   SCAN-I @ SOURCE-U @ >= if SOURCE@ 0 exit then
   TOKEN-START!
   begin SCAN-I @ SOURCE-U @ < if SCAN-C@ 32 > else 0 0= 0= then while
      SCAN-C+ drop
   repeat
   SOURCE@ TOKEN-START @ +  SCAN-I @ TOKEN-START @ - ;

: SC-LEAD? ( n -- bool )
   dup $73 = over $53 = or over $63 = or swap $43 = or ;

: STRING-LEAD? ( n -- bool )
   dup SC-LEAD? swap $2E = or ;

: NORMAL-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> IF 0 0= 0= EXIT THEN
   a 1 BYTE@ $22 <> IF 0 0= 0= EXIT THEN
   a 0 BYTE@ STRING-LEAD? ;

: ESCAPED-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 3 <> IF 0 0= 0= EXIT THEN
   a 1 BYTE@ $5C <> IF 0 0= 0= EXIT THEN
   a 2 BYTE@ $22 <> IF 0 0= 0= EXIT THEN
   a 0 BYTE@ STRING-LEAD? ;

: STRING-OPENER? ( ptr u8 n -- bool )
   2dup NORMAL-STRING-OPENER? IF 2drop 0 0= EXIT THEN
   ESCAPED-STRING-OPENER? ;

: SKIP-ESCAPED-QUOTE ( -- )
   0 FOUND !
   begin SCAN-I @ SOURCE-U @ < while
      SCAN-C+
      dup 92 = if
         drop
         SCAN-I @ SOURCE-U @ < if SCAN-C+ drop then
      else
         34 = if -1 FOUND ! exit then
      then
   repeat ;

\ Skipped top-level string literals feed a two-slot ring so a bare top-level
\ `s" NAME" s" SIG" TRUST` (strings the scanner would otherwise discard) can be
\ replayed as a trust. The ring resets per NEXT-SCAN call, so at a TRUST token it
\ holds exactly the two preceding literals from the same statement.
: STR-RING-RESET ( -- )
   0 STR-PREV-A !  0 STR-PREV-U !
   0 STR-LAST-A !  0 STR-LAST-U ! ;

: STR-RING-PUSH ( ptr u8 n -- ) {: a:ptr u:n :}
   STR-LAST-A @ STR-PREV-A !
   STR-LAST-U @ STR-PREV-U !
   a STR-LAST-A !
   u STR-LAST-U ! ;

: RECORD-SKIPPED-STRING ( n -- ) {: pfx:n :}
   SCAN-I @ TOKEN-START @ - pfx - 1 - {: vlen:n :}
   vlen 0 < IF EXIT THEN
   SOURCE@ TOKEN-START @ + pfx + vlen STR-RING-PUSH ;

: NEXT ( -- ptr u8 n )
   BEGIN
      NEXT-RAW
      dup 0= IF EXIT THEN
      2dup 1 = swap c@ 92 = and IF 2drop 10 SKIP-PAST ELSE
      2dup 1 = swap c@ 40 = and IF 2drop 41 SKIP-PAST ELSE
      SKIP-STRINGS @ 0= 0= IF
         2dup ESCAPED-STRING-OPENER? IF 2drop SKIP-ESCAPED-QUOTE 4 RECORD-SKIPPED-STRING ELSE
         2dup NORMAL-STRING-OPENER? IF 2drop 34 SKIP-PAST 3 RECORD-SKIPPED-STRING ELSE EXIT THEN THEN
      ELSE EXIT THEN
      THEN THEN
   AGAIN ;

: NEXT-SCAN ( -- ptr u8 n )
   -1 SKIP-STRINGS !
   STR-RING-RESET
   NEXT ;

: NEXT-BODY ( -- ptr u8 n )
   0 SKIP-STRINGS !
   NEXT ;

: RAW! ( -- )
   NEXT-RAW  TOKEN-U !  TOKEN-A ! ;

: BODY! ( -- )
   NEXT-BODY  TOKEN-U !  TOKEN-A ! ;

\ A body buffer that cannot represent its input RAISES; it never truncates.
\
\ This used to skip the one token that would not fit and keep appending the
\ shorter ones after it, on the reasoning that an over-cap body would be caught
\ downstream by the engine's own TDECL-CAP anyway. That reasoning died with the
\ registration-only replay entries: they parse whatever tokens arrive and have no
\ length gate, so a dropped token produces a declaration that is WELL-FORMED and
\ WRONG. Measured on the previous commit, a 1302-variant compact ENUM whose body
\ exceeds BODYBUF-CAP replayed with rc 0 and registered 1142 variants — 160
\ silently missing, and every tag after the first gap shifted, which is exactly
\ the kind of quiet registry divergence the parity suite exists to prevent.
\
\ The code is the declaration layer's own "declaration too long" (sumtype.f
\ E-TDECL-CAP), re-declared locally the way structure-decl.f and enum-decl.f
\ re-declare their reject codes, because that is precisely the condition: this
\ source is too long for the path that carries it. Source that trips this bound
\ also trips the engine's TDECL-CAP, so both paths answer the same code.
7118 constant E-VS-BODY-CAP

: BODY-APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   BODY-U @ u + 1 + BODYBUF-CAP > IF E-VS-BODY-CAP throw THEN
   0 BEGIN dup u < WHILE
      dup a + c@  BODY-BUF BODY-U @ + c!
      BODY-U @ 1 + BODY-U !
      1 +
   REPEAT drop
   32 BODY-BUF BODY-U @ + c!  BODY-U @ 1 + BODY-U ! ;

: MAYBE-SIGNATURE ( -- )
   SKIP-WS
   SCAN-I @ SOURCE-U @ < IF
      SOURCE@ SCAN-I @ + c@ 40 = IF
         SCAN-I @ TOKEN-START !
         41 SKIP-PAST
         FOUND @ 0= IF s" verify-source: unterminated signature" 74 die THEN
         SOURCE@ TOKEN-START @ +  SCAN-I @ TOKEN-START @ -  BODY-APPEND
      THEN
   THEN ;

: REQUIRE-SIGNATURE ( -- ptr u8 n )
   SKIP-WS
   SCAN-I @ SOURCE-U @ >= IF s" verify-source: missing signature" 74 die THEN
   SOURCE@ SCAN-I @ + c@ 40 <> IF s" verify-source: missing signature" 74 die THEN
   SCAN-I @ 1+ TOKEN-START !
   41 SKIP-PAST
   FOUND @ 0= IF s" verify-source: unterminated signature" 74 die THEN
   SOURCE@ TOKEN-START @ + SCAN-I @ TOKEN-START @ - 1 - ;

: STRING-REST ( ptr u8 n -- ptr u8 n ) {: opener:ptr openeru:n :}
   SCAN-I @ {: start:n :}
   opener openeru ESCAPED-STRING-OPENER? IF
      SKIP-ESCAPED-QUOTE
   ELSE
      34 SKIP-PAST
   THEN
   FOUND @ 0= IF s" verify-source: unterminated string" 74 die THEN
   SOURCE@ start + SCAN-I @ start - ;

: APPEND-STRING ( ptr u8 n -- ) {: a:ptr u:n :}
   a u BODY-APPEND
   a u STRING-REST BODY-APPEND ;

: SKIP-STRING-REST ( ptr u8 n -- )
   STRING-REST 2drop ;

: PARSE-NEXT? ( ptr u8 n -- bool )
   2dup s" char" CORE-STR= IF 2drop 0 0= exit THEN
   s" [char]" CORE-STR= ;

: APPEND-NEXT-BODY ( -- )
   BODY!
   TOKEN-U @ 0= IF s" verify-source: missing parsed token" 74 die THEN
   TOKEN-A @ TOKEN-U @ BODY-APPEND ;

: APPEND-BODY-TOKEN ( -- )
   TOKEN-A @ TOKEN-U @ PARSE-NEXT? IF
      TOKEN-A @ TOKEN-U @ BODY-APPEND
      APPEND-NEXT-BODY
      exit
   THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF
      TOKEN-A @ TOKEN-U @ APPEND-STRING
   ELSE
      TOKEN-A @ TOKEN-U @ BODY-APPEND
   THEN ;

: SKIP-NEXT-BODY ( -- )
   BODY!
   TOKEN-U @ 0= IF s" verify-source: missing parsed token" 74 die THEN ;

: SKIP-BODY-TOKEN ( -- )
   TOKEN-A @ TOKEN-U @ PARSE-NEXT? IF SKIP-NEXT-BODY exit THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF TOKEN-A @ TOKEN-U @ SKIP-STRING-REST THEN ;

TRUSTED: CHECK-BODY ( ptr u8 n -- n )
   CHECK! dup 1 = JSON-DIAGS @ 0= and DIAG-QUIET @ 0= and IF DIAGXT THEN ;

\ Checker-internal mode flag read; the checker registry does not publish
\ MULTI-ERR? to later checked loads, so this rides the same trusted boundary
\ class as CHECK-BODY above.
TRUSTED: MULTI-ERR-MODE? ( -- bool )
   MULTI-ERR? ;

\ In MULTI-ERR mode a verdict-0 reject RETURNS instead of throwing: CHECK has
\ already emitted the diagnostic, counted MULTI-ERR-N, and recorded the
\ declared signature (no-cascade), so the scan continues at the next
\ definition. Verdict-1 (uncheckable) still throws in BOTH modes: MULTI-ERR-N
\ counts verdict-0 only, so continuing past uncheckables would let an
\ all-uncheckable file exit 0 - fail-open.
: VERIFY-BODY ( -- )
   BODY-BUF BODY-U @ CHECK-BODY {: v:n :}
   v -1 = IF EXIT THEN
   v 0 = MULTI-ERR-MODE? and IF EXIT THEN
   70 throw ;

TRUSTED: CHECK-DOES-BODY ( ptr u8 n ptr u8 n -- n )
   CHECK-DOES! ;

: VERIFY-DOES-BODY ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   BODY-BUF BODY-U @ sig sigu CHECK-DOES-BODY {: v:n :}
   v -1 = IF EXIT THEN
   v 0 = MULTI-ERR-MODE? and IF EXIT THEN
   70 throw ;

: VERIFY-DOES ( -- )
   VERIFY-BODY
   REQUIRE-SIGNATURE {: sig:ptr sigu:n :}
   0 BODY-U !
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: unterminated does body" 74 die THEN
      BODY-U @ 0= if TOKEN-ORIGIN! then
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF sig sigu VERIFY-DOES-BODY EXIT THEN
      APPEND-BODY-TOKEN
   AGAIN ;

TRUSTED: TRUST-SIGNATURE ( ptr u8 n ptr u8 n -- )
   TRUST ;

: CAST-TRUST ( -- )
   DTC-NAME$ DTC-SIG$ TRUST-SIGNATURE ;

: RECORD-CAST-IN ( ptr u8 n ptr u8 n -- )
   DTC-BUILD-IN
   CAST-TRUST ;

: RECORD-CAST-OUT ( ptr u8 n ptr u8 n -- )
   DTC-BUILD-OUT
   CAST-TRUST ;

: FOLD-C ( n -- n )
   dup $41 < IF EXIT THEN
   dup $5A > IF EXIT THEN
   $20 or ;

: STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> IF 0 0= 0= EXIT THEN
   0 BEGIN dup u < WHILE
      dup a + c@ FOLD-C
      over b + c@ FOLD-C <> IF drop 0 0= 0= EXIT THEN
      1+
   REPEAT drop 0 0= ;

: TRUST-NEXT ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   NEXT-SCAN
   dup 0= IF s" verify-source: missing defining-word name" 74 die THEN
   sig sigu TRUST-SIGNATURE ;

\ SIG-RAW-DEFINER! is a checker-internal word (no charted effect), so it rides a
\ TRUSTED: boundary here exactly like MULTI-ERR-MODE? above.
TRUSTED: SIG-RAW-MODE! ( n -- ) SIG-RAW-DEFINER! ;

\ RAW-TRUST-NEXT: like TRUST-NEXT, but registers the created word's effect with
\ TVK-RAW type vars (SIG-RAW-MODE! brackets the checker's signature parse).
\ Used for the raw storage definers create/variable/constant/PTR-VARIABLE so a
\ fetch from their raw cell yields a RAW value that cannot launder into a nominal
\ atom or family (habu-nominal-storage-raw, VALUE side).
: RAW-TRUST-NEXT ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   NEXT-SCAN
   dup 0= IF s" verify-source: missing defining-word name" 74 die THEN
   -1 SIG-RAW-MODE!
   sig sigu TRUST-SIGNATURE
   0 SIG-RAW-MODE! ;

: TRUST-DEFER-SIGNATURE ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu REQUIRE-SIGNATURE TRUST-SIGNATURE
   name nameu CHECKER-DEFER ;

: TRUST-DEFER ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing defer name" 74 die THEN
   name nameu TRUST-DEFER-SIGNATURE ;

: SKIP-TRUSTED-BODY ( -- )
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: unterminated trusted definition" 74 die THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF EXIT THEN
      SKIP-BODY-TOKEN
   AGAIN ;

: TRUSTED-DEFINITION ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing trusted name" 74 die THEN
   name nameu REQUIRE-SIGNATURE TRUST-SIGNATURE
   SKIP-TRUSTED-BODY ;

: UNDEFINE-WORD ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing undefine name" 74 die THEN
   name nameu CHECKER-UNDEFINE ;

: RECORD-PACKAGE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing package name" 74 die THEN
   name nameu CHECKER-PACKAGE ;

: RECORD-PUBLIC ( -- )
   CHECKER-PUBLIC ;

: RECORD-PRIVATE ( -- )
   CHECKER-PRIVATE ;

: RECORD-END-PACKAGE ( -- )
   CHECKER-END-PACKAGE ;

\ DEFTYPE NAME declares a value nominal (lib/type/deftype.f): a
\ package-scoped arity-0 type family whose lowercase tail is the surface name
\ folded (SERIAL -> serial) and whose converter pair >NAME ( n -- tail ) /
\ NAME>N ( tail -- n ) keeps a plain n from standing in for the nominal. The
\ static recorder mirrors the runtime mint: register the family, then trust the
\ two derived converter signatures so later definitions that use the tail and
\ the converters verify without loading deftype.f.
$40 constant NOM-TAIL-CAP
create NOM-TAIL-BUF NOM-TAIL-CAP allot
variable NOM-TAIL-U

\ MANGLE ( ptr u8 n -- ptr u8 n ) folds the UPPER-CASE surface name to the
\ lowercase family tail, matching deftype.f's ASCII-LOWER fold.
: MANGLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u NOM-TAIL-CAP > IF s" verify-source: nominal name too long" 74 die THEN
   0 NOM-TAIL-U !
   0 BEGIN dup u < WHILE
      dup a + c@ FOLD-C  NOM-TAIL-BUF NOM-TAIL-U @ + c!
      NOM-TAIL-U @ 1 + NOM-TAIL-U !  1+
   REPEAT drop
   NOM-TAIL-BUF NOM-TAIL-U @ ;

: RECORD-DEFTYPE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing nominal name" 74 die THEN
   name nameu MANGLE {: tail:ptr tailu:n :}
   tail tailu s" 0" CHECKER-DEFFAMILY
   name nameu tail tailu RECORD-CAST-IN
   name nameu tail tailu RECORD-CAST-OUT ;

: RECORD-DEFLINEAR ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing deflinear name" 74 die THEN
   name nameu CHECKER-DEFLINEAR ;

: VALUE-RECORD-END? ( ptr u8 n -- bool )
   s" END-VALUE-RECORD" STR=CI ;

: SUMTYPE-END? ( ptr u8 n -- bool )
   s" ;SUMTYPE" STR=CI ;

\ Missing name/arity are reported by CHECKER-DEFFAMILY through the declaration
\ packet (E-BAD-DECLARATION), matching the native path -- no raw pre-check die (§24).
: RECORD-NEWTYPE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   NEXT-SCAN {: ar:ptr aru:n :}
   name nameu ar aru CHECKER-DEFFAMILY ;

: RECORD-SUMTYPE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   0 BODY-U !
   BEGIN
      NEXT-SCAN
      dup 0= IF                        \ EOF before ;SUMTYPE -> declaration packet (§24)
         2drop
         name nameu BODY-BUF BODY-U @ CHECKER-DEFSUM-NOEND
         EXIT
      THEN
      2dup SUMTYPE-END? IF
         2drop
         name nameu BODY-BUF BODY-U @ CHECKER-DEFSUM
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: ENUM-END? ( ptr u8 n -- bool )
   s" ;ENUM" STR=CI ;

\ The token reader for a REPLAYED declaration window, and the reason it is not
\ NEXT-SCAN.
\
\ NEXT-SCAN launders comments: a bare `\` makes it skip to the newline and a bare
\ `(` makes it skip to the `)`. That is right for scanning a FILE, where comments
\ are inert between definitions. It is wrong inside a declaration body, because
\ the engine does not scan a declaration body — the live keyword reads it with
\ `parse-name`, which has no comment rule at all, so `\` and `(` arrive as
\ ordinary tokens and hit the name gate. Measured: `ENUM c red \ note` through
\ the live front end rejects 7101 "name must be a lowercase tail at '\'".
\ Stripping them here would let a replay ACCEPT source the engine refuses, and
\ register a family that can never exist.
\
\ NEXT-RAW is exactly `parse-name`'s rule — the next whitespace-delimited token,
\ no comment or string interpretation — so the replayed body is the same token
\ sequence the live keyword would have read. The substitution is scoped to the
\ two replay windows below; every other scan in this file keeps NEXT-SCAN, since
\ outside a declaration comments really are inert.
: DECL-TOKEN ( -- ptr u8 n ) NEXT-RAW ;

\ Registration-only replay of `ENUM name .. ;ENUM` (mirrors RECORD-SUMTYPE):
\ buffer the body through ;ENUM and register the family from it.
\
\ This drives the unified ENUM front end's replay entry rather than sumtype.f's
\ CHECKER-DEFENUM, which the type-DSL cutover deletes. It also widens what this
\ arm understands: the legacy entry only ever read a compact list of bare
\ variant names, while the replay entry runs the real grammar and so accepts the
\ full `arity VARIANT name FIELD f t ;VARIANT` form too. The terminator is
\ buffered with the body because the front end parses its own terminator.
: RECORD-ENUM ( -- )
   DECL-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing enum name" 74 die THEN
   0 BODY-U !
   BEGIN
      DECL-TOKEN
      dup 0= IF s" verify-source: missing ;ENUM" 74 die THEN
      2dup ENUM-END? IF
         BODY-APPEND
         name nameu BODY-BUF BODY-U @ ENUM-DECL:ED-REPLAY
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: STRUCTURE-DECL-END? ( ptr u8 n -- bool )
   s" ;STRUCTURE" STR=CI ;

\ Registration-only replay of the unified `STRUCTURE name arity FIELD f t ..
\ ;STRUCTURE` (mirrors RECORD-ENUM). Distinct from RECORD-STRUCTURE above, which
\ handles the Forth-standard `BEGIN-STRUCTURE .. END-STRUCTURE` layout facility;
\ these are different declarations that happen to share a word stem.
\
\ Without this arm a STRUCTURE family was never registered on this path, so a
\ later signature or payload type naming it could not resolve. Registration
\ includes the family's MAKE/UNMAKE variant rows and constructor package, so
\ `FAMILY:MAKE` in the same source resolves; no dictionary word is defined.
: RECORD-STRUCTURE-DECL ( -- )
   DECL-TOKEN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing structure name" 74 die THEN
   0 BODY-U !
   BEGIN
      DECL-TOKEN
      dup 0= IF s" verify-source: missing ;STRUCTURE" 74 die THEN
      2dup STRUCTURE-DECL-END? IF
         BODY-APPEND
         name nameu BODY-BUF BODY-U @ STRUCTURE-DECL:SD-REPLAY
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: PRODUCT-END? ( ptr u8 n -- bool )
   s" ;PRODUCT" STR=CI ;

\ Metadata-only replay of `PRODUCT name arity FIELD f t .. ;PRODUCT` (mirrors
\ RECORD-SUMTYPE): buffer the `arity FIELD ..` body through ;PRODUCT and
\ register the TK-PRODUCT family + its generated-word metadata rows so later
\ signatures in this source resolve the family. No dictionary words are
\ generated on this path (engine-definer-only, sum parity).
: RECORD-PRODUCT ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing product name" 74 die THEN
   0 BODY-U !
   BEGIN
      NEXT-SCAN
      dup 0= IF s" verify-source: missing ;PRODUCT" 74 die THEN
      2dup PRODUCT-END? IF
         2drop
         name nameu BODY-BUF BODY-U @ CHECKER-DEFPRODUCT
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: RECORD-LAYOUT-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:ptr countu:n :}
   NEXT-SCAN {: name:ptr nameu:n :}
   NEXT-SCAN {: type:ptr typeu:n :}
   type typeu count countu name nameu CHECKER-DEFLAYOUT-BUFFER ;

\ TYPED-BUFFER / TYPED-VARIABLE gate registration (dot habu-nominal-storage-typed).
\ A stored type may be `ptr* base` or a spaced `[ in -- out ]` xt<effect> quotation
\ (dot habu-typed-xt-storage-ddad4af8), so the type is a contiguous multi-token
\ span from the scanner buffer, not one token.
variable STG-A
variable STG-U
variable STG-START

: STG-PTR-TOK? ( ptr u8 n -- bool )
   s" ptr" CORE-STR= ;

: STG-QUOT-OPEN? ( ptr u8 n -- bool )
   s" [" CORE-STR= ;

: STG-QUOT-CLOSE? ( ptr u8 n -- bool )
   s" ]" CORE-STR= ;

: SCAN-STORAGE-QUOT ( -- )   \ consume `[ in -- out ]` through the closer
   BEGIN STG-A @ STG-U @ STG-QUOT-CLOSE? 0= WHILE
      NEXT-SCAN STG-U !  STG-A !
      STG-U @ 0= IF s" verify-source: missing storage ]" 74 die THEN
   REPEAT ;

: SCAN-STORAGE-TYPE ( -- ptr u8 n )
   NEXT-SCAN STG-U !  STG-A !
   STG-U @ 0= IF s" verify-source: missing storage type" 74 die THEN
   STG-A @ STG-START !
   BEGIN STG-A @ STG-U @ STG-PTR-TOK? WHILE
      NEXT-SCAN STG-U !  STG-A !
      STG-U @ 0= IF s" verify-source: missing storage pointee" 74 die THEN
   REPEAT
   STG-A @ STG-U @ STG-QUOT-OPEN? IF SCAN-STORAGE-QUOT THEN
   STG-START @  STG-A @ STG-U @ + STG-START @ - ;

: RECORD-TYPED-BUFFER ( -- )
   TOP-PREV-A @ TOP-PREV-U @ {: count:ptr countu:n :}
   NEXT-SCAN {: name:ptr nameu:n :}
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   type typeu count countu name nameu CHECKER-DEFTYPED-BUFFER ;

: RECORD-TYPED-VARIABLE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   SCAN-STORAGE-TYPE {: type:ptr typeu:n :}
   type typeu name nameu CHECKER-DEFTYPED-VARIABLE ;

: RECORD-VALUE-RECORD ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing value-record name" 74 die THEN
   0 BODY-U !
   BEGIN
      NEXT-SCAN
      dup 0= IF s" verify-source: missing END-VALUE-RECORD" 74 die THEN
      2dup VALUE-RECORD-END? IF
         2drop
         name nameu BODY-BUF BODY-U @ CHECKER-DEFRECORD
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: RECORD-TRUST ( -- )
   STR-LAST-U @ 0= IF s" verify-source: TRUST missing signature string" 74 die THEN
   STR-PREV-U @ 0= IF s" verify-source: TRUST missing name string" 74 die THEN
   STR-PREV-A @ STR-PREV-U @
   STR-LAST-A @ STR-LAST-U @
   TRUST-SIGNATURE ;

\ EXPORT has two documented roles split by package context (dot
\ habu-compiler-pkg-re-688212c1): inside an open package it is the re-export
\ declaration (CHECKER-EXPORT aliases the source's checked effect under its
\ tail); at top level it is the hb-build --repl export directive, which the
\ build strips via COMMENT-EXPORTS before engine load — replay consumes the
\ name and records nothing, exactly like the engine never seeing the line.
: RECORD-EXPORT ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing EXPORT name" 74 die THEN
   CHECKER-PACKAGE-ACTIVE? IF name nameu CHECKER-EXPORT THEN ;

\ A package primitive row has two closers, and this verifier models them exactly
\ as the ratchet parser does (tools/primitive-effect-inventory.f PRIVATE-CLOSE?):
\ `PPRIM;` interns the axiom into the package public wordlist and `CLOSE-PRIVATE`
\ interns it into the package private one. Visibility is part of the row, not a
\ different row shape, so either token ends a `PPRIM:` row. A bare `PRIM:` row has
\ no package to be private in, so it declares no alternate closer and
\ `CLOSE-PRIVATE` stays an ordinary effect token there.
: ROW-CLOSER? ( ptr u8 n ptr u8 n -- bool ) {: end:ptr endu:n alt:ptr altu:n :}
   TOKEN-A @ TOKEN-U @ end endu STR=CI IF 0 0= EXIT THEN
   altu 0= IF 0 0= 0= EXIT THEN
   TOKEN-A @ TOKEN-U @ alt altu STR=CI ;

\ PRIM:/PPRIM: bodies use the canonical body scanner so parsing words consume
\ their comments, strings, and raw operands before a live closer is considered.
: RECORD-PRIM-ROW ( ptr u8 n ptr u8 n -- ) {: end:ptr endu:n alt:ptr altu:n :}
   NEXT-RAW dup 0= IF s" verify-source: missing primitive name" 74 die THEN
   2drop
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: missing primitive row closer" 74 die THEN
      end endu alt altu ROW-CLOSER? IF EXIT THEN
      SKIP-BODY-TOKEN
   AGAIN ;

: RECORD-PRIM ( -- )
   s" PRIM;" s" " RECORD-PRIM-ROW ;

: RECORD-PPRIM ( -- )
   NEXT-RAW dup 0= IF s" verify-source: missing primitive package" 74 die THEN
   2drop
   s" PPRIM;" s" CLOSE-PRIVATE" RECORD-PRIM-ROW ;

: STRUCTURE-END? ( ptr u8 n -- bool )
   s" END-STRUCTURE" STR=CI ;

: STRUCTURE-PTR-FIELD? ( ptr u8 n -- bool )
   s" PTR-FIELD:" STR=CI ;

: STRUCTURE-CFIELD? ( ptr u8 n -- bool )
   s" CFIELD:" STR=CI ;

: STRUCTURE-CELL-FIELD? ( ptr u8 n -- bool )
   s" +FIELD" STR=CI ;

: TRUST-STRUCTURE-FIELD ( ptr u8 n ptr u8 n -- )
   TRUST-SIGNATURE ;

: RECORD-STRUCTURE-FIELD ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing structure field name" 74 die THEN
   name nameu sig sigu TRUST-STRUCTURE-FIELD ;

\ Record the size word (`-- n`) then each field accessor with its runtime effect
\ so BEGIN-STRUCTURE layouts self-certify their field uses.
: RECORD-STRUCTURE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing structure name" 74 die THEN
   name nameu s" -- n" TRUST-SIGNATURE
   BEGIN
      NEXT-SCAN
      dup 0= IF s" verify-source: missing END-STRUCTURE" 74 die THEN
      2dup STRUCTURE-END? IF 2drop EXIT THEN
      2dup STRUCTURE-PTR-FIELD? IF 2drop s" ptr a -- ptr ptr a" RECORD-STRUCTURE-FIELD ELSE
      2dup STRUCTURE-CFIELD? IF 2drop s" ptr a -- ptr u8" RECORD-STRUCTURE-FIELD ELSE
      2dup STRUCTURE-CELL-FIELD? IF 2drop s" ptr a -- ptr a" RECORD-STRUCTURE-FIELD ELSE
      2drop
      THEN THEN THEN
   AGAIN ;

: RECORD-DEFINER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" package" STR=CI IF RECORD-PACKAGE 0 0= EXIT THEN
   a u s" public" STR=CI IF RECORD-PUBLIC 0 0= EXIT THEN
   a u s" private" STR=CI IF RECORD-PRIVATE 0 0= EXIT THEN
   a u s" ;package" STR=CI IF RECORD-END-PACKAGE 0 0= EXIT THEN
   a u s" deftype" STR=CI IF RECORD-DEFTYPE 0 0= EXIT THEN
   a u s" deflinear" STR=CI IF RECORD-DEFLINEAR 0 0= EXIT THEN
   a u s" value-record" STR=CI IF RECORD-VALUE-RECORD 0 0= EXIT THEN
   a u s" begin-structure" STR=CI IF RECORD-STRUCTURE 0 0= EXIT THEN
   a u s" structure" STR=CI IF RECORD-STRUCTURE-DECL 0 0= EXIT THEN
   a u s" newtype" STR=CI IF RECORD-NEWTYPE 0 0= EXIT THEN
   a u s" sumtype" STR=CI IF RECORD-SUMTYPE 0 0= EXIT THEN
   a u s" enum" STR=CI IF RECORD-ENUM 0 0= EXIT THEN
   a u s" product" STR=CI IF RECORD-PRODUCT 0 0= EXIT THEN
   a u s" LAYOUT-BUFFER" STR=CI IF RECORD-LAYOUT-BUFFER 0 0= EXIT THEN
   a u s" TYPED-BUFFER" STR=CI IF RECORD-TYPED-BUFFER 0 0= EXIT THEN
   a u s" TYPED-VARIABLE" STR=CI IF RECORD-TYPED-VARIABLE 0 0= EXIT THEN
   \ `constant` bakes one physical cell, so its trust is the one-cell `-- a`
   \ model — identical to native C-CONSTANT, all-errors (which funnels here),
   \ and public-signatures. This is the PERMANENT contract (TFAM 12 verdict
   \ 2026-07-09, habu-tfam-12-layout): the interpret stack is untyped by
   \ design, so no path has a sound shape source, and a wider-than-cell layout
   \ value never lands there (DNAME-WIDE dispatch gate). Any layout USE of the
   \ constant fails closed downstream; parity locked by check-all-errors-test
   \ const-layout-narrow.
   a u s" constant" STR=CI IF s" -- a" RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" create" STR=CI IF s" -- ptr a" RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" variable" STR=CI IF s" -- ptr a" RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" PTR-VARIABLE" STR=CI IF s" -- ptr ptr a" RAW-TRUST-NEXT 0 0= EXIT THEN
   a u s" defer" STR=CI IF TRUST-DEFER 0 0= EXIT THEN
   a u s" PRIM:" STR=CI IF RECORD-PRIM 0 0= EXIT THEN
   a u s" PPRIM:" STR=CI IF RECORD-PPRIM 0 0= EXIT THEN
   a u s" trusted:" STR=CI IF TRUSTED-DEFINITION 0 0= EXIT THEN
   a u s" undefine" STR=CI IF UNDEFINE-WORD 0 0= EXIT THEN
   a u s" trust" STR=CI IF RECORD-TRUST 0 0= EXIT THEN
   a u s" immediate" STR=CI IF 0 0= EXIT THEN
   a u s" export" STR=CI IF RECORD-EXPORT 0 0= EXIT THEN
   0 0= 0= ;

: VERIFY-DEFINITION ( -- )
   0 BODY-U !
   BODY!
   TOKEN-U @ 0= if s" verify-source: missing word name" 74 die then
   TOKEN-ORIGIN!
   TOKEN-A @ TOKEN-U @ BODY-APPEND
   MAYBE-SIGNATURE
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: unterminated definition" 74 die THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF VERIFY-BODY EXIT THEN
      TOKEN-A @ TOKEN-U @ s" does>" CORE-STR= IF VERIFY-DOES EXIT THEN
      APPEND-BODY-TOKEN
   AGAIN ;

: VERIFY-SOURCE ( -- )
   SCAN-RESET
   0 TOP-PREV-A !  0 TOP-PREV-U !
   BEGIN
      NEXT-SCAN dup 0 > WHILE
      2dup TOP-CUR-U ! TOP-CUR-A !
      2dup s" :" CORE-STR= IF 2drop VERIFY-DEFINITION ELSE
      2dup RECORD-DEFINER? IF 2drop ELSE 2drop THEN THEN
      TOP-CUR-A @ TOP-PREV-A !  TOP-CUR-U @ TOP-PREV-U !
   REPEAT 2drop ;

: THROW-RESULT ( n -- )
   dup 0= IF drop exit THEN
   throw ;

: RUN ( -- )
   [: VERIFY-SOURCE ;] catch THROW-RESULT ;

public

: SOURCE-BUF-IN-SCOPE ( ptr u8 n -- )
   SOURCE!
   RUN ;

: SOURCE-BUF-AT-IN-SCOPE ( ptr u8 n n n n -- )
   SOURCE-AT!
   RUN ;

: SOURCE-BUF ( ptr u8 n -- )
   SOURCE!
   CHECKER-CANDIDATE-SCOPE-START
   [: RUN ;] catch
   CHECKER-CANDIDATE-SCOPE-DONE
   THROW-RESULT ;

;package
