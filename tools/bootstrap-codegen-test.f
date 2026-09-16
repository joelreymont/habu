\ bootstrap-codegen-test.f - the bootstrap-codegen invariants no build event observes.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f tools/bootstrap-codegen-test.f
\
\ This file once held 259 source-substring assertions over the emitters, the
\ recovery script and the fixpoint driver. A substring assertion is satisfied
\ by a comment carrying the expected text and misses nothing when the real
\ code changes shape, so every assertion whose subject a real build event
\ observes - the fixpoint rebuild, the bootstrap recovery gates, the DDC, or
\ production execution - was deleted (dot habu-delete-src-substring-2564d854).
\ What remains carries invariants NO build event observes, each expressed
\ through execution or through the real lexer rather than through text:
\
\   BCG-CAP  - the source-arena capacity must agree across the native engine,
\              the Gforth mirror, stage2 and the maker. The
\              mirror's arena is exercised only during no-binary recovery, so
\              an undersized mirror constant fails no scheduled gate; it fails
\              a recovery months later. The check reads each declaration
\              through tools/lint/source-lex.f token classification - a
\              comment or string carrying the constant cannot satisfy it, and
\              the hostile fixtures below prove that.
\   BCG-HIDE - the BFR-* earliest-marker hide words, driven directly against
\              the live dictionary. A real build always finds both markers
\              present and every record unique, so the duplicate-record,
\              case-folding and missing-marker edges are reachable only here.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package BCG
private

\ Large enough to read the largest file these checks scan through the lexer:
\ src/core/checker.f, the biggest cold-prefix row, at 681 KiB and growing.
\ LOAD refuses a truncated read rather than scanning the front of a file.
$100000 constant SRC-CAP

create SRC-BUF SRC-CAP allot
variable SRC-LEN

public

: SRC ( -- ptr u8 n )
   SRC-BUF SRC-LEN @ ;

: LOAD ( ptr u8 n -- )
   SRC-BUF SRC-CAP READ-ALL SRC-LEN !
   SRC-LEN @ SRC-CAP < TTRUE ;

\ Hostile-fixture seam: place synthetic source text in the scan buffer so the
\ lexer path below can be proven comment- and string-proof.
: SET ( ptr u8 n -- ) {: a:ptr u:n :}
   u SRC-CAP <= TTRUE
   a SRC-BUF u BYTE-COPY
   u SRC-LEN ! ;

;package

\ The emitters, fixpoint driver and maker must name the same source-arena capacity.
package BCG-CAP
using BCG

32 constant TOK-CAP

create CAP-TOK TOK-CAP allot
create IBUF-TOK TOK-CAP allot
variable CAP-U
variable IBUF-U
variable DEF-I
variable DEF-N

: TOK=CI ( n ptr u8 n -- bool ) {: idx:n a:ptr u:n :}
   idx LINT-LEX:TOKEN a u LINT-STR=CI ;

: DEF? ( n ptr u8 n -- bool ) {: idx:n name:ptr nameu:n :}
   idx 0 <= if 0 0= 0= exit then
   idx 1 + LINT-LEX:COUNT >= if 0 0= 0= exit then
   idx 1 - LINT-LEX:KIND@ LINT-LEX:WORD <> if 0 0= 0= exit then
   idx LINT-LEX:KIND@ LINT-LEX:WORD <> if 0 0= 0= exit then
   idx 1 + LINT-LEX:KIND@ LINT-LEX:WORD <> if 0 0= 0= exit then
   idx s" constant" TOK=CI 0= if 0 0= 0= exit then
   idx 1 + name nameu TOK=CI ;

: DEF-SCAN ( ptr u8 n -- ) {: name:ptr nameu:n :}
   SRC LINT-LEX:SOURCE
   -1 DEF-I !
   0 DEF-N !
   0 begin dup LINT-LEX:COUNT < while
      dup name nameu DEF? if
         dup DEF-I !
         DEF-N @ 1 + DEF-N !
      then
      1+
   repeat drop ;

: DEF-VALUE ( ptr u8 n -- ptr u8 n )
   DEF-SCAN
   DEF-N @ 1 = dup TTRUE 0= if s" " exit then
   DEF-I @ 1 - LINT-LEX:TOKEN ;

: TOK-SAVE ( ptr u8 n ptr u8 ptr n -- )
   {: src:ptr u:n dst:ptr lenp:ptr :}
   u TOK-CAP <= TTRUE
   src dst u BYTE-COPY
   u lenp ! ;

: SAVE-TOKENS ( -- )
   s" SOURCE-ARENA-CAP" DEF-VALUE
   CAP-TOK CAP-U TOK-SAVE
   s" IBUFSZ" DEF-VALUE
   IBUF-TOK IBUF-U TOK-SAVE ;

: CHECK-TOKENS ( -- )
   s" SOURCE-ARENA-CAP" DEF-VALUE
   CAP-TOK CAP-U @ T$=
   s" IBUFSZ" DEF-VALUE
   IBUF-TOK IBUF-U @ T$= ;

: OWNER ( -- )
   s" SOURCE-ARENA-CAP" DEF-SCAN DEF-N @ 1 T=
   s" IBUFSZ" DEF-SCAN DEF-N @ 1 T= ;

public

: TEST ( -- )
   SOURCE-ARENA-CAP IBUFSZ T=
   s" src/habu/layout.f" LOAD
   OWNER
   SAVE-TOKENS
   s" bootstrap/cg/forth.fs" LOAD
   OWNER
   CHECK-TOKENS
   s" src/habu/stage2.f" LOAD
   s" SOURCE-CAP" DEF-VALUE s" SOURCE-ARENA-CAP" T$=
   s" src/habu/maker.f" LOAD
   s" MK-SOURCE-CAP" DEF-VALUE s" SOURCE-ARENA-CAP" T$= ;

;package

\ Hostile fixtures: the constant name inside a comment, a paren comment and a
\ string literal must not read as a declaration, and a real declaration must
\ still be found beside those decoys with its real value. This is what makes
\ BCG-CAP's token scan a check a comment-only edit cannot satisfy - the
\ failure mode that retired this file's substring battery.
package BCG-CAP
using BCG

: HOSTILE-DECOYS-ONLY ( -- )
   s\" \\ $80000 constant SOURCE-ARENA-CAP\n( $80000 constant SOURCE-ARENA-CAP )\ns\" $80000 constant SOURCE-ARENA-CAP\" drop drop drop\n" SET
   s" SOURCE-ARENA-CAP" DEF-SCAN
   DEF-N @ 0 T= ;

: HOSTILE-DECOYS-BESIDE ( -- )
   s\" \\ $40 constant SOURCE-ARENA-CAP\n$80000 constant SOURCE-ARENA-CAP\n( $40 constant SOURCE-ARENA-CAP )\n" SET
   s" SOURCE-ARENA-CAP" DEF-VALUE s" $80000" T$= ;

public

: HOSTILE ( -- )
   HOSTILE-DECOYS-ONLY
   HOSTILE-DECOYS-BESIDE ;

;package

\ --- the cold prefix: its budget, and that the boot reader may strip it ---
\ The engine reads its prefix rows through LSRCRDP, which deletes each file's
\ comment and blank lines (src/habu/habu2.f EMIT-SOURCE-READ-PREFIX). Nothing a
\ normal day runs observes either half: the installed bin/hb is seeded and opens
\ no prefix source at all, so an oversized prefix or a line the reader drops
\ wrongly is a recovery failure months later - the same shape as the arena
\ constant above, and the reason both live in this file.
\
\ Neither fact is listed here. The row set is read out of habu2.f's own
\ `PFX-LOAD-ROW` rows, so a file added to the prefix is covered the moment the
\ row is written. The strippability rule is the reader's, checked against the
\ shared lexer: the reader drops a line whose first token is the one-byte `\`
\ and a line carrying no token at all, and it has no string state, so the one
\ place the two can disagree is a line START that falls INSIDE a token - a
\ string literal, a paren comment or a registry row that spans the newline.
\ That form is absent from the prefix today and this is what keeps it absent.
package BCG-PFX
using BCG

128 constant ROW-MAX             \ 58 rows on linux-aarch64 today
96 constant ROW-PATH-CAP
0 constant KIND-COMMON
1 constant KIND-LINUX
2 constant KIND-MACOS
5 constant BUDGET-PARTS          \ refuse a prefix above BUDGET-TAKEN/BUDGET-PARTS
4 constant BUDGET-TAKEN          \ of the arena: 80 percent

create ROW-PATH ROW-MAX ROW-PATH-CAP * allot
create ROW-U ROW-MAX cells allot
create ROW-KIND ROW-MAX cells allot
variable ROW-N

variable LINE-P   variable KEPT
variable TOK-I    variable DISAGREE
variable PFX-LINUX-N   variable PFX-MACOS-N   variable PFX-RAW-N

: ROW-A ( n -- ptr u8 ) {: k:n :}
   ROW-PATH k ROW-PATH-CAP * + ;

: ROW$ ( n -- ptr u8 n ) {: k:n :}
   k ROW-A  k cells ROW-U + @ ;

: ROW-KIND@ ( n -- n ) {: k:n :}
   k cells ROW-KIND + @ ;

: ROW+ ( n ptr u8 n -- ) {: kind:n a:ptr u:n :}
   ROW-N @ ROW-MAX < TTRUE
   u ROW-PATH-CAP <= TTRUE
   a ROW-N @ ROW-A u BYTE-COPY
   u ROW-N @ cells ROW-U + !
   kind ROW-N @ cells ROW-KIND + !
   ROW-N @ 1+ ROW-N ! ;

\ ---- the reader's own line rule, in the reader's order ----------------------
: LINE-NEXT ( ptr u8 n n -- n ) {: a:ptr u:n p:n :}
   p
   begin dup u < while
      dup a + c@ $0A = if 1+ exit then
      1+
   repeat ;

: FIRST-TOKEN-BYTE ( ptr u8 n n -- n ) {: a:ptr u:n p:n :}
   p
   begin dup u < while
      dup a + c@ $0A = if exit then
      dup a + c@ $20 > if exit then
      1+
   repeat ;

: DROPPED? ( ptr u8 n n -- bool ) {: a:ptr u:n p:n :}
   a u p FIRST-TOKEN-BYTE {: i:n :}
   i u >= if 0 0= exit then                       \ delimiters to the end of the file
   i a + c@ $0A = if 0 0= exit then                \ no token on this line
   i a + c@ $5C <> if 0 0= 0= exit then
   i 1+ u >= if 0 0= exit then                     \ `\` is the last byte
   i 1+ a + c@ $20 <= ;                            \ `\x...` is one token, not a comment

: STRIPPED ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 KEPT !  0 LINE-P !
   begin LINE-P @ u < while
      a u LINE-P @ DROPPED? 0= if
         a u LINE-P @ LINE-NEXT LINE-P @ - KEPT +!
      then
      a u LINE-P @ LINE-NEXT LINE-P !
   repeat
   KEPT @ ;

\ ---- the one disagreement: a dropped line that starts inside a token ---------
\ A paren comment and a registry row are their own TOKEN span, but a string
\ literal's TOKEN is only the opening word and its payload arrives through
\ CONTENT - so the span that can swallow a line start is the opener, the one
\ delimiter byte after it, the payload and the closing quote.
: TOKEN-END ( n -- n ) {: k:n :}
   k LINT-LEX:TOKEN {: ta:ptr tu:n :}
   k LINT-LEX:BYTE@ tu + {: e:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD <> if e exit then
   k LINT-LEX:CONTENT {: ca:ptr cu:n :}
   cu 0 = if e exit then
   e cu + 2 + ;

\ Tokens arrive in source order and never overlap, so the cursor only moves
\ forward across a whole file: this is a merge walk, not a search per line.
: INSIDE-TOKEN? ( n -- bool ) {: p:n :}
   begin TOK-I @ LINT-LEX:COUNT < while
      TOK-I @ TOKEN-END p > if
         TOK-I @ LINT-LEX:BYTE@ p < exit
      then
      TOK-I @ 1+ TOK-I !
   repeat
   0 0= 0= ;

: SCAN-SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= TTRUE
   0 TOK-I !  0 LINE-P !
   begin LINE-P @ u < while
      LINE-P @ 0 > if
         a u LINE-P @ DROPPED? if
            LINE-P @ INSIDE-TOKEN? if 0 0= DISAGREE ! then
         then
      then
      a u LINE-P @ LINE-NEXT LINE-P !
   repeat ;

\ ---- the row set, read out of the emitter ------------------------------------
: KIND-OF ( n -- n ) {: k:n :}
   k LINT-LEX:TOKEN {: a:ptr u:n :}
   a u s" PFX-COMMON" LINT-STR=CI if KIND-COMMON exit then
   a u s" PFX-LINUX" LINT-STR=CI if KIND-LINUX exit then
   a u s" PFX-MACOS" LINT-STR=CI if KIND-MACOS exit then
   -1 ;

: NAMES-ROW-WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD <> if 0 0= 0= exit then
   k LINT-LEX:TOKEN s" PFX-LOAD-ROW" LINT-STR=CI ;

\ The emitter mentions the word twice: as the row emitter's own definition, and
\ as every row that calls it. The definition is the one occurrence that carries
\ no path, so it is named here rather than absorbed by a shape test that would
\ also swallow a row whose spelling drifted.
: DEFINES-ROW-WORD? ( n -- bool ) {: k:n :}
   k NAMES-ROW-WORD? 0= if 0 0= 0= exit then
   k 1 - LINT-LEX:TOKEN s" :" LINT-STR=CI ;

: ROW-AT? ( n -- bool ) {: k:n :}
   k 3 < if 0 0= 0= exit then
   k NAMES-ROW-WORD? 0= if 0 0= 0= exit then
   k DEFINES-ROW-WORD? 0= ;

: TAKE-ROW ( n -- ) {: k:n :}
   k 3 - KIND-OF {: kd:n :}
   kd 0 >= TTRUE
   k 1 - LINT-LEX:CONTENT {: a:ptr u:n :}
   u 0 > TTRUE                                     \ the path arrives as a string literal
   kd a u ROW+ ;

: COLLECT ( -- )
   0 ROW-N !
   SRC LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= TTRUE
   0 begin dup LINT-LEX:COUNT < while
      dup ROW-AT? if dup TAKE-ROW then
      1+
   repeat drop ;

: MEASURE-ROW ( n -- ) {: k:n :}
   k ROW$ LOAD
   SRC {: a:ptr u:n :}
   a u STRIPPED {: s:n :}
   k ROW-KIND@ {: kd:n :}
   kd KIND-MACOS <> if s PFX-LINUX-N +!  u PFX-RAW-N +! then
   kd KIND-LINUX <> if s PFX-MACOS-N +! then
   a u SCAN-SOURCE ;

\ Both emitters must route the rows through the stripping entry. The Gforth
\ mirror's copy of the reader runs only during no-binary recovery, so a mirror
\ left on the raw entry would build an hb-stage0 whose prefix is 41 percent
\ larger than every other engine's and nothing scheduled would say so.
: DEF-INDEX ( -- n )
   0 begin dup LINT-LEX:COUNT < while
      dup DEFINES-ROW-WORD? if exit then
      1+
   repeat drop -1 ;

: BODY-NAMES? ( ptr u8 n -- bool ) {: name:ptr nu:n :}
   DEF-INDEX {: d:n :}
   d 0 >= TTRUE
   d 1+ begin dup LINT-LEX:COUNT < while
      dup LINT-LEX:KIND@ LINT-LEX:WORD = if
         dup LINT-LEX:TOKEN s" ;" LINT-STR=CI if drop 0 0= 0= exit then
         dup LINT-LEX:TOKEN name nu LINT-STR=CI if drop 0 0= exit then
      then
      1+
   repeat drop 0 0= 0= ;

: ROUTES-THROUGH-STRIP ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LOAD
   SRC LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= TTRUE
   s" LSRCRDP" BODY-NAMES? TTRUE
   s" LSRCRD" BODY-NAMES? 0= TTRUE ;

: UNDER-BUDGET? ( n -- bool ) {: bytes:n :}
   bytes BUDGET-PARTS * SOURCE-ARENA-CAP BUDGET-TAKEN * <= ;

public

: TEST ( -- )
   0 PFX-LINUX-N !  0 PFX-MACOS-N !  0 PFX-RAW-N !
   0 0= 0= DISAGREE !
   s" src/habu/habu2.f" ROUTES-THROUGH-STRIP
   s" bootstrap/cg/forth.fs" ROUTES-THROUGH-STRIP
   s" src/habu/habu2.f" LOAD
   COLLECT
   ROW-N @ 40 > TTRUE                              \ the rows were found, not zero of them
   ROW-N @ 0 ?do i MEASURE-ROW loop
   DISAGREE @ 0= TTRUE
   PFX-LINUX-N @ PFX-RAW-N @ < TTRUE               \ the reader does remove something
   PFX-LINUX-N @ UNDER-BUDGET? TTRUE
   PFX-MACOS-N @ UNDER-BUDGET? TTRUE ;

;package

\ Hostile fixtures for the two halves. The strip cases pin the reader's rule
\ against the forms that look like comments and are not (and the reverse); the
\ disagreement cases put a droppable line inside each kind of token that can
\ span a newline, which is the whole reason trailing and paren comments are left
\ alone. A rule that keyed on "the line begins with a backslash" passes none of
\ the first three.
package BCG-PFX
using BCG

: STRIP-CASE ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u STRIPPED want T= ;

: DISAGREES? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 0= 0= DISAGREE !
   a u SCAN-SOURCE
   DISAGREE @ ;

: STRIP-RULE ( -- )
   s\" : A ;\n" 6 STRIP-CASE
   s\" \\ c\n: A ;\n" 6 STRIP-CASE
   s\"   \\ c\n: A ;\n" 6 STRIP-CASE
   s\" \n\n: A ;\n" 6 STRIP-CASE
   s\" : A ; \\ t\n" 10 STRIP-CASE
   s\" \\foo\n" 5 STRIP-CASE
   s\" : A ( a -- b ) ;\n" 17 STRIP-CASE ;

: DISAGREEMENTS ( -- )
   s\" : A s\" x\n\\ y\n\" drop drop ;\n" DISAGREES? TTRUE
   s\" : A s\" x\n\n\" drop drop ;\n" DISAGREES? TTRUE
   s\" PRIM: 2swap PE-A PE-IN\n\\ y\n   PE-B PE-IN PRIM;\n" DISAGREES? TTRUE
   s\" ( a\n\\ y\n b )\n" DISAGREES? TTRUE ;

: AGREEMENTS ( -- )
   s\" \\ c\n\n: A ( -- ) ;\n: B ; \\ t\n( inline )\n" DISAGREES? 0= TTRUE
   s\" : A s\" x y\" drop drop ;\n\\ c\n" DISAGREES? 0= TTRUE ;

public

: HOSTILE ( -- )
   STRIP-RULE
   DISAGREEMENTS
   AGREEMENTS ;

;package

\ --- earliest-marker hide behavior ---
\ tools/bootstrap.sh's BOOT-* hide prelude mirrors src/habu/hide.f's BFR-*
\ words, so the native mirror is the executable spec and is driven directly
\ below; no shell is spawned (that would add host-glue surface), so the script
\ body itself stays pinned by the substring assertions above. hide.f is baked
\ into the engine prelude and truncated away after use, so `require` would be
\ skipped as already provided; include reloads the BFR-* words here. The include
\ publishes those BFR-* words globally, so it stays outside every package.
include src/habu/hide.f

\ The watermark has to be read at top level BETWEEN the two duplicate fixture
\ records, and packages do not nest, so BCG-HIDE opens once to publish the
\ recording word and reopens below for the checks that read it.
package BCG-HIDE
private

variable MID                            \ ndict watermark between the duplicate fixture records

public

: MARK-MID ( -- )
   ndict@ MID ! ;

;package

\ Two packages export the same tail on purpose: the earlier record must win.
package BCG-DUP-EARLY
public
: DUP-MARK ( -- ) ;
;package

BCG-HIDE:MARK-MID

package BCG-DUP-LATE
public
: DUP-MARK ( -- ) ;
;package

package BCG-HIDE
private

: REC ( ptr u8 n -- n )
   BFR-FIND-FIRST-INDEX ;

: IMK-REC ( -- n )
   s" IMK-NDICT0" REC ;

: SEQ-REC ( -- n )
   s" SEQ" REC ;

\ The production markers exist in the live dictionary with IMK-NDICT0 (util.f's
\ first record) earlier than SEQ; the hide index must pick the earlier record
\ in either argument order.
: EARLIEST-MARKER ( -- )
   IMK-REC 0 >= TTRUE
   SEQ-REC 0 >= TTRUE
   IMK-REC SEQ-REC < TTRUE
   s" IMK-NDICT0" s" SEQ" BFR-MARKER-INDEX IMK-REC T=
   s" SEQ" s" IMK-NDICT0" BFR-MARKER-INDEX IMK-REC T= ;

\ Earliest-hide depends on FIND-FIRST returning the FIRST record of a name: the
\ duplicate fixture record published before the MID watermark must win, and the
\ match must fold case like the shell's BOOT-XREF-STR=CI. The dictionary record
\ of a package word stores its bare tail, so the searched name is `DUP-MARK`.
\ Naming both fixture words keeps the duplicate load-bearing: if either package
\ stopped publishing the tail, the file would fail to load here instead of
\ leaving the index assertions below trivially satisfiable by a single record.
: FIRST-RECORD ( -- )
   BCG-DUP-EARLY:DUP-MARK
   BCG-DUP-LATE:DUP-MARK
   s" DUP-MARK" REC 0 >= TTRUE
   s" DUP-MARK" REC MID @ < TTRUE
   s" dup-mark" REC s" DUP-MARK" REC T= ;

\ One marker missing falls back to the found one; both missing is asserted at
\ the component level (FIND -> NOT-FOUND, MIN-FOUND keeps NOT-FOUND) because
\ BFR-MARKER-INDEX's both-missing path is a process exit (die 76) by design.
: MISSING-FALLBACK ( -- )
   s" IMK-NDICT0" s" BCG-NO-SUCH-MARKER" BFR-MARKER-INDEX IMK-REC T=
   s" BCG-NO-SUCH-MARKER" s" IMK-NDICT0" BFR-MARKER-INDEX IMK-REC T=
   s" BCG-NO-SUCH-MARKER" REC BFR-NOT-FOUND T=
   BFR-NOT-FOUND BFR-NOT-FOUND BFR-MIN-FOUND BFR-NOT-FOUND T=
   5 BFR-REQUIRE-INDEX 5 T= ;

public

: TEST ( -- )
   EARLIEST-MARKER
   FIRST-RECORD
   MISSING-FALLBACK ;

;package

package BCG
public

: MAIN ( -- )
   T-RESET
   BCG-CAP:TEST
   BCG-CAP:HOSTILE
   BCG-PFX:TEST
   BCG-PFX:HOSTILE
   BCG-HIDE:TEST
   T-REPORT
   s" bootstrap-codegen-test: ok" type cr ;

;package

BCG:MAIN
