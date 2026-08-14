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
\   BCG-CAP  - the source-arena capacity and headroom must agree across the
\              native engine, the Gforth mirror, stage2 and the maker. The
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

\ habu2.f is 262,867 bytes on the ENGINE-ERROR cutover tree; 25% headroom is
\ 328,584 bytes, so the next power-of-two arena is $80000.
$80000 constant SRC-CAP

create SRC-BUF SRC-CAP allot
variable SRC-LEN

public

: SRC ( -- ptr u8 n )
   SRC-BUF SRC-LEN @ ;

: LOAD ( ptr u8 n -- )
   SRC-BUF SRC-CAP READ-ALL SRC-LEN ! ;

\ Hostile-fixture seam: place synthetic source text in the scan buffer so the
\ lexer path below can be proven comment- and string-proof.
: SET ( ptr u8 n -- ) {: a:ptr u:n :}
   u SRC-CAP <= TTRUE
   a SRC-BUF u BYTE-COPY
   u SRC-LEN ! ;

;package

\ The shared arena constants: the emitters, the fixpoint driver and the maker must
\ all name the same source-arena capacity and headroom percentage.
package BCG-CAP
using BCG

32 constant TOK-CAP

create PCT-TOK TOK-CAP allot
create CAP-TOK TOK-CAP allot
create IBUF-TOK TOK-CAP allot
variable PCT-U
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
   s" SOURCE-HEADROOM-PCT" DEF-VALUE
   PCT-TOK PCT-U TOK-SAVE
   s" SOURCE-ARENA-CAP" DEF-VALUE
   CAP-TOK CAP-U TOK-SAVE
   s" IBUFSZ" DEF-VALUE
   IBUF-TOK IBUF-U TOK-SAVE ;

: CHECK-TOKENS ( -- )
   s" SOURCE-HEADROOM-PCT" DEF-VALUE
   PCT-TOK PCT-U @ T$=
   s" SOURCE-ARENA-CAP" DEF-VALUE
   CAP-TOK CAP-U @ T$=
   s" IBUFSZ" DEF-VALUE
   IBUF-TOK IBUF-U @ T$= ;

: OWNER ( -- )
   s" SOURCE-HEADROOM-PCT" DEF-SCAN DEF-N @ 1 T=
   s" SOURCE-ARENA-CAP" DEF-SCAN DEF-N @ 1 T=
   s" IBUFSZ" DEF-SCAN DEF-N @ 1 T= ;

public

: TEST ( -- )
   SOURCE-HEADROOM-PCT 25 T=
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
   BCG-HIDE:TEST
   T-REPORT
   s" bootstrap-codegen-test: ok" type cr ;

;package

BCG:MAIN
