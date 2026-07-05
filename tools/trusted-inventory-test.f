\ trusted-inventory-test.f - checked fixtures for tools/trusted-inventory.f.
\ Run: printf '' | bin/hb --load tools/trusted-inventory-test.f
\ Requiring the tool executes its CLI main: with no script args it scans the
\ repo and prints the TSV report. The capture buffer below records that live
\ run so its rows, ordering, and the committed TRUSTED.md derived ratchet ceiling
\ are asserted before the in-memory classifier fixtures reuse the row tables.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/sort.f
require tools/lint/text.f
require tools/argv.f

$40000 constant TIT-CAP
create TIT-BUF TIT-CAP allot

\ Mock an empty argv around the require so the tool's require-time main runs
\ in report mode even inside a gate worker that carries script arguments, and
\ capture its live report through the shared lint sink.
ARGV-MOCK-CLEAR
TIT-BUF TIT-CAP LINT-OUT-BUFFER!
require tools/trusted-inventory.f
LINT-OUT-BUFFER-OFF
ARGV-USE-SCRIPT

package TINV-TEST

$40000 constant MD-CAP
create MD-BUF MD-CAP allot

variable LIVE-ROWS
variable LIVE-TRUSTED
variable LIVE-TRUST
variable LIVE-SETCHECK
variable LIVE-BARE
variable LIVE-HOOK
variable ALL-SORTED
variable SORT-I

\ ---- live repo scan (performed by the tool's require-time main) ------------
: SAVE-LIVE ( -- )
   TINV:ROWS LIVE-ROWS !
   TINV:TRUSTED# LIVE-TRUSTED !
   TINV:TRUST# LIVE-TRUST !
   TINV:SETCHECK# LIVE-SETCHECK !
   TINV:BARE# LIVE-BARE !
   TINV:HOOK# LIVE-HOOK ! ;

: LIVE-COUNTS ( -- )
   LIVE-ROWS @ 0 > TTRUE
   LIVE-TRUSTED @ 0 > TTRUE
   LIVE-TRUST @ 0 > TTRUE
   LIVE-SETCHECK @ 0 > TTRUE
   LIVE-HOOK @ 0 > TTRUE
   LIVE-TRUSTED @ LIVE-TRUST @ + LIVE-SETCHECK @ + LIVE-BARE @ + LIVE-HOOK @ +
   LIVE-ROWS @ T= ;

: LIVE-SORTED ( -- )
   LINT-TRUE ALL-SORTED !
   1 SORT-I !
   begin SORT-I @ LIVE-ROWS @ < while
      SORT-I @ 1- TINV:IX-ROW SORT-I @ TINV:IX-ROW TINV:ROW-BEFORE? 0= if
         LINT-FALSE ALL-SORTED !
      then
      SORT-I @ 1+ SORT-I !
   repeat
   ALL-SORTED @ TTRUE ;

: LIVE-OUTPUT ( -- )
   LINT-OUT$ s" trusted-inventory: TRUSTED " CONTAINS? TTRUE
   LINT-OUT$ s" tools/lint/text.f" CONTAINS? TTRUE ;

\ The committed TRUSTED.md classification block is the derived ratchet ceiling:
\ it must parse and, tallied against the live scanned rows, show no grown, stale,
\ uncovered, or count-unset row.
: LIVE-BASELINE ( -- )
   s" TRUSTED.md" MD-BUF MD-CAP READ-FILE TINV:PARSE-CLASSES$ TTRUE
   TINV:RATCHET-TALLY
   TINV:RATCHET-BAD# 0 T=
   TINV:RATCHET-STALE# 0 T= ;

\ The committed TRUSTED.md classification must cover every live site.
: LIVE-CLASSES ( -- )
   TINV:CLASSES# 0 > TTRUE
   TINV:UNCLASSIFIED# 0 T= ;

\ ---- in-memory classifier fixtures ------------------------------------------
: FIX-SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   TINV:RESET
   s" fixture.f" a u TINV:SCAN-SOURCE ;

: ROW0-IS ( n ptr u8 n n -- ) {: cls:n na:ptr nu:n line:n :}
   0 TINV:ROW-CLASS cls T=
   0 TINV:ROW-NAME$ na nu T$=
   0 TINV:ROW-LINE line T= ;

: FIX-SKIP-PATHS ( -- )
   s" ./.jj-ws/other/tools/x.f" TINV:SKIP-PATH? TTRUE
   s" ./.dots/x.f" TINV:SKIP-PATH? TTRUE
   s" .jj-ws/other/x.f" TINV:SKIP-PATH? TTRUE
   s" .dots/x.f" TINV:SKIP-PATH? TTRUE
   s" ./tools/trusted-inventory.f" TINV:SKIP-PATH? TFALSE ;

: FIX-TRUSTED ( -- )
   s" TRUSTED: TIF-A ( n -- n ) dup ;" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-TRUSTED s" TIF-A" 1 ROW0-IS ;

: FIX-TRUSTED-CASE ( -- )
   s" trusted: tif-b ( -- )" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-TRUSTED s" tif-b" 1 ROW0-IS ;

: FIX-TRUSTED-NAMELESS ( -- )
   s" TRUSTED:" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-TRUSTED s" -" 1 ROW0-IS ;

: FIX-TRUSTED-DEFINER-REF ( -- )
   s" : TRUSTED: ( -- ) ;" FIX-SCAN
   TINV:ROWS 0 T=
   s" postpone TRUSTED:" FIX-SCAN
   TINV:ROWS 0 T= ;

: FIX-COMMENTS ( -- )
   S\" \\ TRUSTED: NOPE ( n -- n )\n( TRUSTED: NOPE )\n( s\q x\q s\q -- \q TRUST )\n\\ 0 set-check" FIX-SCAN
   TINV:ROWS 0 T= ;

: FIX-STRINGS ( -- )
   S\" : TIF-C ( -- ) s\q TRUSTED: NOPE\q 2drop s\q 0 set-check\q 2drop ;" FIX-SCAN
   TINV:ROWS 0 T=
   S\" : TIF-D ( -- ) s\q s\\q x\\q s\\q -- \\q TRUST\q 2drop ;" FIX-SCAN
   TINV:ROWS 0 T= ;

: FIX-TRUST ( -- )
   S\" s\q TIF-W\q s\q n -- n\q TRUST" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-TRUST s" TIF-W" 1 ROW0-IS ;

: FIX-TRUST-COMMENT ( -- )
   S\" s\q TIF-X\q s\q -- \q ( audited ) TRUST" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-TRUST s" TIF-X" 1 ROW0-IS ;

: FIX-TRUST-PARTIAL ( -- )
   S\" s\q TIF-Y\q TRUST" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-BARE s" TRUST" 1 ROW0-IS
   s" 1 2 TRUST" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-BARE s" TRUST" 1 ROW0-IS
   s" TIF-N$ TIF-E$ TRUST" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-BARE s" TRUST" 1 ROW0-IS
   s" : TRUST ( -- ) ;" FIX-SCAN
   TINV:ROWS 0 T=
   s" postpone TRUST" FIX-SCAN
   TINV:ROWS 0 T= ;

: FIX-SETCHECK ( -- )
   s" 0 set-check" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-SETCHECK s" 0 set-check" 1 ROW0-IS
   s" TIF-FLAG @ set-check" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-BARE s" set-check" 1 ROW0-IS
   s" set-check" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-BARE s" set-check" 1 ROW0-IS
   s" ' set-check" FIX-SCAN
   TINV:ROWS 0 T= ;

\ Every hook install is counted as HOOK-INSTALL, named by the installed hook,
\ so a rogue install is visible in the TSV and trips the ratchet.
: FIX-HOOK ( -- )
   s" ' TIF-HOOK set-check" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-HOOK s" TIF-HOOK" 1 ROW0-IS
   S\" ['] TIF-HOOK set-check" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-HOOK s" TIF-HOOK" 1 ROW0-IS
   s" ' EVIL-HOOK set-check" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-HOOK s" EVIL-HOOK" 1 ROW0-IS
   S\" 0 set-check\n' TIF-HOOK set-check" FIX-SCAN
   TINV:ROWS 2 T=
   TINV:HOOK# 1 T=
   TINV:SETCHECK# 1 T=
   1 TINV:ROW-CLASS TINV:CL-HOOK T=
   1 TINV:ROW-LINE 2 T= ;

: FIX-LINES ( -- )
   S\" \\ hdr\nTRUSTED: TIF-E ( -- )\n1 2 +\ns\q TIF-F\q s\q -- \q TRUST\n0 set-check" FIX-SCAN
   TINV:ROWS 3 T=
   0 TINV:ROW-CLASS TINV:CL-TRUSTED T=
   0 TINV:ROW-LINE 2 T=
   1 TINV:ROW-CLASS TINV:CL-TRUST T=
   1 TINV:ROW-LINE 4 T=
   1 TINV:ROW-NAME$ s" TIF-F" T$=
   2 TINV:ROW-CLASS TINV:CL-SETCHECK T=
   2 TINV:ROW-LINE 5 T= ;

\ Scanner parity pins: the streaming scanner is the single trust-site lexer,
\ so its edge behavior is pinned here against engine lexing rules.
\ CRLF sources tokenize cleanly (CR is whitespace) and line numbers count LF.
: FIX-CRLF ( -- )
   S\" TRUSTED: TIF-CR ( -- )\r\n0 set-check\r\n" FIX-SCAN
   TINV:ROWS 2 T=
   TINV:CL-TRUSTED s" TIF-CR" 1 ROW0-IS
   0 TINV:ROW-EFFECT$ s" --" T$=
   1 TINV:ROW-CLASS TINV:CL-SETCHECK T=
   1 TINV:ROW-LINE 2 T= ;

\ A TRUST row split across lines still pairs its two string literals; the row
\ is pinned to the TRUST token's line.
: FIX-TRUST-MULTILINE ( -- )
   S\" s\q TIF-M\q\ns\q n -- n\q\nTRUST" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-TRUST s" TIF-M" 3 ROW0-IS
   0 TINV:ROW-EFFECT$ s" n -- n" T$= ;

\ Engine parity: `(` comments only as a standalone token; a `(`-initial token
\ like `(CMP)` is a word, so it lands in the ring and breaks literal shapes
\ fail-closed instead of being swallowed to the next `)`.
: FIX-PAREN-WORD ( -- )
   s" ( n ) 0 set-check" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-SETCHECK s" 0 set-check" 1 ROW0-IS
   s" 0 (CMP) set-check" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-BARE s" set-check" 1 ROW0-IS
   s" (CMP) 0 set-check" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-SETCHECK s" 0 set-check" 1 ROW0-IS
   s" : (OK) ( -- n ) 2 ;" FIX-SCAN
   TINV:ROWS 0 T=
   S\" s\q TIF-PW\q (CMP) s\q -- \q TRUST" FIX-SCAN
   TINV:ROWS 1 T=
   TINV:CL-BARE s" TRUST" 1 ROW0-IS ;

: FIX-SORT ( -- )
   TINV:RESET
   s" z.f" s" TRUSTED: TIF-Z1 ( -- )" TINV:SCAN-SOURCE
   s" a.f" s" TRUSTED: TIF-A1 ( -- )" TINV:SCAN-SOURCE
   TINV:SORT-ROWS
   0 TINV:IX-ROW TINV:ROW-PATH$ s" a.f" T$=
   1 TINV:IX-ROW TINV:ROW-PATH$ s" z.f" T$= ;

\ ---- derived ratchet: per-row committed count vs live tally --------------------
\ RATCHET-TALLY resolves every scanned site to its winning classification row and
\ tallies the matched count, so the ratchet is per row: a file-level row's count
\ N is the ceiling for the sites no named row owns, a named row implies 1 (or an
\ explicit count for a name at several sites). Reloading a classes block and
\ re-tallying against the same scanned rows exercises ok/grew/stale/unset.
: RT-SCAN ( -- )
   TINV:RESET
   s" fixture.f" S\" TRUSTED: TIF-A ( -- )\nTRUSTED: TIF-B ( -- )\nTRUSTED: TIF-C ( -- )\ns\q TIF-W\q s\q -- \q TRUST" TINV:SCAN-SOURCE ;

: RT-CLASSES ( ptr u8 n -- )
   TINV:PARSE-CLASSES$ TTRUE TINV:RATCHET-TALLY ;

: FIX-RATCHET ( -- )
   \ file-level count 3 covers the 3 TRUSTED: sites; TIF-W named row covers 1: ok
   RT-SCAN
   TINV:ROWS 4 T=
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot 3\nfixture.f:TIF-W prim-axiom dot\n-->" RT-CLASSES
   TINV:RATCHET-BAD# 0 T=
   TINV:RATCHET-STALE# 0 T=
   \ file-level count 2 but 3 sites: GREW, so BAD counts the one grown row
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot 2\nfixture.f:TIF-W prim-axiom dot\n-->" RT-CLASSES
   TINV:RATCHET-GREW# 1 T=
   TINV:RATCHET-BAD# 1 T=
   TINV:RATCHET-STALE# 0 T=
   \ file-level count 4 but 3 sites: STALE (discharge lower the count)
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot 4\nfixture.f:TIF-W prim-axiom dot\n-->" RT-CLASSES
   TINV:RATCHET-STALE# 1 T=
   TINV:RATCHET-BAD# 0 T=
   \ file-level row with no committed count is UNSET and fails fail-closed
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot\nfixture.f:TIF-W prim-axiom dot\n-->" RT-CLASSES
   TINV:RATCHET-UNSET# 1 T=
   TINV:RATCHET-BAD# 1 T=
   \ an uncovered scanned site (no row for its file) is unclassified and fails
   RT-SCAN
   s" ghost.f" s" 0 set-check" TINV:SCAN-SOURCE
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot 3\nfixture.f:TIF-W prim-axiom dot\n-->" RT-CLASSES
   TINV:UNCLASSIFIED# 1 T=
   TINV:RATCHET-BAD# 1 T=
   \ a name at two sites (definition + install) needs an explicit count: 2 ok, 1 GREW
   TINV:RESET
   s" fixture.f" S\" TRUSTED: DUP ( -- )\n' DUP set-check" TINV:SCAN-SOURCE
   TINV:ROWS 2 T=
   S\" <!-- trusted-inventory-classes\nfixture.f:DUP prim-axiom dot 2\n-->" RT-CLASSES
   TINV:RATCHET-BAD# 0 T=
   TINV:RATCHET-STALE# 0 T=
   S\" <!-- trusted-inventory-classes\nfixture.f:DUP prim-axiom dot\n-->" RT-CLASSES
   TINV:RATCHET-GREW# 1 T=
   TINV:RATCHET-BAD# 1 T=
   TINV:CLASSES-RESET ;

: FIX-EFFECTS ( -- )
   s" TRUSTED: TIF-A ( n -- n ) dup ;" FIX-SCAN
   0 TINV:ROW-EFFECT$ s" n -- n" T$=
   s" TRUSTED: TIF-N" FIX-SCAN
   0 TINV:ROW-EFFECT$ s" -" T$=
   s" TRUSTED: TIF-O dup" FIX-SCAN
   0 TINV:ROW-EFFECT$ s" -" T$=
   S\" s\q TIF-W\q s\q n n -- bool\q TRUST" FIX-SCAN
   0 TINV:ROW-EFFECT$ s" n n -- bool" T$=
   s" 0 set-check" FIX-SCAN
   0 TINV:ROW-EFFECT$ s" -" T$= ;

: FIX-CLASSES ( -- )
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot-a\nfixture.f:TIF-W prim-axiom dot-b\nother.f builder-emit dot-c\n-->" TINV:PARSE-CLASSES$ TTRUE
   TINV:CLASSES# 3 T=
   S\" TRUSTED: TIF-A ( -- )\ns\q TIF-W\q s\q -- \q TRUST" FIX-SCAN
   TINV:ROWS 2 T=
   0 TINV:ROW-CLASS$ s" test-metaprog" T$=
   0 TINV:ROW-DOT$ s" dot-a" T$=
   1 TINV:ROW-CLASS$ s" prim-axiom" T$=
   1 TINV:ROW-DOT$ s" dot-b" T$=
   TINV:UNCLASSIFIED# 0 T=
   TINV:RESET
   s" unmapped.f" s" 0 set-check" TINV:SCAN-SOURCE
   TINV:ROWS 1 T=
   0 TINV:ROW-CLASS$ s" -" T$=
   0 TINV:ROW-DOT$ s" -" T$=
   TINV:UNCLASSIFIED# 1 T=
   s" no marker anywhere" TINV:PARSE-CLASSES$ TFALSE
   S\" <!-- trusted-inventory-classes\nfixture.f bogus-class dot-x\n-->" TINV:PARSE-CLASSES$ TFALSE
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog\n-->" TINV:PARSE-CLASSES$ TFALSE
   S\" <!-- trusted-inventory-classes\na.f test-metaprog d\n-->\n<!-- trusted-inventory-classes\nb.f test-metaprog d\n-->" TINV:PARSE-CLASSES$ TFALSE
   TINV:CLASSES-RESET ;

\ ---- scratch owning-dot fixture tree (no dependence on live .dots/) ----------
\ FIX-DOTS builds a private temp tree with one plain-layout dot (<id>.md) and one
\ parent-layout dot (<id>/<id>.md), points TINV's dots root at it, asserts the
\ two exist and a never-created id is missing, then resets the root and removes
\ the tree. Obviously-fake ids keep these fixtures from being mistaken for real
\ dots, and nothing here reads the live .dots/ tree.
$400 constant DF-CAP
create DF-BASE DF-CAP allot
create DF-ROOT DF-CAP allot
create DF-DIR  DF-CAP allot
create DF-FILE DF-CAP allot
variable DF-BASE-U
variable DF-ROOT-U
variable DF-DIR-U
variable DF-FILE-U

: DF-PLAIN-ID ( -- ptr u8 n )   s" tinv-fix-plain-00000001" ;
: DF-PARENT-ID ( -- ptr u8 n )  s" tinv-fix-parent-00000002" ;
: DF-MISSING-ID ( -- ptr u8 n ) s" tinv-fix-missing-00000003" ;

: DF-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: DF-BASE$ ( -- ptr u8 n ) DF-BASE DF-BASE-U @ ;
: DF-ROOT$ ( -- ptr u8 n ) DF-ROOT DF-ROOT-U @ ;
: DF-DIR$  ( -- ptr u8 n ) DF-DIR  DF-DIR-U  @ ;
: DF-FILE$ ( -- ptr u8 n ) DF-FILE DF-FILE-U @ ;

\ Build "<dir>/<id>.md" into DF-FILE for the given dir and dot id.
: DF-FILE-PATH! ( ptr u8 n ptr u8 n -- ) {: da:ptr du:n ia:ptr iu:n :}
   SB-RESET
   da du SB-APPEND FS-MUT-SLASH SB-APPEND-C ia iu SB-APPEND
   s" .md" SB-APPEND
   SB$ DF-FILE DF-FILE-U DF-COPY! ;

: DF-MK-BASE ( -- )
   s" tinv-fix-dots" TMPDIR-MKDIR DF-BASE DF-BASE-U DF-COPY! ;

\ The root passed to TINV:DOTS-ROOT! ends in '/' like the default .dots/.
: DF-MK-ROOT ( -- )
   SB-RESET
   DF-BASE$ SB-APPEND FS-MUT-SLASH SB-APPEND-C
   SB$ DF-ROOT DF-ROOT-U DF-COPY! ;

: DF-MK-PLAIN ( -- )
   DF-BASE$ DF-PLAIN-ID DF-FILE-PATH!
   DF-FILE$ s" plain fixture dot" WRITE-ALL ;

: DF-MK-PARENT ( -- )
   SB-RESET
   DF-BASE$ SB-APPEND FS-MUT-SLASH SB-APPEND-C DF-PARENT-ID SB-APPEND
   SB$ DF-DIR DF-DIR-U DF-COPY!
   DF-DIR$ MAKE-DIR
   DF-DIR$ DF-PARENT-ID DF-FILE-PATH!
   DF-FILE$ s" parent fixture dot" WRITE-ALL ;

: DF-SETUP ( -- )
   DF-MK-BASE
   DF-MK-ROOT
   DF-MK-PLAIN
   DF-MK-PARENT
   DF-ROOT$ TINV:DOTS-ROOT! ;

: DF-TEARDOWN ( -- )
   TINV:DOTS-ROOT-RESET
   DF-BASE$ REMOVE-TREE ;

\ Strict validates that every owning dot referenced by the mapping exists under
\ the dots root (plain <id>.md or parent-dot <id>/<id>.md); each distinct missing
\ dot is reported once. The root points at the scratch tree for this fixture.
: FIX-DOTS ( -- )
   DF-SETUP
   DF-PLAIN-ID TINV:DOT-EXISTS? TTRUE
   DF-PARENT-ID TINV:DOT-EXISTS? TTRUE
   DF-MISSING-ID TINV:DOT-EXISTS? TFALSE
   \ Plain id resolves in the scratch tree; the missing id (referenced twice) is
   \ reported once, so DOTS-MISSING# dedupes to 1.
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog tinv-fix-plain-00000001\nother.f test-metaprog tinv-fix-missing-00000003\nthird.f test-metaprog tinv-fix-missing-00000003\n-->" TINV:PARSE-CLASSES$ TTRUE
   TIT-BUF TIT-CAP LINT-OUT-BUFFER!
   TINV:DOTS-MISSING# 1 T=
   LINT-OUT-BUFFER-OFF
   LINT-OUT$ s" missing owning dot tinv-fix-missing-00000003" CONTAINS? TTRUE
   TINV:CLASSES-RESET
   DF-TEARDOWN ;

\ Strict reports mapping rows no scanned site matches (dead) and repeated
\ file[:name] keys (duplicates, name compare case-insensitive).
: FIX-CMAP-DEAD-DUP ( -- )
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog tinv-fix-dot-00000000\nfixture.f:TIF-W prim-axiom tinv-fix-dot-00000000\ngone.f test-metaprog tinv-fix-dot-00000000\nfixture.f:GONE-NAME test-metaprog tinv-fix-dot-00000000\nfixture.f test-metaprog tinv-fix-dot-00000000\nfixture.f:tif-w prim-axiom tinv-fix-dot-00000000\n-->" TINV:PARSE-CLASSES$ TTRUE
   TINV:CLASSES# 6 T=
   S\" TRUSTED: TIF-A ( -- )\ns\q TIF-W\q s\q -- \q TRUST" FIX-SCAN
   TIT-BUF TIT-CAP LINT-OUT-BUFFER!
   TINV:CMAP-DEAD# 2 T=
   TINV:CMAP-DUP# 2 T=
   LINT-OUT-BUFFER-OFF
   LINT-OUT$ s" dead mapping row gone.f" CONTAINS? TTRUE
   LINT-OUT$ s" dead mapping row fixture.f:GONE-NAME" CONTAINS? TTRUE
   LINT-OUT$ s" duplicate mapping row fixture.f" CONTAINS? TTRUE
   TINV:CLASSES-RESET ;

\ Strict emits a by-file line per source listing its non-zero per-class counts;
\ file-level and file:name rows resolve independently within one file.
: FIX-BY-FILE ( -- )
   S\" <!-- trusted-inventory-classes\nalpha.f prim-axiom tinv-fix-dot-00000000\nbeta.f test-metaprog tinv-fix-dot-00000000\nbeta.f:BX prim-axiom tinv-fix-dot-00000000\n-->" TINV:PARSE-CLASSES$ TTRUE
   TINV:RESET
   s" alpha.f" S\" TRUSTED: AX ( -- )\nTRUSTED: AY ( -- )" TINV:SCAN-SOURCE
   s" beta.f" S\" TRUSTED: BW ( -- )\nTRUSTED: BX ( -- )" TINV:SCAN-SOURCE
   TINV:SORT-ROWS
   TIT-BUF TIT-CAP LINT-OUT-BUFFER!
   TINV:CLASS-BY-FILE-REPORT
   LINT-OUT-BUFFER-OFF
   LINT-OUT$ s" by-file alpha.f prim-axiom 2" CONTAINS? TTRUE
   LINT-OUT$ s" by-file beta.f test-metaprog 1 prim-axiom 1" CONTAINS? TTRUE
   TINV:CLASSES-RESET ;

\ The classification block carries the ratchet count in the fourth token: a
\ file-level row needs an explicit positive count; a named row may omit it
\ (implies 1) or carry one; a zero, negative, non-numeric, or extra token rejects
\ the whole block fail-closed.
: FIX-COUNT-PARSE ( -- )
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot 7\n-->" TINV:PARSE-CLASSES$ TTRUE
   TINV:CLASSES# 1 T=
   S\" <!-- trusted-inventory-classes\nfixture.f:N prim-axiom dot\n-->" TINV:PARSE-CLASSES$ TTRUE
   S\" <!-- trusted-inventory-classes\nfixture.f:N prim-axiom dot 2\n-->" TINV:PARSE-CLASSES$ TTRUE
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot 0\n-->" TINV:PARSE-CLASSES$ TFALSE
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot x\n-->" TINV:PARSE-CLASSES$ TFALSE
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot -1\n-->" TINV:PARSE-CLASSES$ TFALSE
   S\" <!-- trusted-inventory-classes\nfixture.f test-metaprog dot 1 extra\n-->" TINV:PARSE-CLASSES$ TFALSE
   TINV:CLASSES-RESET ;

: TIT-MAIN ( -- )
   T-RESET
   SAVE-LIVE
   LIVE-COUNTS
   LIVE-SORTED
   LIVE-OUTPUT
   LIVE-BASELINE
   LIVE-CLASSES
   FIX-SKIP-PATHS
   FIX-TRUSTED
   FIX-TRUSTED-CASE
   FIX-TRUSTED-NAMELESS
   FIX-TRUSTED-DEFINER-REF
   FIX-COMMENTS
   FIX-STRINGS
   FIX-TRUST
   FIX-TRUST-COMMENT
   FIX-TRUST-PARTIAL
   FIX-SETCHECK
   FIX-HOOK
   FIX-LINES
   FIX-CRLF
   FIX-TRUST-MULTILINE
   FIX-PAREN-WORD
   FIX-SORT
   FIX-EFFECTS
   FIX-CLASSES
   FIX-DOTS
   FIX-CMAP-DEAD-DUP
   FIX-BY-FILE
   FIX-RATCHET
   FIX-COUNT-PARSE
   T-REPORT ;

TIT-MAIN

end-package
