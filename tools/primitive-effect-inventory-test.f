\ primitive-effect-inventory-test.f - checked fixtures for tools/primitive-effect-inventory.f.
\ Run: printf '' | bin/hb --load tools/primitive-effect-inventory-test.f
\ Requiring the tool executes its CLI main: with no script args it scans the three
\ PES-bearing sources and prints the TSV report. The capture buffer records that
\ live run; the live assertions then prove the parsed rows equal #PE, cross-check
\ the live registry, and satisfy the committed TRUSTED.md manifest. In-memory
\ fixtures reuse the row tables to pin identity, formatting stability, and every
\ tamper class (add/delete/duplicate/reorder/mutation) with the exact named row.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f
require tools/argv.f

$40000 constant PIT-CAP
create PIT-BUF PIT-CAP allot

\ Mock an empty argv around the require so the tool's require-time main runs in
\ report mode even inside a gate worker carrying script arguments, capturing its
\ live report through the shared lint sink.
ARGV-MOCK-CLEAR
PIT-BUF PIT-CAP LINT-OUT-BUFFER!
require tools/primitive-effect-inventory.f
LINT-OUT-BUFFER-OFF
ARGV-USE-SCRIPT

package PEINV-TEST

\ ---- helpers ----------------------------------------------------------------
: RSCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   PEINV:RESET  s" fixture.f" a u PEINV:SCAN-SOURCE  PEINV:BUILD-IDS ;
: CAP-BAD# ( -- n )   \ RATCHET-BAD# with diagnostics captured into PIT-BUF
   PIT-BUF PIT-CAP LINT-OUT-BUFFER!
   PEINV:RATCHET-BAD#
   LINT-OUT-BUFFER-OFF ;

\ ---- live repo scan (performed by the tool's require-time main) --------------
\ The report ran at require time, so the row table already holds every live PES
\ axiom row with canonical identities built.
: LIVE-COUNTS ( -- )
   PEINV:ROWS 0 > TTRUE
   PEINV:ROWS PEINV:LIVE-COUNT T= ;

: LIVE-OUTPUT ( -- )
   LINT-OUT$ s" primitive-effect-inventory: PRIM " CONTAINS? TTRUE
   LINT-OUT$ s" src/core/checker.f" CONTAINS? TTRUE ;

\ Every parsed row matches the authoritative in-image registry row-for-row.
: LIVE-CROSS ( -- )
   PEINV:CROSS-CHECK-BAD# 0 T= ;

\ The committed TRUSTED.md manifest matches the live source rows exactly.
: LIVE-BASELINE ( -- )
   s" TRUSTED.md" PEINV:RATCHET-LOAD
   PEINV:BASELINE# 0 > TTRUE
   PEINV:RATCHET-BAD# 0 T= ;

\ ---- identity round-trip ----------------------------------------------------
: FIX-PRIM ( -- )
   s" PRIM: dup PE-A PE-IN PE-A PE-OUT PE-A PE-OUT PRIM;" RSCAN
   PEINV:ROWS 1 T=
   0 PEINV:ROW-KIND 0 T=
   0 PEINV:ROW-PKG$ s" " T$=
   0 PEINV:ROW-NAME$ s" dup" T$=
   0 PEINV:ROW-EFF$ s" pe-a pe-in pe-a pe-out pe-a pe-out" T$=
   0 PEINV:ROW-DIN 1 T=
   0 PEINV:ROW-DOUT 2 T=
   0 PEINV:TRUSTED-ONLY? TFALSE
   0 PEINV:ROW-ID$ s" prim - dup - pe-a pe-in pe-a pe-out pe-a pe-out" T$= ;

: FIX-PPRIM ( -- )
   s" PPRIM: LOWER-CERT MAGIC PE-N PE-OUT PPRIM;" RSCAN
   PEINV:ROWS 1 T=
   0 PEINV:ROW-KIND 1 T=
   0 PEINV:ROW-PKG$ s" lower-cert" T$=
   0 PEINV:ROW-NAME$ s" magic" T$=
   0 PEINV:ROW-DIN 0 T=
   0 PEINV:ROW-DOUT 1 T=
   0 PEINV:ROW-ID$ s" pprim lower-cert magic - pe-n pe-out" T$= ;

: FIX-EMPTY-EFF ( -- )
   s" PRIM: prof-report PRIM;" RSCAN
   PEINV:ROWS 1 T=
   0 PEINV:ROW-EFF$ s" " T$=
   0 PEINV:ROW-ID$ s" prim - prof-report - -" T$= ;

\ A multiline row still pairs its whole effect sequence and counts arity.
: FIX-MULTILINE ( -- )
   S\" PRIM: 2swap PE-A PE-IN PE-B PE-IN PE-C PE-IN PE-D PE-IN\n   PE-C PE-OUT PE-D PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;" RSCAN
   PEINV:ROWS 1 T=
   0 PEINV:ROW-NAME$ s" 2swap" T$=
   0 PEINV:ROW-DIN 4 T=
   0 PEINV:ROW-DOUT 4 T=
   0 PEINV:ROW-EFF$ s" pe-a pe-in pe-b pe-in pe-c pe-in pe-d pe-in pe-c pe-out pe-d pe-out pe-a pe-out pe-b pe-out" T$= ;

\ Line/paren comments and real string bodies never produce a row; the definer
\ definitions of the openers/closers are skipped by the name-position guard.
: FIX-COMMENTS-DEFINERS ( -- )
   S\" \\ PRIM: NOPE PE-N PRIM;\n( PRIM: NOPE PRIM; )\n: PRIM: ( -- ) ;\n: PRIM; ( -- ) ;\n: PPRIM: ( -- ) ;" RSCAN
   PEINV:ROWS 0 T= ;

\ A real string that mentions PRIM: is skipped at idle, but a primitive NAMED
\ with a string-opener lexeme (s", ." ...) is read literally, not swallowed.
: FIX-STRING-NAMES ( -- )
   S\" : FOO ( -- ) s\q hello PRIM: world\q 2drop ;\nPRIM: s\q    PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;\nPRIM: .\q     PRIM;" RSCAN
   PEINV:ROWS 2 T=
   0 PEINV:ROW-NAME$ S\" s\q" T$=
   0 PEINV:ROW-DIN 0 T=
   0 PEINV:ROW-DOUT 2 T=
   1 PEINV:ROW-NAME$ S\" .\q" T$=
   1 PEINV:ROW-EFF$ s" " T$= ;

\ PRIM-TRUSTED-ONLY! marks the immediately preceding row.
: FIX-TRUSTED-ONLY ( -- )
   S\" PRIM: patch32 PE-N PE-IN PE-N PE-IN PRIM;\nPRIM-TRUSTED-ONLY!\nPRIM: snap-rebase PE-N PE-IN PRIM;" RSCAN
   PEINV:ROWS 2 T=
   0 PEINV:TRUSTED-ONLY? TTRUE
   0 PEINV:ROW-ID$ s" prim - patch32 trusted-only pe-n pe-in pe-n pe-in" T$=
   1 PEINV:TRUSTED-ONLY? TFALSE ;

\ Formatting-only edits (case, whitespace, an inline comment, a line break)
\ preserve identity; the two spellings yield the same canonical id.
: FIX-STABILITY ( -- )
   s" PRIM: dup PE-A PE-IN PE-A PE-OUT PE-A PE-OUT PRIM;" RSCAN
   S\" prim:   DUP   pe-a pe-in ( note ) PE-A  PE-OUT\n   pe-a   pe-out   prim;" PEINV:RESET
   s" other.f" 2swap PEINV:SCAN-SOURCE PEINV:BUILD-IDS
   PEINV:ROWS 1 T=
   0 PEINV:ROW-ID$ s" prim - dup - pe-a pe-in pe-a pe-out pe-a pe-out" T$= ;

\ Same spelling, different effect OR different package => distinct identities.
: FIX-DISTINCT ( -- )
   S\" PRIM: dup PE-A PE-IN PE-A PE-OUT PE-A PE-OUT PRIM;\nPRIM: dup PE-A PE-IN PRIM;" RSCAN
   PEINV:ROWS 2 T=
   0 PEINV:ROW-ID$ 1 PEINV:ROW-ID$ T$<>
   S\" PPRIM: alpha foo PE-N PE-OUT PPRIM;\nPPRIM: beta foo PE-N PE-OUT PPRIM;" RSCAN
   PEINV:ROWS 2 T=
   0 PEINV:ROW-ID$ s" pprim alpha foo - pe-n pe-out" T$=
   1 PEINV:ROW-ID$ s" pprim beta foo - pe-n pe-out" T$=
   0 PEINV:ROW-ID$ 1 PEINV:ROW-ID$ T$<> ;

\ ---- ratchet tamper matrix (2-row source, varying committed manifest) --------
: SRC2 ( -- ptr u8 n )
   S\" PRIM: dup PE-A PE-IN PE-A PE-OUT PE-A PE-OUT PRIM;\nPRIM: drop PE-A PE-IN PRIM;" ;
: MAN2 ( -- ptr u8 n )
   S\" <!-- primitive-effect-inventory-manifest\nprim - dup - pe-a pe-in pe-a pe-out pe-a pe-out\nprim - drop - pe-a pe-in\n-->" ;

: FIX-RATCHET-OK ( -- )
   SRC2 RSCAN
   MAN2 PEINV:PARSE-MANIFEST$ TTRUE
   PEINV:BASELINE# 2 T=
   PEINV:RATCHET-BAD# 0 T= ;

: FIX-RATCHET-ADD ( -- )
   SRC2 RSCAN
   S\" <!-- primitive-effect-inventory-manifest\nprim - dup - pe-a pe-in pe-a pe-out pe-a pe-out\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   CAP-BAD# 0 > TTRUE
   LINT-OUT$ s" added row (no committed identity)" CONTAINS? TTRUE
   LINT-OUT$ s" prim - drop - pe-a pe-in" CONTAINS? TTRUE ;

: FIX-RATCHET-DELETE ( -- )
   s" PRIM: dup PE-A PE-IN PE-A PE-OUT PE-A PE-OUT PRIM;" RSCAN
   MAN2 PEINV:PARSE-MANIFEST$ TTRUE
   CAP-BAD# 0 > TTRUE
   LINT-OUT$ s" removed row (committed identity absent from source)" CONTAINS? TTRUE
   LINT-OUT$ s" prim - drop - pe-a pe-in" CONTAINS? TTRUE ;

: FIX-RATCHET-DUP ( -- )
   S\" PRIM: dup PE-A PE-IN PE-A PE-OUT PE-A PE-OUT PRIM;\nPRIM: dup PE-A PE-IN PE-A PE-OUT PE-A PE-OUT PRIM;" RSCAN
   S\" <!-- primitive-effect-inventory-manifest\nprim - dup - pe-a pe-in pe-a pe-out pe-a pe-out\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   CAP-BAD# 0 > TTRUE
   LINT-OUT$ s" duplicated identity (excess copy)" CONTAINS? TTRUE
   LINT-OUT$ s" prim - dup - pe-a pe-in pe-a pe-out pe-a pe-out" CONTAINS? TTRUE ;

: FIX-RATCHET-REORDER ( -- )
   SRC2 RSCAN
   S\" <!-- primitive-effect-inventory-manifest\nprim - drop - pe-a pe-in\nprim - dup - pe-a pe-in pe-a pe-out pe-a pe-out\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   CAP-BAD# 0 > TTRUE
   LINT-OUT$ s" reordered row at position 0" CONTAINS? TTRUE ;

\ A legitimately-repeated identical axiom (e.g. path0/PATH0) with a manifest
\ recording the repeat passes; a THIRD copy fails as an excess.
: FIX-REPEAT-OK ( -- )
   S\" PRIM: path0 PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-OUT PRIM;\nPRIM: PATH0 PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-OUT PRIM;" RSCAN
   S\" <!-- primitive-effect-inventory-manifest\nprim - path0 - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-out\nprim - path0 - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-out\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   PEINV:RATCHET-BAD# 0 T=
   S\" PRIM: path0 PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-OUT PRIM;\nPRIM: path0 PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-OUT PRIM;\nPRIM: path0 PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-OUT PRIM;" RSCAN
   S\" <!-- primitive-effect-inventory-manifest\nprim - path0 - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-out\nprim - path0 - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-out\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   CAP-BAD# 0 > TTRUE
   LINT-OUT$ s" duplicated identity (excess copy)" CONTAINS? TTRUE ;

\ Each mutation class changes identity, surfacing as a removed old + added new row.
: FIX-MUTATIONS ( -- )
   \ effect mutation
   s" PRIM: dup PE-A PE-IN PE-A PE-OUT PRIM;" RSCAN
   S\" <!-- primitive-effect-inventory-manifest\nprim - dup - pe-a pe-in pe-a pe-out pe-a pe-out\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   CAP-BAD# 0 > TTRUE
   LINT-OUT$ s" added row (no committed identity)" CONTAINS? TTRUE
   LINT-OUT$ s" removed row (committed identity absent from source)" CONTAINS? TTRUE
   \ package mutation
   s" PPRIM: alpha foo PE-N PE-OUT PPRIM;" RSCAN
   S\" <!-- primitive-effect-inventory-manifest\npprim beta foo - pe-n pe-out\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   CAP-BAD# 0 > TTRUE
   LINT-OUT$ s" pprim alpha foo - pe-n pe-out" CONTAINS? TTRUE
   \ spelling mutation
   s" PRIM: dup PE-A PE-IN PRIM;" RSCAN
   S\" <!-- primitive-effect-inventory-manifest\nprim - drop - pe-a pe-in\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   CAP-BAD# 0 > TTRUE
   LINT-OUT$ s" prim - dup - pe-a pe-in" CONTAINS? TTRUE
   \ trusted-only mutation
   S\" PRIM: foo PE-N PE-IN PRIM;\nPRIM-TRUSTED-ONLY!" RSCAN
   S\" <!-- primitive-effect-inventory-manifest\nprim - foo - pe-n pe-in\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   CAP-BAD# 0 > TTRUE
   LINT-OUT$ s" prim - foo trusted-only pe-n pe-in" CONTAINS? TTRUE ;

\ Updating the manifest to match a changed source is the explicit migration: pass.
: FIX-MIGRATION ( -- )
   S\" PRIM: dup PE-A PE-IN PE-A PE-OUT PE-A PE-OUT PRIM;\nPRIM: drop PE-A PE-IN PRIM;\nPRIM: newprim PE-N PE-IN PE-N PE-OUT PRIM;" RSCAN
   S\" <!-- primitive-effect-inventory-manifest\nprim - dup - pe-a pe-in pe-a pe-out pe-a pe-out\nprim - drop - pe-a pe-in\nprim - newprim - pe-n pe-in pe-n pe-out\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   PEINV:RATCHET-BAD# 0 T= ;

\ Manifest parse is fail-closed: missing marker, doubled marker, malformed line.
: FIX-MANIFEST-PARSE ( -- )
   s" no marker here at all" PEINV:PARSE-MANIFEST$ TFALSE
   S\" <!-- primitive-effect-inventory-manifest\nprim - dup - pe-a pe-in\n-->" PEINV:PARSE-MANIFEST$ TTRUE
   S\" <!-- primitive-effect-inventory-manifest\nnotakind - foo - pe-n\n-->" PEINV:PARSE-MANIFEST$ TFALSE
   S\" <!-- primitive-effect-inventory-manifest\nprim - foo maybe pe-n\n-->" PEINV:PARSE-MANIFEST$ TFALSE
   S\" <!-- primitive-effect-inventory-manifest\nprim - foo\n-->" PEINV:PARSE-MANIFEST$ TFALSE
   S\" <!-- primitive-effect-inventory-manifest\nprim - dup - pe-a pe-in\n-->\n<!-- primitive-effect-inventory-manifest\nprim - drop - pe-a pe-in\n-->" PEINV:PARSE-MANIFEST$ TFALSE ;

\ Cross-check count-mismatch path: a fixture with fewer rows than #PE fails.
: FIX-CROSS-COUNT ( -- )
   s" PRIM: dup PE-A PE-IN PE-A PE-OUT PE-A PE-OUT PRIM;" RSCAN
   PIT-BUF PIT-CAP LINT-OUT-BUFFER!
   PEINV:CROSS-CHECK-BAD# 0 > TTRUE
   LINT-OUT-BUFFER-OFF
   LINT-OUT$ s" differs from live #PE" CONTAINS? TTRUE ;

: TEST-MAIN ( -- )
   T-RESET
   LIVE-COUNTS
   LIVE-OUTPUT
   LIVE-CROSS
   LIVE-BASELINE
   FIX-PRIM
   FIX-PPRIM
   FIX-EMPTY-EFF
   FIX-MULTILINE
   FIX-COMMENTS-DEFINERS
   FIX-STRING-NAMES
   FIX-TRUSTED-ONLY
   FIX-STABILITY
   FIX-DISTINCT
   FIX-RATCHET-OK
   FIX-RATCHET-ADD
   FIX-RATCHET-DELETE
   FIX-RATCHET-DUP
   FIX-RATCHET-REORDER
   FIX-REPEAT-OK
   FIX-MUTATIONS
   FIX-MIGRATION
   FIX-MANIFEST-PARSE
   FIX-CROSS-COUNT
   T-REPORT ;

TEST-MAIN

;package
