\ manifest-lint-core.f - the engine manifest against the compiler/JIT/REPL closure.
\
\ Joel's rule (2026-09-16): the only packages in the Habu binary are the compiler,
\ the JIT and the REPL; no package and no type signature reaches the image unless
\ one of those three requires it. src/habu/native-runtime.f is the manifest that
\ decides, and it is a list of `s" path" required` rows anyone can append to. This
\ lint is what stops that. It reads the rows, walks the require graph out of the
\ entry points declared below, and refuses a row that nothing in the closure
\ requires and that is not itself an entry point.
\
\ An ENTRY POINT is a file the engine loads for its own sake rather than because
\ something requires it: the checker completions, the seal passes, the compiler,
\ the REPL and its terminal. Each entry carries the reason it is in the image, so
\ adding one is a deliberate edit reviewed as policy rather than a line appended
\ to the manifest. Everything else must earn its place through a require edge.
\
\ SCOPE. The lint reads the manifest's `required` rows. The `provided` rows above
\ them are the boot prefix the engine already carries when the manifest runs; they
\ are the engine's own source, not a choice this lint can second-guess.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/lib.f

package MANIFEST-LINT
private

-9260 constant E-ML-PATHS      \ more distinct paths than the table holds
-9261 constant E-ML-TEXT       \ more path text than the buffer holds
-9262 constant E-ML-FINDING    \ a manifest row is outside the closure

$4000 constant ML-TEXT-CAP
$200 constant ML-MAX

create ML-TEXT ML-TEXT-CAP allot
create ML-OFF ML-MAX cells allot
create ML-LEN ML-MAX cells allot
create ML-FLAG ML-MAX cells allot

1 constant ML-ROW              \ named by a manifest row
2 constant ML-REACHED          \ required by something in the closure
4 constant ML-ENTRY            \ declared entry point
8 constant ML-SCANNED          \ its own requires have been read

variable ML-N        variable ML-U
variable ML-I        variable ML-FOUND
variable ML-CUR      variable ML-MARK
variable ML-MODE     variable ML-BAD
variable ML-MORE

: NL ( -- ) 10 emit ;

20 constant ML-NCAP
create ML-NBUF ML-NCAP allot
variable ML-NI

: EMIT-U ( n -- ) {: v:n :}                      \ unsigned decimal, digits built tail first
   v 0= if 48 emit exit then
   ML-NCAP ML-NI !
   v begin dup 0 > while
      dup 10 mod 48 +
      ML-NI @ 1- ML-NI !
      ML-NBUF ML-NI @ + c!
      10 /
   repeat drop
   ML-NBUF ML-NI @ +  ML-NCAP ML-NI @ -  type ;

: ML-PATH$ ( n -- ptr u8 n ) {: i:n :}
   ML-TEXT i cells ML-OFF + @ +  i cells ML-LEN + @ ;

: ML-FLAG@ ( n -- n )
   cells ML-FLAG + @ ;

: ML-FLAG+ ( n n -- ) {: bit:n i:n :}
   i ML-FLAG@ bit or  i cells ML-FLAG + ! ;

: ML-HAS? ( n n -- bool ) {: bit:n i:n :}
   i ML-FLAG@ bit and 0 <> ;

: ML-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}      \ index, or -1 when absent
   -1 ML-FOUND !
   0 ML-I !
   begin ML-I @ ML-N @ <  ML-FOUND @ 0 < and while
      ML-I @ ML-PATH$ a u LINT-STR= if ML-I @ ML-FOUND ! then
      ML-I @ 1+ ML-I !
   repeat
   ML-FOUND @ ;

: ML-INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}    \ index, adding the path once
   a u ML-FIND dup 0 >= if exit then drop
   ML-N @ ML-MAX >= if E-ML-PATHS throw then
   ML-U @ u + ML-TEXT-CAP > if E-ML-TEXT throw then
   ML-U @  ML-N @ cells ML-OFF + !
   u       ML-N @ cells ML-LEN + !
   0       ML-N @ cells ML-FLAG + !
   a  ML-TEXT ML-U @ +  u >LEN BYTE-COPY-LEN
   ML-U @ u + ML-U !
   ML-N @  ML-N @ 1+ ML-N ! ;

\ ---- one require edge, in whichever mode the scan is running -------------------
\ Manifest mode records a row; closure mode records reachability and queues the
\ file. A path can be both, which is the ordinary case for lib/string.f.

: ML-NOTE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u ML-INTERN {: i:n :}
   ML-MODE @ 0= if ML-ROW i ML-FLAG+ exit then
   ML-REACHED i ML-FLAG+ ;

\ ---- line shapes ---------------------------------------------------------------
\ `require <path>` and `s" <path>" required` are the two spellings the tree uses.
\ Both are read from the TRIMMED line's leading token, so a `\` comment and a
\ path inside a generated source string (`s" ... require x ..." GE-SRC-LINE`) are
\ not require edges and are not read as one.

: ML-SQ? ( ptr u8 n -- bool ) {: a:ptr u:n :}    \ the token `s"`
   u 2 <> if 0 0= 0= exit then
   a c@ 115 <> if 0 0= 0= exit then
   a 1 + c@ 34 = ;

: ML-UNQUOTE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ drop one trailing `"`
   u 0= if a u exit then
   a u 1 - + c@ 34 = if a u 1 - exit then
   a u ;

: ML-REQUIRE-LINE ( -- )                          \ `require <path>`
   1 LINT-SPLIT:S@ ML-NOTE ;

: ML-REQUIRED-LINE ( -- )                         \ `s" <path>" required`
   1 LINT-SPLIT:S@ ML-UNQUOTE ML-NOTE ;

: ML-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LINT-TRIM {: b:ptr v:n :}
   v 0= if exit then
   b v LINT-SPLIT:SPLIT-WHITESPACE
   LINT-SPLIT:SN# @ 2 < if exit then
   0 LINT-SPLIT:S@ s" require" LINT-STR= if ML-REQUIRE-LINE exit then
   LINT-SPLIT:SN# @ 3 < if exit then
   0 LINT-SPLIT:S@ ML-SQ? 0= if exit then
   2 LINT-SPLIT:S@ s" required" LINT-STR= if ML-REQUIRED-LINE then ;

: ML-TEXT-LINES ( ptr u8 n -- ) {: a:ptr u:n :}
   0 ML-CUR !  0 ML-MARK !
   begin ML-CUR @ u < while
      a ML-CUR @ + c@ 10 = if
         a ML-MARK @ +  ML-CUR @ ML-MARK @ -  ML-LINE
         ML-CUR @ 1+ ML-MARK !
      then
      ML-CUR @ 1+ ML-CUR !
   repeat
   ML-MARK @ u < if a ML-MARK @ +  u ML-MARK @ -  ML-LINE then ;

: ML-SCAN-FILE ( ptr u8 n -- ) {: p:ptr pu:n :}
   p pu FILE? 0= if exit then
   p pu LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT ML-TEXT-LINES ;

\ ---- the declared entry points -------------------------------------------------
\ Every row states why the engine carries the file. A file that no row names and
\ that nothing in the closure requires does not belong in the image.

: ML-ENTRY+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u ML-INTERN {: i:n :}
   ML-ENTRY i ML-FLAG+
   ML-REACHED i ML-FLAG+ ;

: ML-ENTRY-POINTS ( -- )
   s" src/core/enums.f" ML-ENTRY+                              \ the ENUM front end the checker parses
   s" src/core/sha256.f" ML-ENTRY+                             \ content keys the compiler and snapshots write
   s" src/core/type-family-sha.f" ML-ENTRY+                    \ the family identity the checker hashes
   s" src/core/combinators.f" ML-ENTRY+                        \ the quotation combinators the checker certifies
   s" src/habu/code-span.f" ML-ENTRY+                          \ the code spans the emitters record
   s" src/habu/xref.f" ML-ENTRY+                               \ the cross-reference registry the REPL reads
   s" src/core/generated-declaration-dictionary.f" ML-ENTRY+   \ generated declarations the checker publishes
   s" src/core/generated-declaration-protection.f" ML-ENTRY+   \ their protection pass
   s" src/core/layout-buffer-seal.f" ML-ENTRY+                 \ seals the layout-buffer boundary
   s" src/core/dynamic-storage.f" ML-ENTRY+                    \ the dynamic buffers the compiler allocates
   s" src/core/checker-owner-guard.f" ML-ENTRY+                \ the cast-ownership guard
   s" src/core/lower-cert-seal.f" ML-ENTRY+                    \ seals the lowering certificate
   s" src/os/script-argv.f" ML-ENTRY+                          \ the argv the REPL and the drivers read
   s" src/core/internal-mark.f" ML-ENTRY+                      \ the seal pass over the pre-checker definitions
   s" src/compiler/native/compiler.f" ML-ENTRY+                \ the compiler and the JIT
   s" src/os/linux/repl-term.f" ML-ENTRY+                      \ the REPL terminal, linux
   s" src/os/macos/repl-term.f" ML-ENTRY+                      \ the REPL terminal, macos
   s" src/habu/repl.f" ML-ENTRY+                               \ the REPL
   s" src/core/top-row.f" ML-ENTRY+ ;                          \ the top-level row tracker the REPL warns from

\ ---- the closure walk ----------------------------------------------------------

: ML-NEXT-UNSCANNED ( -- n )                     \ a reached, unscanned index, or -1
   -1 ML-FOUND !
   0 ML-I !
   begin ML-I @ ML-N @ <  ML-FOUND @ 0 < and while
      ML-REACHED ML-I @ ML-HAS?  ML-SCANNED ML-I @ ML-HAS? 0= and
         if ML-I @ ML-FOUND ! then
      ML-I @ 1+ ML-I !
   repeat
   ML-FOUND @ ;

: ML-WALK-ONE ( n -- ) {: i:n :}
   ML-SCANNED i ML-FLAG+
   i ML-PATH$ ML-SCAN-FILE ;

: ML-WALK ( -- )
   1 ML-MODE !
   -1 ML-MORE !
   begin ML-MORE @ while
      ML-NEXT-UNSCANNED dup 0 < if drop 0 ML-MORE ! else ML-WALK-ONE then
   repeat ;

\ ---- findings ------------------------------------------------------------------

: ML-STRAY? ( n -- bool ) {: i:n :}
   ML-ROW i ML-HAS? 0= if 0 0= 0= exit then
   ML-ENTRY i ML-HAS? if 0 0= 0= exit then
   ML-REACHED i ML-HAS? 0= ;

: ML-REPORT-STRAY ( n -- ) {: i:n :}
   s" manifest-lint: " type
   i ML-PATH$ type
   s"  is in the manifest but nothing in the compiler, JIT or REPL closure" type NL
   s" manifest-lint:   requires it and it is not a declared entry point" type NL
   ML-BAD @ 1+ ML-BAD ! ;

: ML-CHECK ( -- )
   0 ML-I !
   begin ML-I @ ML-N @ < while
      ML-I @ ML-STRAY? if ML-I @ ML-REPORT-STRAY then
      ML-I @ 1+ ML-I !
   repeat ;

: ML-COUNT ( n -- n ) {: bit:n :}               \ how many paths carry the bit
   0 ML-FOUND !
   0 ML-I !
   begin ML-I @ ML-N @ < while
      bit ML-I @ ML-HAS? if ML-FOUND @ 1+ ML-FOUND ! then
      ML-I @ 1+ ML-I !
   repeat
   ML-FOUND @ ;

: ML-RESET ( -- )
   0 ML-N !  0 ML-U !  0 ML-BAD ! ;

: ML-SUMMARY ( -- )
   s" manifest-lint: " type
   ML-ROW ML-COUNT EMIT-U s"  manifest row(s), " type
   ML-ENTRY ML-COUNT EMIT-U s"  entry point(s), " type
   ML-REACHED ML-COUNT EMIT-U s"  file(s) in the closure, " type
   ML-BAD @ EMIT-U s"  finding(s)" type NL ;

public

\ Test seam: read one literal as if it were a file, in closure mode, and answer
\ how many distinct require edges it yielded. tools/manifest-lint-test.f drives
\ the two line shapes and the near misses through this; it touches no file and
\ leaves the table reset for the next caller.
: EDGES-IN ( ptr u8 n -- n )
   ML-RESET
   1 ML-MODE !
   ML-TEXT-LINES
   ML-REACHED ML-COUNT ;

\ The one pass: read a manifest's rows, walk the closure, report the strays.
\ The path is an argument so a fixture manifest can be checked against the real
\ entry points and the real tree - which is the only way to prove the lint still
\ says no (tools/manifest-lint-stray-fixture.f).
: RUN-MANIFEST ( ptr u8 n -- ) {: a:ptr u:n :}
   ML-RESET
   ML-ENTRY-POINTS
   0 ML-MODE !
   a u ML-SCAN-FILE
   ML-WALK
   ML-CHECK
   ML-SUMMARY ;

: RUN ( -- )
   s" src/habu/native-runtime.f" RUN-MANIFEST ;

: FINDINGS ( -- n )
   ML-BAD @ ;

: STRICT ( -- )
   RUN
   ML-BAD @ 0 > if E-ML-FINDING throw then ;

;package
