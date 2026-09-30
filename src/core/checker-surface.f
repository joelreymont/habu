\ checker-surface.f - which checker symbols a capture keeps.
\
\ The checker names every word it knows by a symbol, and a capture used to bake
\ all of them: the symbols of words whose records the image strips, of private
\ words no source can reach, and of lookups that never became words - 359,115 B
\ of a product engine's checker stores described words with no record in it.
\ src/core/checker.f CHECKER-SWEEP retires what this policy does not keep, at
\ capture and nowhere else; this file only answers, per symbol, whether the image
\ being written can still spell it. The checker keeps its own axioms (primitive
\ rows and the primitive control prefix) without asking.
\
\ A SYMBOL STAYS EXACTLY WHEN A RECORD THE IMAGE SHIPS WITH ITS NAME RESOLVES TO
\ IT, read the way source reads it:
\   - the record is the live row its scope's wordlist holds under its name:
\     wordlist 0 for a global, the package's public or private wordlist for a
\     package symbol. A DNAME-INT row counts like any other - such a word is not
\     callable from checked source, but a TRUSTED: body still calls it, and the
\     native compiler asks this checker for its arity;
\   - whether that record ships with its name is the capture's own answer
\     (CHECKER-SWEEP:NAMED?): every live record for a snapshot, the native
\     build's naming decision for its image (src/habu/aot-capture.f);
\   - a private symbol also needs a package that can be reopened. A sealed
\     package's public wordlist carries the protected bit, the one wid habu2.f
\     C-PACKAGE-PROT-GUARD reads before refusing `package NAME`, and a private
\     word has no qualified spelling (habu1.f FIND-NMATCH qualifies through the
\     public wid), so a sealed package leaves its privates no spelling at all;
\   - a symbol whose package has no namespace row names nothing;
\   - the whitebox image keeps everything, the verdict src/core/internal-mark.f
\     wrote into the image, because its suites reopen engine packages by design.
require lib/prelude.f
require src/habu/xref.f
require src/core/internal-mark.f

package CHECKER-SURFACE
private

\ The dictionary index of the live row `wid` holds under the name, -1 for none.
\ The trusted-only primitive is the lookup that returns a DNAME-INT row too:
\ `search-wl` hides one, and whether such a row ships is not this file's call.
TRUSTED: RECORD-INDEX ( ptr u8 n n -- n )
   xref-search-wl dup 0= if drop -1 exit then
   dbase@ - DREC / ;

: SHIPPED? ( ptr u8 n n -- bool )
   RECORD-INDEX dup 0 < if drop false exit then
   CHECKER-SWEEP:NAMED? ;

\ The scope key CHECKER-SWEEP hands over: package name, public?, word name. An
\ empty package is the global scope.
: KEEP? ( ptr u8 n bool ptr u8 n -- bool ) {: pa:ptr pu:n pub:bool na:ptr nu:n :}
   ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-WHITEBOX = if true exit then
   pu 0= if na nu 0 SHIPPED? exit then
   pa pu XREF-NAMESPACE-WL RECORD-INDEX {: row:n :}
   row 0 < if false exit then
   row XREF-REC {: pkg:ptr :}
   pub if na nu pkg XREF-PKG-PUBLIC SHIPPED? exit then
   pkg XREF-PKG-PUBLIC XREF-WID-PROTECTED? if false exit then
   na nu pkg XREF-PKG-PRIVATE SHIPPED? ;

: INSTALL ( -- )
   [: KEEP? ;] CHECKER-SWEEP:INSTALL ;

' INSTALL
;package
execute
