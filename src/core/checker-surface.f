\ checker-surface.f - which checker symbols a capture keeps. src/core/checker.f
\ CHECKER-SWEEP retires what this policy does not keep, at capture only; the
\ checker keeps its own axioms (primitive rows and the primitive control prefix)
\ without asking. KEEP? states the rule.
require src/lib/prelude.f
require src/habu/xref.f
require src/core/internal-mark.f

package CHECKER-SURFACE
private

\ The dictionary index of the live row `wid` holds under the name, -1 for none.
\ xref-search-wl, this package's private row (src/habu/prims.f), is the lookup
\ that returns a DNAME-INT row too: `search-wl` hides one, and whether such a
\ row ships is not this file's call.
: RECORD-INDEX ( ptr u8 n n -- n )
   xref-search-wl dup 0= if drop -1 exit then
   BYTE-VIEW NULL-PTR BYTE-VIEW - dbase@ - DREC / ;

: SHIPPED? ( ptr u8 n n -- bool )
   RECORD-INDEX dup 0 < if drop false exit then
   CHECKER-SWEEP:NAMED? ;

\ The scope key CHECKER-SWEEP hands over: package name, public?, word name; an
\ empty package is the global scope. A symbol stays exactly when a record the
\ image ships with its name (CHECKER-SWEEP:NAMED?) resolves to it as source reads
\ it. A DNAME-INT row counts: a TRUSTED: body still calls it and the native
\ compiler asks its arity. A sealed package's privates have no spelling: habu1.f
\ FIND-NMATCH qualifies through the public wid, and habu2.f C-PACKAGE-PROT-GUARD
\ refuses reopening it. The whitebox image keeps everything: its suites reopen
\ engine packages (src/core/internal-mark.f).
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
