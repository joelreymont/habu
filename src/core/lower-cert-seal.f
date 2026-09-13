\ lower-cert-seal.f — erase lowering-certificate producer authority.
\ Loaded after xref installs `undefine`; compiled calls retain direct xts.
\
\ Both producers are now reached through declared dispatch cells (`defer`), so
\ this file retires the two things that can still GRANT authority: the name of
\ each dispatch cell, without which no later source can write `is` at it, and
\ the flag that records the grant. Undefining a name never removes the cell or
\ the code, which is why the already-compiled callers below the seal keep
\ dispatching and why the cells stay in the snapshot address-cell table and go
\ on being relocated (dot habu-declare-persisted-producer-76fbce09).

undefine CHECKER-CERT:INSTALL
undefine CHECKER-CERT:PRODUCE

package CHECKER-CERT
undefine PRODUCER-XT
undefine PRODUCER-SET
;package

package LOWER-CERT
undefine FULL-INSTALL
undefine FULL-PRODUCE-INSTALL
undefine FULL-PRODUCE
undefine DISPATCH-INSTALL
undefine DISPATCH
undefine FULL-XT
undefine FULL-SET
;package

\ ---------------------------------------------------------------------------
\ THE CORE PREFIX ENDS HERE, and this records where.
\
\ The mark's invariant is POSITION: it must be taken at the boundary, and the
\ boundary is the end of this file. src/habu/habu2.f EMIT-HOST-LOAD-PREFIX
\ loads it after the core files and their provided rows, before the stdlib.
\
\ WHO READS IT. src/habu/prefix-rewind.f, at the head of every generated engine
\ source. That source used to truncate the dictionary back to util.f's first
\ record and recompile this whole prefix on top of the orphaned copy; it rewinds
\ to HERE instead, so the compiling host's own boot copy stays live.
\
\ EVERY CURSOR THAT ADVANCES PAST THIS POINT goes back through its owner's
\ truncation seam, and the count is not written down here: the checker owns the
\ list of marks a scope invalidates, so it owns the boundary that carries them
\ (src/core/checker.f CHECKER-BOUND), and CURSORS reports how many off the record
\ rather than from a sentence that ages. Two more are the engine's own and are
\ read here: the dictionary end, and the include registry's row count. Each one
\ left out is a store still describing records the rewind removes - measured,
\ three times: an image that claimed to carry the stdlib and did not, a
\ `duplicate family at 'option'` on the first `require`, and a warm image whose
\ first type declaration stored through a stale pointer and died.
\
\ The dictionary boundary follows this package's final private word, BOUNDARY.
\ Resolve its wordlist through the live namespace: an AOT restore relocates
\ record ordinals and WIDs. The package is protected at the mark, so ordinary
\ source cannot reopen it or retire/rebind that private word. A public marker
\ would be insufficient: `undefine` may retire a public word even in a protected
\ package. The include registry and checker marks remain their owners' counts.
\
\ The capture is this file's last act, so the package's own records are below
\ the mark and survive the rewind that reads them.

package PREFIX-MARK

private

variable RQ
variable CU

public

\ NOT spelled NDICT@ / UEND@. Habu folds case, so a tail spelled like the
\ engine word it wraps IS that word inside this package block, and the capture
\ below would read its own empty accessor instead of the dictionary. Measured:
\ the mark recorded 0, and a build takes that as "truncate everything".
: DICT ( -- n )
   s" PREFIX-MARK" XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if
      drop s" prefix boundary: source namespace is missing" 76 die
   then
   XREF-LEN
   s" BOUNDARY" rot XREF-FIND-WL-INDEX
   dup 0 < if s" prefix boundary: final source record is missing" 76 die then
   1+ ;

: REQ ( -- n )
   RQ @ ;

\ The build's capability probe (tools/build-fixpoint.f). It is DATA, not a
\ spelling: only CHECKER-BOUND:MARK gives it a value, so an engine that carries
\ the words but never took the boundary reads zero here and is refused before a
\ byte is emitted.
: CURSORS ( -- n )
   CU @ ;

private

: BOUNDARY ( -- ) ;

get-current prot-wid-add
public
get-current prot-wid-add
private

CHECKER-BOUND:MARK
REQUIRE-REG:COUNT RQ !
CHECKER-BOUND:CURSORS CU !

;package
