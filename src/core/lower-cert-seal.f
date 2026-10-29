\ lower-cert-seal.f — erase lowering-certificate producer authority, and record
\ the end of the core prefix (PREFIX-MARK). Loaded after xref installs
\ `undefine`. Undefining a dispatch cell's name stops a later `is` at it; the
\ cell and its code stay, so compiled callers keep dispatching and the snapshot
\ address-cell table keeps relocating the cell.

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

\ The core prefix ends here, and the mark must be taken at the end of this file:
\ src/habu/habu2.f EMIT-HOST-LOAD-PREFIX loads it after the core files and their
\ provided rows, before the stdlib. src/habu/prefix-rewind.f, at the head of
\ every generated engine source, rewinds to it so the compiling host's boot copy
\ stays live.
\
\ Every cursor that advances past this point goes back through its owner's
\ truncation seam: the checker owns the marks a scope invalidates
\ (src/core/checker.f CHECKER-BOUND) and CURSORS reports how many. The dictionary
\ end and the include registry's row count are read here. Each one left out is a
\ store describing records the rewind removed (measured three times: a missing
\ stdlib, `duplicate family at 'option'`, a store through a stale pointer).
\
\ The mark is BOUNDARY's code entry, not an ordinal: an AOT restore re-registers
\ every record, so the index moves, while the entry is relocated with the code
\ (`xt!` declares the cell). The package is protected at the mark and `undefine`
\ leaves the record in place, so neither can move the boundary. The capture is
\ this file's last act, so the package's own records survive the rewind.

package PREFIX-MARK

private

variable RQ
variable CU
\ The marker's entry, stored after BOUNDARY exists and declared to the engine's
\ address-cell table by `xt!` so the restore relocates it. Declared HERE, above
\ the boundary, because everything this package owns must sit below the mark.
variable B-XT

public

\ NOT spelled NDICT@ / UEND@. Habu folds case, so a tail spelled like the
\ engine word it wraps IS that word inside this package block, and the capture
\ below would read its own empty accessor instead of the dictionary. Measured:
\ the mark recorded 0, and a build takes that as "truncate everything".
: DICT ( -- n )
   B-XT @ {: entry:n :}
   entry 0= if
      s" prefix boundary: the marker's entry was never taken" 76 die
   then
   ndict@ 0 ?do
      i XREF-REC XREF-START entry = if i 1+ unloop exit then
   loop
   s" prefix boundary: final source record is missing" 76 die ;

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

' BOUNDARY B-XT xt!

get-current prot-wid-add
public
get-current prot-wid-add
private

CHECKER-BOUND:MARK
REQUIRE-REG:COUNT RQ !
CHECKER-BOUND:CURSORS CU !

;package
