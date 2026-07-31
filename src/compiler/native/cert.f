\ cert.f - the source-bound checking result: one verdict, and the two digests
\ that say exactly what it was reached over.
\
\ docs/compiler-ir-design.md section 7.1: "the existing checker may continue to
\ run over source text, but its result and lowering certificate must bind to
\ the source-tape digest". This file is that binding as a value. A stage that
\ holds one of these can prove the tape in its hand is the tape the checker
\ read, and the bytes behind that tape are the bytes the checker read - or it
\ is refused.
\
\ WHY TWO DIGESTS AND NOT ONE. The tape digest covers the cells the tape owns:
\ kinds, modes, spans, spelling ordinals, literals, origins. It does not cover
\ the bytes behind a span or behind a spelling, and it cannot: a module numbers
\ its own symbols, so a definition and the same definition with one name letter
\ changed intern their names at the same ordinal, produce spans of the same
\ length, and digest to the same tape. The registry's per-source content digest
\ is what tells those two apart. Binding one without the other would leave a
\ result that a one-byte edit can slip past, so a result binds both, and
\ src/compiler/native/tape.f said so before this file existed: "a stage that
\ needs content identity binds both".
\
\ WHAT IT REFUSES. Exactly one thing: a presented tape or source registry whose
\ digest is not the one this result bound. It is not a second checker - it has
\ no opinion about whether the verdict was the right verdict, only about what
\ the verdict was reached over.
\
\ WHAT IT DOES NOT DECIDE. Nothing about lowering. src/core/lower-cert-base.f
\ owns the engine's lowering certificate and its own body hash; this result is
\ the native pipeline's, it is a value rather than a byte blob, and neither one
\ reads the other's fields.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/arena.f
require src/compiler/ir/source.f
require src/compiler/native/tape.f

package NCERT
public

\ The verdict, the source it was read from, the digest of the tape that source
\ was read into, and the digest of the source's own bytes. The verdict is the
\ checker's own scale - certified, rejected, or uncheckable - and this package
\ does not reinterpret it. There is no derived equality: `DERIVE eq` is a
\ scalar-field facility and both digests are structures, so two results are
\ compared through the two digest readers below, each against CDIGEST's own
\ typed compare.
STRUCTURE result 0
   FIELD verdict n
   FIELD src IR-ID:ir-source-id
   FIELD tape CDIGEST:digest
   FIELD text CDIGEST:digest
;STRUCTURE

\ ---- readers -----------------------------------------------------------------
\ A result is unmade at entry rather than bound to a local: the checker cannot
\ yet bind a local of a multi-cell structure type (dot
\ habu-bind-multi-cell-d2e153ed), the same step src/compiler/native/tape.f
\ takes with its spans.
: VERDICT ( NCERT:result -- n )
   NCERT-RESULT:UNMAKE
   CDIGEST-DIGEST:UNMAKE 2drop 2drop
   CDIGEST-DIGEST:UNMAKE 2drop 2drop
   drop ;

\ The source the scan was read from. A caller that wants the bytes, their
\ length, or a span back has to name the ordinal the registry minted inside the
\ scan, and this result is where that ordinal is published.
: SOURCE ( NCERT:result -- IR-ID:ir-source-id )
   NCERT-RESULT:UNMAKE
   CDIGEST-DIGEST:UNMAKE 2drop 2drop
   CDIGEST-DIGEST:UNMAKE 2drop 2drop
   nip ;

: TAPE-DIGEST ( NCERT:result -- CDIGEST:digest )
   NCERT-RESULT:UNMAKE
   CDIGEST-DIGEST:UNMAKE 2drop 2drop
   CDIGEST-DIGEST:UNMAKE {: p0:n p1:n p2:n p3:n :}
   drop drop
   p0 p1 p2 p3 CDIGEST-DIGEST:MAKE ;

: TEXT-DIGEST ( NCERT:result -- CDIGEST:digest )
   NCERT-RESULT:UNMAKE
   CDIGEST-DIGEST:UNMAKE {: t0:n t1:n t2:n t3:n :}
   CDIGEST-DIGEST:UNMAKE 2drop 2drop
   drop drop
   t0 t1 t2 t3 CDIGEST-DIGEST:MAKE ;

\ ---- the binding -------------------------------------------------------------
\ Recompute both digests and refuse a result that does not match. The tape half
\ goes through NTAPE:VERIFY, so the tape's own authority does the comparing and
\ answers with its own name; the source half asks the frozen registry for the
\ content digest it recorded when the bytes were registered. The two views are,
\ in order, the sealed tape and the module's frozen source registry.
: VERIFY ( IR-ARENA:view IR-ARENA:view NCERT:result -- )
   NCERT-RESULT:UNMAKE
   CDIGEST-DIGEST:UNMAKE {: t0:n t1:n t2:n t3:n :}
   CDIGEST-DIGEST:UNMAKE {: p0:n p1:n p2:n p3:n :}
   {: tv:IR-ARENA:view sv:IR-ARENA:view verdict:n src:IR-ID:ir-source-id :}
   tv  p0 p1 p2 p3 CDIGEST-DIGEST:MAKE  NTAPE:VERIFY
   sv src IR-SOURCE:FDIGEST@  t0 t1 t2 t3 CDIGEST-DIGEST:MAKE  CDIGEST-DIGEST:EQ
   0= if E-NCERT-DIGEST throw then ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
