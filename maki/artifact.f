\ maki/artifact.f - the built-artifact identity registry (dot
\ habu-public-producers-for-7084d81c).
\
\ Semantic identity: a built artifact IS its section-7.4 store key (the
\ region/target/shape/dtype/layout key that maki/sched-key.f SK-KEY$ renders and
\ maki/store.f keys every artifact row on). REGISTER interns that key into an
\ append-only registry: equal keys intern to ONE CAD-KIND:artifact-id
\ (content-addressed object identity, MODEL-CAD-V2-PLAN.md section 9.1), and every id
\ is the genuine registered identity - never a free-floating raw. This is the public
\ producer the R7 evidence/policy layer was missing: before it, the promotion suite
\ fabricated artifact ids through a test-only TRUSTED boundary (the retired T>AID).
\
\ Raw representation conversions stay PRIVATE (the maki/target/target.f RAW>TARGET-ID /
\ TARGET-ID>RAW pattern): the only public producer is REGISTER, bound to a real key
\ registration, so a raw n cannot forge an artifact-id. maki -> habu only; artifact owns
\ -5244..-5246.

require lib/prelude.f
require lib/string.f
require maki/cad-kinds.f

-5244 constant E-ARTIFACT-CAP    \ registry count or key-store bytes exhausted
-5245 constant E-ARTIFACT-KEY    \ empty key or key over the per-key byte cap
-5246 constant E-ARTIFACT-ID     \ artifact-id outside the registered range

package ARTIFACT
private

256 constant ART-CAP                     \ max distinct registered artifacts (v1)
256 constant ART-KEY-MAX                  \ max bytes per key
ART-CAP ART-KEY-MAX * constant ART-KEY-CAP

create ART-KEYS ART-KEY-CAP allot         \ interned key bytes, back to back
create ART-KO   ART-CAP cells allot        \ per-id key offset into ART-KEYS
create ART-KL   ART-CAP cells allot        \ per-id key length
variable ART-KEY-U                          \ bytes used in ART-KEYS
variable ART-N                              \ registered count

TRUSTED: RAW>ARTIFACT-ID ( n -- CAD-KIND:artifact-id ) ;
TRUSTED: ARTIFACT-ID>RAW ( CAD-KIND:artifact-id -- n ) ;

: ID-CK ( CAD-KIND:artifact-id -- n )
   ARTIFACT-ID>RAW dup 0 < over ART-N @ >= or if E-ARTIFACT-ID throw then ;

: KEY-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u ART-KEY-MAX > or if E-ARTIFACT-KEY throw then ;

: KEY@ ( n -- ptr u8 n ) {: raw:n :}         \ stored key bytes for a validated raw id
   ART-KEYS raw cells ART-KO + @ +  raw cells ART-KL + @ ;

: KEY-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}  \ raw id of an equal registered key, or -1
   ART-N @ 0 ?do
      i KEY@ a u STR= if i unloop exit then
   loop -1 ;

: KEY-PUT ( ptr u8 n n -- ) {: a:ptr u:n raw:n :}
   ART-KEY-U @ u + ART-KEY-CAP > if E-ARTIFACT-CAP throw then
   ART-KEY-U @ {: off:n :}
   a ART-KEYS off + u BYTE-COPY
   off raw cells ART-KO + !
   u   raw cells ART-KL + !
   off u + ART-KEY-U ! ;

public

\ REGISTER interns an artifact by its store key: an equal key returns the same id
\ (content-addressed), a fresh key appends a new registry slot and mints its id.
: REGISTER ( ptr u8 n -- CAD-KIND:artifact-id ) {: a:ptr u:n :}
   a u KEY-CK
   a u KEY-FIND {: found:n :}
   found 0 >= if found RAW>ARTIFACT-ID exit then
   ART-N @ ART-CAP >= if E-ARTIFACT-CAP throw then
   ART-N @ {: raw:n :}
   a u raw KEY-PUT
   raw 1+ ART-N !
   raw RAW>ARTIFACT-ID ;

\ KEY$ projects an artifact id back to its registered store key.
: KEY$ ( CAD-KIND:artifact-id -- ptr u8 n )  ID-CK KEY@ ;

\ EQUAL? is same-artifact identity: interning makes equal keys share one raw id, so
\ raw equality is exactly key equality. It is the value-level artifact comparison
\ POLICY:CHECK / ART:PROMOTE use (retiring maki/evidence/policy.f's AID= boundary).
: EQUAL? ( CAD-KIND:artifact-id CAD-KIND:artifact-id -- bool )
   {: x:CAD-KIND:artifact-id y:CAD-KIND:artifact-id :}
   x ARTIFACT-ID>RAW y ARTIFACT-ID>RAW = ;

\ VALIDATE fails closed on an out-of-range id (the target.f VALIDATE precedent).
: VALIDATE ( CAD-KIND:artifact-id -- CAD-KIND:artifact-id )  dup ID-CK drop ;

: COUNT ( -- n )  ART-N @ ;

;package
