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
\ -5244..-5246 + -5248.
\
\ CROSS-PROCESS WIRE FORM (KEY>WIRE / WIRE>KEY, § 23.9 origin-class table, content-
\ addressed registry intern row). The artifact-id / dependency wire form the envelope
\ codec (maki/db/artifact.f) first shipped stored the PROCESS-LOCAL registry raw, which
\ means a different artifact in another process. KEY>WIRE / WIRE>KEY are the cross-process
\ codec: the content key is SHA-256 over the interned store key (the region/target/shape/
\ dtype/layout identity), fixed 32-byte little-endian, so equal store keys digest
\ identically in every process and the id survives process death. WIRE>KEY resolves the
\ 32 bytes against the local registry BY CONTENT, never by registration order. The per-id
\ content key is interned at REGISTER (ART-CK).

require lib/prelude.f
require lib/string.f
require maki/cad-kinds.f

-5244 constant E-ARTIFACT-CAP    \ registry count or key-store bytes exhausted
-5245 constant E-ARTIFACT-KEY    \ empty key or key over the per-key byte cap
-5246 constant E-ARTIFACT-ID     \ artifact-id outside the registered range
-5248 constant E-ARTIFACT-WIRE   \ KEY>WIRE output buffer smaller than the fixed content-key width

package ARTIFACT
public

\ WIRE>KEY decode result (the § 23.9 art-result custom-sum idiom): `ok` carries the
\ refined nominal id; the reject arms are the fixed-width content-key refusals (wrong
\ width, unresolved content key). A bespoke per-package sum, not result<a,b>, so a total
\ ok construction leaves no free error variable.
SUMTYPE id-result 1
   VARIANT ok a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;SUMTYPE

private

256 constant ART-CAP                     \ max distinct registered artifacts (v1)
256 constant ART-KEY-MAX                  \ max bytes per key
ART-CAP ART-KEY-MAX * constant ART-KEY-CAP
32 constant CK-BYTES                      \ SHA-256 content-key width (cross-process wire form)

create ART-KEYS ART-KEY-CAP allot         \ interned key bytes, back to back
create ART-KO   ART-CAP cells allot        \ per-id key offset into ART-KEYS
create ART-KL   ART-CAP cells allot        \ per-id key length
create ART-CK   ART-CAP CK-BYTES * allot   \ per-id SHA-256 content key over the store key
variable ART-KEY-U                          \ bytes used in ART-KEYS
variable ART-N                              \ registered count

\ Both rows stay trust rows, for two different reasons. CAST: refuses the mint
\ with 7135 E-CAST-OWNER because package CAD-KIND declares the family. The
\ projection out would certify, but its NAME repeats its owner, so the package
\ lint refuses any edit to that line until it is renamed - a cascade owned by
\ habu-cast-definer-330-1f5980b8, not by this file.
TRUSTED: RAW>ARTIFACT-ID ( n -- CAD-KIND:artifact-id ) ;
TRUSTED: ARTIFACT-ID>RAW ( CAD-KIND:artifact-id -- n ) ;

\ id-result wrappers use an IDR- prefix so they never collide with the envelope
\ codec's art-result R-* wrappers when maki/db/artifact.f reopens package ARTIFACT.
: IDR-OK ( a -- id-result<a> )          ARTIFACT-ID--RESULT:OK ;
: IDR-WRONG-WIDTH ( -- id-result<a> )   ARTIFACT-ID--RESULT:WRONG-WIDTH ;
: IDR-UNKNOWN ( -- id-result<a> )       ARTIFACT-ID--RESULT:UNKNOWN ;

: CK@ ( n -- ptr u8 )   CK-BYTES * ART-CK + ;   \ per-id content-key slot base

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
   a u  raw CK@  SHA256                      \ intern the cross-process content key
   off u + ART-KEY-U ! ;

: CK-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}   \ fixed 32-byte content-key compare
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: CK-FIND ( ptr u8 -- n ) {: p:ptr :}   \ raw id whose content key equals p's 32 bytes, or -1
   ART-N @ 0 ?do
      i CK@ p CK-EQ? if i unloop exit then
   loop -1 ;

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

\ VALIDATE-ID fails closed on an out-of-range id (the target.f VALIDATE
\ precedent). Named -ID (not bare VALIDATE) because package ARTIFACT also owns the
\ envelope-bytes codec (maki/db/artifact.f), whose contract leg is
\ ARTIFACT:VALIDATE ( owned-bytes -- result<content-digest,diag-set> )
\ (MODEL-CAD-V2-PLAN.md § 23.9); a package public wordlist rejects duplicate tails,
\ so the identity-registry range check keeps the -ID tail and frees bare VALIDATE
\ for the envelope leg.
: VALIDATE-ID ( CAD-KIND:artifact-id -- CAD-KIND:artifact-id )  dup ID-CK drop ;

\ KEY>WIRE writes the id's cross-process content key (SHA-256 over the interned store
\ key, 32 fixed bytes) into the caller buffer; total for a valid id (E-ARTIFACT-WIRE if
\ the cap cannot hold CK-BYTES). WIRE>KEY is the audited cross-process boundary: it reads
\ the 32-byte content key and resolves it against the local registry BY CONTENT (never by
\ registration order), refining to the nominal id only on a content match. These are the
\ digest-covered / durable id + dependency wire form the envelope codec consumes.
: KEY>WIRE ( CAD-KIND:artifact-id ptr u8 n -- n )
   {: id:CAD-KIND:artifact-id out:ptr cap:n :}
   cap CK-BYTES < if E-ARTIFACT-WIRE throw then
   id ID-CK CK@  out  CK-BYTES  BYTE-COPY
   CK-BYTES ;

: WIRE>KEY ( ptr u8 n -- id-result<CAD-KIND:artifact-id> )
   {: a:ptr u:n :}
   u CK-BYTES <> if IDR-WRONG-WIDTH exit then
   a CK-FIND {: raw:n :}
   raw 0 < if IDR-UNKNOWN exit then
   raw RAW>ARTIFACT-ID IDR-OK ;

: COUNT ( -- n )  ART-N @ ;

;package
