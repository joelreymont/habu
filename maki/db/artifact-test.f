\ maki/db/artifact-test.f - acceptance for the canonical artifact envelope codec
\ (maki/db/artifact.f; dot habu-v2-canonical-artifact-ee5121b4).
\
\ Proves the frozen § 23.9 acceptance, each item by a named test:
\   ADT-EQ-BYTES / ADT-EQ-HASH  : equal values encode byte-identically and hash identically
\   ADT-DEP-ORDER               : dependency-set insertion order does not change the bytes
\   ADT-FIELD-DIGEST            : one semantic field change changes the digest ...
\   ADT-EXCLUDED-DIGEST         : ... while the digest-excluded created-event does not
\   ADT-ROUNDTRIP               : decode round-trips encode (re-encode is byte-identical)
\   ADT-UNKNOWN-REQ             : an unknown REQUIRED field -> unknown-required (typed, not a throw)
\   ADT-DIGEST-MISMATCH         : a corrupted stored digest -> digest-mismatch (typed)
\   ADT-MALFORMED               : truncated bytes -> malformed
\   ADT-NONCANON                : non-ascending field order -> noncanonical
\   ADT-DUP                     : a repeated field tag -> duplicate
\   ADT-BOUNDS                  : a length outside its declared range -> bounds
\   ADT-KIND-MISMATCH           : a weight envelope decoded as a kernel -> kind-mismatch
\   ADT-MIGRATION               : an unsupported schema version -> unsupported-migration
\   ADT-OPAQUE                  : an unknown OPTIONAL field is retained and re-emitted verbatim
\
\ The test reopens package ARTIFACT (a friend) to reach the private wire constants
\ and craft adversarial byte buffers; happy-path calls use the public API.

require lib/test.f
require maki/db/artifact.f

package ARTIFACT

\ ---- test buffers -------------------------------------------------------------
create ADT-A 512 allot
create ADT-B 512 allot
create ADT-C 512 allot
create ADT-TB 512 allot            \ hand-crafted adversarial buffer
variable ADT-TB-U

\ ---- result inspection --------------------------------------------------------
: ADT-CODE ( art-result<n> -- n )          \ 0=ok, else the taxonomy ordinal
   MATCH art-result
      ok OF drop 0 ENDOF
      malformed OF 1 ENDOF
      noncanonical OF 2 ENDOF
      bounds OF 3 ENDOF
      duplicate OF 4 ENDOF
      unknown-required OF 5 ENDOF
      kind-mismatch OF 6 ENDOF
      unsupported-migration OF 7 ENDOF
      digest-mismatch OF 8 ENDOF
   ;MATCH ;

: ADT-OK? ( art-result<n> -- n bool )      \ ok -> ( slot true ), else ( 0 false )
   MATCH art-result
      ok OF true ENDOF
      malformed OF 0 false ENDOF
      noncanonical OF 0 false ENDOF
      bounds OF 0 false ENDOF
      duplicate OF 0 false ENDOF
      unknown-required OF 0 false ENDOF
      kind-mismatch OF 0 false ENDOF
      unsupported-migration OF 0 false ENDOF
      digest-mismatch OF 0 false ENDOF
   ;MATCH ;

: ADT-BYTES-EQ? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr an:n b:ptr bn:n :}
   an bn <> if false exit then
   0 begin dup an < while
      dup {: k:n :}
      a k + c@  b k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ ---- fixtures: distinct interned identities -----------------------------------
: ADT-ID1 ( -- CAD-KIND:artifact-id )   s" adt-id-1" REGISTER ;
: ADT-ID2 ( -- CAD-KIND:artifact-id )   s" adt-id-2" REGISTER ;
: ADT-DEP1 ( -- CAD-KIND:artifact-id )  s" adt-dep-1" REGISTER ;
: ADT-DEP2 ( -- CAD-KIND:artifact-id )  s" adt-dep-2" REGISTER ;

\ a canonical two-dependency weight envelope with the given schema/producer/event
: ADT-BUILD-STD ( n n n -- weight-artifact ) {: ver:n pver:n event:n :}
   DEPS-RESET  ADT-DEP1 DEP+  ADT-DEP2 DEP+
   ver pver ADT-ID1 event BUILD-WEIGHT ;

\ ---- adversarial byte builder (uses the private wire constants) ----------------
: ADT-TB-RESET ( -- )   0 ADT-TB-U ! ;
: ADT-TB-U8 ( n -- ) {: c:n :}
   c ADT-TB ADT-TB-U @ + c!  ADT-TB-U @ 1+ ADT-TB-U ! ;
: ADT-TB-LE ( n n -- ) {: v:n w:n :}
   v ADT-TB ADT-TB-U @ + w LE-PUT  ADT-TB-U @ w + ADT-TB-U ! ;
: ADT-TB-U64-FIELD ( n n n -- ) {: tag:n flags:n v:n :}     \ tag flags value
   tag ADT-TB-U8  flags ADT-TB-U8  U64W U32W ADT-TB-LE  v U64W ADT-TB-LE ;
: ADT-TB$ ( -- ptr u8 n )   ADT-TB ADT-TB-U @ ;

T-RESET

\ ---- 1. equal values encode byte-identically and hash identically -------------
: ADT-EQ-BYTES ( -- bool )
   1 1 0 ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   1 1 0 ADT-BUILD-STD ADT-B 512 ENCODE-WEIGHT {: lb:n :}
   ADT-A la ADT-B lb ADT-BYTES-EQ? ;
ADT-EQ-BYTES TTRUE

: ADT-EQ-HASH ( -- bool )
   1 1 0 ADT-BUILD-STD DIGEST-WEIGHT
   1 1 0 ADT-BUILD-STD DIGEST-WEIGHT
   DIGEST-EQ? ;
ADT-EQ-HASH TTRUE

\ dependency insertion order must not matter (builder canonicalises the set)
: ADT-DEP-ORDER ( -- bool )
   DEPS-RESET ADT-DEP1 DEP+ ADT-DEP2 DEP+  1 1 ADT-ID1 0 BUILD-WEIGHT
   ADT-A 512 ENCODE-WEIGHT {: la:n :}
   DEPS-RESET ADT-DEP2 DEP+ ADT-DEP1 DEP+  1 1 ADT-ID1 0 BUILD-WEIGHT
   ADT-B 512 ENCODE-WEIGHT {: lb:n :}
   ADT-A la ADT-B lb ADT-BYTES-EQ? ;
ADT-DEP-ORDER TTRUE

\ ---- 2. one semantic field change changes the digest; excluded field does not --
: ADT-FIELD-DIGEST ( -- bool )
   1 1 0 ADT-BUILD-STD DIGEST-WEIGHT
   2 1 0 ADT-BUILD-STD DIGEST-WEIGHT       \ schema-version 1 -> 2
   DIGEST-EQ? 0= ;
ADT-FIELD-DIGEST TTRUE

: ADT-EXCLUDED-DIGEST ( -- bool )
   1 1 0 ADT-BUILD-STD DIGEST-WEIGHT
   1 1 9 ADT-BUILD-STD DIGEST-WEIGHT       \ created-event 0 -> 9 (excluded)
   DIGEST-EQ? ;
ADT-EXCLUDED-DIGEST TTRUE

\ ---- 3. decode round-trips encode ---------------------------------------------
: ADT-ROUNDTRIP ( -- bool )
   1 1 0 ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   ADT-A la DECODE-WEIGHT ADT-OK? {: slot:n ok:bool :}
   ok 0= if false exit then
   slot WEIGHT-OF ADT-B 512 ENCODE-WEIGHT {: lb:n :}
   ADT-A la ADT-B lb ADT-BYTES-EQ? ;
ADT-ROUNDTRIP TTRUE

\ ---- 4. unknown required + digest mismatch -> typed diagnostics ----------------
: ADT-UNKNOWN-REQ ( -- n )                 \ valid bytes + an unknown REQUIRED field
   1 1 0 ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   200 ADT-A la + c!                       \ tag = 200 (unknown, > known max)
   FLAG-REQUIRED ADT-A la 1+ + c!          \ flags = required
   0 ADT-A la 2 + + U32W LE-PUT            \ len = 0
   ADT-A la HDR-W + U32W + DECODE-WEIGHT ADT-CODE ;
ADT-UNKNOWN-REQ 5 T=

: ADT-DIGEST-MISMATCH ( -- n )
   1 1 0 ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   \ layout tail: [digest field 38B][event field 14B]; digest payload = [la-46 .. la-14).
   la 30 -  {: dpos:n :}                   \ a byte well inside the 32-byte digest payload
   ADT-A dpos + c@ 1 xor  ADT-A dpos + c!
   ADT-A la DECODE-WEIGHT ADT-CODE ;
ADT-DIGEST-MISMATCH 8 T=

\ ---- 5. malformed / noncanonical / kind-mismatch (+ duplicate, bounds, migration)
: ADT-MALFORMED ( -- n )                   \ truncate a valid envelope
   1 1 0 ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   ADT-A la 1-  DECODE-WEIGHT ADT-CODE ;
ADT-MALFORMED 1 T=

: ADT-NONCANON ( -- n )                    \ TAG-KIND before TAG-VER (descending)
   ADT-TB-RESET
   TAG-KIND FLAG-REQUIRED KIND-WEIGHT ADT-TB-U64-FIELD
   TAG-VER  FLAG-REQUIRED 1 ADT-TB-U64-FIELD
   ADT-TB$ DECODE-WEIGHT ADT-CODE ;
ADT-NONCANON 2 T=

: ADT-DUP ( -- n )                         \ TAG-VER twice
   ADT-TB-RESET
   TAG-VER FLAG-REQUIRED 1 ADT-TB-U64-FIELD
   TAG-VER FLAG-REQUIRED 1 ADT-TB-U64-FIELD
   ADT-TB$ DECODE-WEIGHT ADT-CODE ;
ADT-DUP 4 T=

: ADT-BOUNDS ( -- n )                      \ a scalar field with a non-8 declared length
   ADT-TB-RESET
   TAG-VER ADT-TB-U8  FLAG-REQUIRED ADT-TB-U8
   4 U32W ADT-TB-LE                         \ declared len = 4 (a scalar field must be 8)
   0 U32W ADT-TB-LE                         \ 4-byte payload
   ADT-TB$ DECODE-WEIGHT ADT-CODE ;
ADT-BOUNDS 3 T=

: ADT-KIND-MISMATCH ( -- n )               \ a kernel envelope decoded as weight
   DEPS-RESET ADT-DEP1 DEP+
   1 1 ADT-ID1 0 BUILD-KERNEL ADT-A 512 ENCODE-KERNEL {: la:n :}
   ADT-A la DECODE-WEIGHT ADT-CODE ;
ADT-KIND-MISMATCH 6 T=

: ADT-MIGRATION ( -- n )                   \ mutate the schema-version payload to an unsupported value
   1 1 0 ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   99 ADT-A HDR-W U32W + + U64W LE-PUT      \ first field payload = version 99
   ADT-A la DECODE-WEIGHT ADT-CODE ;
ADT-MIGRATION 7 T=

\ ---- 6. unknown OPTIONAL field is retained opaquely and re-emitted -------------
: ADT-OPAQUE ( -- bool )
   1 1 0 ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   \ append an unknown OPTIONAL field (tag 210, flags 0, len 8, value 7)
   210 ADT-A la + c!
   0 ADT-A la 1+ + c!
   U64W ADT-A la 2 + + U32W LE-PUT
   7 ADT-A la HDR-W + U32W + + U64W LE-PUT
   la HDR-W + U32W + U64W +  {: full:n :}   \ total appended length
   ADT-A full DECODE-WEIGHT ADT-OK? {: slot:n ok:bool :}
   ok 0= if false exit then
   slot WEIGHT-OF ADT-C 512 ENCODE-WEIGHT {: lc:n :}
   ADT-A full ADT-C lc ADT-BYTES-EQ? ;      \ re-emit is byte-identical (retained verbatim)
ADT-OPAQUE TTRUE

T-REPORT

;package
