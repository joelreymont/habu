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
\ Second slice (foreign-id fields via the owner ID>WIRE/WIRE>ID codecs + VALIDATE):
\   ADT-{SCHEMA,PRODUCER,CONFIG,NPOL}-DIGEST : each digest-covered foreign id flips the
\                                 content digest when it changes (two-id ENCODE flip)
\   ADT-TARGET-DIGEST           : target-id is digest-covered too, proven via the DECODE
\                                 path (its registry is a full 16-slot shared resource in
\                                 the integration suite, so no fresh target can be minted)
\   ADT-FID-UNKNOWN (per tag)   : an out-of-range foreign id -> bounds (WIRE>ID unknown)
\   ADT-FID-WIDTH (per tag)     : a wrong-width foreign id -> malformed (WIRE>ID wrong-width)
\   ADT-VALIDATE-OK             : VALIDATE accepts the golden weight envelope
\   ADT-VALIDATE-KERNEL         : VALIDATE is kind-agnostic (a kernel envelope validates)
\   ADT-VALIDATE-{MALFORMED,DIGEST,UNKNOWN-REQ} : VALIDATE rejects each corruption class
\ Byte-identical decode->re-encode (ADT-ROUNDTRIP) proves every foreign id round-trips.
\
\ Third slice (the last expressible envelope fields, via the JOURNAL / REV codecs):
\   ADT-EXCLUDED-DIGEST         : the created-event is a CAD-KIND:audit-event-id now, and
\                                 changing it (a DISTINCT appended event) does NOT change
\                                 the digest (audit-event-id is the one digest-EXCLUDED id)
\   ADT-SREVS-DIGEST            : the source-revisions[] rev-id set IS digest-covered - a
\                                 different set flips the content digest
\   ADT-SREVS-ORDER            : source-revision insertion order does not change the bytes
\   ADT-SREVS-ROUNDTRIP        : a populated source-revision set decode->re-encodes exactly
\   ADT-VALIDATE-SREVS         : VALIDATE accepts an envelope carrying source-revisions
\   TAG-EVENT ADT-FID-{UNKNOWN,WIDTH} : the event field folds JOURNAL:WIRE>ID unknown ->
\                                 bounds, wrong-width -> malformed (the slice-2 fold)
\   ADT-SREV-UNKNOWN           : an out-of-range rev in the set -> bounds (REV:WIRE>ID unknown)
\   ADT-SREV-DUP               : a duplicate rev in the set -> duplicate
\   ADT-SREV-NONCANON          : a descending source-revision set -> noncanonical
\
\ The test reopens package ARTIFACT (a friend) to reach the private wire constants
\ and craft adversarial byte buffers; happy-path calls use the public API. The
\ foreign-id fixtures mint real ids through their owner constructors (SCHEMA:REGISTER,
\ PRODUCER:REGISTER, CONFIG:REGISTER, NPOL:REGISTER, TARGET:REGISTER/SM87, REV:COMMIT,
\ JOURNAL:APPEND) - never a raw cast - exactly as a real producer would. A stable event
\ is cached in a TYPED-VARIABLE because JOURNAL:APPEND is occurrence-identified (each
\ call mints a fresh id), so byte-identical builds must reuse one already-minted event.

require lib/test.f
require maki/db/artifact.f
require maki/schema.f
require maki/producer.f
require maki/config.f
require maki/numpolicy.f
require maki/target/target.f
require maki/journal.f
require maki/rev.f

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

\ foreign-id fixtures (minted through the owner constructors; two distinct per family
\ so a per-field digest flip has something to change to).
: ADT-SCHEMA ( -- CAD-KIND:schema-id )         s" adt-schema-a"   SCHEMA:REGISTER ;
: ADT-SCHEMA2 ( -- CAD-KIND:schema-id )        s" adt-schema-b"   SCHEMA:REGISTER ;
: ADT-PRODUCER ( -- CAD-KIND:producer-id )     s" adt-producer-a" PRODUCER:REGISTER ;
: ADT-PRODUCER2 ( -- CAD-KIND:producer-id )    s" adt-producer-b" PRODUCER:REGISTER ;
: ADT-CONFIG ( -- CAD-KIND:config-id )         s" adt-config-a"   CONFIG:REGISTER ;
: ADT-CONFIG2 ( -- CAD-KIND:config-id )        s" adt-config-b"   CONFIG:REGISTER ;
: ADT-NPOL ( -- CAD-KIND:numeric-policy-id )   NPOL-DOM:EXACT NPOL:REGISTER ;
: ADT-NPOL2 ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:ULP   NPOL:REGISTER ;
: ADT-TARGET ( -- CAD-KIND:target-id )         TARGET:SM87 ;
\ target-id lives in a small SHARED registry (16 slots) that the full maki suite
\ fills, so - unlike the room-having schema/producer/config/npol families - a fresh
\ alternative target cannot be minted when integrated. We therefore prove target-id
\ digest coverage by the DECODE path (swap the encoded raw for another ALREADY-valid
\ target, require digest-mismatch), which needs no new slot but does need at least
\ two registered targets. In isolation only sm87 (raw 0) exists, so register one more
\ IFF the registry has no alternative yet; a no-op once the suite has populated it.
: ADT-ENSURE-ALT-TARGET ( -- )
   TARGET:COUNT 2 < if
      s" adt-tgt-alt"
      TARGET:ISA-PTX 80 32 1024 49152 TARGET:CAP-PTX TARGET:DESCRIPTOR
      TARGET:REGISTER drop
   then ;

\ source-revision fixtures: rev-id is content-addressed, so REV:COMMIT of the same
\ canonical revision content returns the SAME id every call (two distinct contents so a
\ set has two members).
: ADT-REV1 ( -- CAD-KIND:rev-id )   s" adt-rev-1 parent=- writes=obj-a" REV:COMMIT ;
: ADT-REV2 ( -- CAD-KIND:rev-id )   s" adt-rev-2 parent=r1 writes=obj-b" REV:COMMIT ;

\ audit-event fixtures: an audit event is occurrence-identified (every JOURNAL:APPEND
\ mints a fresh sequence), so a STABLE event must be minted once and cached to reuse
\ across byte-identical builds. ADT-EV / ADT-EV-ALT are two DISTINCT cached events.
TYPED-VARIABLE ADT-EV0 CAD-KIND:audit-event-id
TYPED-VARIABLE ADT-EV1 CAD-KIND:audit-event-id
variable ADT-EV-READY
: ADT-EV-SETUP ( -- )
   ADT-EV-READY @ 0= if
      s" adt-evt-0" JOURNAL:APPEND ADT-EV0 !
      s" adt-evt-1" JOURNAL:APPEND ADT-EV1 !
      true ADT-EV-READY !
   then ;
: ADT-EV ( -- CAD-KIND:audit-event-id )      ADT-EV-SETUP ADT-EV0 @ ;
: ADT-EV-ALT ( -- CAD-KIND:audit-event-id )  ADT-EV-SETUP ADT-EV1 @ ;

\ the rev-id wire scalar (its REV:ID>WIRE canonical form as one LE cell), used to craft
\ adversarial source-revision set bytes.
create ADT-WBUF 64 allot
: ADT-REV-WIRE ( CAD-KIND:rev-id -- n )
   ADT-WBUF 64 REV:ID>WIRE {: w:n :}
   ADT-WBUF w LE-GET ;

\ a canonical two-dependency weight envelope with the given schema-version /
\ producer-version and an EMPTY source-revision set, the default foreign ids, and the
\ supplied CAD-KIND:audit-event-id created-event. Source-revisions is held empty so the
\ adversarial tail-offset tests keep a fixed layout; ADT-SREVS-* build populated sets.
: ADT-BUILD-STD ( n n CAD-KIND:audit-event-id -- weight-artifact )
   {: ver:n pver:n event:CAD-KIND:audit-event-id :}
   DEPS-RESET  ADT-DEP1 DEP+  ADT-DEP2 DEP+
   SREVS-RESET
   ver pver ADT-ID1
   ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET
   event BUILD-WEIGHT ;

\ a two-dependency weight envelope with schema-version/producer-version/event fixed
\ and the five foreign ids supplied, so a per-field digest flip varies exactly one.
: ADT-BUILD-IDS ( CAD-KIND:schema-id CAD-KIND:producer-id CAD-KIND:config-id CAD-KIND:numeric-policy-id CAD-KIND:target-id -- weight-artifact )
   {: schema:CAD-KIND:schema-id producer:CAD-KIND:producer-id
      config:CAD-KIND:config-id npol:CAD-KIND:numeric-policy-id
      target:CAD-KIND:target-id :}
   DEPS-RESET  ADT-DEP1 DEP+  ADT-DEP2 DEP+
   SREVS-RESET
   1 1 ADT-ID1  schema producer config npol target  ADT-EV BUILD-WEIGHT ;

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
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   1 1 ADT-EV ADT-BUILD-STD ADT-B 512 ENCODE-WEIGHT {: lb:n :}
   ADT-A la ADT-B lb ADT-BYTES-EQ? ;
ADT-EQ-BYTES TTRUE

: ADT-EQ-HASH ( -- bool )
   1 1 ADT-EV ADT-BUILD-STD DIGEST-WEIGHT
   1 1 ADT-EV ADT-BUILD-STD DIGEST-WEIGHT
   DIGEST-EQ? ;
ADT-EQ-HASH TTRUE

\ dependency insertion order must not matter (builder canonicalises the set)
: ADT-DEP-ORDER ( -- bool )
   DEPS-RESET ADT-DEP1 DEP+ ADT-DEP2 DEP+  SREVS-RESET
   1 1 ADT-ID1 ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-EV BUILD-WEIGHT
   ADT-A 512 ENCODE-WEIGHT {: la:n :}
   DEPS-RESET ADT-DEP2 DEP+ ADT-DEP1 DEP+  SREVS-RESET
   1 1 ADT-ID1 ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-EV BUILD-WEIGHT
   ADT-B 512 ENCODE-WEIGHT {: lb:n :}
   ADT-A la ADT-B lb ADT-BYTES-EQ? ;
ADT-DEP-ORDER TTRUE

\ ---- 2. one semantic field change changes the digest; excluded field does not --
: ADT-FIELD-DIGEST ( -- bool )
   1 1 ADT-EV ADT-BUILD-STD DIGEST-WEIGHT
   2 1 ADT-EV ADT-BUILD-STD DIGEST-WEIGHT       \ schema-version 1 -> 2
   DIGEST-EQ? 0= ;
ADT-FIELD-DIGEST TTRUE

: ADT-EXCLUDED-DIGEST ( -- bool )
   1 1 ADT-EV ADT-BUILD-STD DIGEST-WEIGHT
   1 1 ADT-EV-ALT ADT-BUILD-STD DIGEST-WEIGHT  \ created-event -> a DISTINCT audit event (excluded)
   DIGEST-EQ? ;
ADT-EXCLUDED-DIGEST TTRUE

\ ---- 3. decode round-trips encode ---------------------------------------------
: ADT-ROUNDTRIP ( -- bool )
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   ADT-A la DECODE-WEIGHT ADT-OK? {: slot:n ok:bool :}
   ok 0= if false exit then
   slot WEIGHT-OF ADT-B 512 ENCODE-WEIGHT {: lb:n :}
   ADT-A la ADT-B lb ADT-BYTES-EQ? ;
ADT-ROUNDTRIP TTRUE

\ ---- 4. unknown required + digest mismatch -> typed diagnostics ----------------
: ADT-UNKNOWN-REQ ( -- n )                 \ valid bytes + an unknown REQUIRED field
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   200 ADT-A la + c!                       \ tag = 200 (unknown, > known max)
   FLAG-REQUIRED ADT-A la 1+ + c!          \ flags = required
   0 ADT-A la 2 + + U32W LE-PUT            \ len = 0
   ADT-A la HDR-W + U32W + DECODE-WEIGHT ADT-CODE ;
ADT-UNKNOWN-REQ 5 T=

: ADT-DIGEST-MISMATCH ( -- n )
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   \ layout tail: [digest field 38B][event field 14B]; digest payload = [la-46 .. la-14).
   la 30 -  {: dpos:n :}                   \ a byte well inside the 32-byte digest payload
   ADT-A dpos + c@ 1 xor  ADT-A dpos + c!
   ADT-A la DECODE-WEIGHT ADT-CODE ;
ADT-DIGEST-MISMATCH 8 T=

\ ---- 5. malformed / noncanonical / kind-mismatch (+ duplicate, bounds, migration)
: ADT-MALFORMED ( -- n )                   \ truncate a valid envelope
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
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
   DEPS-RESET ADT-DEP1 DEP+  SREVS-RESET
   1 1 ADT-ID1 ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-EV BUILD-KERNEL
   ADT-A 512 ENCODE-KERNEL {: la:n :}
   ADT-A la DECODE-WEIGHT ADT-CODE ;
ADT-KIND-MISMATCH 6 T=

: ADT-MIGRATION ( -- n )                   \ mutate the schema-version payload to an unsupported value
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   99 ADT-A HDR-W U32W + + U64W LE-PUT      \ first field payload = version 99
   ADT-A la DECODE-WEIGHT ADT-CODE ;
ADT-MIGRATION 7 T=

\ ---- 6. unknown OPTIONAL field is retained opaquely and re-emitted -------------
: ADT-OPAQUE ( -- bool )
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
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

\ ---- 7. each digest-covered foreign id flips the content digest ----------------
\ Vary exactly ONE foreign id (all others held at the default fixture) and require
\ the digest to change: every foreign id is a digest-covered semantic field.
: ADT-SCHEMA-DIGEST ( -- bool )
   ADT-SCHEMA  ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-BUILD-IDS DIGEST-WEIGHT
   ADT-SCHEMA2 ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-BUILD-IDS DIGEST-WEIGHT
   DIGEST-EQ? 0= ;
ADT-SCHEMA-DIGEST TTRUE

: ADT-PRODUCER-DIGEST ( -- bool )
   ADT-SCHEMA ADT-PRODUCER  ADT-CONFIG ADT-NPOL ADT-TARGET ADT-BUILD-IDS DIGEST-WEIGHT
   ADT-SCHEMA ADT-PRODUCER2 ADT-CONFIG ADT-NPOL ADT-TARGET ADT-BUILD-IDS DIGEST-WEIGHT
   DIGEST-EQ? 0= ;
ADT-PRODUCER-DIGEST TTRUE

: ADT-CONFIG-DIGEST ( -- bool )
   ADT-SCHEMA ADT-PRODUCER ADT-CONFIG  ADT-NPOL ADT-TARGET ADT-BUILD-IDS DIGEST-WEIGHT
   ADT-SCHEMA ADT-PRODUCER ADT-CONFIG2 ADT-NPOL ADT-TARGET ADT-BUILD-IDS DIGEST-WEIGHT
   DIGEST-EQ? 0= ;
ADT-CONFIG-DIGEST TTRUE

: ADT-NPOL-DIGEST ( -- bool )
   ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL  ADT-TARGET ADT-BUILD-IDS DIGEST-WEIGHT
   ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL2 ADT-TARGET ADT-BUILD-IDS DIGEST-WEIGHT
   DIGEST-EQ? 0= ;
ADT-NPOL-DIGEST TTRUE

\ target-id is digest-covered, proven via DECODE: swapping the encoded target raw for
\ another VALID target changes the recomputed digest, so DECODE reports digest-mismatch
\ (8). Target now sits just before the (empty in ADT-BUILD-STD) source-revision field,
\ so its wire raw sits at a fixed offset from the end: tail =
\ [target 14][source-revisions 14][digest 38][event 14]; target payload = [la-74, la-66).
\ The replacement (r0+1 mod COUNT) is always a valid, distinct raw once COUNT >= 2.
ADT-ENSURE-ALT-TARGET
: ADT-TARGET-DIGEST ( -- n )
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   la 74 -  {: tpos:n :}
   ADT-A tpos + U64W LE-GET  1+ TARGET:COUNT mod       \ another valid, distinct target raw
   ADT-A tpos + U64W LE-PUT
   ADT-A la DECODE-WEIGHT ADT-CODE ;
ADT-TARGET-DIGEST 8 T=

\ ---- 8. per-field foreign-id decode rejects surface the right taxonomy ----------
\ A single foreign-id field is enough to reach its owner WIRE>ID: an out-of-range raw
\ decodes as `unknown` -> bounds (3); a wrong declared width decodes as `wrong-width`
\ -> malformed (1). Each family is exercised at its own tag.
: ADT-FID-UNKNOWN ( n -- n ) {: tag:n :}    \ single field, out-of-range raw
   ADT-TB-RESET
   tag FLAG-REQUIRED 9999 ADT-TB-U64-FIELD
   ADT-TB$ DECODE-WEIGHT ADT-CODE ;
: ADT-FID-WIDTH ( n -- n ) {: tag:n :}      \ single field, wrong declared width (4)
   ADT-TB-RESET
   tag ADT-TB-U8  FLAG-REQUIRED ADT-TB-U8
   4 U32W ADT-TB-LE   0 U32W ADT-TB-LE
   ADT-TB$ DECODE-WEIGHT ADT-CODE ;

TAG-SCHEMA   ADT-FID-UNKNOWN 3 T=
TAG-SCHEMA   ADT-FID-WIDTH   1 T=
TAG-PRODUCER ADT-FID-UNKNOWN 3 T=
TAG-PRODUCER ADT-FID-WIDTH   1 T=
TAG-CONFIG   ADT-FID-UNKNOWN 3 T=
TAG-CONFIG   ADT-FID-WIDTH   1 T=
TAG-NPOL     ADT-FID-UNKNOWN 3 T=
TAG-NPOL     ADT-FID-WIDTH   1 T=
TAG-TARGET   ADT-FID-UNKNOWN 3 T=
TAG-TARGET   ADT-FID-WIDTH   1 T=
\ the created-event is a single audit-event-id wire field too: it folds JOURNAL:WIRE>ID
\ the same way (unknown sequence -> bounds, wrong width -> malformed).
TAG-EVENT    ADT-FID-UNKNOWN 3 T=
TAG-EVENT    ADT-FID-WIDTH   1 T=

\ ---- 9. envelope VALIDATE: accepts the golden, kind-agnostic, rejects corruption -
: ADT-VALIDATE-OK ( -- n )                 \ golden weight envelope validates
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   ADT-A la VALIDATE ADT-CODE ;
ADT-VALIDATE-OK 0 T=

: ADT-VALIDATE-KERNEL ( -- n )             \ kind-agnostic: a kernel envelope also validates
   DEPS-RESET ADT-DEP1 DEP+  SREVS-RESET
   1 1 ADT-ID1 ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-EV BUILD-KERNEL
   ADT-A 512 ENCODE-KERNEL {: la:n :}
   ADT-A la VALIDATE ADT-CODE ;
ADT-VALIDATE-KERNEL 0 T=

: ADT-VALIDATE-MALFORMED ( -- n )          \ truncated bytes -> malformed
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   ADT-A la 1- VALIDATE ADT-CODE ;
ADT-VALIDATE-MALFORMED 1 T=

: ADT-VALIDATE-DIGEST ( -- n )             \ corrupted stored digest -> digest-mismatch
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   la 30 -  {: dpos:n :}
   ADT-A dpos + c@ 1 xor  ADT-A dpos + c!
   ADT-A la VALIDATE ADT-CODE ;
ADT-VALIDATE-DIGEST 8 T=

: ADT-VALIDATE-UNKNOWN-REQ ( -- n )        \ unknown REQUIRED field -> unknown-required
   1 1 ADT-EV ADT-BUILD-STD ADT-A 512 ENCODE-WEIGHT {: la:n :}
   200 ADT-A la + c!
   FLAG-REQUIRED ADT-A la 1+ + c!
   0 ADT-A la 2 + + U32W LE-PUT
   ADT-A la HDR-W + U32W + VALIDATE ADT-CODE ;
ADT-VALIDATE-UNKNOWN-REQ 5 T=

\ ---- 10. source-revisions[] rev-id set (slice 3) ------------------------------
\ source-revisions[] IS a digest-covered semantic field: two envelopes identical
\ except for their source-revision set MUST digest differently ({rev1} vs {rev1,rev2}).
: ADT-SREVS-DIGEST ( -- bool )
   DEPS-RESET ADT-DEP1 DEP+ ADT-DEP2 DEP+
   SREVS-RESET ADT-REV1 SREV+
   1 1 ADT-ID1 ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-EV BUILD-WEIGHT
   DIGEST-WEIGHT
   DEPS-RESET ADT-DEP1 DEP+ ADT-DEP2 DEP+
   SREVS-RESET ADT-REV1 SREV+ ADT-REV2 SREV+
   1 1 ADT-ID1 ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-EV BUILD-WEIGHT
   DIGEST-WEIGHT
   DIGEST-EQ? 0= ;
ADT-SREVS-DIGEST TTRUE

\ the builder canonicalises the set, so source-revision insertion order is irrelevant.
: ADT-SREVS-ORDER ( -- bool )
   DEPS-RESET ADT-DEP1 DEP+
   SREVS-RESET ADT-REV1 SREV+ ADT-REV2 SREV+
   1 1 ADT-ID1 ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-EV BUILD-WEIGHT
   ADT-A 512 ENCODE-WEIGHT {: la:n :}
   DEPS-RESET ADT-DEP1 DEP+
   SREVS-RESET ADT-REV2 SREV+ ADT-REV1 SREV+
   1 1 ADT-ID1 ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-EV BUILD-WEIGHT
   ADT-B 512 ENCODE-WEIGHT {: lb:n :}
   ADT-A la ADT-B lb ADT-BYTES-EQ? ;
ADT-SREVS-ORDER TTRUE

\ a populated source-revision set decode->re-encodes byte-identically (every element
\ round-trips through REV:ID>WIRE / REV:WIRE>ID).
: ADT-SREVS-ROUNDTRIP ( -- bool )
   DEPS-RESET ADT-DEP1 DEP+ ADT-DEP2 DEP+
   SREVS-RESET ADT-REV1 SREV+ ADT-REV2 SREV+
   1 1 ADT-ID1 ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-EV BUILD-WEIGHT
   ADT-A 512 ENCODE-WEIGHT {: la:n :}
   ADT-A la DECODE-WEIGHT ADT-OK? {: slot:n ok:bool :}
   ok 0= if false exit then
   slot WEIGHT-OF ADT-B 512 ENCODE-WEIGHT {: lb:n :}
   ADT-A la ADT-B lb ADT-BYTES-EQ? ;
ADT-SREVS-ROUNDTRIP TTRUE

\ VALIDATE covers the new fields: an envelope carrying a source-revision set + an
\ audit-event id validates (structure + digest over the extended prefix).
: ADT-VALIDATE-SREVS ( -- n )
   DEPS-RESET ADT-DEP1 DEP+
   SREVS-RESET ADT-REV1 SREV+ ADT-REV2 SREV+
   1 1 ADT-ID1 ADT-SCHEMA ADT-PRODUCER ADT-CONFIG ADT-NPOL ADT-TARGET ADT-EV BUILD-WEIGHT
   ADT-A 512 ENCODE-WEIGHT {: la:n :}
   ADT-A la VALIDATE ADT-CODE ;
ADT-VALIDATE-SREVS 0 T=

\ ---- 11. source-revision set decode rejects surface the right taxonomy ----------
\ Each crafted set field is a single envelope field, enough to reach the per-element
\ REV:WIRE>ID and the set-invariant checks in TAKE-SREVS.
: ADT-SREV-UNKNOWN ( -- n )                \ an out-of-range rev -> bounds (REV:WIRE>ID unknown)
   ADT-TB-RESET
   TAG-SREVS ADT-TB-U8  FLAG-REQUIRED ADT-TB-U8
   U64W 1 SREV-WIRE-W * +  U32W ADT-TB-LE   \ len = count word + 1 element
   1 U64W ADT-TB-LE                          \ count = 1
   9999 SREV-WIRE-W ADT-TB-LE               \ element = an unregistered rev raw
   ADT-TB$ DECODE-WEIGHT ADT-CODE ;
ADT-SREV-UNKNOWN 3 T=

: ADT-SREV-DUP ( -- n )                    \ a duplicate rev in the set -> duplicate
   ADT-REV1 ADT-REV-WIRE {: r:n :}
   ADT-TB-RESET
   TAG-SREVS ADT-TB-U8  FLAG-REQUIRED ADT-TB-U8
   U64W 2 SREV-WIRE-W * +  U32W ADT-TB-LE
   2 U64W ADT-TB-LE
   r SREV-WIRE-W ADT-TB-LE
   r SREV-WIRE-W ADT-TB-LE                  \ same rev twice
   ADT-TB$ DECODE-WEIGHT ADT-CODE ;
ADT-SREV-DUP 4 T=

: ADT-SREV-NONCANON ( -- n )               \ a descending set -> noncanonical
   ADT-REV1 ADT-REV-WIRE {: a:n :}
   ADT-REV2 ADT-REV-WIRE {: b:n :}
   a b > if a b else b a then {: hi:n lo:n :}   \ emit the larger wire form first
   ADT-TB-RESET
   TAG-SREVS ADT-TB-U8  FLAG-REQUIRED ADT-TB-U8
   U64W 2 SREV-WIRE-W * +  U32W ADT-TB-LE
   2 U64W ADT-TB-LE
   hi SREV-WIRE-W ADT-TB-LE
   lo SREV-WIRE-W ADT-TB-LE
   ADT-TB$ DECODE-WEIGHT ADT-CODE ;
ADT-SREV-NONCANON 2 T=

T-REPORT

;package
