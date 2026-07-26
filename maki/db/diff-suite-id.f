\ maki/db/diff-suite-id.f - the durable DifferentialSuite identity registry (the
\ suite-id leg of the MODEL-CAD-V2-PLAN.md § 23.9 "Foreign identity constructors and
\ wire codecs" per-family contract; dot habu-v2-differential-runner-13359019).
\
\ CONCERN: the content-addressed owner of CAD-KIND:suite-id. REGISTER interns a SEALED
\ DIFFSUITE:suite by its DIGEST-INTO content key (SHA-256 over the twelve semantic
\ fields; equal suites - any insertion order - digest identically and intern to ONE id),
\ the private RAW>SUITE-ID / SUITE-ID>RAW refinements stay owner-only, and the
\ KEY>WIRE / WIRE>KEY pair is the cross-process content-key codec (total encode; audited
\ fail-closed decode BY CONTENT). This is a DIFFERENT concern from the suite VALUE +
\ its encode/digest (package DIFFSUITE, maki/db/diff-suite.f): that file owns the suite
\ STRUCTURE and its cross-process content key; this file owns the durable INTERNED
\ IDENTITY of a suite - the maki/db/evidence.f evidence-id / maki/experiment/run.f run-id
\ content-key-intern precedent, keyed off the suite's own DIGEST-INTO.
\
\ WHY REUSE THE SUITE DIGEST. DIFFSUITE:DIGEST-INTO is already the suite's canonical
\ cross-process content key (every semantic field digest-covered, sets canonicalised),
\ so a suite-id is exactly "an interned suite digest". A different suite (any
\ digest-covered field flipped) mints a distinct id; two equal suites share one id. So
\ suite identity composes with the suite's own content addressing; it does not fork a
\ competing digest.
\
\ WIRE FORM. KEY>WIRE writes the id's 32-byte suite digest; WIRE>KEY resolves 32 bytes
\ against the local registry BY CONTENT (never by intern order), so the id survives
\ process death. Fail closed: an out-of-range id, a wrong-width or unresolved wire
\ buffer, or a full registry are named throws / reject variants. maki -> habu only;
\ suite-id owns -5412..-5414.

require lib/prelude.f
require maki/cad-kinds.f            \ CAD-KIND:suite-id nominal identity
require maki/db/diff-suite.f       \ DIFFSUITE:suite value + DIGEST-INTO (the interned key source)

-5412 constant E-SUITEID-CAP       \ registry count exhausted (distinct interned suite digests)
-5413 constant E-SUITEID-ID        \ suite-id outside the interned range
-5414 constant E-SUITEID-WIRE      \ KEY>WIRE output buffer smaller than the fixed content-key width

package SUITEID
public

\ WIRE>KEY decode result (the § 23.9 id-result custom-sum idiom): `ok` carries the
\ refined nominal id in its `id` field; the reject arms are the fixed-width content-key
\ refusals (wrong width, unresolved content key). A bespoke per-package sum, not
\ result<a,b>, so a total ok construction leaves no free error variable. Declared
\ through the unified ENUM front end in full mode (the arity after the name selects
\ it), so the payload is a named FIELD rather than a positional one; the generated
\ SUITEID-ID--RESULT:OK / :WRONG-WIDTH / :UNKNOWN constructors and every MATCH site are
\ unchanged, because both spellings and payload binding order derive from the package,
\ the family tail, and the declaration order, none of which the mode changes.
ENUM id-result 1
   VARIANT ok FIELD id a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;ENUM

private

64 constant SID-CAP                \ max distinct interned suite digests (v1 first-slice pool)
32  constant CKW                   \ content-key / digest width (SHA-256 cross-process form)

\ Private representation refinements (owner-only, TRUSTED:, never public), the
\ maki/db/evidence.f RAW>EVIDENCE-ID / EVIDENCE-ID>RAW precedent. The only public mint is
\ REGISTER, bound to a real interned suite digest, so a raw n cannot forge a suite id.
TRUSTED: RAW>SUITE-ID ( n -- CAD-KIND:suite-id ) ;
TRUSTED: SUITE-ID>RAW ( CAD-KIND:suite-id -- n ) ;

\ ---- suite-id registry (per-id 32-byte content key = suite digest) --------------
create SID-CK SID-CAP CKW * allot   \ interned per-id content keys (the suite digests)
variable SID-N                       \ interned count
create SID-SCRATCH CKW allot         \ one recomputed suite digest (REGISTER)

: CK@ ( n -- ptr u8 )   CKW * SID-CK + ;         \ per-id content-key slot base

: CK-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup CKW < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: CK-FIND ( ptr u8 -- n ) {: p:ptr :}            \ raw id whose content key equals p, or -1
   SID-N @ 0 ?do
      i CK@ p CK-EQ? if i unloop exit then
   loop -1 ;

: ID-CK ( CAD-KIND:suite-id -- n )               \ validate + project a suite-id to its raw
   SUITE-ID>RAW dup 0 < over SID-N @ >= or if E-SUITEID-ID throw then ;

: INTERN-CK ( ptr u8 -- CAD-KIND:suite-id ) {: p:ptr :}   \ intern a 32-byte digest
   p CK-FIND {: found:n :}
   found 0 >= if found RAW>SUITE-ID exit then
   SID-N @ SID-CAP >= if E-SUITEID-CAP throw then
   SID-N @ {: raw:n :}
   p raw CK@ CKW BYTE-COPY
   raw 1+ SID-N !
   raw RAW>SUITE-ID ;

: R-OK ( a -- id-result<a> )          SUITEID-ID--RESULT:OK ;
: R-WRONG-WIDTH ( -- id-result<a> )   SUITEID-ID--RESULT:WRONG-WIDTH ;
: R-UNKNOWN ( -- id-result<a> )       SUITEID-ID--RESULT:UNKNOWN ;

public

\ REGISTER interns a sealed suite by its DIGEST-INTO content key: an equal suite (any
\ insertion order) returns the same id (content-addressed), a fresh suite appends a new
\ registry slot and mints its id. The ONLY authority-bearing public mint, bound to a
\ real interned suite digest, never a raw cast.
: REGISTER ( DIFFSUITE:suite -- CAD-KIND:suite-id ) {: h:DIFFSUITE:suite :}
   h SID-SCRATCH CKW DIFFSUITE:DIGEST-INTO drop
   SID-SCRATCH INTERN-CK ;

\ EQUAL? is same-suite identity: interning makes equal suite digests share one raw id,
\ so raw equality is exactly suite-digest equality.
: EQUAL? ( CAD-KIND:suite-id CAD-KIND:suite-id -- bool )
   {: x:CAD-KIND:suite-id y:CAD-KIND:suite-id :}
   x SUITE-ID>RAW y SUITE-ID>RAW = ;

: VALIDATE ( CAD-KIND:suite-id -- CAD-KIND:suite-id )   dup ID-CK drop ;

\ KEY>WIRE writes the id's 32-byte suite digest into the caller buffer; total for a valid
\ id. WIRE>KEY resolves 32 bytes against the local registry BY CONTENT (never by intern
\ order), refining to the nominal id only on a content match.
: KEY>WIRE ( CAD-KIND:suite-id ptr u8 n -- n ) {: id:CAD-KIND:suite-id out:ptr cap:n :}
   cap CKW < if E-SUITEID-WIRE throw then
   id ID-CK CK@  out  CKW  BYTE-COPY
   CKW ;

: WIRE>KEY ( ptr u8 n -- id-result<CAD-KIND:suite-id> ) {: a:ptr u:n :}
   u CKW <> if R-WRONG-WIDTH exit then
   a CK-FIND {: raw:n :}
   raw 0 < if R-UNKNOWN exit then
   raw RAW>SUITE-ID R-OK ;

: COUNT ( -- n )   SID-N @ ;

private

: SID-INIT ( -- )   0 SID-N ! ;
SID-INIT

;package
