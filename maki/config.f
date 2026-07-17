\ maki/config.f - the build/config identity registry + wire codec (the config-id
\ leg of the MODEL-CAD-V2-PLAN.md § 23.9 "Foreign identity constructors and wire
\ codecs" per-family contract; dot habu-config-config-id-06aa21bd).
\
\ CONCERN: the content-addressed owner of CAD-KIND:config-id. REGISTER interns the
\ canonical build/config FACT STRING into an append-only registry (equal fact sets
\ intern to ONE id), the private RAW>CONFIG-ID / CONFIG-ID>RAW refinements stay
\ owner-only, and the ID>WIRE / WIRE>ID pair is the fixed-width wire codec (total
\ encode; audited fail-closed decode).
\
\ ENGINEERING DECISION - which config facts are canonical for identity (§ 23.9
\ NEEDS-DECISION, resolved in-dot from the plan's fact split). The envelope binds
\ "target/config/numeric-policy facts" (§ 23.9 Canonical typed artifacts) - a THREE-
\ WAY split of the facts that make a build reproducible:
\   - TARGET facts (isa/arch/warp/threads/shared/caps) are owned by target-id
\     (maki/target/target.f);
\   - NUMERIC proof-domain facts are owned by numeric-policy-id (maki/numpolicy.f);
\   - config-id owns exactly the REMAINDER: the deterministic build/toolchain
\     configuration facts that govern a reproducible build and are neither target
\     hardware nor numeric proof domain (§ 9.1 "target/toolchain facts", § 9.2 pass
\     "target/config dependencies; deterministic configuration", § 23.9 ptxas
\     "target/toolchain/config" provenance).
\ The interned content is the canonical, deterministically-ordered SERIALIZATION of
\ those remainder facts as a byte string (content-addressed: equal fact sets share
\ one id). The concrete fact VOCABULARY (which toolchain/build knobs exist) grows
\ with the builder and is intentionally NOT a closed field set here: config-id is the
\ identity of whatever canonical config-fact string the build owner produces, so this
\ leg fixes the OWNERSHIP BOUNDARY (remainder-of-the-split) and the identity
\ mechanism, not a frozen fact list. Hardcoding a closed field set would over-commit
\ the toolchain contract; the boundary (exclude target facts / numeric facts) is the
\ decision the plan's split fixes.
\
\ WIRE FORM (this round): the process-local registry raw as a fixed-width 8-byte
\ little-endian scalar, matching the landed maki/target/target.f, maki/numpolicy.f,
\ maki/schema.f, and maki/producer.f precedents and the maki/db/artifact.f
\ id-on-wire. The § 23.9 origin-class table names the cross-process content key
\ (SHA-256, 32 bytes) as the eventual form; the plan marks reconciling the process-
\ local raw with that content key OUT OF SCOPE for this contract round (the envelope
\ implementation dot owns the migration).
\
\ Fail closed: an empty/oversized fact string, an out-of-range id, a wrong-width or
\ unresolved wire buffer are named throws / reject variants. maki -> habu only;
\ config owns -5338..-5341.

require lib/prelude.f
require lib/string.f
require maki/cad-kinds.f            \ CAD-KIND:config-id nominal identity

-5338 constant E-CONFIG-WIRE      \ ID>WIRE output buffer smaller than the fixed wire width
-5339 constant E-CONFIG-CAP       \ registry count or fact-store bytes exhausted
-5340 constant E-CONFIG-KEY       \ empty config-fact string or string over the per-fact byte cap
-5341 constant E-CONFIG-ID        \ config-id outside the registered range

package CONFIG
public

\ WIRE>ID decode result (the § 23.9 art-result custom-sum idiom): `ok` carries the
\ refined nominal id; the reject arms are the fixed-width byte-decode refusals the
\ contract names (wrong width, unresolved/out-of-range raw). A bespoke per-package
\ sum, not result<a,b>, so a total ok construction leaves no free error variable.
SUMTYPE id-result 1
   VARIANT ok a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;SUMTYPE

private

256 constant CFG-CAP                      \ max distinct registered config fact sets (v1)
256 constant CFG-KEY-MAX                    \ max bytes per canonical config-fact string
CFG-CAP CFG-KEY-MAX * constant CFG-KEY-CAP
8 constant WIRE-BYTES                        \ fixed little-endian wire width of the registry raw

create CFG-KEYS CFG-KEY-CAP allot            \ interned fact bytes, back to back
create CFG-KO   CFG-CAP cells allot           \ per-id fact offset into CFG-KEYS
create CFG-KL   CFG-CAP cells allot           \ per-id fact length
variable CFG-KEY-U                             \ bytes used in CFG-KEYS
variable CFG-N                                  \ registered count

\ Private representation refinements (owner-only, TRUSTED:, never public), the
\ maki/artifact.f RAW>ARTIFACT-ID / ARTIFACT-ID>RAW precedent. The only public
\ producer is REGISTER, bound to a real fact-set registration, so a raw n cannot
\ forge a config identity.
TRUSTED: RAW>CONFIG-ID ( n -- CAD-KIND:config-id ) ;
TRUSTED: CONFIG-ID>RAW ( CAD-KIND:config-id -- n ) ;

: ID-CK ( CAD-KIND:config-id -- n )
   CONFIG-ID>RAW dup 0 < over CFG-N @ >= or if E-CONFIG-ID throw then ;

: KEY-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u CFG-KEY-MAX > or if E-CONFIG-KEY throw then ;

: KEY@ ( n -- ptr u8 n ) {: raw:n :}          \ stored fact bytes for a validated raw id
   CFG-KEYS raw cells CFG-KO + @ +  raw cells CFG-KL + @ ;

: KEY-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}   \ raw id of an equal registered fact set, or -1
   CFG-N @ 0 ?do
      i KEY@ a u STR= if i unloop exit then
   loop -1 ;

: KEY-PUT ( ptr u8 n n -- ) {: a:ptr u:n raw:n :}
   CFG-KEY-U @ u + CFG-KEY-CAP > if E-CONFIG-CAP throw then
   CFG-KEY-U @ {: off:n :}
   a CFG-KEYS off + u BYTE-COPY
   off raw cells CFG-KO + !
   u   raw cells CFG-KL + !
   off u + CFG-KEY-U ! ;

: LE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      v k 8 * rshift $FF and  a k + c!
      1+
   repeat drop ;

: LE-GET ( ptr u8 n -- n ) {: a:ptr w:n :}
   0  0 begin dup w < while
      dup {: k:n :}
      a k + c@ k 8 * lshift  rot or swap
      1+
   repeat drop ;

: R-OK ( a -- id-result<a> )          CONFIG-ID--RESULT:OK ;
: R-WRONG-WIDTH ( -- id-result<a> )   CONFIG-ID--RESULT:WRONG-WIDTH ;
: R-UNKNOWN ( -- id-result<a> )       CONFIG-ID--RESULT:UNKNOWN ;

public

\ REGISTER interns a build config by its canonical fact string: an equal fact set
\ returns the same id (content-addressed), a fresh fact set appends a new registry
\ slot and mints its id. The ONLY authority-bearing public mint, bound to a real
\ fact-set registration, never a raw cast.
: REGISTER ( ptr u8 n -- CAD-KIND:config-id ) {: a:ptr u:n :}
   a u KEY-CK
   a u KEY-FIND {: found:n :}
   found 0 >= if found RAW>CONFIG-ID exit then
   CFG-N @ CFG-CAP >= if E-CONFIG-CAP throw then
   CFG-N @ {: raw:n :}
   a u raw KEY-PUT
   raw 1+ CFG-N !
   raw RAW>CONFIG-ID ;

\ FACTS$ projects a config id back to its registered canonical fact string.
: FACTS$ ( CAD-KIND:config-id -- ptr u8 n )  ID-CK KEY@ ;

\ EQUAL? is same-config identity: interning makes equal fact sets share one raw id,
\ so raw equality is exactly fact-set equality.
: EQUAL? ( CAD-KIND:config-id CAD-KIND:config-id -- bool )
   {: x:CAD-KIND:config-id y:CAD-KIND:config-id :}
   x CONFIG-ID>RAW y CONFIG-ID>RAW = ;

: VALIDATE ( CAD-KIND:config-id -- CAD-KIND:config-id )  dup ID-CK drop ;

\ ID>WIRE is total for a valid id (E-CONFIG-WIRE if the cap cannot hold the fixed
\ width); WIRE>ID reads the fixed width, validates the raw FAIL-CLOSED against the
\ append-only registry, and refines to the nominal id only on success (§ 23.9).
: ID>WIRE ( CAD-KIND:config-id ptr u8 n -- n )
   {: id:CAD-KIND:config-id out:ptr cap:n :}
   cap WIRE-BYTES < if E-CONFIG-WIRE throw then
   id ID-CK  out  WIRE-BYTES  LE-PUT
   WIRE-BYTES ;

: WIRE>ID ( ptr u8 n -- id-result<CAD-KIND:config-id> )
   {: a:ptr u:n :}
   u WIRE-BYTES <> if R-WRONG-WIDTH exit then
   a WIRE-BYTES LE-GET {: raw:n :}
   raw 0 < raw CFG-N @ >= or if R-UNKNOWN exit then
   raw RAW>CONFIG-ID R-OK ;

: COUNT ( -- n )  CFG-N @ ;

;package
