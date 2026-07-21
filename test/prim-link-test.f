\ prim-link-test.f - focused ARM64-contract link query over the primitive-effect
\ (PES) axiom rows (dot habu-link-arm64-contracts-8cca6cc1).
\
\ Proves the checker's package-scoped PRIM-LINK query (src/core/checker.f) binds
\ an emitted ARM64 primitive/callable contract to exactly ONE immutable axiom row
\ by the row's stable identity - defining package + word spelling - and rejects a
\ link that is missing (unknown primitive / wrong package), ambiguous (a duplicate
\ spelling with no single row), or stale (the row's identity fingerprint drifted
\ from the contract's recorded value). The API:
\   PRIM-LINK:COUNT   ( pkg-a pkg-u name-a name-u -- n )       live PES rows for key
\   PRIM-LINK:RESOLVE ( pkg-a pkg-u name-a name-u -- bool )    true iff exactly one
\   PRIM-LINK:FP      ( -- fp )                                identity of that row
\   PRIM-LINK:CHECK   ( pkg-a pkg-u name-a name-u expect-fp -- bool )  sound link
\
\ The fingerprint is a SHAPE identity (arity + per-slot EFAM-* family + the
\ PE-TRUSTED-ONLY flag), so two rows of identical shape share one - CHECK always
\ combines it with the (pkg,spelling) key. The pinned constants below are the
\ committed fingerprints an ARM64 contract records; a mutation to the linked axiom
\ flips the fingerprint and fails CHECK, so these rows are the staleness ratchet.
\ dup ( a -- a a ), swap ( a b -- b a ), and TYPE-FIELD:FAMILY@ ( n -- n ) carry
\ immutable-by-design effects, so the constants are stable.
\
\ NEGATIVE REGRESSION (surface pin): renaming or removing a PRIM-LINK entry makes
\ its qualified name undefined at load -> rc 70, so this test fails loudly; a
\ changed fingerprint packing or family projection flips a pinned T= and fails.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f test/prim-link-test.f

require lib/errors.f
require lib/string.f
require lib/test.f

532736  constant PL-DUP-FP        \ dup   ( a -- a a ) : din 1 gray, dout 2 gray
2163712 constant PL-SWAP-FP       \ swap  ( a b -- b a ): din 2 gray, dout 2 gray
133157  constant PL-FAMILY-FP     \ TYPE-FIELD:FAMILY@ ( n -- n ): din 1 scalar, dout 1 scalar

variable PL-TMP

\ The link query reads raw PES state, so - like the effect-read API and the
\ top-row hook - it is consumed from an unchecked window. `0 set-check` opens it;
\ T= / TTRUE / TFALSE / T-LABEL from lib/test.f stay callable across it.
0 set-check

: PL-RESOLVE-UNIQUE ( -- )
   s" a global prim resolves exactly one immutable stack-effect row" T-LABEL
   s" " s" dup" PRIM-LINK:COUNT 1 T=
   s" " s" dup" PRIM-LINK:RESOLVE TTRUE
   PRIM-LINK:FP PL-DUP-FP T=
   s" the fingerprint is deterministic across re-resolution" T-LABEL
   s" " s" dup" PRIM-LINK:RESOLVE TTRUE   PRIM-LINK:FP PL-DUP-FP T= ;

: PL-PACKAGE-KEYED ( -- )
   s" a packaged pprim resolves under its defining package" T-LABEL
   s" TYPE-FIELD" s" FAMILY@" PRIM-LINK:COUNT 1 T=
   s" TYPE-FIELD" s" FAMILY@" PRIM-LINK:RESOLVE TTRUE
   PRIM-LINK:FP PL-FAMILY-FP T=
   s" TYPE-FIELD" s" FAMILY@" PL-FAMILY-FP PRIM-LINK:CHECK TTRUE ;

: PL-WRONG-PACKAGE ( -- )
   s" the same spelling under the global or a wrong package resolves no row" T-LABEL
   s" " s" FAMILY@" PRIM-LINK:COUNT 0 T=
   s" LOWER-CERT" s" FAMILY@" PRIM-LINK:COUNT 0 T=
   s" " s" FAMILY@" PRIM-LINK:RESOLVE TFALSE
   s" LOWER-CERT" s" FAMILY@" PL-FAMILY-FP PRIM-LINK:CHECK TFALSE ;

: PL-DUPLICATE ( -- )
   s" an overloaded spelling has more than one row and is ambiguous" T-LABEL
   s" " s" +" PRIM-LINK:COUNT 1 > TTRUE
   s" " s" +" PRIM-LINK:RESOLVE TFALSE
   s" " s" +" PL-DUP-FP PRIM-LINK:CHECK TFALSE ;

: PL-UNKNOWN ( -- )
   s" an unknown primitive resolves no row" T-LABEL
   s" " s" zz-no-such-prim" PRIM-LINK:COUNT 0 T=
   s" " s" zz-no-such-prim" PRIM-LINK:RESOLVE TFALSE
   s" " s" zz-no-such-prim" PL-DUP-FP PRIM-LINK:CHECK TFALSE ;

: PL-STALE ( -- )
   s" a sound link accepts its row's committed identity" T-LABEL
   s" " s" dup"  PL-DUP-FP  PRIM-LINK:CHECK TTRUE
   s" " s" swap" PL-SWAP-FP PRIM-LINK:CHECK TTRUE
   s" a row whose identity drifted from the recorded fingerprint rejects" T-LABEL
   PL-DUP-FP PL-SWAP-FP <> TTRUE                     \ the fingerprint discriminates arity
   s" " s" dup"  PL-SWAP-FP PRIM-LINK:CHECK TFALSE   \ dup carrying swap's identity
   s" " s" swap" PL-DUP-FP  PRIM-LINK:CHECK TFALSE ; \ swap carrying dup's identity

: PL-FAMILY-SENSITIVE ( -- )
   s" equal arity but different slot families yields a different identity" T-LABEL
   s" " s" negate" PRIM-LINK:COUNT 1 T=
   s" " s" @"      PRIM-LINK:COUNT 1 T=
   s" " s" negate" PRIM-LINK:RESOLVE TTRUE  PRIM-LINK:FP PL-TMP !
   s" " s" @"      PRIM-LINK:RESOLVE TTRUE  PRIM-LINK:FP PL-TMP @ <> TTRUE ;

\ Focused ARM64 contract probe: model a typed ARM64 routine effect schema whose
\ emitted callable body links each primitive it lowers to one audited axiom row.
\ Each contract row records (defining package, spelling, expected identity); the
\ schema validates every link through the one query, and a contract whose recorded
\ identity no longer matches the live row is rejected before it can be emitted.
: PL-ARM64-CONTRACT ( -- )
   s" every ARM64 contract link resolves its one axiom row and matches identity" T-LABEL
   s" " s" dup"  PL-DUP-FP    PRIM-LINK:CHECK TTRUE
   s" " s" swap" PL-SWAP-FP   PRIM-LINK:CHECK TTRUE
   s" TYPE-FIELD" s" FAMILY@" PL-FAMILY-FP PRIM-LINK:CHECK TTRUE
   s" a contract linking a duplicate, unknown, mispackaged, or drifted row rejects" T-LABEL
   s" " s" +"       PL-DUP-FP    PRIM-LINK:CHECK TFALSE   \ ambiguous (duplicate spelling)
   s" " s" zz-none" PL-DUP-FP    PRIM-LINK:CHECK TFALSE   \ unknown primitive
   s" LOWER-CERT" s" FAMILY@" PL-FAMILY-FP PRIM-LINK:CHECK TFALSE   \ wrong package
   s" " s" dup"     PL-SWAP-FP   PRIM-LINK:CHECK TFALSE ; \ stale identity (row mutation)

: PL-MAIN ( -- )
   T-RESET
   PL-RESOLVE-UNIQUE
   PL-PACKAGE-KEYED
   PL-WRONG-PACKAGE
   PL-DUPLICATE
   PL-UNKNOWN
   PL-STALE
   PL-FAMILY-SENSITIVE
   PL-ARM64-CONTRACT
   T-REPORT
   s" prim-link: ok" type cr ;

PL-MAIN
