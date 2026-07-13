\ owner-wid-guard.f - fail fast when an owner-wid suite runs outside its harness.
\
\ The owner-wid suites are build-context tests: test/run.f's forge harness
\ (owner-wid-internal.f -> owner-wid-child.f) builds a test-only hb-stdin image
\ whose cold hook fills the owner registry to OWNER-WID-MAX, then runs the
\ read-only suites under that image. A plain `bin/hb --load test/owner-wid-<x>.f`
\ lacks that context and only produces misleading failures, so each suite guards
\ its load head and dies with GUARD-RC before any assertion runs.
\ test/owner-wid-emitter.f is bundle-injected and cannot require this file; it
\ carries the same inline guard (message and rc kept in sync by hand).

package OWNER-WID-GUARD

78 constant GUARD-RC

: FORGED-REGISTRY? ( -- bool )
   data-base OWNER-WID-N-CELL + @ OWNER-WID-MAX = ;

: HARNESS-ENV? ( -- bool )
   s" HABU_OWNER_WID_HARNESS" GETENV nip 0 > ;

: FAIL ( -- )
   s" owner-wid suites run inside test/run.f's forge harness" GUARD-RC die ;

public

: REQUIRE-FORGED ( -- )
   FORGED-REGISTRY? if exit then
   FAIL ;

: REQUIRE-HARNESS ( -- )
   HARNESS-ENV? if exit then
   FAIL ;

;package
