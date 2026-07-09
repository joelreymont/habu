---
title: "Snap certify regression + labeled snapshot-loader exits"
status: open
priority: 2
issue-type: task
created-at: "2026-07-09T14:18:36.266908+02:00"
---

Tracker for two follow-ups exposed while landing TFAM 12 item 6. The trailer
regression fixture itself LANDED in tools/build-fixpoint-test.f
(BFT-TEST-SNAP-TRAILER, step row `snap trailer`) on the runtime route
(hb-stdin < hb-snap-src -> hb-snap0, ~0.9s added suite cost); this dot is NOT
the fixture home.

(1) `-- snap` certify regression (hard dependency): `bin/hb --load ...
tools/build-fixpoint.f -- snap` FAILS CLOSED at BF-CERTIFY-SNAP.
VERIFY:SOURCE-BUF (src/habu/verify-source.f VERIFY-SOURCE) does NOT honor
`0 set-check`, so it checks snap.f SNAP-RETIRE-GO (a deliberate 0 set-check
boundary) and rejects on undefined SNAPGO (defined in require'd
src/habu/snap-lib.f, not textually in the certify buffer; also uses
prefix-only CHECKER-SNAPSHOT-PREPARE/INCLUDE-SNAPSHOT-PREPARE). Repro: rc 74,
'certify: hb-snap-src rejected rc 70', E-BUILD-STATUS (-2802). Regressed by
commit 2017301c33b7 'Make fixpoint source certification blocking' (before it,
certify warned+proceeded). This is the certify-honors-injected-set-check gap
owned by habu-staged-fixpoint-src-0b5fc6e6 (itself blocked on
habu-builder-trust-checked/habu-fix-sig-clobber). The runtime snapshot build
works (hb-stdin < hb-snap-src -> hb-snap0 rc 0, 11.5MB); only the blocking
certify gate stops `-- snap`. When the dependency lands, re-enable the
`-- snap` route end-to-end (BF-BUILD-SNAP-FRESH) and point the fixture's
builder at it. Depends on: habu-staged-fixpoint-src-0b5fc6e6.

(2) Labeled exits for the snapshot loader rejects: EM-SNAPSHOT-RESTORE
(src/habu/habu2.f snbad/snbadver) exits 79 (corrupt trailer) / 80
(E-SNAP-VERSION) with a bare NR-EXIT-GROUP - NO diagnostic text on any fd.
Silent rc-only capacity/validation exits are the disease class the campaign
has been fixing (cf. the IBUFSZ silent-74 lesson; silent capacity exits are
already dotted to print their own name before dying). Emit a short named
diagnostic ('hb: snapshot trailer corrupt' / 'hb: snapshot format version
unsupported') on fd 2 before the exit, mirror in the Gforth bootstrap
emitter, and extend BFT-TEST-SNAP-TRAILER to assert the text.

Ground-truth loader matrix (measured 2026-07-09, macOS/arm64, each leg a
fresh re-codesigned copy; encoded in BFT-TEST-SNAP-TRAILER): control rc 0;
version cell (magic+40) 255 -> rc 80; region-len (magic+24) middle byte $FF
-> rc 79; ndict (magic+16) middle byte $FF -> rc 79; magic corrupted -> COLD
BOOT rc 0 (both trailer probes miss - fall-through, not a rejection);
data-len top-region bytes can SIGSEGV (-11) - not a fixture leg; un-resigned
patched image -> SIGKILL (rc -9) before the loader runs. The trailer magic
sits at file-offset size-57392 (SNAP-EXTRA-SIZE 16488 pad + ~40KB codesign
blob follow it), so fixtures must SCAN for the last SNAP-MAGIC occurrence,
never use FILE-SIZE-relative offsets.
