---
title: "Single prefix load: the four-leg implementation"
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T19:57:31.271581+02:00"
---

The 87a370ae probe answered YES with a working experiment (patch banked in the session scratchpad; byte-identical fixpoint c2910ab7 x2, maki green, install 17.1->14.1s, 9 prefix loads not 15, emitted source 2.18MB->800KB, FULL host/target prefix isomorphism - same order same addresses, which retires 5a992a38's and ef47ad69's ground). Four legs, RULED: (1) CERTIFY-ONLY BOOT-PREFIX ASSEMBLY is REQUIRED - the experiment drops certify census 4580->1493 and those 3087 definitions' ONLY gate was the build pre-pass (the 0b5fc6e6 invariant; hide.f's own comment cites it): emit the boot-prefix bytes into a certify-only buffer through VERIFY:SOURCE-BUF, never fed to the compile - a new build phase, justified by preserving a named invariant while deleting 1.38MB of double-compile. (2) The watermark lives in a DEDICATED prefix source file (one concern), wired by the aot-decl/aot-arm manifest precedent - READ the veto record at generated-declaration.f:395-402 and its dot first; if that veto binds THIS edit shape, checkpoint. The rewind is TWO-PART by measurement: ndict! AND USIGS-RESTORE-END (the store's real seam - raw UEND! leaves broken index heads; the silent-78 class). (3) snap: BF-APPEND-SNAP-KEEP re-loads the prefix onto sealed one-shot defers - same file-list surgery; bonus, erases the 4.48MB-orphan-in-every-snapshot class. (4) hide.f keeps BOTH rewind forms - the Gforth recovery host has no watermark (bootstrap-mirror-lint + bootstrap-codegen-test are the tripwires). build-fixpoint-test's emitted-source substring pins rewrite to the new contract. Full seed-affecting gates; the probe's three-way proof inverted is the regression (probe prints ONCE, base word resolves, window reaches the FIRST copy). Relationship: sibling+amplifier of 8010f67c, composes, neither subsumes.

WATERMARK PLACEMENT RULING 2026-08-17 (the lane's checkpoint):
OPTION (B) - the dedicated file src/core/prefix-mark.f, ruling
as written, and THE FLEET RESEED IS ACCEPTABLE NOW: zero worker
lanes are live (all workspaces retired this cycle; only
merge-gate and the lane itself hold engines) - this is the
cheapest moment the tree will ever have for a cold-prefix file
addition. The seven-place cascade is tripwired (boot-pin-test
reds until bumped - bump it WITH attribution), and mixing the
watermark into lower-cert-seal.f's erase-authority concern is
the exact misleading-reader debt this campaign paid down
elsewhere. ALSO RULED IN: (i) the lane's capability-probe rewind
form - the emitted line resolves the watermark at runtime and
falls back to BFR-MARKER-INDEX when absent, an existence check,
both paths failing loud; BFR-USIG-CUT/-EARLIEST die as dead
code; (ii) the STALE VETO record at generated-declaration.f:
395-402 and its dot get updated with the lane's measurement
(build-fixpoint.f is packaged now; the lint is live - the
break-and-watch probe proved it); (iii) leg 4's true size
acknowledged - each replacement pin mutation-backed, none a
spelling of the new implementation; (iv) the watermark point
stays AT THE PROBE'S POINT (the only proven one; SEAL-NDICT-CELL
is past the chain rows and would collide - the lane's
duplicate-78 measurement is the argument).

PLACEMENT RULING REVERSED 2026-08-17 (the lane's leg-2 evidence;
my (B) ruling rested on its own one-file-early veto measurement -
the veto MOVED, it was not stale, and the lint refuses (B) in
three legacy files): OPTION (C) - the mark records at the END OF
lower-cert-seal.f, the file that IS the core prefix's last file.
The re-derivation: the structural invariant is POSITION (the mark
must be taken at the boundary), not file identity - the boundary
is the end of that file, so recording it there is placement AT
the fact, and a dedicated file was the expensive answer: a lint
migration (packaging three files other dots own), a proven
two-stage landing (no existing engine bakes the new row - the
build refuses until an engine rebuilds without the mark), a
fleet reseed, and nine hand lists, against four lines at the
boundary. THE GOOD PARTS OF LEG 2 ALL SURVIVE THE MOVE: the
words stay in their own package block (package PREFIX-MARK
inside lower-cert-seal.f - the lint accepts packaged words in
any file; the NDICT@-shadow lesson and its DICT/USIG naming
stand), the capability probe stays IN THE BUILD
(BF-REQUIRE-WATERMARK refusing before a byte is emitted, proven
firing), the regression test stands, and a 3-line comment at
the mark STATES the derivation and prices the rejected
alternative so the concern-mixing is a documented decision, not
drift. The stale-veto correction on generated-declaration.f
gets its own honest wording: the veto moved (build-fixpoint.f
packaged; habu2.f/boot-pin still unpackaged - their packaging
belongs to their own dots).

