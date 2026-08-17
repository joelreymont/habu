---
title: Seeded signature loses package qualifiers on round-trip
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T01:57:29.789185+02:00"
---

src/compiler/ir/source.f:371 declares ( IR-ARENA:arena IR-ID:ir-source-id n n -- IR-SOURCE:span ); the seeded store renders it as ' IR-ID:ir-source-id n n -- span' - IR-ARENA:arena gone to a bare space, IR-SOURCE:span stripped to span, IR-ID:ir-source-id intact. Re-parse at intake then fails 'checker: a seeded signature does not parse in the engine it was baked into' (exit 76), reding ~13 battery phases (test/compiler/ir-op.f, ir-build.f, native-tape.f, ir-attr.f, ir-context.f, ir-source.f, native-*.f). Naming IR-SOURCE:SPAN by hand certifies on product and hb-host alike, so the type registry travelled: the defect is the signature store's RENDER of package-qualified family types, not the registry. Acceptance gap: every family-typed case to date put the family in the TEST's signature (SIGFT's IR-ARENA:view); none exercised a seeded word whose own signature names a family. Blocks habu-seeded-words-invisible-c7505a49.
