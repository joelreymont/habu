---
title: Export public NOM handle type names
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-15T12:22:40.894085+02:00\""
---

Substrate gap found by the cadeffect lane 2026-07-15: lib/nominal declares its handle families (row/path/binding) in package NOM's PRIVATE section, so no external signature can name them - src/cad/effect.f needs an effect-row brand bridged by two audited no-op TRUSTED: casts (NOM:ROW>EFF / NOM:EFF>ROW, the only words naming row, in a package-NOM reopen). Fix: export the three family names publicly from lib/nominal (public TYPEFAMILY declarations or EXPORT aliases) so consumers brand/consume typed handles without casts; then retire the two bridge casts + their TRUSTED.md rows (this dot owns those rows until then). Acceptance: effect.f names NOM:row directly, bridge casts gone, trust manifest shrinks by 2, effect suites green. Files: lib/nominal/*.f, src/cad/effect.f, TRUSTED.md, lib/std.manifest if rows change. Verify: nominal + effect suites, trust/inventory, lint-manifest. Ownership: nominal substrate surface.

Claim: agent=nomexport workspace=.jj-ws/fable-nomexport
