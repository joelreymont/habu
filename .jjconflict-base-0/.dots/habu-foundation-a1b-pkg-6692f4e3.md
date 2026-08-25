---
title: "Foundation A1b: package-scoped nominal role resolution"
status: open
priority: 3
issue-type: task
created-at: "2026-07-18T17:42:45.520291+02:00"
---

Blocked fork split out of habu-foundation-a1-declarable-98aebe7b. The CT-ROLE declaration mechanism (DEFTYPE) already mints distinct nominals past CC-MAX with converters, persistence, and rollback (proven by test/type-nominal-suite.f). The remaining requirement - package-scoping - requires making the con-resolution path package-aware (CON-OF/CT-FIND at checker.f:2432, TYPE-RESERVED? at checker.f:2511, DEFTYPE name storage in roles.f:90, converter naming, CT-SNAPSHOT-PERSIST name format at checker.f:5656). Today DEFTYPE registers a global bare name and a second package declaring the same tail hard-dies (exit 70). Decide package-first-bare vs qualified-only semantics (mirroring SIG-FAM?/TFAM-RESOLVE package scoping for families), implement, add negative fixtures (two packages same tail distinct; bare tail unresolved outside its package), prove byte-fixpoint + full gate. Must resolve the substrate decision first (see blocks).

Re-scope 2026-07-18 (substrate decision landed, docs/extent-substrate.md): CT-role package scoping is NO LONGER on the extent/BTC critical path — EXTENT:/idx<#M> mints on TFAM families, which are already package-scoped (TFAM-DECL; proven: two packages declared the same tail with no E-TFAM-DUP). CT roles (DEFTYPE, A1) stay global-flat, correct for syscall-style VALUE nominals (pid/fd/rc). Implement the package-scoped CT-role resolution (CON-OF/CT-FIND/TYPE-RESERVED? restructure) ONLY when a concrete consumer needs a package-private value nominal that a TFAM arity-0 nominal scalar cannot serve (MISSING.md Foundation A application-port class: frame-idx, camera-serial — real but not flagship-critical). Kept open at p3 as that contingency; the CON-OF-restructure design notes in this dot are the starting point.

OBVIATED FOR VALUE NOMINALS 2026-07-18 (orchestrator, from the value-nominal lane's probe matrix in docs/value-nominal-substrate.md): NOMINAL: shipped on TFAM arity-0 families, which are already package-scoped, so the CON-OF/CT-FIND package-keyed restructure this dot describes is NOT needed for the value-nominal surface. Keep this dot only if a consumer appears that specifically needs package-scoped CT-roles (DEFTYPE-substrate nominals); otherwise it is a candidate for retirement when the CT-role surface is next audited.
