---
title: Gate reserved variant names in the event module
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T17:18:31.145740+02:00"
---

Why: DECL-EVENT claims that variant registration rejects reserved names, but SUMV-ADD enforces only canonical spelling and duplicates. The unified full and compact ENUM paths therefore accept names such as `n` and `if`. The legacy parser rejects them, but new code must not depend on the unowned legacy parser scheduled for deletion.

Owner and interface: add sealed pre-hook package TYPE-NAME beside the family registry. Its only public word is `VARIANT-REQUIRE ( ptr u8 n -- )`. It is the unified path's sole variant-name policy. It rejects an empty token with 7107, non-canonical spelling with 7101, and every reserved or colliding name with 7110. Reserved means a single character, a value-record name, `field`, a concrete checker type, an atom or fresh-atom token, a Forth control word, or a type-declaration grammar keyword. Collision means an exact global family or an exact family in the active package. Preserve the legacy scope rule: unrelated package families do not reserve the tail.

Boundary and call site: DECL-EVENT adds exactly one audited pre-hook bridge, `DEV-NAME-VARIANT-REQUIRE ( ptr u8 n -- )`, whose body is the qualified call `TYPE-NAME:VARIANT-REQUIRE`. Inventory it under this dot. `DEV-VARIANT` calls it after validating the live declaration token and before SUMV-ADD or any ordinal, event, field, or registry mutation. SUMV-ADD remains the canonical duplicate registrar. Do not add a `using`, copy the policy into either front end, edit legacy TDECL globals, broaden field-name policy, or add another public/raw/trusted surface. The legacy authority is deleted by `habu-delete-legacy-variant-ad234821`; it is not semantically edited under the package-migration gate.

Acceptance: red-first production tests through ENUM-DECL full and compact modes and the raw DECL-EVENT transaction reject empty, `n`, `if`, `variant`, a concrete type, a value-record, an atom token, an existing global family, and an active-package family with the exact codes above and byte-identical registry/event/field/ordinal rollback. A same tail owned only by another package remains legal. Existing canonical names and duplicate-name precedence remain unchanged. Mutations removing the policy call, moving it after SUMV-ADD, dropping the single-character case, dropping control/grammar checks, or changing package-aware family lookup fail. Correct the false DECL-EVENT comments. Files: src/core/type-family.f, src/core/decl-event.f, focused type-family/decl-event/enum-decl tests, TRUSTED.md, and inventories only if required. Run those suites plus structure-decl, exact typed-local and package diff lints, strict trust/inventory, candidate validation, and native fixpoint. Smallest owning path: `bin/hb --load test/enum-decl-suite.f`.
