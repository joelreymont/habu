---
title: Convert the lint file buffer to the runtime slab
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T05:46:50.929480+02:00"
---

Found by the create-axiom lane: src/core/checker.f is 43 bytes from a hard lint failure - repo, maki-dep and error-code lints each read whole files into a FIXED 524288-byte buffer and die 'file exceeds buffer' past it; tools/lint/text.f already ships the runtime-sized slab and its own prose says the fixed buffer is the wrong shape for a growing authoritative input. Convert the three lints to the slab; then the create-axiom row's why-derivation (parked in the suite header as recorded debt) can return beside the code. NEVER a cap bump - the file grows every axiom. Files: tools lint readers. Depends: none.

PARTIALLY DONE by the recorder landing (2f988d14, forced scope):
shadow-lint, ptx-emitter-lint, error-code-lint-core and
maki-dep-lint-core converted to LINT-SLAB (they sat 59 bytes from
red on master); maki-dep-lint gained its package under the lint's
own rule. REMAINING fixed whole-file arenas, none reading
checker.f today, all the same landmine: namespace-lint-core,
dot-dep-lint-core, diag-origin-core, json-only,
repair-packet-core. Verify repo-lint's reader too (named in this
leaf's original list, not in the converted set). Also from that
landing: the census SRC-CAP ($4000) prose corrected but the value
stands - a comment-heavy definition past 16K still stops a run.
