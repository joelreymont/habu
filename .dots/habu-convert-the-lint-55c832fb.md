---
title: Convert the lint file buffer to the runtime slab
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T05:46:50.929480+02:00"
---

Found by the create-axiom lane: src/core/checker.f is 43 bytes from a hard lint failure - repo, maki-dep and error-code lints each read whole files into a FIXED 524288-byte buffer and die 'file exceeds buffer' past it; tools/lint/text.f already ships the runtime-sized slab and its own prose says the fixed buffer is the wrong shape for a growing authoritative input. Convert the three lints to the slab; then the create-axiom row's why-derivation (parked in the suite header as recorded debt) can return beside the code. NEVER a cap bump - the file grows every axiom. Files: tools lint readers. Depends: none.
