---
title: Keep scratch text out of objects and products
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T22:53:52.715301+02:00"
---

Problem: linker.f's object stores ERROR's LINE text (src/lib/error.f LINE, LINE-U, DEFINE) and util.f's DIGITS buffer (src/core/util.f DIGITS$) raw into earlier files' data, and hb carries the last text ("-9693 constant E-WORDLIST" at hb-b 0x33de10). The values do not depend on layout, so warm == cold holds; they are working buffers, not program state. Acceptance: the buffers are scratch, objects carry no store into them, and the product's buffers are zero. Files: src/lib/error.f, src/core/util.f. Verify: object diff of two cold builds shows no store into those buffers; hb builds and passes error-test.f. Depends: incremental builds 100% (Joel 2026-11-09: everything else after). Ownership: carl. Claim: unassigned.
