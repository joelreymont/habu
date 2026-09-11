---
title: Report the local-name length limit accurately
status: open
priority: 1
issue-type: task
created-at: "2026-09-11T16:07:57.885292+03:00"
---

Owner: unassigned; reported by Rowan on BB20260911-125858.119-rowan-0ae4. `: T17 ( n -- n ) {: abcdefghijklmnopq:n :} abcdefghijklmnopq ;` reports E-UNDEFINED for abcdefghijklmnopq:n then ncomp cannot compile T17. Sixteen-byte spelling works; source LOC-NAME-W is16 and checker.f documents that internal limit, public docs/forth.md does not. Maki shortened its name; no Habu fix yet.

Acceptance: reject the17-byte local with a precise length-limit diagnostic before native emission, accepted16-byte control, consistent checked --load/JIT/AOT behavior and concise public limit documentation. Do not raise a fixed buffer without examining its representation.

Reconfirmed on current integrated binary SHA1b965ddf: both tier0 and tier1 accept16-byte typed local and return3; both reject17-byte name with misleading E-UNDEFINED (0.238-0.261s). Current source locations: checker.f LOC-NAME-W9861, overflow guard9987; native local capture habu2.f4270. No fix applied.
