---
title: Pack captured code at startup
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-28T22:24:59.347269+02:00\""
blocks:
  - habu-pack-startup-data-0c5b8d64
---

The accepted 5661 engine stores a 1,383,192-byte final bound code blob. A bounded measurement with the reviewed local encoder reconstructs every byte and reduces its provisional framed representation to 610,304 bytes (22 independent blocks). This is file compression, not fewer executed instructions. Startup already copies this code into an anonymous runtime region; decode directly there before publication, relocation and RX protection. Preserve raw reusable AOT13, snapshot10, patch identity, complete/partial/empty seeds, closure ownership and source coordinates. Reuse the DATA codec at the responsible layer after that feature is qualified. Separate physical packed size from logical code views in tools. Count decoder, framing, padding and signature costs; measure native startup and transient memory. Require independent review, real corruption/restore/recapture/merge E2Es, native generation identity, full gate, Maki checks and verified installation. Source design and measurement receipts: ~/.cache/tmp/habu-code-storage-design-20260928-01.md and habu-code-storage-census-completion-20260928-01.md. The shared-decoder design is accepted in habu-code-storage-design-20260928-02.md. Implementation starts in an isolated workspace from the reviewed DATA candidate 6ab7d136 after its B1/B2 and focused E2E passed. DATA startup observations and full qualification continue independently; carry any production fixes forward and wait for the heavy build lane. Landing still depends on DATA qualification.
