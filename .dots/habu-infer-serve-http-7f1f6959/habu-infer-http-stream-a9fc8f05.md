---
title: Infer HTTP stream writer
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T13:24:49.550329+02:00"
blocks:
  - habu-infer-http-resp-4740e122
---

Problem: streamed response bytes can fail after a prefix, so retryable or copied event authority can duplicate wire output. Acceptance: consume the response typestate stream authority and render the sealed head, positive-length chunks, and terminal chunk through a typed writer quotation. Each write event is a public linear type; writer success returns the sole successor authority, while any throw consumes it and leaves the stream permanently unusable. Emit canonical uppercase hexadecimal chunk lengths and exact CRLF framing. Cover failure before any byte and after every prefix, payload, and suffix boundary; no retry or duplicate event certifies. Empty data never masquerades as terminal. Exact wire bytes and exact-capacity buffers pass. Files: stream writer package, focused writer fixtures, Maki manifests and FILEMAP. Verify: every failure boundary, checker duplication negatives, owning Maki slice, typed-local, package, host, filemap, trust, strict inventory, and dot gates. Dependency: habu-infer-http-resp-4740e122. Ownership: failure-propagating chunked writer events only.
