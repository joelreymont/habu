---
title: Remove speculative stdlib skips
status: closed
priority: 1
issue-type: task
created-at: "2026-07-19T21:56:36.274909+02:00"
closed-at: "2026-07-20T20:57:25.192530+02:00"
close-reason: "Merged fc88c546: EXCLUDED?/PREFIX? speculative table removed from the bare-load gate; discovery is purely structural (FLAT-MODULE?); all three formerly reserved modules bare-load and are covered; injected-path fixture pins the structural rule"
---

Current master correctness/process defect: test/stdlib-standalone-load.f:70-83 claims every new flat lib module is covered automatically, but EXCLUDED? preemptively skips future lib/engine-candidate.f and every lib/process-pty* path before those modules exist. Any later matching production module silently bypasses the bare-load gate, contradicting the stated invariant and creating permanent skip logic. Remove speculative exclusions and the prefix policy. A lane that adds a module which does not bare-load must stay red until the module owns its dependency closure; active ownership is not a correctness exemption. Preserve derived discovery and current module ordering. Add a fixture injecting newly discovered flat module paths, including both formerly reserved spellings, and prove each is scheduled; nested and *-test files remain excluded only by structural scope; no path-prefix or future-file allowlist can suppress production modules. Verify the standalone-load, suite-coverage, stdlib, package, host/filemap/dot, and full native gates. Serialize same-file edits with habu-structure-standalone-load-40be1523.

Claim: agent=skips workspace=.jj-ws/habu-remove-speculative-stdlib-aef99234
