---
title: Report a trusted-only tick as a JSON packet
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:38:54.698636+02:00"
---

Problem: ': F4 ( -- ) ['] patch32 drop ;' under tools/check.f --json-errors exits 70 and prints only the plain line 'hb: trusted-only tick: patch32' on stdout, nothing on stderr (src/habu/outer.f:1014 REFUSE, src/habu/habu2.f:167 TRUSTTICKMSG$); the E-CAP-TRUSTED packet with a span that PREVERIFY-BYTES and the verifier image give never reaches the CLI. Acceptance: --json-errors prints an E-CAP-TRUSTED packet with file, line and token span for this source, and prose mode names the location. Files: src/habu/outer.f, tools/check-core.f, tools/check-test-lib.f. Verify: /tmp/tim-dots/f4.f with tools/check.f --json-errors; tools/check-test.f. Depends: none. Ownership: check CLI. Claim: unassigned.
