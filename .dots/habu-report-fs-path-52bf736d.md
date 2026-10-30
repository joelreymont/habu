---
title: "Report FS path refusals at each tool's command line"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T02:44:10.433242+02:00"
---

Problem: the C-string stack (habu-one-c-str-5015ed16, L6 and L7) deleted the private path copiers of imgdump, imagedisasm, json-only, repair-packet-core, diag-to-sarif-core, gate-json-assert-core, lint/text READ-FILE, bundle-lib-core and aot-call-report OPEN-INPUT, so each now ends an over-long path argument in an uncaught E-FS-PATH (-2100, rc 67) and a 0-byte path in an uncaught E-FFI-NUL (-3504) where it died with its own "path too long" line (rc 74 or 1), and bundle-lib FS:FILE? throws where BL-FILE? answered false ("missing module"). tools/check-core.f:854 shows the shape: the tool maps the refusal to its usage message at its command-line boundary (Fable review of the stack, finding 5). Acceptance: each listed tool reports a refused path argument with its usage line and its usual rc through one shared word where the tools share a CLI library; one case per tool family proves the over-long and the 0-byte path. Files: the tools above, tools/check-core.f as the precedent. Verify: each tool with an over-long and a 0-byte path argument; the tool rows in test/gate-stdlib-cases.f. Depends: habu-one-c-str-5015ed16. Ownership: tools/. Claim: unassigned.
