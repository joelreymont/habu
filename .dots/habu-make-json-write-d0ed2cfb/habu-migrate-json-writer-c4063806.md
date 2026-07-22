---
title: Migrate JSON writer test consumers
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T16:07:53.649659+02:00"
blocks:
  - habu-build-explicit-json-399f5929
---

Why: json-read roundtrip and hb-build helper fixtures call the deleted singleton directly even though they do not own production rendering. Exact result: each fixture allocates its own aligned writer state, scratch, and destination; threads the linear writer through STRING or the roundtrip document; MATCHes COPY:result; and closes once. No shared helper state spans two suites. Acceptance: roundtrip bytes remain exact, adversarial escaping remains covered, required-capacity paths leave destination sentinels unchanged, and rg finds no old JSON-WRITE:$ or zero-argument RESET use in these files. Smallest checks: bin/hb --load lib/json-read-test.f and bin/hb --load tools/hb-build-test.f. Depends: Build explicit JSON writer core. Ownership: lib/json-read-test.f, tools/hb-build-test.f, FILEMAP.md only if its rows change. Claim: unassigned.
