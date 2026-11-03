---
title: "Let the DDC audit read and compare today's hb"
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T15:30:42.404273+02:00"
---

Problem (Opus review of the sign lane rlxpkzpu, finding L3, 2026-11-02): tools/ddc-verify.f reads each artifact into a fixed buffer, DDC-BIN-CAP = $400000 (4 MiB, ~:21, comment still says bin/hb ~165 KB), and bin/hb is now about 6.7 MB, so the audit cannot read the hb it audits; and DDC-SAME? (~:59) compares bytes, which two team-signed builds of the same program never match (codesign writes the signing time; DECISIONS 2026-10-31 'comparing signed builds' chose CODESIGN:SAME-IMAGE?). Acceptance: the audit reads each artifact at its own size with no fixed cap and compares through CODESIGN:SAME-IMAGE?; its first-difference report keeps working for unequal images; tools/ddc-verify-test.f covers an artifact larger than 4 MiB and two signings of one image that differ only in the signature time. No public word added or changed. Files: tools/ddc-verify.f, tools/ddc-verify-test.f. Verify: hb --load tools/ddc-verify-test.f rc 0; the explicit audit (docs/bootstrap.md 'DDC Audit') once on a release recovery audit. Depends: the sign lane landed. Ownership: carl. Claim: unassigned.
