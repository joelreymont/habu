---
title: judge reference column needs an ELF reader
status: open
priority: 3
issue-type: task
created-at: "2026-08-23T11:50:22.783045+02:00"
---

Problem: tools/codegen-compare-cc.f DECIDE refuses on HB-TARGET-MACOS? 0= because the reference reader parses Mach-O 'nm -m' / 'size -m' output, so the judge's informational clang column is absent on every Linux host by construction even with /usr/bin/clang present (found 2026-08-23 by the judge-host lane; the checked half no longer depends on it). Acceptance: an ELF leg of the reader (nm/size or readelf on a clang -c object, same byte and symbol columns), the reference column present on this host, docs/codegen-parity.md updated, a fixture with a hand-built ELF object. Files: tools/codegen-compare-cc.f, tools/judge/ref.f, docs/codegen-parity.md. Verify: bin/hb --load tools/judge.f prints the reference column on Linux; ref-test green. Depends: habu-judge-gate-is-8a2af19b. Ownership: judge. Claim: unassigned.
