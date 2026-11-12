---
title: "Find why hb keeps sealed packages' private headers"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T18:27:56.621048+02:00"
---

Problem: docs/architecture.md:96-97 says the linker writes a sealed package's private wordlist empty, so no later source can name its words (Joel: privacy is private sections plus the build's strip). On hb built from the seal stack, a program reaches any engine private without reopening a package: CHECKER-RESOLVE:WL-PROBE (src/core/checker.f:3167, a public wrapper of the trusted-only xref-search-wl) finds MACHO in BOOT:NAMESPACE-WL, then MH-MAGIC64 in that record's XREF-PKG-PRIVATE, and XREF-START executed through a CAST: prints 4277009103, rc 0; the private record is found in NDICT, CHECKER-RESOLVE, MACHO and FS, on the installed hb too. Acceptance: the cause, as path:line, of hb keeping those private headers, and of WL-PROBE publishing a trusted-only primitive; then the fix dot. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/privacy-hunt.
