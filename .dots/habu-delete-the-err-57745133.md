---
title: Delete the error codes no code throws
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:50:32.951981+02:00"
---

Problem: src/lib/errors.f keeps constants nothing throws: -3506 E-FFI-CALLBACK-FULL (errors.f:161, whose comment said retired and retained for numeric compatibility) and the E-PTXTC-* codes (errors.f:144 names TC-ARCH!, a word that does not exist); found by the src/lib comment review, and Habu keeps no legacy codes. Acceptance: the unthrown constants and their comments are gone; every remaining E-PTXTC-* and E-FFI-* code has a throw site. Files: src/lib/errors.f. Verify: rg -n 'E-FFI-CALLBACK-FULL|E-PTXTC-' src tools test prints only throw sites or nothing; hb --load tools/check.f -- src/lib/errors.f rc 0; hb --load tools/error-code-lint.f green. Depends: habu-cut-comments-in-6b103fd8 (the lib comment lane lands first). Ownership: src/lib/errors.f. Claim: unassigned.

Acceptance case 2026-11-10 (bob; merged from habu-delete-the-unused-0f33a3e4 "Delete the unused error codes in errors.f", one cause with this dot): Problem: no code under src, tools or test uses src/lib/errors.f E-CID-* (-6700..-6708), E-CIE-FORM, E-CRL-* (-6880..-6883) or E-FFI-CALLBACK-FULL (-3506, kept 'for numeric compatibility'); Habu keeps no legacy. Acceptance: the codes and their render rows are gone and nothing under src, tools, test or docs names them. Files: src/lib/errors.f, src/core/render.f if it lists them. Verify: rg -n 'E-CID-|E-CIE-FORM|E-CRL-|E-FFI-CALLBACK-FULL' src tools test docs prints nothing; bin/hb --load tools/check.f -- src/lib/errors.f rc 0. Depends: none. Ownership: src/lib/errors.f.
