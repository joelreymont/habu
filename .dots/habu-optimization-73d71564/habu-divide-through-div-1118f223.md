---
title: Divide through (DIV-ZERO) in twelve bytes
status: closed
priority: 1
issue-type: task
created-at: "2026-09-30T10:58:28.235294+02:00"
closed-at: "2026-09-30T14:23:56.946348+02:00"
close-reason: "Landed as 'Divide through (DIV-ZERO) in twelve bytes', stacked with the link-save slice on master after cc8cf301. Each of the 43 guarded division sites branches to the shared cold (DIV-ZERO) routine; census guarded-division bytes 860 -> 516 (-344 B). Gen-2 aot/code-blob with the link-save slice, against master+S12: 1,391,616 -> 1,390,344 (-1,272 B). Engine image 2,477,047 B, unchanged. Gate at the combined tip: full suite 501/501 (curl-http failed once, the same failure master+S12 shows, and passed on rerun); gens 2-5 byte-identical."
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.1). Line references are at master 8c9b75af; re-verify before editing.

Problem: PUT-SDIV (src/compiler/native/emit.f:1477-1483) writes `cbnz; movn; str [x19],#8; bl throw; sdiv`, 20 bytes, and DIV-INSNS is 5 (emit.f:676), which makes INSN-PER-OP = 3 (emit.f:117) false: a word of about thirteen discarded divisions refuses with E-A64EMIT-CAP (emit.f:643). The engine's own `/` uses `cbnz; bl (DIV-ZERO)` (src/habu/habu1.f:1548-1550); the helper (habu1.f:3646-3652) is hidden from the selector's spelling lookup (THROW-ENTRY, select.f:1190-1196; VISIBLE-RECORD?, dict.f:45-52).

Acceptance: the division guard is `cbnz xdiv,+2; bl (DIV-ZERO); sdiv`; DIV-INSNS is 3; a load-time check in emit.f refuses by name when the maximum of INSNS-OF differs from INSN-PER-OP; NDICT:HELPER-TARGET ( ptr u8 n -- n ) resolves a sealed helper by name only inside engine text. test/compiler/native-div-refusal.f gains: caught E-DIV-ZERO from compiled `/` and `mod` at tier 1 with resumption; `MIN-N -1 /` is MIN-N; the guard span read through src/habu/xref.f is exactly cbnz, bl, sdiv; a word of fourteen discarded divisions over two locals compiles and answers (the report shows it refusing with E-A64EMIT-CAP on the unchanged engine); a stripped executable built by tools/hb-build.f whose MAIN catches a compiled zero divide prints -6400. Artifact: the suite output, the stripped image, the guarded-division census row before and after (−8 bytes per site, no helper bytes), tools/engine-size.f before and after, gen 2 == gen 3.

Files: src/compiler/native/emit.f, select.f, dict.f, test/compiler/native-div-refusal.f, a stripped fixture source under test/compiler/.

Verify: tools/native-build.f product; bin/hb --load test/compiler/native-div-refusal.f; census; tools/engine-size.f; tools/two-generation-build.f; bin/hb --load test/run.f. No engine text; no seed mirror.

Depends: habu-count-the-arm64-e13ae0a3 for the census row only; code is independent.

Ownership: the files above; serialized with habu-drop-the-link-f2071a86, habu-select-mask-literals-f082dbf3, habu-select-shifted-idx-c57b5f1b, habu-publish-and-use-db11d4c1, habu-pool-data-addresses-d65bdc94, habu-report-certified-returns-2ddb20af on select.f/emit.f.

Claim: agent=heron workspace=.jj-ws/arm-div
