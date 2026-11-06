---
title: "C2b4: read a definer's created effect from facts text"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T22:04:00.367927+02:00"
---

Problem: design r2 D2: a create ... does> definer's created effect (checker.f NORET-CREATES@ :16208, NORET.CREATES, read at DO-TOK; CTL-GENERATES marks it as a generates: row's) is a whole effect, so its tag carries a second facts text. Acceptance (brief /tmp/jerry-big/brief-c2b4.md): `creates-effect` is always the last tag and the rest of the text is the created words' facts text; SIG-TAGS stops there and leaves the rest's span in SIG-CREATES-A/U, and the caller parses it with a second PARSE-SIG-RAW after building the first effect, so the parses never nest (PSIG resets SGHASR, PD-* and the letter maps); render.f REND-CREATES writes it; a probe over every symbol with a created effect finds the parsed effect E-CONTENT-SAME? as the stored one, and `creates-effect` with no text after it is refused. Files: src/core/checker.f, src/core/render.f. Verify: the probe; bin/hb --load test/run.f -- engine checker-soundness. Depends: C2b1 (parallel with C2b2). Ownership: those files. Claim: unassigned.
