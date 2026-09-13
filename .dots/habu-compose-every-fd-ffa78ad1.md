---
title: Compose every fd-2 refusal as one run so the pad cannot eat its newline
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T11:09:21.780935+03:00"
---

Problem: src/arch/arm64/icode.f BYTES-PAD rounds every BYTES, run up to a 4-byte boundary because ASM-CP counts instruction words, so an engine refusal emitted as a text run followed by a separate 'NL-KW 1 BYTES,' loses its newline to the pad unless the text length is already a multiple of four, and a message assembled from several runs gets NUL bytes wedged between them (measured 2026-09-13: 'hb: snapshot address table full', 31 bytes, had never printed its newline; 'hb: snapshot call map mismatch', 30 bytes, still does not; about twenty fd-2 messages in src/habu/habu2.f share the shape). The address-table lane repaired only its own message with EM-MSG:BOUND, (one composed run with an executed length agreement). Acceptance: every fd-2 message the emitter writes is composed as one run including its newline (through EM-MSG or a sibling composer), each length constant a bare literal with an executed agreement; a regression reads each message out of a built engine and asserts it ends in exactly one newline with no NUL inside; byte identity and the chain (the image changes, since the messages do). Files: src/habu/habu2.f, src/arch/arm64/icode.f (comment), test/. Verify: the regression, the chain, test/run.f. Depends: habu-size-the-snapshot-1ca5db10. Ownership: hazel. Claim: unassigned.
