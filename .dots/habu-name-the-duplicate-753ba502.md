---
title: Name the duplicate a storage definer refuses
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:31:36.463973+02:00"
blocks:
  - habu-a-build-s-87c9c7c2
---

Problem: src/core/layout-buffer.f LBUF-NAME-OK? (:66-:70) throws E-DUP-DEFINITION (78) bare when the name, or a generated suffix name, is already defined, so TYPED-VARIABLE, TYPED-BUFFER, LAYOUT-BUFFER and the heap and arena definers exit 78 with no line at all. Reduced: a file holding only `TYPED-VARIABLE HV n` (HV is an engine global, src/habu/repl.f:20) exits 78 silently on the installed hb, while `variable HV` prints `duplicate definition: HV at file:line`.

Design (lead, after a stopped first lane): layout-buffer.f is a CHECKER boot row (src/habu/boot-files.f), loaded before the kernel's package OUTER exists (outer.f and packages.f load through interpret.f) and two rows before src/core/check-hook.f, so it can reach neither OUTER's THROW-AT nor check-hook.f's private REPORT-DUPLICATE; a first brief that called a new OUTER word failed to build (E-UNDEFINED at layout-buffer.f:68). check-hook.f REPORT-DUPLICATE (:143) with AT-SOURCE, NAME-LINE and BYTE-LINE (:115-:141) already writes what the wall writes for a duplicate only the checker sees, and everything they read is checker.f's (SRC-PATH, SRC-PATHLEN-OFF, SRC-OWNED :22877-:22880, SRCLOC:FILE-BUFFER? :31791, CK-INP-OFF :28812; PEND-OFF and SRC-NAME are data-base offsets). So: those four words and DUP-RC move from check-hook.f into checker.f package CHECK, after the SRCLOC block (:31800), as one public word `CHECK:DUPLICATE ( ptr u8 n -- )` that prints the line and throws $4E, with its row beside SRCLOC FILE-BUFFER?'s (:31797); check-hook.f :295 and LBUF-NAME-OK? both call it. The new row is a checker-owned row of the kind privacy 2c and 2d change, so this waits for 2c to land.

Acceptance: the storage definers print `duplicate definition: NAME at PATH:LINE` and still exit 78; the ordinary definers' line is unchanged. Files: src/core/checker.f, src/core/check-hook.f, src/core/layout-buffer.f, test/gate-dictionary-lib.f. Verify: two cases beside test/gate-dictionary-lib.f:847 (`TYPED-VARIABLE over n`, `4 TYPED-BUFFER over n`) through RUN-BAD-SOURCE expecting $4E and `duplicate definition`; test/gate-dictionary.f and test/load-reject-diag-test.f rc 0 on a twice-built engine. Depends: habu-a-build-s-87c9c7c2 (2c). Ownership: unassigned. Claim: unassigned.
