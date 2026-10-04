---
title: End each boot diagnostic with its newline, not a pad byte
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T19:08:13.918336+02:00"
---

Problem (lane 363 r4-gfstdin): src/habu/habu2.f:660-676 emits each fixed boot diagnostic as 's" msg" BYTES,  NL-KW 1 BYTES,'; BYTES, pads every call to 4 bytes (src/arch/arm64/icode.f:570-575 BYTES-PAD), so a message whose length is not a multiple of 4 gets a NUL between its text and the newline, and the *-MSG-LEN write prints the NUL and never the newline. Measured in bin/hb: snapshot format version unsupported, snapshot call map mismatch, snapshot address map mismatch, snapshot address cell out of range, source prefix buffer full, cannot read source, code region out of BL range, transfer immediate out of range (cell kind mismatch likely); tests match with CONTAINS? (tools/build-fixpoint-test.f:565) so they miss it. Acceptance: each message emitted as one contiguous S\" ...\n" BYTES, (as LPREFMISSMSG at :638), every fixed boot diagnostic ends with LF and contains no NUL (a test asserts exact bytes for one through the real path, e.g. source prefix buffer full), rebuild, g1 == g2, two-gen; the x86-64 kernel's equivalents checked. Files: src/habu/habu2.f, src/habu/kernel-x64.f.
