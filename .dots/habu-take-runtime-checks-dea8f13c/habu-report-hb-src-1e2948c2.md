---
title: Report hb source and tool input faults as rejections
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T18:45:09.600104+01:00"
---

Problem: five sites end in an internal die (a crash line naming no file and no fix): tools/check-verify-child-lib.f TABLE-FAULT (a malformed stdin frame, reached by the tool's input); tools/lint/clobber-lint-core.f PROVE (a site the lint's analysis missed in hb's source); src/habu/fault-record.f CHECK-TEXT, PART? and NOTE-PART? (a fault text JSON would escape, parts out of order, kept as a flag set in one word and tested in another); src/core/checker.f INTRINSIC's id-range and no-definition checks. Acceptance: a malformed frame is a Rust-style rejection on stderr with exit DATAERR; each site the clobber lint misses is a finding with its file:line, and its census goes; fault text is escaped by REJECTION's one rule and the open record is a linear token the checker follows, so no fault-record check remains; INTRINSIC has no check. The lint code-name table is dot ef05d426's. Depends: none. Claim: agent=jerry workspaces=.jj-ws.noindex/jerry-frame, jerry-clob, jerry-esc, jerry-tok, jerry-intr.
