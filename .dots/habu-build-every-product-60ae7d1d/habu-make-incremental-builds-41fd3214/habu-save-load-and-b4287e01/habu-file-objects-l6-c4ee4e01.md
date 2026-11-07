---
title: "File objects L6: compile after linked files"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T12:21:51.570993+02:00"
blocks:
  - habu-file-objects-l5-c2dba0e3
---

Problem: until the checker's facts are stored on the dictionary entries (DECISIONS 2026-11-05 'where the checker's facts live'; jerry's habu-design-the-checker-b77c228b and its lanes), a file cannot compile after a linked one and the build restarts. Acceptance: with the facts on the records, a stale file compiles after linked ones and the restart transition goes. Design: .out/carl-file/design-fobj.md section 2 item 9, section 5 L6. Files: src/habu/linker.f (or its object file), test/file-objects.f. Verify: edit b.f, only b.f compiles (LINKER:COMPILED 1), program cmp-identical to a cold build. Depends: L5 and jerry's facts lanes. Ownership: carl (lead), worker lane. Claim: unassigned.

Question for its design (from L4): the link imports host copies through NPUB:IMPORT-PENDING and its twins, which take no NHOST facts (host-publish.f TAKE). Whether a compile after a link needs those facts is unknown; if it does, the host copy goes through the placed-NEMIT publication path and the object carries the call rows' impl and loc. Fact (L4 review probe /tmp/carl-rev-l4w/probe.f): a compile in the build's body after a linked file is not E-STALE and fails E-UNDEFINED on the linked file's words (`: PROBE-CALL ( -- ) FILE-OBJECTS-COUNT:BUMP FILE-OBJECTS-COUNT:HI ;` evaluated after test/fixtures/file-objects/count.f links: rc 70, "undefined word 'FILE-OBJECTS-COUNT:BUMP'"; the same compile after count.f compiles runs).
