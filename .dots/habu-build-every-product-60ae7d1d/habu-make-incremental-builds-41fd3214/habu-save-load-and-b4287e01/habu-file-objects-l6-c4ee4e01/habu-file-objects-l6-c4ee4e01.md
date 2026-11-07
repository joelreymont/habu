---
title: "File objects L6: compile after linked files"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T12:21:51.570993+02:00"
blocks:
  - habu-file-objects-l5-c2dba0e3
  - habu-c5a-carry-a-a58a2538
---

Problem: until the checker's facts are stored on the dictionary entries (DECISIONS 2026-11-05 'where the checker's facts live'; jerry's habu-design-the-checker-b77c228b and its lanes), a file cannot compile after a linked one, so L5 compiles every file of a build once one is stale. Acceptance: with the facts on the records, a stale file compiles after linked ones and L5's whole-build modes go. Design: .out/carl-file/design-fobj.md section 2 item 9, section 5 L6. Files: src/habu/linker.f (or its object file), test/file-objects.f. Verify: edit b.f, only b.f compiles (LINKER:COMPILED 1), program cmp-identical to a cold build. Depends: L5 and jerry's facts lanes. Ownership: carl (lead), worker lane. Claim: unassigned.

Question for its design (from L4): the link imports host copies through NPUB:IMPORT-PENDING and its twins, which take no NHOST facts (host-publish.f TAKE). Whether a compile after a link needs those facts is unknown; if it does, the host copy goes through the placed-NEMIT publication path and the object carries the call rows' impl and loc. Fact (L4 review probe /tmp/carl-rev-l4w/probe.f): a compile in the build's body after a linked file is not E-STALE and fails E-UNDEFINED on the linked file's words (`: PROBE-CALL ( -- ) FILE-OBJECTS-COUNT:BUMP FILE-OBJECTS-COUNT:HI ;` evaluated after test/fixtures/file-objects/count.f links: rc 70, "undefined word 'FILE-OBJECTS-COUNT:BUMP'"; the same compile after count.f compiles runs).

From L5 (lead's decision 2026-11-07, .out/carl-file/design-l5.md "L6 then"): L6 removes L5's mode, load-list walk, `<out>/loads` and the E-STALE residual, and replaces L5's prefix (every earlier file's key and source sum, too strict) with design-fobj.md section 2 item 6's dependencies: by request (L2e request rows), by reference (FILES rows), by spelled name, laid strings not already in the table, the wordlist counter and loader state at the file's start (the counter waits for S5c: ffi-abi.f:1313 loads only through it). Two commits: drop the machinery and keep the prefix, then the finer dependencies. Acceptance case carried from L4: a string or package an earlier, recompiled file now lays makes a linked file's INTERN:ADOPT or OUTER:PACKAGE-RECORD refuse mid-link, and a value T0 inlined from an earlier file leaves no reference in the object; both must count as stale under the finer dependencies (L5-T's `share` case and probe /tmp/carl-rev-l4w/probe-c.f).

From the F1 lanes (2026-11-07): a throw out of a fresh object's check (FRESH?) or link (LINK-FILE) in linking mode ends the build E-FAILED, but WRITE-STARTED still writes the full load list (mode linking), so the next build walks the same list, finds the same headers fresh and fails the same way, every build, until the out root is deleted. FRESH? mirroring every link refusal (F1, F3) closes the known shapes, but any future mismatch between the check and the link has the same effect. Acceptance for L6's replacement of the list: a file whose object's check or link throws loses its object before the build fails, so the next build compiles it. No natural trigger is left once F1 lands; the test needs one (a crafted object FRESH? does not refuse, or the design names another).
