---
title: Stale statements the decisions census met
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:35:39.709334+02:00"
---

Problem (each found 2026-11-11 on master a947f29a): docs/forth.md:657-664 still states the PKG-FAMILY:tail rule (dot habu-define-generated-family-9061852e open); docs/design.md:190-193 still describes a constructor namespace (dot habu-drop-the-constructor-cc2dfaaa closed); prot-wid-add is still in habu1.f:4777,:5240, prims.f:986, kernel-x64.f:4094, layout.f:697 with both its dots closed; E-BAD-NAME, approved 2026-11, is nowhere in src, docs or tools; dot habu-stream-a-cmd-a9e146a3 line 9 names the callback signature that pull replaced; src/habu/intern.f:1-7 says strings are shared 'within a segment' where the ruling is 'Every string is stored once and shared; a link writes each string an image uses once and points every use at it' ('all strings should be interned'); INTEL.md:110 cites AGENTS.md for workspace, review and push rules it does not state (x86-64: at its step). Acceptance: each is corrected, or traced to the open dot that covers it and listed there. Files: as named. Verify: rg for each name. Depends: none. Ownership: unassigned. Claim: unassigned.
