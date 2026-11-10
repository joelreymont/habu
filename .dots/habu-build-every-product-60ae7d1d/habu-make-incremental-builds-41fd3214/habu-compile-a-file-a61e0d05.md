---
title: "Compile a file again when an earlier file's cell it read changed"
status: active
priority: 1
issue-type: task
created-at: "2026-11-10T07:02:03.704200+02:00"
---

Problem: a file whose top level reads a cell an earlier file owns and stores a new value there (src/lib/error.f ROW, links a row to HEAD's row and makes HEAD its own; FFI's FN-N and table stores follow the same pattern) is linked from its object when a third file, loaded between them and not referred to, changes what that cell holds: the object replays the store without checking what the cell held (src/habu/linker.f STORE-CK), and the dependency rule counts only references and spelled names (linker.f:500-504). Predicted by the layout review (2026-11-10), by reading: z.f and b.f each declare an ERROR:CODE, x.f between them gains one, the warm build links b.f, x.f's row drops off the chain and ERROR:NAME$ answers empty for its code; warm differs from cold. Acceptance: that fixture's warm build equals a cold build of the edited tree, in bytes and in what it prints; the same for a plain user chain. Files: src/habu/linker.f (the STORES rows and FRESH?). Verify: the fixture in .jj-ws.noindex/carl-rowdep/.out/fx, cold, edit, warm, cold, cmp before LC_CODE_SIGNATURE. Depends: none; the object's rows are being reshaped by the timestamp lanes, so the fix lands on their format. Ownership: carl. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-rowdep (root-cause hunt).
