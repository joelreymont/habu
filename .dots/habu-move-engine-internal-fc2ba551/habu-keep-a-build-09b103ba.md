---
title: Reject a build whose caught failed text left words or data
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T20:19:25.247025+01:00"
---

Problem: once habu-a-caught-failed-9bb3e339 keeps a caught failed text's storage, a build has no sign of it (src/habu/linker.f END-CK tests DPHW-CELL, which nothing then raises) and would write the text's retired records and data into the image. END-CK's and GOING's rejections name only the build's file, never the line that ran the failed text or requested the failed load.
Acceptance: in a build, a caught failed text that left records above its frame's mark, moved DP from the frame's, or left held facts above the frame's held mark is rejected E-FAILED with one record at the line that ran it: the caught code, the loaded files whose words it retired, and help (let the failure end the build, or keep a text that can fail from defining, allotting or undefining). A caught failed load is rejected at the line that requested it. A caught failed text that left nothing still links (test/fixtures/file-objects/rollback-none.f). DEFINE-CASE and NONES expect the rejection. The frame's HELD-CUT goes. END-CK's data-space rejection keeps its text for a negative allot or a caught rejected declaration. The parent habu-move-the-interpreter-d35780fa's case on the caught code at END-CK is cut. Lands with habu-a-caught-failed-9bb3e339.
Depends: habu-a-caught-failed-9bb3e339.
Claim: unassigned.
