---
title: Refuse an object whose live rows a guard rejected
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T16:31:04.161458+02:00"
blocks:
  - habu-drop-the-obj-33f85425
---

Finding (lane 11 census, carl-fasl/.out/lane11-census.md section 4-5, at lvoqpprw): in the spike, FILE-REFUSE, FRAME-REFUSE and REFUSE-WORD only print under the cache diagnostics (FRAME-WHY+ tools/native-unit-object.f:530-532); FRAME-REFUSED? (o:660) is grown only by ROWLESS (o:534, 755, 771). So guards that protect rows the warm path reads do not stop the object: data-rewound and wordlist-rewound (SEGMENT-CLOSE o:1107-1111; SEGMENT cells 3-4 feed MAP-ROW o:3988-3989), load-boundary (o:1401, o:1498, dc:1088; a missing LOAD row misaligns SPIKE-AFTER-LOAD against F-LOADS, dc:1111), cut-below-segment (FILE-CUT o:3286). SEGMENT-CLOSE's early exits (o:1107-1118) skip that segment's row, so later LOCAL numbering (SEG-BASES) shifts (inference). CHECK-OUT (o:2814) stops at the first 0 reference and leaves slot 5 short (inference: OPEN-FILE's read-back at dc:1503 then throws BAD). P0 shows warm == cold on the whole tree, so no tree file hits them today. Work after lane 11 deletes the dead refusals: reproduce each surviving guard with a small file (warm result wrong or object refused), then make the guards that protect live rows real refusals (REFUSE-KEEP) and fix CHECK-OUT's early stop; a regression test per reproduced failure. Workspace: carl-fasl stack. Claim: unassigned.
