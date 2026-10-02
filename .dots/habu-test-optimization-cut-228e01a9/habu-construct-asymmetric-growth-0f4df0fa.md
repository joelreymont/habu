---
title: Construct asymmetric-growth SUM variants by signed pass-2
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:34:53.871081+03:00"
---

Found by lane 532 (dot 8a8912cc): src/core/type-family.f TFC-XPAD-NARROW-REJECT (~:5355-5361, called ~:5385 and ~:5414) refuses a SUM whose variants grow asymmetrically, because the width-fact certificate and the trusted validator VALIDATE-WF (habu2.f) accept only add-only corrections and the native emitter only adds cells; test/type-ctor-suite.f:668-716 pins the rejection. The owner 4fc2b960 was superseded into habu-campaign-c3-the-a2477c89 with no child that owns it. Acceptance (from 4fc2b960): signed extra-pad facts flow through the certificate and VALIDATE-WF and the emitter can remove cells; the asymmetric fixture constructs and matches with certified width equal to native width for both variants, and depth probes agree; mis-sized bundles still rejected; the five 'until signed pass-2' comments updated; baked: rebuild, g1 == g2, two-generation build.
