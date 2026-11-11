---
title: "Keep a declaration refusal's place in its repair packet"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:29:30.074272+02:00"
---

Problem (dot c9fe6f17 census, 'found on the way'): tools/repair-packet.f drops line, column and bytes from an E-BAD-DECLARATION packet that carries them (cases ~/.cache/tmp/carl-chkf/c2/). Acceptance: the repair packet of a placed E-BAD-DECLARATION keeps its line, column and bytes; one placed and one unplaced case asserted where repair packets are tested. Files: tools/repair-packet.f, tools/repair-packet-core.f. Verify: bin/hb --load tools/repair-packet.f on c09's --verify-only packet shows the place. Depends: none. Ownership: tools/repair-packet*.f. Claim: unassigned.
