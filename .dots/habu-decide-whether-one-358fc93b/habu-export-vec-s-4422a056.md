---
title: "Export VEC's count and index converters"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T21:01:18.381963+02:00"
closed-at: "2026-10-26T21:39:27.423288+02:00"
close-reason: "Landed: Habu c4657b44, Etch 631c6c72, zen 151f4147"
---

Joel 2026-10-26 (interview): make VEC:N>ITEM ( n -- NUM:item-count, E-VEC-CAPACITY ) and VEC:N>INDEX ( n -- NUM:index, E-VEC-BOUNDS ) public in src/lib/vector.f (private today at :226-227), and delete the identical private copies: Habu src/lib/source-lex.f:101-116 and src/lib/word-docs.f:79-94, Etch src/kicad_read.f (from fdbce183), zen src/document/cfb.f:107-126 (bca6a811). One Habu commit, then one Etch and one zen commit using the public words. Queued behind the Etch root-cause lane (it owns Etch now) and the two-lane cap. Claim: jerry.
