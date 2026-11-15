---
title: Refuse a cast that makes an enum value
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T05:42:02.201109+01:00"
---

Problem: docs/design.md:225-227 says construction from a raw tag goes only through an enum's derived TAG> and that there is no unchecked enum cast, but CAST: admits ( n -- enum ). Reproducer on the installed hb (tree of 2026-11-15 05:33, master ec69b348): 'package CPY public ENUM shade DERIVE eq dark light ;ENUM ;package package CPZ CAST: N>SHADE ( n -- CPY:shade ) public : GO ( -- ) 7 N>SHADE drop ;package CPZ:GO' loads and runs rc 0, making a shade whose tag is 7, and the same cast inside the enum's own package does too. A MATCH on such a value has no arm for it. Acceptance: CAST: refuses an enum (ENUM or SUMTYPE family) in an introduction position, from any package, the owner's included, with a named refusal saying to use the enum's DERIVE tag TAG>; a rejected-program case for the owner and one for another package; projections out of an enum (enum -- n) still admitted; the engine's own casts into an enum move to TAG> in the same change, and each consumer repository that casts into one gets a dot at its owner. Depends: none. Claim: unassigned.
