---
title: A live routine with dead arms is over-framed
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T17:45:38.294476+02:00"
---

A routine with one live path plus a dead arm is compiled CALL-FRAMED (frame + link save) although x30 is intact wherever control actually leaves - the trait means 'contains a Bl' and LINK-KEPT? answers true. NELAB:CALLS-BACK? already answers the finer question and ROUTINE consults it only on the tail branch (noret lane 2026-08-11; such bodies verified compiling and running correctly today - this is bytes, not correctness). Express calls-that-come-back in the contract seam so the frame is bought only by a call control returns through. Files: src/compiler/native/{migrate,abi}.f. Depends: none.
