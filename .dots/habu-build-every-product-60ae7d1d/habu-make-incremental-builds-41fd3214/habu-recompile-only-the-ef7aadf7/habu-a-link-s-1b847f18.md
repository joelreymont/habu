---
title: "A link's refusal names the last token read, not the name that collided"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T20:09:29.387458+02:00"
---

Problem: when INTERPRETER:DEF-RECORD (src/habu/interpreter.f:1620) refuses a record a link makes, PKG-FAIL (:864) prints 'duplicate definition: ' and the reader's last token, which at a link is whatever the requester read last ('h.f', 'HB-BUILD-CLI:HBB-MAIN' in ~/.cache/tmp/carl-d6/master-v3/warm.log), not the colliding name, 'at' the file being linked. A link reaches the refusal only from an object that disagrees with the dictionary (src/habu/linker.f LINK-DEF :4453, MAKE-DEF :4383). Acceptance: a refusal raised while a part links names the record being made and the part's object, or the link turns it into its own refusal that does. Depends: none. Claim: unassigned.
