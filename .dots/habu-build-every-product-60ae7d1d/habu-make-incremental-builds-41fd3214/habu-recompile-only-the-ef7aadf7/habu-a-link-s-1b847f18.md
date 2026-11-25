---
title: "A link's rejection names the last token read, not the name that collided"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T20:09:29.387458+02:00"
---

Problem: when INTERPRETER:DEF-RECORD (src/habu/interpreter.f:1618) rejects a record a link makes, PKG-DUPLICATE (:788), handed TOKEN$ (:1636), prints 'duplicate definition: ' and the reader's last token, which at a link is whatever the requester read last ('h.f', 'HB-BUILD-CLI:HBB-MAIN' in a warm build's log), not the colliding name, 'at' the file being linked. A link reaches the rejection only from an object that disagrees with the dictionary (src/habu/linker.f LINK-DEF :4821, MAKE-DEF :4741). Acceptance: a rejection raised while a part links names the record being made and the part's object, or the link turns it into its own rejection that does. Depends: none. Claim: unassigned.
