---
title: "A NEWTYPE's type argument meets another by value"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T22:44:22.897538+02:00"
---

Problem: after NEWTYPE ph<a>, : G3 ( ph<u8> -- ph<n> ) ; loads (bin/hb --load of 'NEWTYPE ph<a>' / ': G3 ( ph<u8> -- ph<n> ) ;' rc 0), so a ph<u8> passes as a ph<n>. A NEWTYPE has no payload, so its argument names a type and nothing reads it, but its slots keep the default kind PK-CELL (src/core/type-family.f TFAM-PK-RESERVE :649). PARAM-PAIR-SLOT (src/core/checker.f:4300) pairs a slot exactly only when it names a scope, a region or a phantom element type (TFAM-TYPE-SLOT-XT: PK-TYPE and not read by the family's width), and pairs a PK-CELL argument as a stack integer (CON-OK?: u8 widens to n). Giving a NEWTYPE's slots PK-TYPE would bring it under the exact rule. Acceptance: a NEWTYPE's argument pairs exactly (G3 refused, ph<u8> into ph<u8> admitted); a STRUCTURE field's type argument still meets by value (bx<u8> into bx<n> loads). Depends: none. Waiting: no consumer declares a NEWTYPE with parameters (rg in zen, tender, maki: 0); not a lane until one does or the release work reaches it. Claim: unassigned, owner carl.
