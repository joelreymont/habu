---
title: "A NEWTYPE's type argument meets another by value"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T22:44:22.897538+02:00"
---

Problem: after NEWTYPE ph<a>, : G3 ( ph<u8> -- ph<n> ) ; loads (probe ~/.cache/tmp/carl-vwidth/lead/w3.f, installed engine), so a ph<u8> passes as a ph<n>. A NEWTYPE has no payload, so its argument names a type and nothing reads it, but its slots keep the default kind PK-CELL (src/core/type-family.f TFAM-PK-RESERVE :702), and PARAM-PAIR-SLOT (src/core/checker.f :3884) pairs a PK-CELL argument as a stack integer (CON-OK?: u8 widens to n). The width lane's fix (brief .out/briefs/fix-width-exact.md) pairs a slot exactly when it names a scope, a region or a phantom element type (TFAM-TYPE-SLOT-XT: PK-TYPE and not read by the family's width), so a NEWTYPE slot kept at PK-CELL still meets by value; giving a NEWTYPE's slots PK-TYPE would bring it under that rule. Acceptance: a NEWTYPE's argument pairs exactly (G3 refused, ph<u8> into ph<u8> admitted); a STRUCTURE field's type argument still meets by value (bx<u8> into bx<n> loads). Files: src/core/type-family.f or the NEWTYPE declarer, src/core/checker.f, a test beside the NEWTYPE cases, docs/forth.md NEWTYPE bullet (:523). Verify: G3 refused on the new engine and loads on the one before; full suite. Depends: the width-exact fix landed (habu-a-type-param-679d7eed lane 2). Waiting: no consumer declares a NEWTYPE with parameters (rg in zen, tender, maki: 0); not a lane until one does or the release work reaches it. Ownership: carl. Claim: unassigned.
