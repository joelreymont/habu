---
title: Let a quotation body hold a control structure
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T19:06:58.686093+02:00"
---

Found pre-existing by the exceptions lane (6ceb7667, reproduced on the PARENT binary through the pre-catch route - a body handed to a callee declaring a quotation argument): a quotation body holding ANY control structure refuses E-IR-VERIFY-SUCCARG. Bounds the catch production shape alongside the scope ceiling. Files: src/compiler/native (quotation build path - find the owner; the verifier is naming a successor-argument invariant the QBUILD walk does not maintain for branching bodies). Depends: none technical; sibling of habu-compile-a-quotation-7efa798e.
