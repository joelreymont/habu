---
title: Bind the colon-definition frame
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T20:46:39.416805+02:00"
blocks:
  - habu-bind-checker-env-ed4f9f87
---

Full context: src/compiler/native/elaborate.f (package NELAB) elaborates one colon definition off a sealed source tape, and three facts about that definition are still stated by its caller rather than read from the frozen checker environment. First, the two frame words are found by spelling: NELAB:COLON compares token 0 against the bytes ':' and looks for the closing ';' by the same byte comparison, so a program that spells its definition frame differently cannot be elaborated even though the immediate table would classify it. Second, how many values the word takes and leaves are arguments to NELAB:COLON; the checker's accepted stack effect is the authority, and section 7.2 requires the elaborated operations to correspond to it. Third, the function's visibility is fixed to exported, but whether a definition is visible outside its package is the package system's fact. Acceptance: the frame identities, the declared arity and the visibility all come from the frozen checker/environment manifest; a definition whose recorded effect disagrees with the body is refused against that manifest rather than against a caller-supplied number; the elaborator holds no spelling of its own. Dependency: the frozen checker environment manifest.
