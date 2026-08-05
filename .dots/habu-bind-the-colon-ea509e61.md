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

Progress (habu-reconcile-the-produced-26737779, 2026-07-31): the FIRST of the three is done. NELAB holds no spelling of its own any more - the frame words were not merely spelled differently by some other compiler, they are absent from every produced tape, so the elaborator now reads the definition frame off the recorded parser modes (the name is the one row consumed while interpreting) and ends the body at the tape's end. E-NELAB-IMMEDIATE is retired with the frame check and NELAB no longer requires immediate.f at all (follow-up habu-give-the-immediate-73cb0a49). The declared arity and the visibility are UNCHANGED: both are still stated by NELAB:COLON's caller, at one seam, and the concrete route for the arity half - the checker's CHECKER-TAPE:DONE event, where SGIN/SGOUT still hold the verified declared rows and ROW-CELLS answers their widths - is written up in habu-bind-the-checker-b553d480.
