---
title: Emit the safety programs the engine emits
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T08:20:40.364207+02:00"
---

Found by the wide-load lane, pre-existing and systematic at every width: the chain emits NO tag-domain validation program in front of a layout fetch (the engine emits LP2VEMIT/LP2VEXEC) and NO PROT-SPAN guard for any store - a one-cell enum fetch compiled unvalidated before the wide work. Gaps in emitted SAFETY programs, never in answers. Decide the contract: does the cut require chain-compiled code to carry the engine's runtime guards, or is the checker's static proof the replacement (in which case DOCUMENT that as the contract and delete the engine's guards post-cut for parity)? Either answer is a ruling, not a drift. Files: src/compiler/native/{elaborate,select,emit}.f or docs. Depends: sequencing with the cut.
