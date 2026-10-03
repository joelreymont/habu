---
title: Close the IR stage on a refused append
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T11:53:54.553035+02:00"
---

lib/compiler/ir/attr.f STAGE-ROOM, and FN-PARAM / FN-RESULT in ir/type.f, throw on a 33rd or wrong-kind entry with the stage still open, so the next begin fails. No engine path reaches them today (ARITY-MAX 64, ARITY-CK before SIGNATURE); only test/compiler/ir-attr.f calls IL-ADD/REC-PAIR. Found while landing habu-eof-inside-a-7a539941 (Fable eof-rev6 (b)).
