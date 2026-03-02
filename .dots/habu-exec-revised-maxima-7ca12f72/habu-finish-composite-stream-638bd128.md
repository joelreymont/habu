---
title: Finish composite-stream no-hang and line input support
status: active
priority: 1
issue-type: task
created-at: "\"2026-03-07T19:20:07.528531+01:00\""
blocks:
  - habu-delegate-composite-stream-9553a1f8
---

src/runtime/primitives/io.zig:1896-1955 and ../maxima/src/commac.lisp. Root cause: readCharNoHang returns nil placeholders and readLine is NotImplemented for composite streams. Fix: implement delegated no-hang/line behavior and add an asksign/file-answer probe. Why: tyi-raw and related reader paths still misbehave after basic char ops land.
