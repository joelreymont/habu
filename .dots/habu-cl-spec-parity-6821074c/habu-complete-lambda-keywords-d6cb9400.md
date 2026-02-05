---
title: Complete lambda keywords
status: closed
priority: 3
issue-type: task
created-at: "\"2026-02-05T12:17:03.110853+01:00\""
closed-at: "2026-02-05T22:07:22.116983+01:00"
close-reason: Lambda keyword support and macro environment wiring already landed
---

docs/cl-symbols.md:11-18 mark &allow-other-keys/&environment/&whole partial. Root cause: macro lambda list parsing stores env/whole as nil + doesn't enforce allow-other-keys. Fix: implement real macro env object for macroexpansion paths + bind &whole/&environment in lambda list binder; enforce &allow-other-keys in keyword arg parsing; add tests in src/compiler/compile.zig and REPL path.
