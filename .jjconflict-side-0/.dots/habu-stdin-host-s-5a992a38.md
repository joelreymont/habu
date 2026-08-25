---
title: "stdin host's prelude band is declared empty and is not"
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T22:33:05.649998+02:00"
---

Found by bake-chain-7 (2026-08-15) during the wid rebase: src/habu/stdin.f CAPTURE-REPL passes the window's own start to AOT-CAPTURE:PRELUDE-MARK with the claim 'this host compiles only what the target's prefix carries' - FALSE. The metabuild host's pre-window dictionary holds the whole emitter closure (habu2.f, aot-capture.f, aot-arm.f, aot-file.f...), none of which the target engine has. A REPL-window word calling a host-only word (measured: a burn loop calling AOT-ARM:WIDN) bakes a name no target defines; the failure surfaces as exit 81 (boot BL-range assertion, enormous displacement of the unresolved callee) instead of a named capture-time refusal. The band audit cannot catch this class for the stdin host BECAUSE the marks are set to the window start. Fix direction: the host should declare its REAL prelude (marks taken before the closure loads, i.e. at process start in the assembled build source) so the existing two-sided band audit fires; prove with a fixture that a REPL word calling a host-only emitter word is refused BY NAME at capture. Related: the pre-window bitmap host-numbering is ef47ad69's ground, separate.
