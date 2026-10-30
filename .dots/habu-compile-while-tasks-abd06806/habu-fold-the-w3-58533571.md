---
title: Fold the W3 review into the W^X stack
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T02:05:37.894193+02:00"
---

Problem: the W3 review (REVISE, code sound) found stale text and one dead frame left by 'Delete the region write-protection flips' (ulpwprur 20b12154): src/lib/task.f:12-14 states a false live-task reason (the code band flipped in PROT-PAGE-MAX units); flip, window and band comments in src/habu/habu2.f (:3849, :5499, :8155, :10476, :12854, :14135), xref.f:145, definers.f:503, rt.f:31, stack-abi.f:31, src/compiler/native/compiler.f:1241, jit.f:74, test/compiler/tier0-string-literals.f:15 and :127, test/outer-interpret.f:1660, test/tier.f:894, test/match-factor-pin.f:67; habu2.f:2290-2298 LBCHAIN keeps a dead x30 frame and a stale clobber comment, :3683 SLOT-NOP overstates its clobbers; five habu2.f definitions (RECORD, EM-COMPILE-FLUSH-PEND, C-IDENTITY, C-PACKAGE-EXISTING-PRIVATE, EM-COMPILE-TAIL) break card section 2's locals-line rule; src/habu/kernel-x64.f comments name deleted habu1.f words (lane 4's, recorded in carl-wx/.out/wx-facts.md). Acceptance: every site states a fact true on the tree or is deleted; the LBCHAIN frame is gone when the body makes no call; the fold-in is squashed into ulpwprur; the rebuilt engine passes test/code-window.f, src/lib/process-fork-test.f and the clobber-lint row. Files: the files above. Verify: those three rows on the rebuilt engine. Depends: none. Ownership: the W^X stack in carl-wx. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-wx.

Continuation (carl, 2026-10-30): the fold-in is squashed into W3 ulpwprur (310c9dd0 after the fork fixes' squashes into W1 and W3L). Engine from the W3 tip (carl-wx/.out/e3/hb): test/code-window.f, src/lib/process-fork-test.f, tools/lint/clobber-lint.f (737 calls, clean), tools/lint/clobber-lint-test.f and tools/hb-build-stripped-test.f rc 0. Stays active until W4 lands the stack.
