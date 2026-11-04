---
title: Give T0-arm64 control and loops
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T17:37:34.793002+02:00"
closed-at: "2026-11-02T11:22:38.516471+02:00"
close-reason: "Landed 1a91799192 'Give T0-arm64 control and loops'."
---

L5b2, worker in .jj-ws.noindex/jerry-t0-l1 as a new commit on wmlwzszs. Branch list resolved at CLOSE (targets at an epilogue site land on the epilogue), cbz/b.cond/b, loops in J-FRAME's layout on the DATA loop stack. Proof: .out/t0-dis-a64.f with the control samples ported, x86 bins unchanged.
