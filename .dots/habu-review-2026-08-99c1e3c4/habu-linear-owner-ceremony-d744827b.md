---
title: linear-owner ceremony built on TRUSTED identity casts
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.006518+02:00"
---

Problem: 99 TRUSTED: sites in 39 maki files; nine name the same unfinished capability habu-checker-ptr-lifetime-f59d1e9d: maki/gpu-session.f:28-40 (4 casts), gpu-buffer.f:27-29,42, kv-cache.f:94-106, gpt2-model.f:116-194 (M-SAVE/M-TAKE over 34 raw cells, ~80 lines), safetensors.f:339-362 (10 leaves); every public GPT2: word does 'M-TAKE {: 21 locals :} ... M-SAVE' and 48 typed-local-lint allow-bare-local escapes in 9 files carry the multi-cell config through them. The 'proof' is the TRUSTED: line. Acceptance: one shared typed record definer replacing the per-package mint/take (~300 lines), or plain STRUCTURE handles with the linear claim dropped where it is only asserted; the escapes removed; the capability dot referenced with a probe. Files: maki/gpu-session.f, gpu-buffer.f, kv-cache.f, infer/gpt2-model.f, infer/safetensors.f. Verify: maki/test.f. Depends: habu-checker-ptr-lifetime-f59d1e9d for the final form. Ownership: maki ownership model. Claim: unassigned.
