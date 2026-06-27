---
title: "Autograd: higher-order grad (differentiate the backward kernel)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T23:00:02.517920+02:00"
---

PyTorch supports grad-of-grad (create_graph). Maki's AD-REVERSE is source-to-source, so the BACKWARD kernel is itself an ordinary checked kernel - run AD-REVERSE on it to get the second-order pass. Demonstrate on one nonlinear op (e.g. d2(EXP.)/dx2 or MUL): AD-REVERSE(forward) -> backward; AD-REVERSE(backward) -> 2nd-order; gradcheck the Hessian-vector product vs finite differences. Files: lib/ptx/ad.f + ad-test.f. VERIFY: 2nd-order numeric match. This is a place maki's source-to-source approach is structurally CLEANER than a runtime tape. Dep: EPIC; needs habu-ad-thread-saved (saved values) + habu-ad-validate-multi.
