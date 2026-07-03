---
title: "Roll out the MK: wordlist namespace across all maki modules"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T19:24:51.274470+02:00"
---

The runtime wordlist-namespace feature (docs/forth.md) lets maki words live in the MK wordlist, isolated from global/habu (a bare reference does not resolve). maki/onnx.f is the worked example (MK:ONNX-LOWER, gate-green). Roll out MK: to the remaining maki modules so every maki application word is MK:-qualified: tensor.f, array.f, optim.f, loss.f, autograd.f, train.f, fusion.f, eval.f + their gpu-train/gpu siblings and tests + cross-file callers (train.f calls optim/loss; gpu-train.f calls them too). Do it module-by-module, leaf-first, updating each module's callers in the same increment, maki gate green each step. Keep E-MK-* error constants cross-cutting (not MK:-qualified). Consider a maki-namespace lint that flags a top-level maki  definition without the MK: qualifier (except documented boundaries). Dep: none (the feature + onnx pattern have landed).

SUPERSEDED (2026-07-04): folded into habu-maki-subsystem-pkgs-4655e01a — maki gets real subsystem packages + maki.f re-export (compiler re-export capability dot habu-compiler-package-re) instead of one flat MK: namespace. Keep the leaf-first module-by-module migration discipline and the namespace-lint idea from this dot.
