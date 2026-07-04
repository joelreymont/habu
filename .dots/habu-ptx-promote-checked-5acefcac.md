---
title: "PTX: promote checked CUDA driver to lib/ptx"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T17:55:56.922340+02:00"
---

From the zed-WIP retire analysis 2026-07-04. maki/cuda-driver.f's typed FFI: bindings (nominal cuda-dev/ctx/mod/fn/devptr handle roles) are the reusable checked CUDA driver, but they live in the app layer while ~9 tools/ptx launchers (bench.f, cuda-launch.f, softmax-launch.f, sum-launch.f, gradcheck.f, acc/redadd/matmul-device-test.f, softmax-gradcheck.f) hand-roll raw DLSYM + per-file RC0/SYM/die plumbing - tools/ptx cannot dedupe against maki without inverting layering. Move the typed bindings + fail-closed CUDA-HANDLE0/CUDA-RC0 (maki/cuda-types.f) into lib/ptx/cuda-driver.f; add typed convenience helpers (LOAD-MODULE/DEVICE-ALLOC/HTOD/DTOH/PARAM-*) with nominal types; make maki/cuda-driver.f + cuda-types.f thin re-exports. Migrate the launchers off raw DLSYM, keeping PTXTC (assemble) + PTXSENT (sentinel) integration and per-tool goldens; fold in bench.f die->named-throw cleanup. Do NOT reuse the retired zed WIP's generic CALLn-RC design (drops nominal typing; its -3412 codes collide with E-PTX-EMIT). Combine with habu-maki-hyphenated-cuda (rename to CU-DEVICE-GET style during the move - one migration pass, not two). Gates: maki/test.f, test/run.f, ptx-stdlib slice; zed device re-verify every migrated launcher.
