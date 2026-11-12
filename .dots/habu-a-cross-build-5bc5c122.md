---
title: A cross-build refuses the code address a callback row holds
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T09:05:34.279481+02:00"
---

Problem: with ROW-GROW and GROW fixed (commit 6fca1734 in .jj-ws.noindex/carl-cb), a program with one CALLBACK: still fails --target linux-aarch64 on macOS: 'linker: the cell at space +4239328 in EMIT +928 holds CMP-ENTRY +12, which an object cannot name', E-COMP-STATE (-8570), src/habu/linker.f REFUSED (:1486). Inferred, not traced: the callback row's entry cell (src/lib/ffi-callback.f INSTALL-ENTRY :181-183, GROW :104-107) holds a code address inside the host's emission, which the cross linker cannot turn into the target's. The same program builds natively. Logs ~/.cache/tmp/carl-cb/build-*.log. Acceptance: the program of test/stripped-callback-subject.f builds for linux-aarch64 and sorts on Linux arm64 as natively. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-cb (hunt)
