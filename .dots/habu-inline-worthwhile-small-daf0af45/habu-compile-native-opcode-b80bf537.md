---
title: Compile native opcode case tables
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T12:23:24.456136+03:00"
closed-at: "2026-10-22T12:36:34.929316+03:00"
close-reason: "Triage 2026-10-22, DONE (STALE-CLAIM): Same evidence: NTH and FROZEN-NTH are compiled in the 10-20 bin/hb.map (src/compiler/ir/arena.f)."
---

Matching combined68a61228 native build through selected-event recovery host fails after8391compiled definitions: NTH throws E-IR-VERIFY-CAP(-8083), preventing a native engine. Reduce the exact committed verifier capacity and required opcode-case shape; fix the responsible compiler capacity boundary without skipping structural checks or weakening publication. Keep a real native-load regression before edits, then build the combined engine and run independent Astra review plus required native/convergence gates.
