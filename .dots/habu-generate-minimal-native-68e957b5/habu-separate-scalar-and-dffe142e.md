---
title: Separate scalar and relocatable literal emission
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-19T19:51:39.428058+02:00\""
blocks:
  - habu-aot-repl-bl-a71440da
---

Measured at master 3909bbac. C-LIT and C-X9-LIT always emit movz plus three movk instructions, even when one 16-bit chunk is sufficient. A constant K=42 is 28 bytes and disassembles as movz 42 followed by three zero movk instructions; minimal emission is 16 bytes, exactly 12 bytes smaller. A one-byte string word is 64 bytes because its length 1 uses the same four-instruction chain; it should be 52 bytes. Empty and two-one-byte-string fixtures are 60 and 108 bytes and should be 48 and 84. Existing LVMOVK and icode LIT64 already synthesize minimal MOVZ/MOVN+MOVK chains. Root cause: C-LIT conflates ordinary scalar values with addresses that AOT relocation later discovers by recognizing the fixed four-instruction shape and testing whether the value falls inside capture-time DATA or CODE ranges. That representation-driven scan both forces bloated scalars and lacks explicit relocation intent. Fix: split scalar-push, raw-scalar, DATA-address, and CODE-address emitters. Use the shared minimal synthesizer for scalars. Record relocation kind and site explicitly when emitting an address; preserve enough patch space or use a proven position-independent address form. Remove value-range/pattern inference from aot-capture.f after every producer is explicit. Serialize after habu-aot-repl-bl-a71440da because both change AOT site records. Acceptance: exact K and string sizes above; zero, positive/negative 16-bit, MOVN-favorable, and multi-chunk scalar disassembly is minimal; DATA/CODE/quotation addresses relocate exactly; a scalar numerically inside an address range is never relocated; malformed or missing relocation metadata fails closed; AOT REPL, snapshot, stripped AOT, bootstrap mirror, fixpoint x2, both targets, full gates, and exact size ratchets pass. Files: src/habu/habu2.f, src/habu/jit.f shared literal synthesis, src/habu/aot-capture.f, bootstrap/cg/forth.fs, literal/string/AOT tests, and size gates.

Claim: agent=scalit workspace=.jj-ws/fable-scalit machine=spark (owns separating scalar vs relocatable literal emission)
