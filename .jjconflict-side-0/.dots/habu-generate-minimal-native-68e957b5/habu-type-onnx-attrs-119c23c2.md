---
title: Type ONNX attributes
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:24:38.187238+02:00"
---

maki/onnx/graph.f:80-103 and :352-411 parses attributes through raw OGA-KIND=-1 plus independent OGA-F/OGA-F? and OGA-I/OGA-I? pairs. OGA-COMMIT checks five named kinds, then silently treats every other nonnegative integer as axis. Float/int/perm payloads can coexist or mismatch the kind until runtime, and OGN-FIND/OGIC-FIND return bespoke (n bool) results whose payload remains readable on absence. Declare a payload ENUM attribute with exactly alpha(f32-bits), beta(f32-bits), trans-a(bool), trans-b(bool), axis(n), and perm(typed bounded values) variants. Parse into option<attribute>, reject duplicate/conflicting protobuf payload fields while constructing the variant, and commit through exhaustive MATCH with no default. Give interned name slots and int-constant slots distinct nominal types; return option<name-slot>/option<int-constant-id> from lookups. Preserve protobuf field skipping, supported attribute semantics/defaults, malformed-input errors, and imported graph identity. Add checker negatives for name/constant/index and payload/kind swaps; mutation cases cover unknown kind, missing/wrong/duplicate payload, all variants, packed/unpacked perm, and absence/presence. Measure JIT/DATA/CODELEN and parse throughput before/after. Files: maki/onnx/graph.f, import.f adapters and focused tests. Verify ONNX parser/import/model suites, Maki, typed-local diff, type/package/host/dot lints, and full native gate. Ownership: attribute parse state and typed lookup results only; table STRUCTURE migration is separate.
