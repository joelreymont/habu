---
title: Structure ONNX graph tables
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:24:59.348298+02:00"
---

maki/onnx/graph.f:90-229 stores the decoded graph in six hand-correlated table families: name text/length; fourteen node columns; five initializer columns; int-constant name/count/value windows; three graph-input columns; and three graph-output columns. All record ids, name slots, input offsets/counts, rows, columns, payload offsets/lengths, and staged -1 sentinels are n. A wrong column or row id type-checks, and node construction mutates many arrays in place before OND-N commits, making partial staged state observable after a throw. Define distinct nominal ids for names, nodes, initializers, int constants, and input/output rows. Use STRUCTURE node, initializer, int-constant, and shaped-value records with typed fields; retain variable text/input/value arenas behind typed offset/length window fields and one owning table API. Store records in typed layout buffers and construct a complete staged record before one capacity-checked append. Represent absent output/axis/perm and staging with typed option/payload ENUM values, not -1/zero collisions. Accessors accept the correct id and return typed fields; raw protobuf offsets remain only at the wire boundary. Coordinate habu-type-onnx-attrs-119c23c2 for the attribute field. Preserve table capacities, import order, graph identity, shapes, payload spans, unknown-field behavior, errors, and emitted Model IR. Add checker negatives for every id/field/window swap, canary and injected-throw tests proving append atomicity, max-capacity edges, and exact decoded graph snapshots/goldens. Measure source/JIT/DATA/CODELEN, table bytes, and parse/import throughput before/after. Files: maki/onnx/graph.f, import.f and focused tests. Verify ONNX parser/import/model/device suites, Maki, typed-local diff, type/package/host/dot lints, and full native gate. Ownership: decoded flat graph table representation; no recursive graph-by-value design.
