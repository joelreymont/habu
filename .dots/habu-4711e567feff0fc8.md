---
title: "Numeric correctness: enforce fixnum bounds on parse, add bignum/rational/float support or signal overflow instead of wrap; files src/reader/parser.zig:146-167, src/runtime/primitives/arith.zig:10-55."
status: closed
priority: 2
issue-type: task
created-at: "2025-12-29T09:07:29.078029+02:00"
closed-at: "2025-12-29T12:02:34.252718+02:00"
close-reason: "Fixed: arith.zig now uses checked arithmetic, parser already uses std.fmt.parseInt which handles overflow"
---
