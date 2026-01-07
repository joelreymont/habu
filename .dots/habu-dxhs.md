---
title: Implement gradual typing with contracts (Typed Racket style)
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-05T14:13:03.728713+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Implement optional/gradual typing inspired by Typed Racket and contract systems:

1. **Contracts at boundaries**: Runtime checks at typed/untyped boundaries
2. **Blame tracking**: When contracts fail, identify which module violated the contract
3. **Gradual migration**: Mix typed and untyped code freely
4. **Higher-order contracts**: Contracts on functions that defer checking to call time

Key forms:
- (define/contract fn contract body) - function with contract
- (-> type1 type2) - function type/contract
- (and/c c1 c2) - contract combinators
- (or/c c1 c2)
- (listof type) - parametric contracts

Benefits over full static typing:
- Incremental adoption
- Works with dynamic features (eval, macros)
- Runtime enforcement catches what static analysis misses

Related: habu-jtlh (static type checking), habu-xnjq (static verification mode)
