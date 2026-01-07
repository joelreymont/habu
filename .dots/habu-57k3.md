---
title: Implement habu-xref cross-reference system
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-06T12:30:26.811372+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Implement a cross-reference system in the `habu-xref` package, similar to SBCL's sb-introspect.

## Required Functions

### Call tracking
- `(habu-xref:who-calls 'foo)` - functions that call foo
- `(habu-xref:calls-who 'foo)` - functions that foo calls (inverse)

### Variable tracking  
- `(habu-xref:who-binds 'x)` - who binds variable x (let, let*, lambda params)
- `(habu-xref:who-references 'x)` - who reads variable x
- `(habu-xref:who-sets 'x)` - who writes variable x (setq, setf)

### Macro tracking
- `(habu-xref:who-macroexpands 'mac)` - who uses macro mac

### Additional (beyond SBCL)
- `(habu-xref:who-defines 'foo)` - where is foo defined (file, line)
- `(habu-xref:who-exports 'foo)` - which packages export foo
- `(habu-xref:dead-code)` - functions defined but never called

## Existing Infrastructure
- `*all-call-targets*` - tracks call targets for link verification
- `record-call-target` - records a call during compilation
- FASL relocations already track fn-call and extern-call sites

## Implementation Notes
1. Extend compiler to record xref data during compilation
2. Store in a global xref database (hash tables by symbol)
3. Track source locations (file, line, column) - see bead habu-ksj
4. Persist xref data in FASL files for cross-module queries
5. Provide both compile-time and runtime query APIs
