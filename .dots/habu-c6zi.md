---
title: Implement native load for FASL and source
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-05T15:28:59.421488+02:00"
closed-at: "2025-12-25 07:21:12"
close-reason: "Obsolete: Zig rewrite"
---

Implement (load path) that loads FASL or source into the running image.

For FASL:
- Read FASL header, verify compatibility
- Allocate memory for code and data
- Copy code section, apply relocations
- Resolve external symbol references
- Register functions in runtime symbol table
- Execute top-level forms

For source:
- Read and parse
- Compile each form
- Execute

Should auto-detect format by magic bytes or extension.
