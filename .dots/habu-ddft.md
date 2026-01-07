---
title: Implement compile-file for separate compilation
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-05T15:28:59.111317+02:00"
closed-at: "2025-12-25 07:21:40"
close-reason: "Obsolete: Zig rewrite"
---

Implement (compile-file source-path &key output-file) that compiles a source file to FASL.

Should:
- Parse source file
- Compile all forms to IR
- Generate ARM64 code
- Write FASL with relocations for external references
- Track dependencies on other modules

Does NOT link - produces relocatable FASL that load will process.
