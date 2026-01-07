---
title: Document JIT and GC architecture in repo
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-06T06:34:23.750541+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

From design analysis: Comprehensive JIT and GC architecture documentation has been provided but needs to be integrated into the repository's docs/ tree.

## Goal

Add the design documents to the repository to guide future JIT and GC development.

## Files to Add

### 1. docs/codegen/HABU_ARM64_JIT_ARCH.md

Content: The JIT architecture design showing how to layer multi-version JIT on the existing ARM64 backend.

Key sections:
- Current ARM64 codegen architecture
- Function versioning design
- Dispatcher strategies (Lisp-level first, ARM64 stub later)
- Specializing recompile path
- Integration with existing IR and runtime

### 2. docs/runtime/HABU_GC_IMPROVEMENTS.md

Content: GC improvement roadmap based on current implementation.

Key sections:
- Current GC architecture analysis
- Configurable heap sizes
- Tunable thresholds
- Write barrier optimization
- Incremental collection design
- Integration with JIT (safepoints, allocation ABI)

## Patch Application

The patch file is available at:
/Users/joel/Downloads/habu-jit-gc-package/habu-docs-jit-gc.patch

Apply with:
```bash
cd /Users/joel/Work/habu
patch -p1 < /Users/joel/Downloads/habu-jit-gc-package/habu-docs-jit-gc.patch
```

Or create files manually with content from the .md files.

## Review Points

1. Ensure paths match repository structure
2. Verify all referenced files exist (arm64/asm.lisp, runtime/gc.c, etc.)
3. Check consistency with existing docs
4. Update ROADMAP.md or ARCHITECTURE.md to reference new docs

## Benefits

- Guides implementation of JIT versioning
- Documents GC improvement path
- Provides rationale for design decisions
- Helps future contributors understand architecture

## Tasks

1. Review patch content
2. Apply patch or create files manually
3. Verify files are in correct locations
4. Add references from other docs (README, ARCHITECTURE, etc.)
5. Commit with descriptive message

## References

- /Users/joel/Downloads/habu-jit-gc-package/habu-docs-jit-gc.patch
- /Users/joel/Downloads/habu-jit-gc-package/Habu_Codegen_and_JIT.md
- /Users/joel/Downloads/habu-jit-gc-package/Habu_GC_Analysis.md
