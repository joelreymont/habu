# macOS Executable Generation - Solution Summary

## Problem

Manually constructed Mach-O executables were being killed by macOS with SIGKILL (exit code 137) before execution, despite having:
- Perfect binary structure (all load commands correct)
- Correct ARM64 machine code
- All required segments (__PAGEZERO, __TEXT, etc.)
- Proper header flags matching SBCL

The issue: `codesign -s -` (ad-hoc signing) failed with "main executable failed strict validation"

## Root Cause

macOS requires the `LC_CODE_SIGNATURE` load command with an embedded signature blob containing:
- CodeDirectory structure
- SHA256 hashes of all code pages
- Valid signature format (even for ad-hoc signatures)

Manually implementing this is extremely complex and requires deep knowledge of Apple's undocumented code signature format.

## Solution: Use the System Linker

Instead of manually constructing Mach-O executables, we use Apple's standard toolchain:

```
Machine Code → Assembly (.s) → Object File (.o) → Executable
                  ^as              ^ld (adds LC_CODE_SIGNATURE)
```

### Implementation

```lisp
(defun generate-executable-via-linker (code &key (arch :arm64) (output-file "a.out"))
  "Generate executable using system toolchain (as + ld)"
  ;; 1. Convert machine code to .s file with .byte directives
  ;; 2. Run: as -arch arm64 -o file.o file.s
  ;; 3. Run: ld -o file file.o -lSystem -syslibroot SDK -e _main -arch arm64
  ;; 4. ld automatically adds LC_CODE_SIGNATURE
  )
```

### Results

```bash
# Compile a simple expression
$ sbcl --load compiler.lisp --load macho-generator.lisp
* (in-package :habu-compiler)
* (compile-to-executable '(+ 2 3) :output-file "hello")

Generating executable via system linker...
  ✓ Code signature present

# Run it
$ ./hello
$ echo $?
80  # Success! (80 = 5 << 4, tagged fixnum)

# Verify code signature
$ codesign -dvvv hello
Signature=adhoc
flags=0x2(adhoc)

$ otool -l hello | grep -A 3 "LC_CODE_SIGNATURE"
      cmd LC_CODE_SIGNATURE
  cmdsize 16
  dataoff 16560
 datasize 280
```

## Why This Works

The system linker (`ld`) is part of Apple's official toolchain:
- Has full access to Apple's code signing APIs
- Automatically generates proper LC_CODE_SIGNATURE
- Creates valid CodeDirectory with SHA256 hashes
- Produces ad-hoc signed binaries that macOS trusts
- No Apple Developer certificate required
- No notarization required

**This is exactly how SBCL, clang, gcc, and every other compiler on macOS works.**

## Key Changes to Codebase

### 1. New Function: `generate-executable-via-linker`
- Location: `bootstrap/macho-generator.lisp`
- Converts machine code to assembly
- Uses `as` and `ld` to produce signed executable

### 2. Updated: `compile-to-executable`
- Now uses linker approach by default
- Added `:use-linker` parameter (default `t`)
- Changed default arch from `:x86_64` to `:arm64`

### 3. Documentation
- `docs/ARM64_STATUS.md` - Complete status and solution
- `docs/MACOS_EXECUTABLE_SOLUTION.md` - This document

## Comparison: Manual vs Linker Approach

| Aspect | Manual Mach-O | System Linker |
|--------|---------------|---------------|
| Complexity | Very High | Low |
| Code Signing | Must implement ourselves | Automatic |
| Maintenance | Fragile, breaks with macOS updates | Stable, uses Apple APIs |
| Compatibility | Requires deep Mach-O knowledge | Works like all compilers |
| Result | SIGKILL (exit 137) | Works perfectly |

## What We Learned

1. **Don't fight the platform** - Use the standard toolchain instead of reinventing it
2. **Code signing is complex** - Apple's format has CodeDirectory, SuperBlob, SHA256 hashes, etc.
3. **The linker knows best** - It has privileged access to signing APIs we don't
4. **SBCL's secret** - It also uses the system linker (not manual Mach-O construction)

## Impact on Phase 2

With working ARM64 executable generation, we can now:
- Continue Phase 2 implementation (inline heap allocation)
- Test standalone executables directly on macOS
- No need to test on Linux or use Docker containers
- Full native Apple Silicon support

## References

- [Mach-O File Format](https://github.com/aidansteele/osx-abi-macho-file-format-reference)
- `/usr/include/mach-o/loader.h` - Mach-O structures
- `man ld` - System linker documentation
- `man as` - Assembler documentation
- `man codesign` - Code signing tool

---

**Date**: 2025-11-19
**Status**: ✅ Resolved
**Platform**: macOS 15.0 (Sequoia) on Apple Silicon
**Compiler**: Habu Lisp (Phase 2)
