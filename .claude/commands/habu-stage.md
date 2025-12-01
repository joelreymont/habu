# Habu Stage Command

Build and test self-compilation stages for bootstrapping verification.

## Arguments
- `$ARGUMENTS` - Stage number (1, 2, 3) or "verify" to check fixed point

## Workflow

### Stage 1: SBCL compiles Habu sources to native
1. Load all bootstrap/*.lisp files with SBCL
2. Compile the full Habu compiler source to `/tmp/habu_stage1`
3. Verify it runs: `/tmp/habu_stage1 --version` or simple test

### Stage 2: Stage 1 compiles Habu sources to native
1. Run Stage 1 binary to compile Habu sources
2. Output to `/tmp/habu_stage2`
3. Compare binary sizes between Stage 1 and Stage 2

### Stage 3: Stage 2 compiles Habu sources to native
1. Run Stage 2 binary to compile Habu sources
2. Output to `/tmp/habu_stage3`
3. Compare with Stage 2 (should be identical for fixed point)

### Verify: Check fixed point
1. Build Stage 2 and Stage 3
2. Compare binaries byte-by-byte
3. Report differences or confirm fixed point

## Output Format

```
HABU STAGE $N BUILD
===================

Source Files:
  bootstrap/reader.lisp      (5.2 KB)
  bootstrap/compiler.lisp    (42 KB)
  bootstrap/codegen.lisp     (38 KB)
  bootstrap/macho-utils.lisp (12 KB)

Building Stage $N...
  Compiler: [Stage N-1 or SBCL]
  Output:   /tmp/habu_stageN
  Time:     2.3s

Result:
  Binary size: 67,584 bytes
  Functions:   42
  Exit code:   0 (success)

Verification:
  /tmp/habu_stageN --eval "(sys-exit 42)" -> exit 42 (OK)
```

## Fixed Point Verification

```
FIXED POINT CHECK
=================

Stage 2: 67584 bytes, sha256: abc123...
Stage 3: 67584 bytes, sha256: abc123...

Result: FIXED POINT ACHIEVED
```

## Example Usage
```
/habu-stage 1        ; Build Stage 1 with SBCL
/habu-stage 2        ; Build Stage 2 with Stage 1
/habu-stage verify   ; Check if Stage 2 == Stage 3
```

## Important Notes

- Stage 1 is the initial bootstrap from SBCL
- Stage 2 is compiled by native Habu (Stage 1)
- Stage 3 should equal Stage 2 (fixed point)
- Differences indicate compiler bugs or non-determinism
