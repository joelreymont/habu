# Habu Build and Test Command

Build and test a Habu program with comprehensive error reporting.

## Arguments
- `$ARGUMENTS` - Lisp source code string or path to .lisp file

## Workflow

1. **Build Phase**
   - If argument looks like a file path (contains `/` or ends in `.lisp`), read it
   - Otherwise treat as inline source code
   - Compile using `deliver-v3` to `/tmp/habu_test_$$`
   - Capture any compilation errors

2. **Test Phase** (if build succeeds)
   - Run the binary
   - Capture exit code and any output
   - Report: `PASS` (exit 0-127 expected), `CRASH` (exit >= 128)

3. **Error Analysis** (if test fails)
   - For crashes (SIGSEGV=139, SIGKILL=137, etc.):
     - Run `lldb` to get crash location
     - Read `.map` file to identify function
     - Disassemble around crash address
   - Report semantic information: function name, offset, likely cause

## Output Format

```
BUILD: [OK/FAIL]
  Source: <source description>
  Binary: <path>
  Size: <bytes>

TEST: [PASS/FAIL/CRASH]
  Exit code: <code>
  Signal: <if applicable>

ANALYSIS: <if crash>
  PC: <address>
  Function: <name from map>
  Offset: <offset within function>
  Disassembly: <key instructions>
```

## Example Usage
```
/habu-build-test "(defun f (x) (* x 2)) (sys-exit (f 21))"
/habu-build-test tests/test_closures.lisp
```
