# Habu Debug Command

Debug a failing Habu binary with structured analysis.

## Arguments
- `$ARGUMENTS` - Path to a Habu binary that crashes or behaves unexpectedly

## Workflow

1. **Binary Validation**
   - Check file exists and is executable
   - Run `otool -h` to verify Mach-O format
   - Check for `.map` file (same path + ".map")

2. **Initial Run**
   - Execute binary and capture exit code
   - If normal exit, report and done
   - If crash, proceed to analysis

3. **Crash Analysis** (for SIGSEGV/SIGBUS/etc.)
   - Run lldb with:
     ```
     process handle SIGSEGV --stop true
     run
     bt
     register read pc lr x0 x26 x28
     ```
   - Parse PC to get crash address
   - Look up in .map file for function

4. **Disassembly**
   - Disassemble 20 instructions around crash point
   - Identify the faulting instruction
   - Check register values for bad pointers

5. **Semantic Analysis**
   THINK: Before reporting, analyze:
   - What function crashed?
   - What was it trying to do (load/store/call)?
   - What register contains the bad value?
   - Common causes:
     - x0 = -1: Invalid cons cell (cdr of non-cons)
     - x28 bad: Heap corruption
     - Stack overflow: Deep recursion
     - Bad PC: Jump to invalid address

## Output Format

```
BINARY: <path>
STATUS: CRASH (signal <sig>)

LOCATION:
  PC: <address>
  Function: <name> + <offset>
  Instruction: <asm>

REGISTERS:
  x0: <value> (<interpretation>)
  lr: <value> (return to <function>)
  x26: <value> (code base)
  x28: <value> (heap pointer)

DISASSEMBLY:
  <context around crash>

ANALYSIS:
  <Structured reasoning about cause>

SUGGESTED FIX:
  <Based on analysis>
```

## Example Usage
```
/habu-debug /tmp/crash_binary
/habu-debug tests/test_out
```
