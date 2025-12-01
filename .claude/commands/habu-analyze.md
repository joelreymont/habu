# Habu Analyze Error Command

Structured analysis of compiler or runtime errors before attempting fixes.

## Arguments
- `$ARGUMENTS` - Error description, crash info, or "last" to analyze the last failed operation

## Workflow

This command implements STRUCTURED REASONING - think before acting.

1. **Gather Information**
   - What operation failed? (compile, run, test)
   - What was the exact error message?
   - What was the input that caused it?
   - Is there a binary to inspect?

2. **Form Hypotheses** (THINK block)
   Generate 2-4 possible causes:
   - Hypothesis 1: <description> - Likelihood: <H/M/L>
   - Hypothesis 2: <description> - Likelihood: <H/M/L>
   - etc.

3. **Test Hypotheses**
   For each hypothesis (highest likelihood first):
   - What minimal test would confirm/refute it?
   - Run the test
   - Record result

4. **Root Cause Determination**
   - Which hypothesis was confirmed?
   - What is the exact root cause?
   - Where in the code is the bug?

5. **Fix Design**
   - What is the minimal fix?
   - What side effects might it have?
   - What tests verify the fix?

## Output Format

```
ERROR SUMMARY:
  Type: <compile-time/runtime/crash>
  Operation: <what was attempted>
  Message: <error text>

HYPOTHESES:
  1. [HIGH] <cause 1>
  2. [MED] <cause 2>
  3. [LOW] <cause 3>

INVESTIGATION:
  Testing hypothesis 1...
    Test: <minimal test>
    Result: <confirmed/refuted>
  Testing hypothesis 2...
    ...

ROOT CAUSE:
  Location: <file:line or function>
  Issue: <precise description>
  Why: <explanation of mechanism>

RECOMMENDED FIX:
  <Specific code change>

VERIFICATION:
  <Test to run after fix>
```

## Key Principle
**DO NOT** attempt fixes until root cause is identified.
If unclear, ask for more information rather than guessing.

## Example Usage
```
/habu-analyze "SIGSEGV at PC 0x100004528, x0=-1"
/habu-analyze "Test test_closures.lisp failing with exit 139"
/habu-analyze last
```
