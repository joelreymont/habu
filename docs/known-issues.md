# Known Issues

## CRITICAL - REPL Broken (stdlib load failure)

**Status**: BLOCKS interactive testing  
**Tracked as**: habu-fix-lambda-compilation-c9250a2c

### Symptom
REPL fails to load stdlib.habu with "COMPILE FAILED with error.InvalidSyntax: Failed form head: lambda"

### Root Cause
Lambda compilation fails when:
1. Lambda is inside a function call (like `maphash`, `mapcar`)
2. During defun compilation (not at top level)
3. Circular dependencies exist between functions during compilation

### Workarounds Applied
- Removed circular dependency between `maphash` and `hash-table-alist`
- Replaced `maphash (lambda ...)` calls with `dolist` over `hash-table-alist`
- Removed duplicate function definitions

### Current Status
- **Tests pass**: Test suite loads stdlib successfully
- **REPL fails**: Interactive REPL cannot load stdlib
- Different loading mechanisms between test and REPL

### Impact
- Cannot test functions interactively
- Must rely on test suite for validation
- Gap analysis verification limited

### Next Steps
1. Debug difference between test loading and REPL loading
2. Fix lambda compilation in nested contexts
3. OR: Accept test-only validation until lambda bug fixed
