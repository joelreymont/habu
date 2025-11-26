# REPL Consolidation Plan

## Problem

Currently have 4 separate REPL implementations:
- `enhanced-repl.lisp` (32 defuns, 234 lines)
- `programmable-repl.lisp` (37 defuns, 281 lines)
- `recursive-repl.lisp` (40 defuns, 320 lines)
- `extended-recursive-repl.lisp` (43 defuns, 370 lines)

This is confusing and unmaintainable. We should have **ONE** canonical REPL.

## Solution

### Option A: Single REPL (Recommended)

**Step 1: Designate extended-recursive-repl as canonical**
```bash
# Rename to canonical name
mv extended-recursive-repl.lisp habu-repl.lisp

# Update Makefile
# Change all references from "extended" to just "habu-repl"
```

**Step 2: Archive historical REPLs**
```bash
mkdir -p archive/repl-evolution
mv enhanced-repl.lisp archive/repl-evolution/
mv programmable-repl.lisp archive/repl-evolution/
mv recursive-repl.lisp archive/repl-evolution/
```

**Step 3: Update documentation**
- README should mention only `habu-repl`
- Add note about archived REPLs for historical/educational purposes
- Update all references in docs

**Step 4: Simplify Makefile**
- Remove targets for old REPLs
- Keep only `habu-repl` target
- Make it the default REPL

### Option B: Keep as Educational Examples

**Alternative:** Keep all 4 but make it clear they're educational

**Structure:**
```
repl/
  habu-repl.lisp          # Main REPL (= extended)
  examples/
    01-enhanced.lisp      # Tutorial: Basic features
    02-programmable.lisp  # Tutorial: Let and lambda
    03-recursive.lisp     # Tutorial: Defun
    04-extended.lisp      # Tutorial: Advanced features
```

**Makefile:**
- Default: `make repl` → builds `habu-repl`
- Educational: `make repl-examples` → builds all tutorials

## Recommendation: Option A

**Reasons:**
1. Simpler to maintain (one codebase)
2. Less confusion for users
3. Clear what "the REPL" is
4. Historical versions preserved in archive
5. Focus development on single implementation

**Implementation:**
```bash
# 1. Rename main REPL
mv extended-recursive-repl.lisp habu-repl.lisp

# 2. Archive others
mkdir -p archive/repl-evolution
mv enhanced-repl.lisp archive/repl-evolution/01-enhanced-repl.lisp
mv programmable-repl.lisp archive/repl-evolution/02-programmable-repl.lisp
mv recursive-repl.lisp archive/repl-evolution/03-recursive-repl.lisp

# 3. Add README to archive
cat > archive/repl-evolution/README.md << 'EOF'
# REPL Evolution - Historical Implementations

This directory contains the historical progression of the Habu REPL implementation,
preserved for educational and archival purposes.

## Progression

1. **01-enhanced-repl.lisp** (32 defuns, 234 lines)
   - Basic REPL with quote, symbols, arithmetic
   - Type predicates
   - String comparison
   - Reader/parser
   - Basic evaluator

2. **02-programmable-repl.lisp** (37 defuns, 281 lines)
   - Adds let bindings
   - Adds lambda expressions
   - Environment-based evaluation
   - First-class functions

3. **03-recursive-repl.lisp** (40 defuns, 320 lines)
   - Adds defun for function definitions
   - Enables recursive functions
   - Top-level evaluation
   - Function environment

4. **04-extended-repl.lisp** (43 defuns, 370 lines)
   - Adds and, or, not operators
   - Adds cond multi-way conditional
   - Adds <= and >= comparisons
   - Complete feature set

## Current REPL

The current production REPL is `habu-repl.lisp` in the root directory,
which is equivalent to 04-extended-repl.lisp with all features.

## Usage

These files are preserved for:
- Understanding the evolution of the implementation
- Teaching Lisp interpreter construction
- Historical reference
- Comparison of approaches

They are **not** maintained for production use. Use `habu-repl.lisp` instead.
EOF

# 4. Update Makefile (next step)
```

## Makefile Changes

**Current:**
```makefile
habu-enhanced: ...
habu-prog: ...
habu-rec: ...
habu-extended: ...
```

**Proposed:**
```makefile
# Main REPL (default)
habu-repl: runtime/runtime.o runtime/region.o runtime/gc.o runtime/lineedit.o runtime/io.o
	@echo "Building Habu REPL..."
	@sbcl --noinform --non-interactive \
		--eval '(load "bootstrap/c-backend.lisp")' \
		--eval '(habu-compiler::compile-lisp-file "habu-repl.lisp" "habu-repl.c")' 2>&1 | grep -v "^;"
	@echo "Generated C code: habu-repl.c"
	@gcc -std=c11 -Wall -Wextra -Werror -O2 -g -Wno-unused-value -Iruntime \
		-o habu-repl habu-repl.c $^ -lm
	@echo "✓ Habu REPL built ($(shell wc -c < habu-repl | tr -d ' ')KB)"

# Alias for backwards compatibility
repl: habu-repl

# Historical REPLs (educational only, not maintained)
repl-examples: archive/repl-evolution/*.lisp
	@echo "Note: Historical REPLs are archived and not built by default"
	@echo "See archive/repl-evolution/README.md for details"

.PHONY: habu-repl repl repl-examples
```

## Migration Steps

### 1. Backup (safety first)
```bash
git add -A
git commit -m "Snapshot before REPL consolidation"
```

### 2. Execute consolidation
```bash
# Create archive directory
mkdir -p archive/repl-evolution

# Move historical REPLs
mv enhanced-repl.lisp archive/repl-evolution/01-enhanced-repl.lisp
mv programmable-repl.lisp archive/repl-evolution/02-programmable-repl.lisp
mv recursive-repl.lisp archive/repl-evolution/03-recursive-repl.lisp

# Rename main REPL
mv extended-recursive-repl.lisp habu-repl.lisp

# Create archive README (see content above)
cat > archive/repl-evolution/README.md << 'EOF'
(README content from above)
EOF
```

### 3. Update Makefile
- Replace 4 REPL targets with single `habu-repl` target
- Add backwards-compatible `repl` alias
- Update clean target
- Update test targets if needed

### 4. Update documentation
- `README.md` - mention only `habu-repl`
- `REPL_FINAL_STATUS.md` - update to reflect consolidation
- `AUTOMATIC_ROOTING_SUMMARY.md` - update references
- Any other docs mentioning multiple REPLs

### 5. Test
```bash
make clean
make habu-repl
echo "(+ 1 2)" | ./habu-repl
echo "(defun factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))" | ./habu-repl
echo "(factorial 5)" | ./habu-repl
```

### 6. Commit
```bash
git add -A
git commit -m "Consolidate to single canonical REPL

- Rename extended-recursive-repl.lisp → habu-repl.lisp
- Archive historical REPL implementations in archive/repl-evolution/
- Update Makefile to build single habu-repl binary
- Simplify documentation and build process
- Add archive README explaining historical progression

The historical REPLs (enhanced, programmable, recursive) are preserved
for educational purposes but not maintained for production use."
```

## Benefits

1. **Clarity** - One REPL, obvious what to use
2. **Simplicity** - One binary to build and maintain
3. **Focus** - Development energy on one implementation
4. **Preservation** - Historical versions archived, not lost
5. **Documentation** - Easier to document and explain

## Timeline

- **Now**: Review and approve plan
- **Day 1**: Execute consolidation (30 minutes)
- **Day 1**: Update docs (30 minutes)
- **Day 1**: Test (15 minutes)
- **Day 1**: Commit and move forward

## Post-Consolidation

**Focus on:**
- Enhancing `habu-repl` with more features
- Integration with bootstrap compiler
- Path to self-hosting
- Single canonical Lisp implementation

**Stop maintaining:**
- Multiple REPL versions
- Separate binaries for each feature level
- Complex build system for variants

---

**Decision required:** Approve Option A (consolidation) or Option B (educational examples)?

**Recommendation:** Option A - consolidate immediately, move forward with single REPL.
