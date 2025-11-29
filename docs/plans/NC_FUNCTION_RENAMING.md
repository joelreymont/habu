# NC-* Function Renaming Plan

## Status
138 nc-* functions in bootstrap/compiler.lisp

## Safe to Rename (135 functions)
Remove `nc-` prefix from ALL except these 3 that clash with CL:
- nc-append → KEEP (clashes with cl:append)
- nc-compile → KEEP (clashes with cl:compile)  
- nc-read → KEEP (clashes with cl:read)

## Examples of Safe Renames
```
nc-add-imm → add-imm
nc-movz → movz
nc-codegen → codegen
nc-build-fnoffs → build-fnoffs
nc-compile-expr → compile-expr
nc-lift-lambdas → lift-lambdas
```

## Implementation Strategy

Use sed for systematic renaming:

```bash
# Function definitions
sed -i 's/defun nc-\([a-z-]*\)/defun \1/g' bootstrap/compiler.lisp

# Function calls (word boundaries to avoid partial matches)
sed -i 's/\bnc-\([a-z-]*\)\b/\1/g' bootstrap/compiler.lisp

# Fix the 3 that should keep nc- prefix
sed -i 's/\b\(append\|compile\|read\)\b/nc-\1/g' bootstrap/compiler.lisp
```

## Verification

After renaming:
1. Run test suite
2. Verify no undefined functions
3. Test compilation still works
4. Commit with clear message

## Benefits

- Cleaner code
- Easier to read  
- Standard naming convention
- Only use prefixes where necessary (name clashes)

## Next Steps

1. Backup current compiler.lisp
2. Run systematic renaming
3. Test thoroughly
4. Commit if successful
