# Habu IR Command

Inspect intermediate representation (IR) from Habu compilation.

## Arguments
- `$ARGUMENTS` - Lisp source code or file path to compile and show IR

## Workflow

1. **Parse Input**
   - If argument is a file path, read the file
   - If argument is source code, use directly

2. **Compile to IR**
   - Load compiler-sbcl.lisp, compiler.lisp
   - Call `read-all` to parse
   - Call `compile-forms` to get IR and defuns

3. **Display IR Structure**
   - Show main IR expression tree
   - List all defun definitions with their IR
   - Show lambda lifts if any
   - Display function environments

## Output Format

```
HABU IR DUMP
============

Source: (defun fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))

DEFUNS:
  FACT:
    Params: (N)
    Free vars: ()
    IR: (if-ir
          (binop-ir = (var-ir N 0) (lit 0))
          (lit 1)
          (binop-ir *
            (var-ir N 0)
            (call-fn FACT ((binop-ir - (var-ir N 0) (lit 1))))))

MAIN IR:
  (sys-exit-ir (call-fn FACT ((lit 5))))

FUNCTION OFFSETS:
  FACT: 0 bytes from main end
```

## Use Cases

1. **Debug compilation issues** - See how source transforms to IR
2. **Understand optimization** - Compare IR before/after passes
3. **Verify closure capture** - Check free variable detection
4. **Trace call patterns** - See how function calls are structured

## Example Usage
```
/habu-ir "(defun f (x) (* x 2)) (f 21)"
/habu-ir tests/test_closures.lisp
```
