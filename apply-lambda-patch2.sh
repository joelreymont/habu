#!/bin/bash
# Apply LAMBDA and FUNCALL compilation and codegen to habu0.lisp

cat > /tmp/patch_habu2.py << 'PYTHON_SCRIPT'
import re

with open('habu0.lisp', 'r') as f:
    content = f.read()

# 1. Add LAMBDA and FUNCALL cases in h0-compile before the default case
lambda_funcall_compile = """         ;; LAMBDA - create closure
         ((sym= op "LAMBDA")
          (let* ((params (cadr expr))
                 (body (caddr expr))
                 (free-vars (h0-find-free-vars body params env))
                 (free-offsets (h0-get-free-offsets free-vars env))
                 (param-env (h0-make-param-env params free-vars))
                 (body-ir (h0-compile body param-env fenv)))
            (list (ir-tag-lambda) params body-ir free-vars free-offsets)))
         ;; FUNCALL - call function value
         ((sym= op "FUNCALL")
          (let* ((fn-ir (h0-compile (cadr expr) env fenv))
                 (args (cddr expr))
                 (args-ir (h0-compile-args args env fenv)))
            (list (ir-tag-funcall) fn-ir args-ir)))
"""

# Find the default case in h0-compile
default_case_pattern = r"(\s+);; Default - unknown operator - CRASH\n(\s+)\(t \(fatal-error-ir \"h0-compile: Unknown operator\"\)\)\)\)\)\)"
content = re.sub(
    default_case_pattern,
    lambda_funcall_compile + r"\1;; Default - unknown operator - CRASH\n\2(t (fatal-error-ir \"h0-compile: Unknown operator\")))))",
    content
)

# 2. Add codegen stubs before the default case in h0-codegen
lambda_funcall_codegen = """
    ;; LAMBDA: Create closure (stub - needs lambda lifting)
    ((h0-has-tag-n ir (ir-tag-lambda))
     (let* ((params (cadr ir))
            (body-ir (caddr ir))
            (free-vars (cadddr ir))
            (free-offsets (nth #x4 ir)))
       ;; Stub: Full implementation requires lambda lifting
       (fatal-error "h0-codegen: LAMBDA not yet implemented")))

    ;; FUNCALL: Call closure (stub - needs calling convention)
    ((h0-has-tag-n ir (ir-tag-funcall))
     (let* ((fn-ir (cadr ir))
            (args-ir (caddr ir)))
       ;; Stub: Full implementation requires calling convention
       (fatal-error "h0-codegen: FUNCALL not yet implemented")))
"""

# Find the default case in h0-codegen
codegen_default_pattern = r"(\s+);; Default - CRASH: unknown IR tag\n(\s+)\(t \(fatal-error \"h0-codegen: Unknown IR tag\"\)\)\)"
content = re.sub(
    codegen_default_pattern,
    lambda_funcall_codegen + r"\1;; Default - CRASH: unknown IR tag\n\2(t (fatal-error \"h0-codegen: Unknown IR tag\"))",
    content
)

with open('habu0.lisp', 'w') as f:
    f.write(content)

print("Step 2: Added LAMBDA/FUNCALL compilation and codegen")
PYTHON_SCRIPT

python3 /tmp/patch_habu2.py

echo "Patch step 2 applied successfully. Changes:"
echo "1. Added LAMBDA and FUNCALL compilation cases in h0-compile"
echo "2. Added LAMBDA and FUNCALL codegen stub cases in h0-codegen"
