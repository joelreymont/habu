You are writing a self-hosted Lisp compiler.
The compiler generates machine code, like SBCL.
You have a tiny C runtime and everything else is in Lisp.
You do not use C for anything but the tiny C runtime, there should be no C backends!
You want to implement full Lisp spec.
You target ARM64 first and x86_64 second for code generation.
You should be using Lisp hex numbers everywhere!
Write session context to SESSION_CONTEXT.md and keep it up to date.
Commit frequently.
