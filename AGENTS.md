You are writing a self-hosted Lisp compiler.
The compiler generates machine code, like SBCL.
You have a tiny C runtime and everything else is in Lisp.
You do not use C for anything but the tiny C runtime, there should be no C backends!
You want to implement full Lisp spec.
You target ARM64 first and x86_64 second for code generation.
Bootstrapping should be done in Lisp, compiled by SBCL.
You should be using Lisp hex numbers everywhere!
Write session context to CONTEXT.md, save it frequently and keep it up to date.
Use the Lisp tracing facility for debugging.
Commit frequently.

Common Lisp HyperSpec: https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm

Use this git author: Joel Reymont <18791+joelreymont@users.noreply.github.com>

Proactively:

1. Maintain a CONTEXT.md markdown file
2. Update it after each major step
3. Include enough detail that a new session could pick up where we left off

Create a plan, break it down into smal steps.

Execute the full plan without stopping unless you need my input.

Make sure there are no emojis in commit descriptions, code or generated documentation.

There should be one commit per logical feature and its tests. Each commit should have a short and succinct summary. All the "phase completed", "fixed this" and similar commits should be combined with their respective implementation commits.

Make sure tests follow the approach of the existing testing infrastructure. Tests should be as short as possible. You must NEVER use color and NEVER use emoji. Summarise the purpose in a comment at the start of the test add NO further comments unless truly necessary for understanding.

Make sure there’s complete test coverage that follows the style of existing tests in the repo.
You are allowed to say I don’t know and ask for help!

Do not use marketing language. Use technical facts instead of competitive comparisons.

## Code Generation Policy

When adding new ARM64 instructions:
1. Add new intrinsics to `arm64/asm.lisp` in the `:arm64` package
2. Do NOT create new `nc-` prefixed functions in `bootstrap/compiler.lisp`
3. Use existing ARM64 intrinsics from `arm64/asm.lisp` wherever possible
4. The `nc-` functions in `bootstrap/compiler.lisp` are legacy wrappers - prefer direct ARM64 intrinsics for new code
