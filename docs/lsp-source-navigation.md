# Built-in source navigation

The engine records a source-backed word's compiled symbol, live code entry,
source-root-relative path, accepted name and original name span. That span is
the binding locator in verifier packets. It is not a current editor location.
Origin rows retain no source file bytes, source hash or file timestamp.

For definition and references requests, the LSP reads the current text of the
origin file, preferring an open document. `SOURCE-INDEX` asks the verifier
child to scan those bytes in a candidate scope, outside document checking. It
matches a declaration's canonical package, tail and visibility against the
selected live image symbol. A declaration cursor uses the same scan and selects
a symbol only when exactly one live origin in that file has that identity.
Moving a declaration therefore moves its Location without changing what an
existing checked use binds to. A same-spelling call, comment or string is not a
declaration.

The scan uses the verifier's multi-error mode and accepts `ON-DEFINITION`
events from the checker. Colon definitions and `does>` clauses must certify;
trusted declarations must finish their body and shape processing. A refusal
does not invent a definition. After a completed scan, accepted declarations
remain usable even when checker diagnostics refuse other definitions.

Trusted `does>` definitions defer the whole index: their body scanner skips
the clause body and cannot establish its acceptance. This bounded unsupported
case is tracked by `habu-locate-origins-across-3fd3d72d`. Plain trusted
definitions remain asserted and locatable.

Dictionary calls, including loaders, `undefine` and replaceable library
definers, defer the whole index: their execution could change any binding.
Structural native declarations and literal openers can still be scanned.
Consequently an unchanged prefix file containing top-level dictionary calls
has no source locations, including declarations before the call. This includes
`sumtype.f`, `layout-buffer.f`, `structures.f`, `util.f` and `xref.f`. The same
limitation applies when an edit turns a declaration into a call. Supporting
these cases without retained source hashes is tracked by
`habu-locate-origins-across-3fd3d72d`; the index does not model native calls.

A missing or ambiguous declaration contributes no source Location. A deferred
or thrown scan contributes none either, including declarations reported before
the stop. A thrown scan retains its original code in the child log and returns
no complete result. References already bound to the image symbol remain known, and
`includeDeclaration` reports the missing-source warning. Built-in rename still
refuses. Primitive locations use the separate `PRIM-SOURCE` table and validator.
