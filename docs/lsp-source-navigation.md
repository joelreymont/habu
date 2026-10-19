# Built-in source navigation

The engine records a source-backed word's compiled symbol, live code entry,
source-root-relative path, accepted name, original name span and source digest.
The span and digest identify the binding in verifier packets. When the current
source bytes match that digest, definition uses the saved span, and a declaration
cursor selects the live image symbol only at that exact source token.

For definition and references requests, the LSP reads the current text of the
origin file, preferring an open document. When those bytes differ from the
compiled source, `SOURCE-INDEX` asks the verifier child to scan them in a
candidate scope, outside document checking. It matches a declaration's
canonical package, tail and visibility against the selected live image symbol.
A declaration cursor uses that scan and selects a symbol only when exactly one
live origin in the file has that identity. Moving a declaration therefore moves
its Location without changing what an existing checked use binds to. A
same-spelling call, comment or string is not a declaration.

The scan uses the verifier's multi-error mode. It reads native interpreter
declarations and literals. A dictionary word at top level, including a library
definer or loader, may change bindings; the index does not execute it or infer
its effects. It stops and returns deferred. The child emits accepted
declarations directly from `ON-DEFINITION` events; a refusal does not invent a
definition. Rows already emitted remain partial and unusable until the result
is verified or refused. A missing or ambiguous declaration contributes no
source Location.
After a verified or refused scan, accepted declarations remain usable even when
checker diagnostics refuse other definitions. If the scan throws, the child
retains the original code in its log and returns no complete result; none of
its locations are usable, including declarations reported before the throw. References
already bound to the image symbol remain known and
`includeDeclaration` reports the missing-source warning. Built-in rename still
refuses. Primitive locations use the separate `PRIM-SOURCE` table and validator.
