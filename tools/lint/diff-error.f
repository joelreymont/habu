\ diff-error.f - shared unified-diff tooling error codes.
\ Owns the tool-local unified-diff error block -7400..-7499. The shared
\ parser (tools/lint/diff.f) and the framed-artifact codecs consume these
\ names so a single home keeps the codes collision-free.

-7400 constant E-DIFF-SYNTAX
-7401 constant E-DIFF-FRAME-CAP
\ A framed section's raw body must replay through the shared parser as exactly
\ one file diff. Zero, or more than one, parser section event is rejected.
-7402 constant E-DIFF-FRAME-SECTIONS
\ The single parser section's old/new side presence or exact path bytes must
\ agree with the section's declared side fields.
-7403 constant E-DIFF-FRAME-IDENTITY
\ The declared status must agree with the parser's form family and with the
\ side-presence matrix that status implies (e.g. "modified" needs both sides).
-7404 constant E-DIFF-FRAME-STATUS
