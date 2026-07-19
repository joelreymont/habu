\ diff-error.f - shared unified-diff tooling error codes.
\ Owns the tool-local unified-diff error block -7400..-7499. The shared
\ parser (tools/lint/diff.f) and the framed-artifact codecs consume these
\ names so a single home keeps the codes collision-free.

-7400 constant E-DIFF-SYNTAX
-7401 constant E-DIFF-FRAME-CAP
