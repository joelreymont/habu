\ The production interpreter reports an unknown imported package.
require src/lib/test/eval.f
package PROGRAM-DIAG-USING
s" using NOPE-PKG" TEST-EVAL:RC drop
;package

s" ok" type cr
