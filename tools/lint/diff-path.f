\ diff-path.f - checked repository-path validation for framed diff artifacts.
\ A framed side path must be non-empty and must not contain a NUL byte; an
\ absent side must carry an empty path. The framed codec calls VALIDATE-SIDE
\ for both sides of every section.

require lib/prelude.f
require tools/lint/diff-error.f

package DIFF-PATH
public

: VALIDATE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-DIFF-SYNTAX throw then
   0 begin dup u < while
      dup a + c@ 0= if E-DIFF-SYNTAX throw then
      1+
   repeat drop ;

: VALIDATE-SIDE ( bool ptr u8 n -- ) {: present:bool a:ptr u:n :}
   present if a u VALIDATE exit then
   u 0<> if E-DIFF-SYNTAX throw then ;

;package
