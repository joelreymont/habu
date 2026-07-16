\ diff-path.f - checked repository-path validation for diff artifacts.

require tools/lint/diff.f

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
