\ qname.f - canonical qualified-name grammar.

package QNAME

private

$3A constant COLON

public

0 constant BARE
1 constant QUALIFIED
2 constant MALFORMED

7107 constant E-SYNTAX

: SPLIT ( ptr u8 n -- ptr u8 n ptr u8 n n ) {: a:ptr u:n :}
   -1 0
   BEGIN dup u < WHILE
      a over + c@ COLON = IF
         over 1 + over = >r
         dup 0=
         over u 1 - = or
         r> or IF
            2drop
            a 0 a 0 MALFORMED
            EXIT
         THEN
         nip dup 1 +
      ELSE
         1 +
      THEN
   REPEAT
   drop
   dup 0 < IF
      drop
      a 0 a u BARE
      EXIT
   THEN
   {: split:n :}
   a split
   a split 1 + +
   u split - 1 -
   QUALIFIED ;

;package
