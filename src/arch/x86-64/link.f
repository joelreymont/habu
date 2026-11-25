split-str
\ x86-64 emissions use rel32 calls and movabs address literals.
require src/core/exit-status.f
require src/lib/le.f
require src/core/bytes.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f

package LINKER
private
package X64
private

: REL32 ( n -- n )
   dup -2147483648 < over 2147483647 > or if
      s" x64link: rel32 target is out of reach" EXIT-STATUS:ENCODE die
   then ;

public

: CALL! ( n n n ^u8 -- )
   {: to:n kind:n at:n p:^ :}
   kind NEMIT:CALL = if $E8 else $E9 then p c!
   to at - 5 - REL32 p 1+ LE:U32! ;

: CODE-ADDRESS! ( n n ^u8 -- )
   {: to:n at:n p:^ :}
   to p 2 + LE:U64! ;

: ADDRESS! ( n ^u8 n n n -- )
   {: v:n b:^ size:n at:n kind:n :}
   v b at + 2 + LE:U64! ;

: PATCH ( n ^u8 n [ n -- n ] [ n -- n ] -- )
   {: e:n dst:^ at:n data resolve :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: s:n :}
      e i NSHADOW:CALL-TARGET@ resolve execute
      e i NSHADOW:CALL-KIND@ at s + dst s + CALL!
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE@ {: s:n :}
      e i NSHADOW:ADDR-SITE-VALUE@
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if data else resolve then execute
      dst s + 2 + LE:U64!
   loop ;

;package
;package
