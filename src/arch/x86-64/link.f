\ x86-64 emissions use rel32 calls and movabs address literals.
require src/lib/le.f
require src/core/bytes.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f

package LINKER
private
package X64
private

: SPAN? ( n n n -- bool )
   {: size:n at:n width:n :}
   at 0 >= width size <= and at size width - <= and ;

: ADDRESS? ( ^u8 n n n -- bool )
   {: b:^ size:n at:n kind:n :}
   kind NEMIT:ADDR-CODE = kind NEMIT:ADDR-DATA = or
   size at 10 SPAN? and 0= if false exit then
   b at + {: p:^ :}
   p c@ $FE and $48 = p 1+ c@ $F8 and $B8 = and ;

: REL32 ( n -- n )
   dup -2147483648 < over 2147483647 > or if
      s" x64link: rel32 target is out of reach" 74 die
   then ;

public

: CALL! ( n n n ^u8 -- )
   {: to:n kind:n at:n p:^ :}
   kind NEMIT:CALL = if $E8 else $E9 then p c!
   to at - 5 - REL32 p 1+ LE:U32! ;

: CODE-ADDRESS! ( n n ^u8 -- )
   {: to:n at:n p:^ :}
   to p 2 + LE:U64! ;

: ADDRESS@ ( ^u8 n n n -- n bool )
   {: b:^ size:n at:n kind:n :}
   b size at kind ADDRESS? 0= if 0 false exit then
   b at + 2 + LE:U64@ true ;

: ADDRESS! ( n ^u8 n n n -- bool )
   {: v:n b:^ size:n at:n kind:n :}
   b size at kind ADDRESS? 0= if false exit then
   v b at + 2 + LE:U64! true ;

: PATCH ( n ^u8 n [ n -- n ] [ n -- n ] -- )
   {: e:n dst:^ at:n data resolve :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: s:n :}
      e i NSHADOW:CALL-TARGET@ resolve execute
      e i NSHADOW:CALL-KIND@ at s + dst s + CALL!
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE@ {: s:n :}
      e i NSHADOW:ADDR-SITE-KIND@ {: kind:n :}
      dst e NSHADOW:SIZE s kind ADDRESS@ 0= if
         s" x64link: an address site is not movabs" 74 die
      then
      kind NEMIT:ADDR-DATA = if data else resolve then execute
      dst s + 2 + LE:U64!
   loop ;

;package
;package
