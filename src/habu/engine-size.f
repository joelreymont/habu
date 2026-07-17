\ engine-size.f - exact emitted-engine region measurements.
\ Load after src/arch/arm64/icode.f.

package ENGINE-SIZE

$80 constant CAP
74 constant CAP-RC

create NAME-A CAP cells allot
create NAME-U CAP cells allot
create END-OFF CAP cells allot
variable N

: SLOT ( n ptr a -- ptr a )
   {: idx:n base:ptr :}
   base idx cells + ;

: VALIDATE ( n -- n )
   {: idx:n :}
   idx 0 < idx N @ >= or if s" engine-size: row out of range" CAP-RC die then
   idx ;

: ROOM ( -- )
   N @ CAP >= if s" engine-size: row capacity" CAP-RC die then ;

: PREV-END@ ( n -- n )
   {: idx:n :}
   idx 0= if 0 exit then
   idx 1- END-OFF SLOT @ ;

public

: RESET ( -- )
   0 N ! ;

: MARK ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   ROOM
   name N @ NAME-A SLOT !
   nameu N @ NAME-U SLOT !
   ASM-LEN N @ END-OFF SLOT !
   N @ 1+ N ! ;

: COUNT ( -- n )
   N @ ;

: NAME$ ( n -- ptr u8 n )
   VALIDATE {: idx:n :}
   idx NAME-A SLOT @
   idx NAME-U SLOT @ ;

: END@ ( n -- n )
   VALIDATE END-OFF SLOT @ ;

: BYTES@ ( n -- n )
   VALIDATE {: idx:n :}
   idx END-OFF SLOT @ idx PREV-END@ - ;

: REPORT ( -- )
   0 begin dup N @ < while
      dup NAME$ type $20 emit
      dup BYTES@ .
      1+
   repeat drop ;

;package
