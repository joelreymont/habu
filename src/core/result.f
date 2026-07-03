\ result.f - checked result sum API.

package RESULT

$1000 constant ARENA-CAP
$2 constant REC-CELLS
ARENA-CAP REC-CELLS * cells constant ARENA-BYTES
$0 constant MAP-ADDR-ANY
$3 constant MAP-PROT-RW
$1002 constant MAP-PRIVATE-ANON
-$1 constant MAP-ANON-FD
$0 constant MAP-OFF-ZERO
$4C constant RESULT-INTERNAL-RC

variable ARENA-A
variable ARENA-I

: MAP-ARENA-RC ( -- n )
   MAP-ADDR-ANY ARENA-BYTES MAP-PROT-RW MAP-PRIVATE-ANON
   MAP-ANON-FD MAP-OFF-ZERO mmap ;

: ARENA-A-FIELD ( -- ptr ptr a )
   ARENA-A 0 ptr-field ;

: ARENA@ ( -- ptr a )
   ARENA-A-FIELD @ ;

: ARENA! ( ptr a -- )
   ARENA-A-FIELD ! ;

TRUSTED: MAP-ARENA ( -- ptr a )
   MAP-ARENA-RC
   dup 0 < if s" result: arena mmap failed" RESULT-INTERNAL-RC die then ;

: ARENA-ROOM ( -- )
   ARENA-I @ ARENA-CAP >= if s" result: arena full" RESULT-INTERNAL-RC die then ;

: ARENA-REC ( -- ptr a )
   ARENA@ 0= if MAP-ARENA ARENA! then
   ARENA-ROOM
   ARENA@ ARENA-I @ REC-CELLS * cells +
   ARENA-I @ 1 + ARENA-I ! ;

public

TRUSTED: OK ( a -- result<a,b> )
   ARENA-REC >r
   r@ !
   0 r@ cell+ !
   r> ;

TRUSTED: ERR ( b -- result<a,b> )
   ARENA-REC >r
   r@ !
   1 r@ cell+ !
   r> ;

TRUSTED: CASE ( R result<a,b> [ R a -- S ] [ R b -- S ] -- S )
   >r
   swap
   dup cell+ @ if
      @ nip r> execute
   else
      @ swap r> drop execute
   then ;

TRUSTED: MMAP>BYTES ( n -- result<ptr u8,n> )
   dup 0 < if ERR else OK then ;

TRUSTED: MMAP>CELLS ( n -- result<ptr a,n> )
   dup 0 < if ERR else OK then ;

end-package
