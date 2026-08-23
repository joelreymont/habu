\ tail-parity.f - direct/subject result parity shared by the tail gates.

require lib/test.f

package TAIL-PARITY

$800 constant CAP

create SAVED-OUT CAP allot
create SAVED-ERR CAP allot

variable SAVED-OUT-U
variable SAVED-ERR-U
variable SAVED-RC

: SAVE-BYTES ( ptr u8 n ptr u8 ptr n -- )
   {: src:ptr u:n dst:ptr used:ptr :}
   u CAP > if E-STR-CAPACITY throw then
   src dst u BYTE-COPY
   u used ! ;

public

: SNAPSHOT ( ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n err:ptr erru:n rc:n :}
   out outu SAVED-OUT SAVED-OUT-U SAVE-BYTES
   err erru SAVED-ERR SAVED-ERR-U SAVE-BYTES
   rc SAVED-RC ! ;

: SAME ( ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n err:ptr erru:n rc:n :}
   rc SAVED-RC @ T=
   out outu SAVED-OUT SAVED-OUT-U @ T$=
   err erru SAVED-ERR SAVED-ERR-U @ T$= ;

;package
