\ tail-ratchet.f - exact nested-process and elapsed-time ratchet.

require lib/test.f
require lib/process.f

package TAIL-RATCHET

$800 constant CAP

create SAVED-OUT CAP allot
create SAVED-ERR CAP allot

variable DIRECTS
variable SUBJECTS
variable START-NS
variable SAVED-OUT-U
variable SAVED-ERR-U
variable SAVED-RC

: ELAPSED-MS ( -- n )
   mono-ns START-NS @ - PROC-NS-PER-MS / ;

: SAVE-BYTES ( ptr u8 n ptr u8 ptr n -- )
   {: src:ptr u:n dst:ptr used:ptr :}
   u CAP > if E-STR-CAPACITY throw then
   src dst u BYTE-COPY
   u used ! ;

public

: START ( -- )
   0 DIRECTS !
   0 SUBJECTS !
   mono-ns START-NS ! ;

: DIRECT ( -- )
   DIRECTS @ 1+ DIRECTS ! ;

: SUBJECT ( -- )
   SUBJECTS @ 1+ SUBJECTS ! ;

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

: CHECK ( n n n -- ) {: direct:n subject:n max-ms:n :}
   s" exact direct child-process count" T-LABEL
   DIRECTS @ direct T=
   s" exact subject child-process count" T-LABEL
   SUBJECTS @ subject T=
   s" nested child-process group time" T-LABEL
   ELAPSED-MS max-ms <= TTRUE ;

;package
