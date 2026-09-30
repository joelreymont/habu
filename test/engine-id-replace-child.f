\ The child observes its own cached path before that pathname is replaced.
\ Run through test/engine-id-replace-e2e.f so the executable can be copied safely.
require lib/test.f
require lib/fs-mutate.f
require lib/engine-id.f

package ENGINE-ID-REPLACE-CHILD

create HASH-CTX SHA256-FILE-CTX-BYTES allot
create BEFORE 64 allot
create AFTER 64 allot

: DIGEST ( ptr u8 n ptr u8 -- ) {: path:ptr size:n out:ptr :}
   HASH-CTX path size out SHA256-FILE-HEX-IN 0<> if 79 throw then ;

: UNCHANGED ( -- )
   ENGINE-ID:PATH$ BEFORE DIGEST
   ENGINE-ID:KEY$ BEFORE 64 STR= TTRUE ;

: REPLACED ( -- )
   SCRIPT-ARGC 2 T=
   ENGINE-ID:PATH$ BEFORE DIGEST
   ENGINE-ID:PATH$ 1 SCRIPT-ARGV$ RENAME-FILE
   0 SCRIPT-ARGV$ ENGINE-ID:PATH$ RENAME-FILE
   ENGINE-ID:PATH$ AFTER DIGEST
   BEFORE 64 AFTER 64 STR= 0= TTRUE
   HB-TARGET-MACOS? if
      [: ENGINE-ID:KEY$ 2drop ;] catch E-ENGINE-KEY T=
   else
      ENGINE-ID:KEY$ BEFORE 64 STR= TTRUE
   then ;

public

: RUN ( -- )
   T-RESET
   ENGINE-ID:PATH$ 2drop             \ cache PATH$ before the replacement
   SCRIPT-ARGC 0= if UNCHANGED else REPLACED then
   T-REPORT ;

;package

ENGINE-ID-REPLACE-CHILD:RUN
