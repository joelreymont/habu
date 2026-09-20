\ Run by whitebox-engine-key-test.f inside its private invocation root.
require lib/test.f
require test/whitebox-engine.f

package WHITEBOX-MANIFEST-CHILD

create PATH FS-PATH-CAP allot
variable PATH-U

: DIRTY? ( -- bool ) 1 SCRIPT-ARGV$ s" dirty" STR= ;
: KEY ( -- ) 0 SCRIPT-ARGV$ PATH PATH-U WHITEBOX-ENGINE:ENTRY-PATH! ;

: RUN ( -- )
   T-RESET
   0 SCRIPT-ARGV$ DTM:KNOWN? TTRUE
   0 SCRIPT-ARGV$ DISCOVER:RUN
   EVENT-COUNT DIRTY? if 1 else 0 then T=
   [: KEY ;] catch DIRTY? if E-DISC-DYNAMIC else 0 then T=
   T-REPORT ;

RUN
;package
