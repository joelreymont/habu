\ The OS bridge copies only a complete canonical C string into caller storage.
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f

package REALPATH-TEST

PATH-CAP 2 + constant CAP
create OUT CAP allot
create EXPECT CAP allot
create TARGET PATH-CAP allot
create LINK PATH-CAP allot
variable EXPECT-N
variable TARGET-N
variable LINK-N


: CLEAR ( -- )
   CAP 0 ?do $5A OUT i + c! loop ;


: UNTOUCHED ( -- )
   CAP 0 ?do OUT i + c@ $5A T= loop ;


: CWD ( n -- n )
   s" ." FS:>PATHZ OUT 1+ rot realpath ;


: CAPACITY ( -- )
   CLEAR
   PATH-CAP CWD dup 0 > TTRUE EXPECT-N !
   OUT 1+ EXPECT EXPECT-N @ 1+ BYTE-COPY
   OUT c@ $5A T=
   OUT EXPECT-N @ 1+ + c@ 0 T=
   OUT EXPECT-N @ 2 + + c@ $5A T=
   CLEAR
   EXPECT-N @ 1+ CWD EXPECT-N @ T=
   OUT 1+ EXPECT-N @ EXPECT EXPECT-N @ T$=
   OUT c@ $5A T=
   OUT EXPECT-N @ 2 + + c@ $5A T=
   CLEAR
   EXPECT-N @ CWD -2 T=
   UNTOUCHED
   0 CWD -2 T=
   -1 CWD -2 T=
   UNTOUCHED ;


: SYMLINK ( -- )
   s" habu-realpath" FS:MKTEMP-DIR {: root:ptr rootu:n :}
   root rootu FS:CLEANUP-TREE+
   root rootu s" target.f" TARGET FS:JOIN-PATH TARGET-N !
   root rootu s" alias.f" LINK FS:JOIN-PATH LINK-N !
   TARGET TARGET-N @ s" target" FS:WRITE-ALL
   TARGET TARGET-N @ LINK LINK-N @ FS:MAKE-SYMLINK
   TARGET TARGET-N @ FS:>PATHZ EXPECT CAP realpath dup 0 > TTRUE EXPECT-N !
   LINK LINK-N @ FS:>PATHZ OUT CAP realpath EXPECT-N @ T=
   OUT EXPECT-N @ EXPECT EXPECT-N @ T$=
   LINK LINK-N @ FS:REMOVE-FILE
   CLEAR
   LINK LINK-N @ FS:>PATHZ OUT 1+ PATH-CAP realpath -1 T=
   UNTOUCHED ;


public

: RUN ( -- )
   T-RESET FS:CLEANUP-RESET
   CAPACITY
   SYMLINK
   FS:CLEANUP-RUN
   T-REPORT ;

;package

REALPATH-TEST:RUN
