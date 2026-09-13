\ Private transition from an older optimizer with no provenance journal.
\ The output remains explicitly unknown until its own tracked rebuild succeeds.
1 set-tier
require src/habu/prefix-rewind.f
PREFIX-REWIND:TO-CORE
require src/compiler/native/compiler.f

package NATIVE-BOOTSTRAP
private

TRUSTED: INSTALL ( -- )
   ['] NCOMP:COMPILE data-base NCOMP-DISPATCH:XT-CELL + xt! ;
INSTALL

: UNKNOWN-ORIGIN ( n n -- n ) 2drop -1 ;

TRUSTED: BUILD ( -- )
   s" tools/native-build-core.f" required
   ['] UNKNOWN-ORIGIN 0 0= s" NATIVE-BUILD:RUN" evaluate ;

' BUILD
;package
execute
