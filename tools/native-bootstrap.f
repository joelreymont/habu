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

public

\ BUILD's text runs after this package closes, so it names this word qualified.
: UNKNOWN-ORIGIN ( n n -- n ) 2drop -1 ;

private

\ The entry is named in a text because it resolves only after require returns.
: BUILD ( -- )
   s" tools/native-build-core.f" required
   s" ' NATIVE-BOOTSTRAP:UNKNOWN-ORIGIN true NATIVE-BUILD:RUN" evaluate-closed ;

' BUILD
;package
execute
