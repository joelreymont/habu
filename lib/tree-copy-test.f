\ tree-copy-test.f - lib/tree-copy.f refuses a name outside the checkout. Every
\ tool and test that copies a tree passes checkout names, so only this file
\ meets the refusal. The working directory is the checkout.

require lib/errors.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require src/core/include.f
require lib/tree-copy.f

package TREE-COPY-TEST
private

create ROOT FS-PATH-CAP allot   variable ROOT-U
create HERE-F FS-PATH-CAP allot variable HERE-U
create OUT FS-PATH-CAP allot    variable OUT-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: HERE$ ( -- ptr u8 n ) HERE-F HERE-U @ ;

\ A real file outside the checkout on every host, so a missed refusal would
\ copy it; tools/build-fixpoint-test.f copies the same file as a stand-in engine.
: FAR$ ( -- ptr u8 n ) s" /usr/bin/true" ;

\ The path a name would have under the root, joined as JOIN-PATH joins it.
: OUT$ ( ptr u8 n -- ptr u8 n ) ROOT$ 2swap OUT JOIN-PATH OUT-U ! OUT OUT-U @ ;

: OUTSIDE ( -- )
   s" dot-dot name outside the checkout is refused" T-LABEL
   [: s" ../x" ROOT$ TREE-COPY:FILE ;] E-FS-PATH TTHROWSQ
   s" absolute name outside the checkout is refused" T-LABEL
   [: FAR$ ROOT$ TREE-COPY:FILE ;] E-FS-PATH TTHROWSQ
   s" nothing is written under the root" T-LABEL
   FAR$ OUT$ FILE? TFALSE ;

: INSIDE ( -- )
   SOURCE-ROOT:CWD$ s" lib/tree-copy.f" HERE-F JOIN-PATH HERE-U !
   HERE$ ROOT$ TREE-COPY:FILE
   s" absolute checkout name lands at its checkout path" T-LABEL
   s" lib/tree-copy.f" OUT$ FILE? TTRUE ;

public

: RUN ( -- )
   s" tree-copy-root" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   OUTSIDE
   INSIDE
   CLEANUP-RUN
   T-REPORT ;

;package

TREE-COPY-TEST:RUN
