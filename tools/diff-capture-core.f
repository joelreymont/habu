\ diff-capture-core.f - public framed-capture facade.

require tools/diff-capture-types.f
require tools/diff-capture-command.f
require tools/diff-capture-metadata.f
require tools/diff-capture-content.f
require tools/diff-capture-frame.f
require tools/diff-capture-transaction.f
require tools/diff-capture-diagnostic.f

package DIFF-CAPTURE
public

EXPORT DIFF-CMD:LAST-PHASE
EXPORT DIFF-CMD:LAST-OUTCOME
EXPORT DIFF-TXN:LAST-CAPTURE-OUTCOME
EXPORT DIFF-CMD:LAST-RC
EXPORT DIFF-CMD:LAST-CODE
EXPORT DIFF-CMD:LAST-OUT-CODE
EXPORT DIFF-CMD:LAST-ERR-CODE
EXPORT DIFF-TXN:LAST-PRIMARY
EXPORT DIFF-TXN:LAST-CLEANUP
EXPORT DIFF-CMD:LAST-OUT$
EXPORT DIFF-CMD:LAST-ERR$
EXPORT DIFF-DIAG:REPORT$
EXPORT DIFF-TXN:REPORT?
EXPORT DIFF-CMD:COMMAND?
EXPORT DIFF-CONTENT:CONTENT-PROVIDER!
EXPORT DIFF-CONTENT:CONTENT-METADATA$
EXPORT DIFF-CONTENT:CONTENT-METADATA-PATH$
EXPORT DIFF-CONTENT:CONTENT-ROW-COUNT
EXPORT DIFF-CONTENT:CONTENT-ROW!

: RUN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: output:ptr outputu:n from:ptr fromu:n to:ptr tou:n :}
   s" " output outputu from fromu to tou DIFF-TXN:CAPTURE ;

: RUN-IN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   DIFF-TXN:CAPTURE ;

;package
