\ A source declaration produces a real schema reference to a dynamic linear
\ constructor. Its numeric code is not a portable constructor identity.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/habu/code-span.f
require src/habu/xref.f
require src/core/generated-declaration-dictionary.f
require src/core/generated-declaration-protection.f

using SCHEMA-REG
package TFAM
create PCS-BUF $10000 allot
public
TRUSTED: PCS-START ( -- ) CHECKER-REG-AOT-MARK ;
TRUSTED: PCS-SAVE ( -- )
   s" pcs-holder" s" 0 FIELD value pcs-linear ;STRUCTURE" STRUCTURE-DECL:SD-REPLAY
   0
   SCH-N @ 6 REG-AOT-MARK@ ?do
      i SCHEMA-CON? IF i SCHEMA-A@ CC-MAX >= IF 1+ THEN THEN
   loop
   0= IF 79 throw THEN
   s" dynamic constructor schema is present" type cr
   CHECKER-REG-AOT-CLOSE
   PCS-BUF $10000 CHECKER-REG-AOT-SAVE drop ;
;package
TFAM:PCS-START
DEFLINEAR pcs-linear
TFAM:PCS-SAVE
s" process-local schema constructor was incorrectly accepted" 79 die
