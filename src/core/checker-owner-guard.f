split-str
\ Bounds checks for a live declaration-owner record. Loaded after the prefix
\ checker, so the shared ABI remains a constants-only pre-hook dependency.
require src/core/checker-owner-abi.f

package CHECKER-OWNER-GUARD

public

\ The checker that owns the source published a declaration-owner record
\ without the operation op opu, needed at t tu. An empty op names none; a
\ NULL-PTR t leaves the record unplaced.
: MISSING-OP ( str str -- ) {: op:^ opu:n t:^ tu:n :}
   s" E-COMP-OWNER" REJECTION:OPEN
   s" the checker that owns the source carries no " REJECTION:EMIT-STR
   opu 0= IF
      s" operation for this step" REJECTION:EMIT-STR
   ELSE
      96 REJECTION:EMIT1  op opu REJECTION:EMIT-STR
      s" ` operation" REJECTION:EMIT-STR
   THEN
   t NULL-PTR = 0= IF t tu REJECTION:AT  s" needed here" REJECTION:EMIT-STR THEN
   REJECTION:HELP
   s" publish a declaration-owner record that carries every operation"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The checker that owns the source published a declaration-owner record that
\ is out of bounds or not one.
: MALFORMED ( -- )
   s" E-COMP-OWNER" REJECTION:OPEN
   s" the checker that owns the source published a malformed declaration-owner record"
   REJECTION:EMIT-STR
   REJECTION:HELP
   s" publish the record through CHECKER-OWNER-ABI" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

private
EXPORT BOOT:data-base

: BAD ( -- ) MALFORMED ;
: ADDRESS ( ^u8 -- n ) NULL-PTR BYTE-VIEW - ;
: DATA-SPAN ( -- n n ) data-base BYTE-VIEW ADDRESS here BYTE-VIEW ADDRESS ;

public

\ `need` is the exclusive end of the fields the caller reads.
: VALIDATE ( ^u8 n -- ^u8 ) {: owner:^ need:n :}
   DATA-SPAN {: lo:n hi:n :}
   owner ADDRESS {: at:n :}
   at lo < at hi > or at CELL mod 0 <> or IF BAD THEN
   at lo - CHECKER-OWNER-ABI:HEADER-BYTES < IF BAD THEN
   owner CHECKER-OWNER-ABI:HEADER-BYTES - CELL-VIEW @ CHECKER-OWNER-ABI:MAGIC <> IF BAD THEN
   owner 8 - CELL-VIEW @ {: bytes:n :}
   bytes 0 < bytes hi at - > or bytes CELL mod 0 <> or IF BAD THEN
   need 0 < need bytes > or IF BAD THEN
   owner ;

;package
