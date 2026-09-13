\ Bounds checks for a live declaration-owner record. Loaded after the prefix
\ checker, so the shared ABI remains a constants-only pre-hook dependency.
require src/core/checker-owner-abi.f

package CHECKER-OWNER-GUARD
private

\ E-NCOMP-OWNER; this small core helper does not require the library layer.
: BAD ( -- ) -8574 throw ;
TRUSTED: ADDRESS ( ptr u8 -- n ) ;
TRUSTED: DATA-SPAN ( -- n n ) data-base here ;

public

\ Bound the descriptor before reading it, then the complete recorded extent
\ before a caller reads any operation. `need` is the exclusive field end.
: VALIDATE ( ptr u8 n -- ptr u8 ) {: owner:ptr need:n :}
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
