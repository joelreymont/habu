\ Frozen-certificate access through the checker this compiler scanned with.
require src/compiler/native/checker-owner.f
require src/core/checker-fetch-abi.f

package CHECKER-OWNER
private

\ A retained prefix exposes bounded scalar readers before the appended callback
\ exists. Copy through those checked readers; the raw BYTES authority stays
\ unavailable to checked callers.
DYNAMIC-BUFFER SOURCE-CERT-CELLS n

: SOURCE-CERTIFICATE ( n -- ptr u8 n ) {: expected:n :}
   LOWER-CERT:CELL-COUNT {: count:n :}
   count LOWER-CERT:HEADER-CELLS < if E-NCOMP-OWNER throw then
   LOWER-CERT:BODY-HASH-CELL LOWER-CERT:CELL@ expected <> if E-NCOMP-OWNER throw then
   count SOURCE-CERT-CELLS-RESERVE
   count 0 ?do i LOWER-CERT:CELL@ i SOURCE-CERT-CELLS ! loop
   0 SOURCE-CERT-CELLS BYTE-VIEW count cells ;

public

\ Same callback shape as a family name reader: one scalar query, one byte span.
\ A captured compiler requires the appended field, with no by-name fallback.
: CERTIFICATE ( n -- ptr u8 n )
   CHECKER-FETCH-ABI:CERTIFICATE-OFF s" frozen source certificate" FIELD
   dup 0= if drop SOURCE-CERTIFICATE exit then
   AS-FAMILY-NAME execute ;

: RELEASE-CERTIFICATE ( -- )
   SOURCE-CERT-CELLS-RELEASE ;

;package
