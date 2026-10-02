\ Two committed layouts with equal size but distinct record identities.
package C2-RECORDS-TYPES
public
STRUCTURE pair 0 DERIVE init FIELD left n FIELD right n ;STRUCTURE
STRUCTURE other 0 DERIVE init FIELD left n FIELD right n ;STRUCTURE
STRUCTURE box 2 DERIVE init FIELD view read-view<a,a,b> ;STRUCTURE
STRUCTURE shelf 4 DERIVE init
   FIELD source read-view<a,b,u8>
   FIELD decoded read-view<c,d,u8>
   FIELD mark n
;STRUCTURE
;package
