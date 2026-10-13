\ Two committed layouts with equal size but distinct record identities.
package C2-RECORDS-TYPES
public
STRUCTURE pair DERIVE init FIELD left n FIELD right n ;STRUCTURE
STRUCTURE other DERIVE init FIELD left n FIELD right n ;STRUCTURE
STRUCTURE box<a,b> DERIVE init FIELD view read-view<a,a,b> ;STRUCTURE
STRUCTURE shelf<a,b,c,d> DERIVE init
   FIELD source read-view<a,b,u8>
   FIELD decoded read-view<c,d,u8>
   FIELD mark n
;STRUCTURE
;package
