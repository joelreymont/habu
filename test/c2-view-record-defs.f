\ Public field schemas kept in a saved image for a fresh consumer.
STRUCTURE c2vinner<a,b> FIELD source read-view<a,b,u8> FIELD mark n ;STRUCTURE
STRUCTURE c2vouter<a,b> FIELD item c2vinner<a,b> ;STRUCTURE

STRUCTURE c2vpair FIELD x n FIELD y n ;STRUCTURE
STRUCTURE c2vtypedinner<a,b,c> FIELD source read-view<a,b,c> ;STRUCTURE
STRUCTURE c2vtypedouter<a,b> FIELD source c2vtypedinner<a,b,c2vpair> ;STRUCTURE
ENUM c2vmaybe<a,b> VARIANT held FIELD source read-view<a,b,u8> ;VARIANT VARIANT vacant ;VARIANT ;ENUM

package C2-VIEW-RECORD-GENERICS
public

: DROP-ELEM ( t read-view<p,q,t> -- read-view<p,q,t> ) swap drop ;
: FETCH-DROP ( ^t read-view<p,q,t> -- read-view<p,q,t> ) swap @ drop ;

;package
