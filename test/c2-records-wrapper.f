\ Higher-rank table and element loan effects saved in a native image.
require lib/c2-memory.f

package C2-RECORDS-WRAPPER
public
STRUCTURE pair 0 DERIVE init FIELD left n FIELD right n ;STRUCTURE

: APPLY
   ( n mut-view<b,l,a,u8> n pair forall<i inside [l,pair],[ n records<b,i,a,pair> -- n records<b,i,a,pair> | U -- U ]> | U -- n mut-view<b,l,a,u8> | U )
   C2-MEM:WITH-RECORDS ;

: ELEMENT
   ( records<b,i,a,pair> n forall<j inside i,[ mut-view<b,j,a,init<i,pair>> -- mut-view<b,j,a,init<i,pair>> | U -- U ]> | U -- records<b,i,a,pair> | U )
   C2-MEM:WITH-RECORD ;

;package
