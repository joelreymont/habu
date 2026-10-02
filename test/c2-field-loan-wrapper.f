\ An explicit child and projected-region callback scheme survives image saving.
require lib/c2-memory.f

package C2-FIELD-LOAN-WRAPPER
public
STRUCTURE pair 0 DERIVE init FIELD left n FIELD right n ;STRUCTURE
STRUCTURE outer 0 DERIVE init
   FIELD marker n
   FIELD inner pair
   FIELD guard n
;STRUCTURE

: APPLY
   ( mut-view<b,i,a,init<i,outer>> forall<j inside i,forall-region<c,[ mut-view<b,j,c,init<i,pair>> -- mut-view<b,j,c,init<i,pair>> | U -- U ]>> | U -- mut-view<b,i,a,init<i,outer>> | U )
   C2-MEM:WITH-FIELD inner ;

;package
