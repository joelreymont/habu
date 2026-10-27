\ Gate adapter for the shared keyed-image publisher.
require tools/keyed-image.f
require test/image-grant.f
require test/suite-budget.f

package KEYED-IMAGE
public
: ENSURE ( ptr u8 ptr u8 n ptr u8 n ptr u8 n [ ptr u8 n -- ] -- )
   {: key:ptr fam:ptr famu:n eng:ptr engu:n prog:ptr progu:n args :}
   fam famu IMAGE-GRANT:CHECK
   key fam famu eng engu prog progu args SUITE-BUDGET:CHILD-MS ENSURE-WITH drop ;
;package
