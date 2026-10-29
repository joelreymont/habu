\ Gate adapter for the shared keyed-image publisher.
require tools/keyed-image.f
require test/image-grant.f
require src/lib/process.f

package KEYED-IMAGE
public
: ENSURE ( ptr u8 ptr u8 n ptr u8 ptr u8 n ptr u8 n [ ptr u8 n -- ] -- )
   {: key:ptr fam:ptr famu:n ekey:ptr eng:ptr engu:n prog:ptr progu:n args :}
   fam famu IMAGE-GRANT:CHECK
   key fam famu ekey eng engu prog progu args PROCESS:WAIT-FOREVER MS>N ENSURE-WITH drop ;
;package
