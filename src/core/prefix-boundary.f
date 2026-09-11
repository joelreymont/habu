\ The first source-prefix record in the running engine's dictionary.
require src/habu/xref.f

package CORE-PREFIX
public

: FIRST-RECORD ( -- n )
   s" IMK-NDICT0" 0 XREF-FIND-WL-INDEX
   dup 0 < if s" prefix boundary: first source record is missing" 76 die then ;

;package
