split-str
\ The checker owner's later fields. This module loads before the checker owner
\ is published.

package CHECKER-OWNER-EXT
public

$1E8 constant RECORD-PUBLISHED-OFF
$200 constant DP-CUT-OFF
$210 constant PUBLICATION-USE-OFF
$218 constant VERIFY-CTOR-DECL-OFF
\ ( ^u8 n ^u8 n -- bool ) declares a TRUSTED: definition's row as
\ EFFECT-OFF's registrar does and answers whether the checker rejected it.
$248 constant DECL-ROW-OFF
DECL-ROW-OFF CELL + constant OWNER-BYTES

;package
