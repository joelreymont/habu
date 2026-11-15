\ The checker owner's later fields. This module loads before the checker owner
\ is published.

package CHECKER-OWNER-EXT
public

$1E8 constant RECORD-PUBLISHED-OFF
$1F0 constant GRID-BEFORE-OFF
$1F8 constant GRID-AFTER-OFF
$200 constant GRID-REVOKE-OFF
$208 constant GRID-RETIRE-OFF
$210 constant PUBLICATION-USE-OFF
$218 constant VERIFY-CTOR-DECL-OFF
\ ( -- ) closes the created body pending at a load boundary (checker.f
\ CHECKER-BODY-CLOSE).
$240 constant BODY-CLOSE-OFF
\ ( ^u8 n ^u8 n -- bool ) declares a TRUSTED: definition's row as
\ EFFECT-OFF's registrar does and answers whether the checker rejected it.
$248 constant DECL-ROW-OFF
DECL-ROW-OFF CELL + constant OWNER-BYTES

;package
