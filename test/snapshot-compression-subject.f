\ More than a million absent bytes, bounded by present cells. The final marker
\ also pins a byte in the buffer's partial last cell.
package SNAPSHOT-COMPRESSION-SUBJECT
1048593 constant SIZE
create GAP SIZE allot
: SET ( -- )
   45 GAP c! 77 GAP SIZE + 1- c!
   \ This private child never parses or formats the minimum i64. Its digit
   \ table is nonzero in the baked seed: clearing all 19 bytes guarantees a
   \ whole absent cell must overwrite seeded bytes when the image restores.
   STR-MIN-I64$ c@ 57 <> if s" snapshot compression: seed table" 74 die then
   STR-I64-DIGITS 0 ?do 0 STR-MIN-I64$ i + c! loop ;
public
: VERIFY ( -- )
   STR-I64-DIGITS 0 ?do
      STR-MIN-I64$ i + c@ 0<> if s" snapshot compression: seeded zero" 74 die then
   loop
   GAP c@ 45 <> if s" snapshot compression: first byte" 74 die then
   GAP SIZE + 1- c@ 77 <> if s" snapshot compression: last byte" 74 die then
   SIZE 1- 1 ?do
      GAP i + c@ 0<> if s" snapshot compression: zero hole" 74 die then
   loop
   s" snapshot compression: ok" type cr ;
SET
;package
