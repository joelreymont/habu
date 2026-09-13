\ Loaded after the real source-window reset, including from a grown snapshot.
package ADDRESS-CELL-OWNER
: addr-cells-abi ( -- n ) 0 ;
TRUSTED: VERSION-XT ( n -- [ -- n ] ) ;
: CHECK ( -- )
   addr-cells-abi 0 <> if s" local ABI shadow changed" 76 die then
   s" addr-cells-abi" 0 search-wl {: xt:n :}
   xt 0= if s" engine ABI lost across source reset" 76 die then
   xt VERSION-XT execute 1 <> if s" wrong engine address ABI" 76 die then
   s" address-cell-owner: ok" type cr ;
CHECK
;package
