\ defname-kernel.f - a definition named `kernel:`, where top level reads `:`.
package MINE
public
: kernel: ( -- n ) 5 ;
: T ( -- n ) kernel: ;
;package
MINE:T PDEP:SHOW
