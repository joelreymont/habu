\ baked-owner-child.f - load one source on the product engine, whose build
\ sealed every package it bakes (src/core/internal-mark.f SEAL-PACKAGES).
\
\ Run: bin/hb --load test/baked-owner-child.f -- <source>
\
\ The source loads through `required`, the real load path, and the child prints
\ `owner-ok` only when the whole source loaded. test/baked-owner.f runs this
\ once per product-path source.

require src/os/script-argv.f

package BAKED-OWNER-CHILD

public

: LOAD ( -- )
   SCRIPT-ARGC 1 <> if s" baked-owner-child: want one source path" 64 die then
   0 SCRIPT-ARGV$ required
   s" owner-ok" type cr ;

;package

BAKED-OWNER-CHILD:LOAD
