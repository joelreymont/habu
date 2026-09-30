\ baked-owner-child.f - load one source on an engine whose baked packages are
\ sealed (test/baked-owner-seal.f).
\
\ Run: bin/hb --load test/baked-owner-child.f -- <source>
\
\ test/baked-owner-seal.f seals the packages; the source then loads through
\ `required`, the real load path, and the child prints `owner-ok` only when the
\ whole source loaded. test/baked-owner.f runs this once per product-path
\ source.

require src/os/script-argv.f
require test/baked-owner-seal.f

package BAKED-OWNER-CHILD

public

: LOAD ( -- )
   SCRIPT-ARGC 1 <> if s" baked-owner-child: want one source path" 64 die then
   0 SCRIPT-ARGV$ required
   s" owner-ok" type cr ;

;package

BAKED-OWNER-SEAL:SEAL-BAKED
BAKED-OWNER-CHILD:LOAD
