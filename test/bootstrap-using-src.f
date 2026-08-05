\ bootstrap-using-src.f - stage0 imports a package's public words with `using`.

\ Built by the Gforth recovery emitter (bootstrap/cg/forth.fs) and run directly,
\ so it exercises the emitted engine itself, not the native one. It covers the
\ behaviours the recovery path depends on: a bare public name resolves after
\ `using NAME`, the same name resolves inside a compiled body and under `'`, a
\ package-private name never joins the search, an import never shadows a name
\ that already resolves, and `;using` really pops the import. The marker line
\ only prints once every check has produced its value.

: BUS-SHADOW ( -- n ) 3 ;

package BUS-A
public
: BUS-VALUE ( -- n ) 7 ;
private
: BUS-SHADOW ( -- n ) 5 ;
;package

package BUS-B
public
: BUS-OTHER ( -- n ) 9 ;
;package

using BUS-A
BUS-VALUE .
: BUS-CALLER ( -- n ) BUS-VALUE ;
BUS-CALLER .
' BUS-VALUE execute .
BUS-SHADOW .
;using

\ A second import after the first one closed: the wid stack is popped, not leaked.
using BUS-B
BUS-OTHER .
;using

s" BOOTSTRAP-USING-OK" type cr
