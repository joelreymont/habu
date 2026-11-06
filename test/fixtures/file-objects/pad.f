\ pad.f - build B loads it first, so every later file's records, emissions and
\ DATA sit at other indices and addresses than in build A.

package FILE-OBJECTS-PAD
public
create SCRATCH 64 allot           \ BUFFER: is E-UNDEFINED in a build
: MARK ( -- ) 7 SCRATCH c! ;
: FIRST ( -- n ) SCRATCH c@ ;
;package
