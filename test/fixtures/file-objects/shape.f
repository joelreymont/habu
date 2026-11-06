\ shape.f - a private word behind a public one, and a re-export of count.f's
\ BUMP. Loaded after count.f.

package FILE-OBJECTS-SHAPE
private
: TWICE ( n -- n ) 2 * ;
public
: DOUBLE ( n -- n ) TWICE ;
EXPORT FILE-OBJECTS-COUNT:BUMP
;package
