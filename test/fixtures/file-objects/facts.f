\ facts.f - words build.f's `facts` mode compiles callers of after the file
\ links: BOX, a does> definer a caller wraps, FAIL, a word that ends in a throw,
\ so a token after a call of it is dead code, and TAKE, whose input a string
\ misuses. It holds no defer: a defer in a build's file ends the build, rc 70.

package FILE-OBJECTS-FACTS
public
: BOX ( n -- ) create , does> ( -- n ) @ ;
: FAIL ( -- ) 1 throw ;
: TAKE ( n -- ) drop ;
;package
