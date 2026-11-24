\ unclean.f - a request inside a package (an unclean cut), then one at top level (a clean one):
\ the first two parts compile in every build, and the third links once its object exists.

package FILE-OBJECTS-UNCLEAN public
require test/fixtures/file-objects/unclean-in.f
: A ( -- n ) 7 ;
;package
require test/fixtures/file-objects/unclean-e.f
package FILE-OBJECTS-UNCLEAN public : C ( -- n ) 9 ; ;package
