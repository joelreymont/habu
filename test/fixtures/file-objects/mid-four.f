\ mid-four.f - four parts, each request a clean cut; only the second calls
\ FILE-OBJECTS-MID-W:W, so a change of W's file compiles that part alone.

package FILE-OBJECTS-MID-FOUR public : A ( -- n ) 10 ; ;package
require test/fixtures/file-objects/mid-e1.f
package FILE-OBJECTS-MID-FOUR public : B ( -- n ) FILE-OBJECTS-MID-W:W ; ;package
require test/fixtures/file-objects/mid-e2.f
package FILE-OBJECTS-MID-FOUR public : C ( -- n ) 30 ; ;package
require test/fixtures/file-objects/mid-e3.f
package FILE-OBJECTS-MID-FOUR public : D ( -- n ) 40 ; ;package
