\ flip.f - on every host one [if] requests flip-b.f, so the part after it starts at [then]; when
\ flip-a.f is already loaded its require cuts no part, and that part starts there.

package FILE-OBJECTS-FLIP public : P ( -- n ) 1 ; ;package
require test/fixtures/file-objects/flip-a.f
package FILE-OBJECTS-FLIP public : Q ( -- n ) 2 ; ;package
HB-TARGET-MACOS? [if] require test/fixtures/file-objects/flip-b.f [then]
HB-TARGET-MACOS? 0= [if] require test/fixtures/file-objects/flip-b.f [then]
package FILE-OBJECTS-FLIP public : R ( -- n ) 3 ; ;package
