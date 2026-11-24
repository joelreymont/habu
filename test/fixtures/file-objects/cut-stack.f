\ cut-stack.f - a require of cut-stack-a.f, which cuts a part only when nothing loaded it before;
\ a clean request; a request with a cell on the stack (an unclean cut at top scope).

require test/fixtures/file-objects/cut-stack-a.f
require test/fixtures/file-objects/cut-stack-d.f
1 require test/fixtures/file-objects/cut-stack-b.f drop
package FILE-OBJECTS-CUT-STACK public : S ( -- n ) 6 ; ;package
