\ test.f - public checked test framework interface. It loads the assertion
\ words (src/lib/test/assert.f), package TEST suite orchestration
\ (src/lib/test/suite.f) and package TEST-EVAL (src/lib/test/eval.f); each
\ module's header states its contract.
\
\ STORAGE CLASS. No state of its own; each loaded module states its class.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/test/suite.f
require src/lib/test/eval.f
