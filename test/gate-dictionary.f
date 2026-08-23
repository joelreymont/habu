\ gate-dictionary.f - entry wrapper for dictionary/checker contracts.
\
require lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/check-all-errors-core.f
require test/gate-common.f
include test/gate-dictionary-lib.f

GATE-DICTIONARY:RUN
