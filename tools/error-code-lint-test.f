\ error-code-lint-test.f - checked fixtures for the E- throw-code uniqueness lint.
\ Run: bin/hb --load tools/error-code-lint-test.f
\ Requiring the core also lets MECLT-LIVE enforce the real repo ledger (clean).
\ Load after lib/test.f and tools/error-code-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/error-code-lint-core.f

: MECLT-DETECT ( -- )
   \ one code under two names is a finding; distinct codes are not
   s" -9001 constant E-XA  -9001 constant E-XB" ECL-COUNT 1 T=
   s" -9001 constant E-XA  -9002 constant E-XB" ECL-COUNT 0 T=
   \ hex and decimal literals claim the same numeric code
   s" -$10 constant E-HA  -16 constant E-HB" ECL-COUNT 1 T=
   \ three claimants report every colliding pair
   s" -9001 constant E-XA  -9001 constant E-XB  -9001 constant E-XC" ECL-COUNT 3 T= ;

: MECLT-ALLOWANCES ( -- )
   \ exact (code, name) re-registration is a shim, not a collision
   s" -9001 constant E-XA  -9001 constant E-XA" ECL-COUNT 0 T=
   \ positive values are sysexits-style process exit codes, shared by design
   s" 76 constant E-XA  76 constant E-XB" ECL-COUNT 0 T=
   \ -FIRST/-LAST range sentinels alias member codes deliberately
   s" -9100 constant E-X-FIRST  -9100 constant E-XM" ECL-COUNT 0 T=
   s" -9199 constant E-X-LAST  -9199 constant E-XZ" ECL-COUNT 0 T= ;

: MECLT-NO-FALSE-POSITIVE ( -- )
   \ `\` line comments are stripped by TOKENIZE
   s" \ -9001 constant E-XA  -9001 constant E-XB" ECL-COUNT 0 T=
   \ a claim inside an s" ... " string body is not a claim
   s\" .\" -9001 constant E-XA\"  -9001 constant E-XB" ECL-COUNT 0 T=
   \ non-numeric values (constant aliases) and non-E- names are not claims
   s" E-XA constant E-XB  -9001 constant E-XB" ECL-COUNT 0 T=
   s" -9001 constant XA  -9001 constant E-XB" ECL-COUNT 0 T= ;

: MECLT-LIVE ( -- )
   \ the real tree is clean: every negative E- code has exactly one owner
   ERROR-CODE-LINT-STRICT ;

: MECLT-MAIN ( -- )
   T-RESET
   MECLT-DETECT
   MECLT-ALLOWANCES
   MECLT-NO-FALSE-POSITIVE
   T-REPORT
   MECLT-LIVE ;

MECLT-MAIN
