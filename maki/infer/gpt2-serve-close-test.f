\ gpt2-serve-close-test.f - post-service owner census.

require maki/infer/gpt2-serve.f

package GPT2-SERVE
private

-7696 constant E-CLOSE

SAFET:LIVE-OWNERS constant T-OWNERS
SAFET-MAP:LIVE constant T-MAPS

;package

require tools/gpt2-serve.f

package GPT2-SERVE
private

: T-CLOSE-CHECK ( -- )
   SAFET:LIVE-OWNERS T-OWNERS <> if E-CLOSE throw then
   SAFET-MAP:LIVE T-MAPS <> if E-CLOSE throw then ;

T-CLOSE-CHECK

;package
