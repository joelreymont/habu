\ gpt2-serve-close-test.f - post-service owner census.

require maki/infer/gpt2-serve.f

package GPT2-SERVE
private

SAFET:LIVE-OWNERS constant T-OWNERS
SAFET-MAP:LIVE constant T-MAPS

;package

require tools/gpt2-serve.f

package GPT2-SERVE
private

: T-CLOSE-CHECK ( -- )
   SAFET:LIVE-OWNERS T-OWNERS <> if GPT2-GEN:E-STATE throw then
   SAFET-MAP:LIVE T-MAPS <> if GPT2-GEN:E-STATE throw then ;

T-CLOSE-CHECK

;package
