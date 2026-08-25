\ aot-chain-decoy.f - a child that says the words and does nothing.
\
\ The fail-closed control for test/aot-chain-capture-suite.f: it prints the
\ capture's refusal sentence on stdout and exits 0. A suite that matched text
\ without checking the exit code would read this as a refusal, so the suite
\ asserts its detector reads it as what it is.

package AOT-CHAIN-DECOY
public
: RUN ( -- )
   s" aot-capture: window call into the prelude band" type cr ;
;package

AOT-CHAIN-DECOY:RUN
