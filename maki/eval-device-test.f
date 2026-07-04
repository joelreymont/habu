\ maki/eval-device-test.f - device-golden autograder demo (load after maki/eval-device.f).
\ correct SAXPY -> GREEN(2); x+y (certifies, device-wrong) -> TYPED-WRONG(1); missing-store -> REJECTED(0).
\ Reopens `package MAKI` so maki/eval-device.f's GRADE-CANDIDATE / EVD-* resolve by bare name.

package MAKI

T-RESET

\ correct SAXPY -> certifies AND device-correct -> GREEN(2)
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE"  GRADE-CANDIDATE  2 T=
\ WRONG-but-certified (x+y) -> certifies but device output != golden -> TYPED-WRONG(1)
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y g LOAD +. y g STORE"           GRADE-CANDIDATE  1 T=
\ ill-typed (missing store) -> checker rejects -> REJECTED(0)
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +."             GRADE-CANDIDATE  0 T=

\ device-gated pass@k: 3 candidates, only the device-correct one is GREEN
EVD-RESET
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE"  EVD-SCORE
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y g LOAD +. y g STORE"           EVD-SCORE
s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +."             EVD-SCORE
EVD-TOTAL @  3 T=
EVD-PASS  @  1 T=                                   \ ONE green (device-correct), vs 2 that merely certify

s" device-golden pass@k: green(certify AND device-correct)=" type EVD-PASS @ . s" / total=" type EVD-TOTAL @ . cr

T-REPORT

end-package
