\ eval-device-sm-test.f - device-golden authoring tests for the softmax-rows task.
\ The candidates that used to live at the tail of maki/eval-device-sm.f; split out so the
\ GRADE-SM library is reusable (maki/eval-author.f). Orin-only. Load after
\ maki/eval-device-sm.f.

T-RESET

\ correct softmax -> certify AND device-correct -> GREEN(2)
s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-MAX {: mx :} x mx B- EXP. {: e :} e BLOCK-SUM {: s :} e s B/ out r ROW-SPAN c ROW-STORE"  GRADE-SM  2 T=
\ B- instead of B/ (TYPE-IDENTICAL) -> certifies but device output != softmax -> TYPED-WRONG(1)
s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-MAX {: mx :} x mx B- EXP. {: e :} e BLOCK-SUM {: s :} e s B- out r ROW-SPAN c ROW-STORE"  GRADE-SM  1 T=
\ ill-typed (missing ROW-STORE) -> checker rejects -> REJECTED(0)
s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-MAX {: mx :} x mx B- EXP. {: e :} e BLOCK-SUM {: s :} e s B/"  GRADE-SM  0 T=

s" softmax authoring grade: correct=GREEN, B-not-B/=TYPED-WRONG (device gate), no-store=REJECTED" type cr

T-REPORT
