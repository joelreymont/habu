\ eval-author-test.f - GRADE-AUTHOR over correct + buggy SAXPY and softmax candidates.
\
\ Proves the unified authoring grader returns the right verdict per task: a correct kernel
\ is GREEN (2), a type-identical semantic bug is device-wrong (1), an ill-typed kernel is
\ checker-rejected (0), and an unknown task fails closed. Orin-only (each candidate is
\ emitted, ptxas-assembled, and run on the device). Load after maki/eval-device.f,
\ maki/eval-device-sm.f, and maki/eval-author.f.

: BAD-TASK ( -- )  s" K ( -- )" 99 GRADE-AUTHOR drop ;   \ unknown task

: EVAL-AUTHOR-MAIN ( -- )
   T-RESET

   \ --- SAXPY task (golden a*x+y = 6.0) ---
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE"  TASK-SAXPY GRADE-AUTHOR  2 T=   \ correct -> GREEN
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD y g LOAD +. y g STORE"            TASK-SAXPY GRADE-AUTHOR  1 T=   \ x+y (forgot scale) -> device-wrong
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +."              TASK-SAXPY GRADE-AUTHOR  0 T=   \ no store -> checker-rejected

   \ --- softmax-rows task ---
   s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-MAX {: mx :} x mx B- EXP. {: e :} e BLOCK-SUM {: s :} e s B/ out r ROW-SPAN c ROW-STORE"  TASK-SOFTMAX GRADE-AUTHOR  2 T=   \ correct -> GREEN
   s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-MAX {: mx :} x mx B- EXP. {: e :} e BLOCK-SUM {: s :} e s B- out r ROW-SPAN c ROW-STORE"  TASK-SOFTMAX GRADE-AUTHOR  1 T=   \ B- not B/ -> device-wrong

   \ --- unknown task fails closed ---
   ['] BAD-TASK E-MK-EVAL TTHROWS

   s" eval-author: unified GRADE-AUTHOR grades SAXPY + softmax from the committed tree (/tmp graders retired)" type cr
   T-REPORT ;

EVAL-AUTHOR-MAIN
