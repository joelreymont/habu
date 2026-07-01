\ netpbm-test.f - oracle tests against src/netpbm.zig.
\ Part of the NETPBM package: reopens `package NETPBM` and calls WRITE-P5/DECODE unqualified.
\ Run: ../habu/bin/hb --load odin/netpbm-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/float.f
require lib/fmt.f
require odin/netpbm.f

\ writeP5: 2x2 image, step 3 (last byte of each row is padding to drop)
package NETPBM
private
create DATA  1 c, 2 c, 99 c, 3 c, 4 c, 99 c,
\ expected: "P5\n2 2\n255\n" + {1,2,3,4}
create WANT  80 c, 53 c, 10 c, 50 c, 32 c, 50 c, 10 c, 50 c, 53 c, 53 c, 10 c, 1 c, 2 c, 3 c, 4 c,
here WANT - constant WANT-LEN

\ decode P5: "P5\n2 1\n255\n" + {10,20}
create NP5  80 c, 53 c, 10 c, 50 c, 32 c, 49 c, 10 c, 50 c, 53 c, 53 c, 10 c, 10 c, 20 c,
here NP5 - constant NP5-LEN
create NP5-PIX  10 c, 20 c,

\ decode P6: "P6\n1 1\n255\n" + {255,0,0} -> luma 54
create NP6  80 c, 54 c, 10 c, 49 c, 32 c, 49 c, 10 c, 50 c, 53 c, 53 c, 10 c, 255 c, 0 c, 0 c,
here NP6 - constant NP6-LEN
create NP6-PIX  54 c,

\ decode with a # comment line
create NP5C  80 c, 53 c, 10 c, 35 c, 32 c, 104 c, 105 c, 10 c, 49 c, 32 c, 49 c, 10 c, 50 c, 53 c, 53 c, 10 c, 77 c,
here NP5C - constant NP5C-LEN
create NP5C-PIX  77 c,

: RUN ( -- )
   T-RESET
   DATA 2 2 3 WRITE-P5   WANT WANT-LEN T$=
   NP5 NP5-LEN DECODE drop  NP5-PIX 2 T$=     \ drop ok flag, compare luma span
   WIDTH @ 2 T=  HEIGHT @ 1 T=
   NP6 NP6-LEN DECODE drop  NP6-PIX 1 T$=
   NP5C NP5C-LEN DECODE drop NP5C-PIX 1 T$= ; \ comment skipped, 1x1 image -> {77}

RUN
T-REPORT
end-package
