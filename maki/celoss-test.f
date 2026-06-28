\ maki/celoss-test.f - gradcheck softmax cross-entropy.
\ logits=[1,2,3], target=[0,0,1] (class 2). y=softmax=[0.0900,0.2447,0.6652].
\ L=-ln(y2)=0.4076 ; dL/dlogits=y-t=[0.0900,0.2447,-0.3348]. Checked vs central FD.

T-RESET

create CLL 3 cells allot   create CLY 3 cells allot
create CLT 3 cells allot   create CLD 3 cells allot

1.0 CLL 0 T-SET   2.0 CLL 1 T-SET   3.0 CLL 2 T-SET
0.0 CLT 0 T-SET   0.0 CLT 1 T-SET   1.0 CLT 2 T-SET

CLL CLY 3 SM-FWD                                  \ y = softmax(logits)
CLY CLT 3 CE-LOSS  1000.0 f* 0.5 f+ f>s  408 T=   \ -ln(0.6652)=0.40757

CLY CLT CLD 3 SOFTMAX-CE-BWD
CLD 0 T-GET  1000.0 f* 0.5 f+ f>s    90 T=        \ y0 - 0
CLD 1 T-GET  1000.0 f* 0.5 f+ f>s   245 T=        \ y1 - 0
CLD 2 T-GET  1000.0 f* 0.5 f- f>s  -335 T=        \ y2 - 1 = -0.3348

\ central finite difference dL/dlogits[2] = -0.3348
: CE-NOW ( -- r )  CLL CLY 3 SM-FWD  CLY CLT 3 CE-LOSS ;
3.001 CLL 2 T-SET  CE-NOW
2.999 CLL 2 T-SET  CE-NOW
f-  0.002 f/  1000.0 f* 0.5 f- f>s  -335 T=
3.0 CLL 2 T-SET

T-REPORT
