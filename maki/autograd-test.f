\ maki/autograd-test.f - numeric gradient verification (element-level gradcheck).
\
\ For each op, the analytic VJP must match the central finite difference
\ (f(x+h) - f(x-h)) / 2h with h = 0.001. Both sides scale x1000 + round.

T-RESET

\ --- forward values ---
3.0 4.0 ADD-F   1000.0 f* 0.5 f+ f>s  7000 T=   \ 3+4 = 7
3.0 4.0 MUL-F   1000.0 f* 0.5 f+ f>s 12000 T=   \ 3*4 = 12
2.0 RELU-F      1000.0 f* 0.5 f+ f>s  2000 T=   \ relu(2)=2
-2.0 RELU-F     1000.0 f* 0.5 f+ f>s     0 T=   \ relu(-2)=0

\ --- ADD backward: dx=dy=dz=1 ; matches d/dx and d/dy of x+y = 1 ---
1.0 ADD-BWD                                     \ ( dx dy )
   1000.0 f* 0.5 f+ f>s  1000 T=                \ dy = 1
   1000.0 f* 0.5 f+ f>s  1000 T=                \ dx = 1

\ --- MUL backward at x=3,y=4: dx=dz*y=4, dy=dz*x=3 ---
1.0 3.0 4.0 MUL-BWD                             \ ( dx dy )
   1000.0 f* 0.5 f+ f>s  3000 T=                \ dy = 3
   1000.0 f* 0.5 f+ f>s  4000 T=                \ dx = 4
\ finite-difference d(x*y)/dx at x=3 (y=4): (MUL(3.001,4)-MUL(2.999,4))/0.002
3.001 4.0 MUL-F  2.999 4.0 MUL-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  4000 T=

\ --- RELU backward: gate on x's sign ; matches finite diff away from 0 ---
1.0 2.0 RELU-BWD   1000.0 f* 0.5 f+ f>s  1000 T=   \ x>0 -> dz = 1
1.0 -2.0 RELU-BWD  1000.0 f* 0.5 f+ f>s     0 T=   \ x<0 -> 0
\ finite-difference d relu/dx at x=2: (RELU(2.001)-RELU(1.999))/0.002 = 1
2.001 RELU-F  1.999 RELU-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  1000 T=
\ finite-difference at x=-2: 0
-1.999 RELU-F  -2.001 RELU-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  0 T=

T-REPORT
