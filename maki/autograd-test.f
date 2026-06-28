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

\ --- SUB forward + backward: z=x-y ; dx=dz, dy=-dz ---
4.0 3.0 SUB-F   1000.0 f* 0.5 f+ f>s  1000 T=   \ 4-3 = 1
1.0 SUB-BWD                                     \ ( dx dy )
   1000.0 f* 0.5 f- f>s -1000 T=                \ dy = -1
   1000.0 f* 0.5 f+ f>s  1000 T=                \ dx = 1
\ finite-difference d(x-y)/dx at x=4 (y=3): (SUB(4.001,3)-SUB(3.999,3))/0.002 = 1
4.001 3.0 SUB-F  3.999 3.0 SUB-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  1000 T=

\ --- SQUARE forward + backward at x=3: z=9 ; dx=dz*2x=6 ---
3.0 SQUARE-F    1000.0 f* 0.5 f+ f>s  9000 T=   \ 3*3 = 9
1.0 3.0 SQUARE-BWD  1000.0 f* 0.5 f+ f>s  6000 T=   \ dz*2x = 6
\ finite-difference d(x*x)/dx at x=3: (SQUARE(3.001)-SQUARE(2.999))/0.002 = 6
3.001 SQUARE-F  2.999 SQUARE-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  6000 T=

\ --- DIV forward + backward at x=6,y=2: z=3 ; dx=dz/y=0.5 ; dy=-dz*x/y^2=-1.5 ---
6.0 2.0 DIV-F   1000.0 f* 0.5 f+ f>s  3000 T=   \ 6/2 = 3
1.0 6.0 2.0 DIV-BWD                             \ ( dx dy )
   1000.0 f* 0.5 f- f>s -1500 T=                \ dy = -1.5
   1000.0 f* 0.5 f+ f>s   500 T=                \ dx =  0.5
\ finite-difference d(x/y)/dx at x=6 (y=2): (DIV(6.001,2)-DIV(5.999,2))/0.002 = 0.5
6.001 2.0 DIV-F  5.999 2.0 DIV-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  500 T=
\ finite-difference d(x/y)/dy at y=2 (x=6): (DIV(6,2.001)-DIV(6,1.999))/0.002 = -1.5
6.0 2.001 DIV-F  6.0 1.999 DIV-F  f-  0.002 f/  1000.0 f* 0.5 f- f>s -1500 T=

\ --- SQRT forward + backward at x=4: z=2 ; dx=dz/(2*sqrt(x))=0.25 ---
4.0 SQRT-F      1000.0 f* 0.5 f+ f>s  2000 T=   \ sqrt(4) = 2
1.0 4.0 SQRT-BWD  1000.0 f* 0.5 f+ f>s  250 T=  \ dz/(2*2) = 0.25
\ finite-difference d sqrt/dx at x=4: (SQRT(4.001)-SQRT(3.999))/0.002 = 0.25
4.001 SQRT-F  3.999 SQRT-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  250 T=

\ --- ABS forward + backward: dx = dz*sign(x) ---
-3.0 ABS-F      1000.0 f* 0.5 f+ f>s  3000 T=   \ |-3| = 3
1.0 3.0 ABS-BWD   1000.0 f* 0.5 f+ f>s  1000 T= \ x>0 -> dz = 1
1.0 -3.0 ABS-BWD  1000.0 f* 0.5 f- f>s -1000 T= \ x<0 -> -dz = -1
\ finite-difference d|x|/dx at x=3: 1 ; at x=-3: -1
3.001 ABS-F  2.999 ABS-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  1000 T=
-2.999 ABS-F  -3.001 ABS-F  f-  0.002 f/  1000.0 f* 0.5 f- f>s -1000 T=

\ --- MAX forward + backward: gradient flows to the larger input ---
3.0 5.0 MAX-F   1000.0 f* 0.5 f+ f>s  5000 T=   \ max(3,5) = 5
1.0 3.0 5.0 MAX-BWD                             \ ( dx dy ) x<y -> dx=0, dy=dz
   1000.0 f* 0.5 f+ f>s  1000 T=                \ dy = 1
   1000.0 f* 0.5 f+ f>s     0 T=                \ dx = 0
1.0 5.0 3.0 MAX-BWD                             \ ( dx dy ) x>y -> dx=dz, dy=0
   1000.0 f* 0.5 f+ f>s     0 T=                \ dy = 0
   1000.0 f* 0.5 f+ f>s  1000 T=                \ dx = 1

T-REPORT
