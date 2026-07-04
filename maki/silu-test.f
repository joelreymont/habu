\ maki/silu-test.f - gradcheck SiLU forward + VJP.
\ silu(0)=0, silu(1)=0.7311, silu(2)=1.7616, silu(-1)=-0.2689 ; s'(0)=0.5, s'(1)=0.9277.

require lib/test.f
require maki/silu.f

package MAKI

T-RESET

0.0 SILU-F   1000.0 f* 0.5 f+ f>s     0 T=
1.0 SILU-F   1000.0 f* 0.5 f+ f>s   731 T=        \ 0.73106
2.0 SILU-F   1000.0 f* 0.5 f+ f>s  1762 T=        \ 1.76159
-1.0 SILU-F  1000.0 f* 0.5 f- f>s  -269 T=        \ -0.26894

\ VJP + finite difference
1.0 0.0 SILU-BWD  1000.0 f* 0.5 f+ f>s   500 T=   \ s'(0)=0.5
1.0 1.0 SILU-BWD  1000.0 f* 0.5 f+ f>s   928 T=   \ s'(1)=0.92767
1.001 SILU-F  0.999 SILU-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  928 T=

T-REPORT

end-package
