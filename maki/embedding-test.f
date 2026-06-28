\ maki/embedding-test.f - gradcheck embedding gather + scatter-add VJP.
\
\ E=[[1,2],[3,4],[5,6]], ids=[2,0,2] -> Y=[[5,6],[1,2],[5,6]]. With dY=ones the
\ scatter-ADD gives dE[0]=[1,1], dE[1]=[0,0], dE[2]=[2,2] (token 2 used twice
\ ACCUMULATES). Checked vs a central finite difference on E[2,0].

require lib/test.f
require maki/embedding.f

package MAKI

T-RESET

create EE  6 cells allot   create EID 3 cells allot   create EY  6 cells allot
create EDY 6 cells allot    create EDE 6 cells allot

1.0 EE 0 T-SET  2.0 EE 1 T-SET  3.0 EE 2 T-SET
4.0 EE 3 T-SET  5.0 EE 4 T-SET  6.0 EE 5 T-SET
2.0 EID 0 T-SET  0.0 EID 1 T-SET  2.0 EID 2 T-SET        \ ids = [2,0,2]

\ forward gather
EE EID EY 3 2 EMB-GATHER
EY 0 T-GET  0.5 f+ f>s  5 T=        \ Y[0]=E[2]=[5,6]
EY 1 T-GET  0.5 f+ f>s  6 T=
EY 2 T-GET  0.5 f+ f>s  1 T=        \ Y[1]=E[0]=[1,2]
EY 4 T-GET  0.5 f+ f>s  5 T=        \ Y[2]=E[2]=[5,6]

\ backward scatter-add (dE zeroed, dY = ones)
0.0 EDE 6 T-FILL
1.0 EDY 6 T-FILL
EID EDY EDE 3 2 EMB-SCATTER-ADD
EDE 0 T-GET  0.5 f+ f>s  1 T=       \ dE[0,0]=1 (id 0 used once)
EDE 4 T-GET  0.5 f+ f>s  2 T=       \ dE[2,0]=2 (id 2 used TWICE -> accumulates)
EDE 5 T-GET  0.5 f+ f>s  2 T=       \ dE[2,1]=2
EDE 2 T-GET  0.5 f+ f>s  0 T=       \ dE[1,0]=0 (id 1 never used)

\ central finite difference dL/dE[2,0] (L=sum Y) = 2 (token 2 feeds two outputs)
: EMB-LSUM ( -- r )  EE EID EY 3 2 EMB-GATHER  EY 6 T-SUM ;
5.001 EE 4 T-SET  EMB-LSUM
4.999 EE 4 T-SET  EMB-LSUM
f-  0.002 f/  0.5 f+ f>s  2 T=
5.0 EE 4 T-SET

T-REPORT

end-package
