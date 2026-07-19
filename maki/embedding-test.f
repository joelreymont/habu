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

\ ---- learned positional embedding (wpe SLICE) + token+pos ADD composition ------
\ E=[[1,2],[3,4],[5,6]] (above), ids=[2,0,2], wpe is a MaxT=4 x 2 table sliced to the
\ T=3 sequence positions 0..2. TOKPOS-EMBED gives Y[i,:] = E[ids[i],:] + wpe[i,:].
create TPW  8 cells allot   create TPY  6 cells allot
create TPDY 6 cells allot   create TPDW 8 cells allot   create TPGY 12 cells allot
10.0 TPW 0 T-SET  20.0 TPW 1 T-SET  30.0 TPW 2 T-SET  40.0 TPW 3 T-SET
50.0 TPW 4 T-SET  60.0 TPW 5 T-SET  70.0 TPW 6 T-SET  80.0 TPW 7 T-SET

\ forward composition: gather wte[ids] then add the sliced wpe[0:3]
EE EID TPW TPY 3 4 2 TOKPOS-EMBED
TPY 0 T-GET  0.5 f+ f>s  15 T=        \ Y[0]=E[2]+wpe[0]=[5,6]+[10,20]=[15,26]
TPY 1 T-GET  0.5 f+ f>s  26 T=
TPY 2 T-GET  0.5 f+ f>s  31 T=        \ Y[1]=E[0]+wpe[1]=[1,2]+[30,40]=[31,42]
TPY 4 T-GET  0.5 f+ f>s  55 T=        \ Y[2]=E[2]+wpe[2]=[5,6]+[50,60]=[55,66]

\ backward: the ADD copies dY to BOTH branches - dwte via scatter-add, dwpe via slice-add
0.0 EDE 6 T-FILL  0.0 TPDW 8 T-FILL  1.0 TPDY 6 T-FILL
EID TPDY EDE 3 2 EMB-SCATTER-ADD     \ dwte: token 2 accumulates (used twice)
TPDY TPDW 3 2 WPE-SLICE-ADD          \ dwpe: rows 0..2 get dY, row 3 untouched
EDE 4 T-GET   0.5 f+ f>s  2 T=        \ dwte[2,0]=2
TPDW 0 T-GET  0.5 f+ f>s  1 T=        \ dwpe[0,0]=1
TPDW 4 T-GET  0.5 f+ f>s  1 T=        \ dwpe[2,0]=1
TPDW 6 T-GET  0.5 f+ f>s  0 T=        \ dwpe[3,0]=0 (position 3 outside the T=3 slice)

\ central finite differences through the composition reach BOTH tables (L = sum Y)
: TPE-LSUM ( -- r )  EE EID TPW TPY 3 4 2 TOKPOS-EMBED  TPY 6 T-SUM ;
5.001 EE 4 T-SET   TPE-LSUM   4.999 EE 4 T-SET   TPE-LSUM
f-  0.002 f/  0.5 f+ f>s  2 T=       \ dL/dwte[2,0]=2 (token 2 feeds two outputs)
5.0 EE 4 T-SET
50.001 TPW 4 T-SET  TPE-LSUM   49.999 TPW 4 T-SET  TPE-LSUM
f-  0.002 f/  0.5 f+ f>s  1 T=       \ dL/dwpe[2,0]=1 (position 2 feeds one output)
50.0 TPW 4 T-SET
70.001 TPW 6 T-SET  TPE-LSUM   69.999 TPW 6 T-SET  TPE-LSUM
f-  0.002 f/  0.5 f+ f>s  0 T=       \ dL/dwpe[3,0]=0 (row 3 is past the slice)
70.0 TPW 6 T-SET

\ a sequence length past the table extent is a named reject, never a clamp
: TPE-OVERRUN ( -- )  TPW TPGY 5 4 2 WPE-SLICE ;
' TPE-OVERRUN E-WPE-EXTENT TTHROWS

T-REPORT

;package
