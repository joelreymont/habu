\ native-wide-frame.f - immutable locals across conditional calls.
\ The independent top-page frame encoding case is in native-emit.f.
require src/lib/test.f

1 set-tier

package NWF-FIXTURE
public

variable V
: TICK ( -- ) 1 V +! ;

: WIDE ( -- n )
   V @ 0 + {: a0:n :} V @ 1 + {: a1:n :} V @ 2 + {: a2:n :} V @ 3 + {: a3:n :}
   V @ 4 + {: a4:n :} V @ 5 + {: a5:n :} V @ 6 + {: a6:n :} V @ 7 + {: a7:n :}
   V @ 8 + {: a8:n :} V @ 9 + {: a9:n :} V @ 10 + {: a10:n :}
   V @ 11 + {: a11:n :} V @ 12 + {: a12:n :} V @ 13 + {: a13:n :}
   V @ 14 + {: a14:n :} V @ 15 + {: a15:n :} V @ 16 + {: a16:n :}
   V @ 17 + {: a17:n :} V @ 18 + {: a18:n :} V @ 19 + {: a19:n :}
   V @ 20 + {: a20:n :} V @ 21 + {: a21:n :} V @ 22 + {: a22:n :}
   V @ 23 + {: a23:n :} V @ 24 + {: a24:n :} V @ 25 + {: a25:n :}
   V @ 26 + {: a26:n :} V @ 27 + {: a27:n :} V @ 28 + {: a28:n :}
   V @ 29 + {: a29:n :} V @ 30 + {: a30:n :} V @ 31 + {: a31:n :}
   V @ 32 + {: a32:n :} V @ 33 + {: a33:n :} V @ 34 + {: a34:n :}
   V @ 35 + {: a35:n :} V @ 36 + {: a36:n :} V @ 37 + {: a37:n :}
   V @ 38 + {: a38:n :} V @ 39 + {: a39:n :} V @ 40 + {: a40:n :}
   V @ 41 + {: a41:n :} V @ 42 + {: a42:n :} V @ 43 + {: a43:n :}
   V @ 44 + {: a44:n :} V @ 45 + {: a45:n :} V @ 46 + {: a46:n :}
   V @ 47 + {: a47:n :} V @ 48 + {: a48:n :} V @ 49 + {: a49:n :}
   V @ 50 + {: a50:n :} V @ 51 + {: a51:n :} V @ 52 + {: a52:n :}
   V @ 53 + {: a53:n :} V @ 54 + {: a54:n :} V @ 55 + {: a55:n :}
   V @ 56 + {: a56:n :} V @ 57 + {: a57:n :} V @ 58 + {: a58:n :}
   V @ 59 + {: a59:n :}
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then
   a0 a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 +
   a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + a25 +
   a26 + a27 + a28 + a29 + a30 + a31 + a32 + a33 + a34 + a35 + a36 + a37 +
   a38 + a39 + a40 + a41 + a42 + a43 + a44 + a45 + a46 + a47 + a48 + a49 +
   a50 + a51 + a52 + a53 + a54 + a55 + a56 + a57 + a58 + a59 + ;

;package

package NWF-TEST
private

: WIDE-CASE ( -- )
   s" and runs: every call taken, every local read back" T-LABEL
   1 NWF-FIXTURE:V !
   NWF-FIXTURE:WIDE 60 1770 + T=
   NWF-FIXTURE:V @ 1 64 + T= ;

public

: RUN ( -- )
   WIDE-CASE ;

;package

T-RESET
NWF-TEST:RUN
T-REPORT
