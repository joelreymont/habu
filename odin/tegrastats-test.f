\ tegrastats-test.f - oracle test against src/tegrastats.zig snapshots.
\ Part of the TEGRA package: reopens `package TEGRA` and calls SUMMARY unqualified.
\ Run: cat lib/errors.f lib/string.f lib/test.f lib/float.f lib/fmt.f odin/tegrastats.f odin/tegrastats-test.f | bin/hb

package TEGRA
private

create LINE
   82 c, 65 c, 77 c, 32 c,                                       \ "RAM "
   53 c, 49 c, 50 c, 51 c, 47 c, 49 c, 53 c, 54 c, 53 c, 53 c, 77 c, 66 c, 32 c, \ "5123/15655MB "
   40 c, 108 c, 102 c, 98 c, 32 c, 56 c, 120 c, 52 c, 77 c, 66 c, 41 c, 32 c,    \ "(lfb 8x4MB) "
   83 c, 87 c, 65 c, 80 c, 32 c,                                 \ "SWAP "
   48 c, 47 c, 55 c, 56 c, 50 c, 55 c, 77 c, 66 c, 32 c,         \ "0/7827MB "
   67 c, 80 c, 85 c, 32 c,                                       \ "CPU "
   91 c, 50 c, 53 c, 37 c, 64 c, 49 c, 52 c, 57 c, 55 c, 44 c,   \ "[25%@1497,"
   49 c, 50 c, 37 c, 64 c, 49 c, 52 c, 57 c, 55 c, 44 c,         \ "12%@1497,"
   56 c, 37 c, 64 c, 49 c, 52 c, 57 c, 55 c, 44 c,              \ "8%@1497,"
   51 c, 37 c, 64 c, 49 c, 52 c, 57 c, 55 c, 93 c, 32 c,        \ "3%@1497] "
   71 c, 82 c, 51 c, 68 c, 95 c, 70 c, 82 c, 69 c, 81 c, 32 c,  \ "GR3D_FREQ "
   52 c, 53 c, 37 c, 32 c,                                       \ "45% "
   99 c, 118 c, 48 c, 64 c, 45 c, 50 c, 53 c, 54 c, 67 c, 32 c, \ "cv0@-256C "
   99 c, 112 c, 117 c, 64 c, 53 c, 50 c, 46 c, 53 c, 67 c, 32 c, \ "cpu@52.5C "
   103 c, 112 c, 117 c, 64 c, 53 c, 49 c, 46 c, 53 c, 67 c, 32 c, \ "gpu@51.5C "
   116 c, 106 c, 64 c, 53 c, 50 c, 46 c, 53 c, 67 c, 32 c,      \ "tj@52.5C "
   86 c, 68 c, 68 c, 95 c, 73 c, 78 c, 32 c,                    \ "VDD_IN "
   53 c, 54 c, 55 c, 56 c, 109 c, 87 c, 47 c, 53 c, 54 c, 55 c, 56 c, 109 c, 87 c, \ "5678mW/5678mW"
here LINE - constant LINE-LEN

\ partial line: "RAM 100/200MB CPU [10%@1,30%@1]"
create LINE2
   82 c, 65 c, 77 c, 32 c, 49 c, 48 c, 48 c, 47 c, 50 c, 48 c, 48 c, 77 c, 66 c, 32 c,
   67 c, 80 c, 85 c, 32 c, 91 c, 49 c, 48 c, 37 c, 64 c, 49 c, 44 c, 51 c, 48 c, 37 c, 64 c, 49 c, 93 c,
here LINE2 - constant LINE2-LEN

\ temp-only: "cpu@40.0C gpu@61.5C soc0@-256C"
create LINE3
   99 c, 112 c, 117 c, 64 c, 52 c, 48 c, 46 c, 48 c, 67 c, 32 c,
   103 c, 112 c, 117 c, 64 c, 54 c, 49 c, 46 c, 53 c, 67 c, 32 c,
   115 c, 111 c, 99 c, 48 c, 64 c, 45 c, 50 c, 53 c, 54 c, 67 c,
here LINE3 - constant LINE3-LEN

: RUN ( -- )
   T-RESET
   LINE LINE-LEN SUMMARY
   s" ram=10532/15655MB swap=0/7827MB cpu=12.0% gpu=45.0% temp=52.5C power=5678mW" T$=
   LINE2 LINE2-LEN SUMMARY
   s" ram=100/200MB cpu=20.0% " T$=
   LINE3 LINE3-LEN SUMMARY
   s" temp=61.5C " T$=
   s" hello world no fields here" SUMMARY  s" " T$=    \ no fields -> empty render
   s" " SUMMARY  s" " T$= ;

RUN
T-REPORT

end-package
