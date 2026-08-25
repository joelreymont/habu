\ sha-check.fs — standalone self-test of sha256.fs against FIPS-180 vectors.
\ Prints the summed digest-mismatch count over 3 vectors (abc, 56-char,
\ 100-byte): '0' iff all match. Used by the native gate. (no emit in the
\ standalone -> output via '.'; vectors are canonical test fixtures.)
create DG 32 allot
create VABC 97 c, 98 c, 99 c, 
create EABC 186 c, 120 c, 22 c, 191 c, 143 c, 1 c, 207 c, 234 c, 65 c, 65 c, 64 c, 222 c, 93 c, 174 c, 34 c, 35 c, 176 c, 3 c, 97 c, 163 c, 150 c, 23 c, 122 c, 156 c, 180 c, 16 c, 255 c, 97 c, 242 c, 0 c, 21 c, 173 c, 
create V56 97 c, 98 c, 99 c, 100 c, 98 c, 99 c, 100 c, 101 c, 99 c, 100 c, 101 c, 102 c, 100 c, 101 c, 102 c, 103 c, 101 c, 102 c, 103 c, 104 c, 102 c, 103 c, 104 c, 105 c, 103 c, 104 c, 105 c, 106 c, 104 c, 105 c, 106 c, 107 c, 105 c, 106 c, 107 c, 108 c, 106 c, 107 c, 108 c, 109 c, 107 c, 108 c, 109 c, 110 c, 108 c, 109 c, 110 c, 111 c, 109 c, 110 c, 111 c, 112 c, 110 c, 111 c, 112 c, 113 c, 
create E56 36 c, 141 c, 106 c, 97 c, 210 c, 6 c, 56 c, 184 c, 229 c, 192 c, 38 c, 147 c, 12 c, 62 c, 96 c, 57 c, 163 c, 60 c, 228 c, 89 c, 100 c, 255 c, 33 c, 103 c, 246 c, 236 c, 237 c, 212 c, 25 c, 219 c, 6 c, 193 c, 
create V100 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 97 c, 
create E100 40 c, 22 c, 89 c, 120 c, 136 c, 228 c, 160 c, 211 c, 163 c, 107 c, 130 c, 184 c, 51 c, 22 c, 171 c, 50 c, 104 c, 14 c, 184 c, 240 c, 15 c, 140 c, 211 c, 185 c, 4 c, 214 c, 129 c, 36 c, 109 c, 40 c, 90 c, 14 c, 

: DIFF {: e :} 0 32 0 DO DG i + c@ e i + c@ <> if drop 1 then LOOP ;

: CHK {: a u e :} a u DG SHA256 e DIFF ;

: RUN VABC 3 EABC CHK  V56 56 E56 CHK  V100 100 E100 CHK  + +  . ;
RUN
