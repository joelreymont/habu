\ render-test.f - coverage for the render.f buffer formatters and key/value DSL.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/float.f lib/test.f lib/render.f lib/render-test.f

\ expected "k,3\n" built in the SB builder (separate from the RB render buffer)
: EXP-CVN ( -- ptr u8 n ) SB-RESET s" k,3" SB-APPEND 10 SB-APPEND-C SB$ ;

: RND-RUN ( -- )
   T-RESET
   RB-RESET 12345 RB# RB$ s" 12345" STR= T-ASSERT
   RB-RESET -42 RB# RB$ s" -42" STR= T-ASSERT
   RB-RESET 7 RB-3 RB$ s" 007" STR= T-ASSERT
   RB-RESET 16666667 RB-MILLI3 RB$ s" 16666.667" STR= T-ASSERT
   RB-RESET 1 60 RB-FIXED3 RB$ s" 0.016" STR= T-ASSERT
   RB-RESET 0 5 RB-FIXED3 RB$ s" 0.000" STR= T-ASSERT
   RB-RESET 1 2 RB-RATIO4 RB$ s" 0.5000" STR= T-ASSERT
   RB-RESET 1.25 RB-FFIX3 RB$ s" 1.250" STR= T-ASSERT
   RB-RESET 0 0= RB-BOOL RB$ s" true" STR= T-ASSERT
   RB-RESET 0 0= 0= RB-BOOL RB$ s" false" STR= T-ASSERT
   RB-RESET s" k" 3 CVN RB$ EXP-CVN STR= T-ASSERT ;

RND-RUN
T-REPORT
