\ host-lint-test.f - focused coverage for host-lint policy helpers.
\
\ Load after tools/host-lint.f.

variable HLT-N

: HLT-ASSERT ( bool -- )
   if
      HLT-N @ 1+ HLT-N !
      exit
   then
   s" host-lint-test failed at assertion " type HLT-N @ . cr
   s" host-lint-test failed" 1 die ;

: HLT-PATH-POLICY ( -- )
   s" ./tools/helper.py" HOST-PATH-BAD? HLT-ASSERT
   s" ./bench/llm/drive-python.f" HOST-PATH-BAD? 0= HLT-ASSERT
   s" ./bench/llm/drive-python-test.f" HOST-PATH-BAD? 0= HLT-ASSERT
   s" ./bench/llm/test.py" HOST-PATH-BAD? HLT-ASSERT ;

: HLT-CONTENT-POLICY ( -- )
   s" ./tools/new-host.sh" HOST-SCAN-CONTENT? HLT-ASSERT
   s" ./bench/llm/drive-foreign-lib.f" HOST-SCAN-CONTENT? 0= HLT-ASSERT
   s" ./FILEMAP.md" HOST-SCAN-CONTENT? 0= HLT-ASSERT ;

: HLT-BENCH-SHELL-POLICY ( -- )
   s" ./bench/llm/drive-js.sh" HOST-BENCH-DRIVER-SHELL? HLT-ASSERT
   s" ./bench/llm/drive-python.sh" HOST-BENCH-DRIVER-SHELL? HLT-ASSERT
   s" ./bench/llm/drive-python.f" HOST-BENCH-DRIVER-SHELL? 0= HLT-ASSERT
   s" ./bench/llm/drive-js.sh" HOST-BENCH-BASELINE? 0= HLT-ASSERT
   s" ./bench/llm/drive-js.sh" HOST-RETIRED-SHELL? HLT-ASSERT ;

: HLT-MAIN ( -- )
   1 HLT-N !
   HLT-PATH-POLICY
   HLT-CONTENT-POLICY
   HLT-BENCH-SHELL-POLICY
   s" host-lint-test: ok (" type HLT-N @ 1- . s"  assertions)" type cr ;

HLT-MAIN
