\ top-row-eval.f - test/top-row-storage-test.f CASE-EVAL-SITE: the store on the
\ evaluated string's third line puts a pointer in the raw cell TS-V; the string
\ lies outside this file's bytes, so its refusal names this file and no line.
S\" 1 drop\n\nhere TS-V !" evaluate
