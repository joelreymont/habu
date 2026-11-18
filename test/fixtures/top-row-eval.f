split-str
\ top-row-eval.f - test/top-row-storage-test.f CASE-EVAL-SITE: the store on the
\ evaluated string's third line puts a pointer in the raw cell TS-V; it is placed
\ at this `evaluate`, its line quoted in an evaluated note.
S\" 1 drop\n\nhere TS-V !" evaluate
