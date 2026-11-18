split-str
\ empty.f - a declaration with an empty text is rejected with a record, rc 70
\ (src/lib/error-test.f).

require src/lib/error.f

-9674 s" " ERROR:CODE E-EMPTY-TEXT
