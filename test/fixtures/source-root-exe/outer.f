\ outer.f - test/source-root-exe-test.f's library that requires another by a
\ root-relative name and answers that file's mark beside its own.
require test/fixtures/source-root-exe/inner.f

package SOURCE-ROOT-EXE-OUTER
public
: MARK ( -- n ) 6 ;
: INNER-MARK ( -- n ) SOURCE-ROOT-EXE-INNER:MARK ;
;package
