\ tail.f - for src/lib/error-test.f: a taken code declared under the same name in a second
\ package is rejected with a record, rc 70. The package names are of one length, so the
\ qualified names differ only in their package; one lint claim.

require src/lib/error.f

package ERROR-TAIL-A
public
-9682 s" the first holder" ERROR:CODE E-X
;package

package ERROR-TAIL-B
public
ERROR-TAIL-A:E-X s" the second holder" ERROR:CODE E-X
;package
