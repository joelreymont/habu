require test/fixtures/native-file-unit.f
package FILE-DOWNSTREAM
public
: VALUE ( -- n ) FILE-UNIT:VALUE 1+ ;
;package
