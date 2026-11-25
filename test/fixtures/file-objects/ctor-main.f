\ ctor-main.f - reopens ctor-sum.f's constructor package, which a build refuses
\ with E-CTOR-PROTECTED whether it compiles ctor-sum.f or links its object.

require test/fixtures/file-objects/ctor-sum.f

package FILE--OBJECTS--CTOR ;package
: MAIN ( -- ) ;
