require test/fixtures/native-file-types-unit.f
package FILE-TYPES-CLIENT
public
: USE-ITEM ( FILE-TYPES:item -- FILE-TYPES:item ) ;
: USE-PAIR ( FILE-TYPES:pair -- FILE-TYPES:pair ) ;
: USE-BUNDLE ( FILE-TYPES:bundle<n> -- FILE-TYPES:bundle<n> ) ;
: USE-ANSWER ( FILE-TYPES:answer<n> -- FILE-TYPES:answer<n> ) ;
: USE-LINEAR ( FILE-TYPES:token -- FILE-TYPES:token ) ;
: USE-DEFER ( n -- n ) FILE-TYPES:CHOOSE ;
: RESULT ( -- n ) 8 FILE-TYPES:CHECKED ;
;package
