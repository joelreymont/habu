\ A retained generic quotation with 33 independent record field variables.
VALUE-RECORD box value a END-VALUE-RECORD
defer C2V-D ( box box box box box box box box box box box box box box box box box box box box box box box box box box box box box box box box box -- )
: C2V-DROP ( box box box box box box box box box box box box box box box box box box box box box box box box box box box box box box box box box -- ) drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop ;
: C2V-INSTALL ( -- ) [: C2V-DROP ;] is C2V-D ;
C2V-INSTALL
