package INTELADT
public

STRUCTURE pair 0 FIELD first n FIELD second n ;STRUCTURE
SUMTYPE outcome 0 VARIANT absent ;VARIANT VARIANT present n ;VARIANT ;SUMTYPE

private

: ASSERT= ( n n -- )
   {: got:n want:n :}
   got want <> if s" ADT assert mismatch" 70 die then ;


\ typed-local-lint: allow-bare-local - the pinned engine infers the wide bundle.
: KEEP-PAIR ( pair -- pair )
   {: value :} value ;


: ADD-PAIR ( n n -- n )
   INTELADT-PAIR:MAKE KEEP-PAIR INTELADT-PAIR:UNMAKE + ;


: TAKE-OPTION ( outcome -- n )
   MATCH outcome absent OF -1 ENDOF present OF ENDOF ;MATCH ;


: PRESENT-RT ( n -- n )
   INTELADT-OUTCOME:PRESENT TAKE-OPTION ;


: ABSENT-RT ( -- n )
   INTELADT-OUTCOME:ABSENT TAKE-OPTION ;

public

: RUN ( -- )
   7 11 ADD-PAIR 18 ASSERT=
   23 PRESENT-RT 23 ASSERT=
   ABSENT-RT -1 ASSERT=
   s" real-adt-runtime: ok" type cr ;

;package

INTELADT:RUN
