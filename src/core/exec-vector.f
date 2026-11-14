\ A `defer` holds DEFER-UNSET until `is` installs a word in it. Loads after
\ rejection.f, which prints the rejection, and before checker.f, the first file
\ that declares a `defer`, so the status is CHECKER-REJECT-RC's value.
70 constant EXEC-VECTOR-RC

: DEFER-UNSET ( -- )
   s" E-DEFER-UNSET" REJECTION:OPEN
   s" a `defer` ran with no word installed" REJECTION:EMIT-STR
   REJECTION:HELP
   s" install a word in the `defer` before it runs, as `[: WORD ;] is NAME`"
   REJECTION:EMIT-STR
   EXEC-VECTOR-RC REJECTION:STOP ;
