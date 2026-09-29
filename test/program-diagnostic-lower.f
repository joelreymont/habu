\ A named enum fetch refusal identifies the checked definition.
package PROGRAM-DIAG-LOWER
ENUM color red green ;ENUM
ENUM-DECL:ED-RUN lcnamed 0
   VARIANT empty ;VARIANT
   VARIANT shade FIELD value color ;VARIANT
;ENUM
s" LC-NAMED-BAD ( ptr lcnamed -- color ) @" CHECK! drop
;package

s" ok" type cr
