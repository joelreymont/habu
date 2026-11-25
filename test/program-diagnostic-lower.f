\ A named enum fetch rejection identifies the checked definition.
package PROGRAM-DIAG-LOWER
ENUM color red green ;ENUM
ENUM lcnamed
   VARIANT empty ;VARIANT
   VARIANT shade FIELD value color ;VARIANT
;ENUM
s" LC-NAMED-BAD ( ^lcnamed -- color ) @" CHECK! drop
;package

s" ok" type cr
