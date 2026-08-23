\ judge/check.f - run every code-generator corpus into the live result table.

require tools/judge/row.f
require tools/judge/corpus1.f
require tools/judge/corpus2.f
require tools/judge/corpus3.f
require tools/judge/corpus4.f
require tools/judge/corpus5.f

package JUDGE-CHECK

public

: JUDGE-ALL ( -- )
   JUDGE-ROW:RESET
   JUDGE-CORPUS1:JUDGE
   JUDGE-CORPUS2:JUDGE
   JUDGE-CORPUS3:JUDGE
   JUDGE-CORPUS4:JUDGE
   JUDGE-CORPUS5:JUDGE ;

;package
