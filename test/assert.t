Return 0 if passed 0 into occ
  $ occ '0;' > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp

Return 42 if passed 42 into occ
  $ occ '42;' > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [42]

Allow to plus/minus operation
  $ occ "5+20-4;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [21]

Allow to plus/minus operation
  $ occ " 12 + 34 - 5 ;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [41]

Allow to echo error message
  $ occ " 12 + foo - 5;" > tmp.s
   12 + foo - 5;
         ^ expect failed ';'
  [1]
Allow to calculate
  $ occ "5+6*7;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [47]

Allow to calculate
  $ occ "5*(9-6);" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [15]

Allow to calculate
  $ occ "(3+5)/2;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [4]

Allow to use unary
  $ occ "-10+ +20;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [10]

Allow to use unary
  $ occ "+20- -15;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [35]
