Return 0 if passed 0 into occ
  $ occ 0 > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp

Return 42 if passed 42 into occ
  $ occ 42 > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [42]

Allow to plus/minus operation
  $ occ "5+20-4" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [21]
