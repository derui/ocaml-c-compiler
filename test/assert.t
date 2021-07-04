Return 0 if passed 0 into occ
  $ occ 0 > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp

Return 42 if passed 42 into occ
  $ occ 42 > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [42]

