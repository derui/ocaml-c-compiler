Return 0 if passed 0 into occ
  $ occ 'b = a = 5; b + a;' > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [10]

Return 0 if passed 0 into occ
  $ occ 'b = 5; c = b + 8; a = c * b;' > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [65]
