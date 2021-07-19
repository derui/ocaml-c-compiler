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

Return statement
  $ occ 'b = 5; c = b + 8; return c * b;' > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [65]

Return only last value
  $ occ 'b = 5; c = b + 8; return c * b; return 55' > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [55]
