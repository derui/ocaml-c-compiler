Allow to compare
  $ occ "(3+5)/2 == 2 + 2;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [1]

Allow to compare
  $ occ "(3+5)/2 != 2 + 1;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [1]

Allow to compare with less than
  $ occ "(3+5)/2 < 8;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [1]

Allow to compare with less equal
  $ occ "(3+5)/2 <= 5;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [1]

Allow to compare with greater than
  $ occ "(3+5)/2 > 3;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
  [1]

Allow to compare with greater equal
  $ occ "(3+5)/2 >= 5;" > tmp.s
  $ cc -o tmp tmp.s
  $ ./tmp
