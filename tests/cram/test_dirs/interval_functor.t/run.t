  $ ff test_interval.fun -o test_interval.cpp
  49:13-49:13 Can't unify `() 3.t` with `() 0.int`
  Compilation failed!

  $ cat test_interval.cpp
  cat: test_interval.cpp: No such file or directory
  [1]

  $ $FF test_interval.fun
  49:13-49:13 Can't unify `() 3.t` with `() 0.int`
  Compilation failed!cc1plus: fatal error: test_interval.fun.cpp: No such file or directory
  compilation terminated.
  [1]
  $ ./test_interval.fun.out
  ./test_interval.fun.out: not found
  [127]

