

  $ fun4fun --help
  fun4fun: a functional programming language build for fun
    -o Set output file name
    --debug Enable debug
    --stdout Output to stdout
    -help  Display this list of options
    --help  Display this list of options

  $ fun4fun simple.fun

  $ cat a.out
  Lifted main expression: 
  (EModObject ((FSimple x (EConst (CInt 1)))))
  
  Global C functions: 

  $ fun4fun simple.fun --stdout
  Lifted main expression: 
  (EModObject ((FSimple x (EConst (CInt 1)))))
  
  Global C functions: 

  $ fun4fun simple.fun -o simple.fun.out

  $ cat simple.fun.out
  Lifted main expression: 
  (EModObject ((FSimple x (EConst (CInt 1)))))
  
  Global C functions: 


