

  $ ff --help
  ff: a functional programming language build for fun
    -o Set output file name
    --version Show version number
    --debug Enable debug
    --stdout Output to stdout
    -help  Display this list of options
    --help  Display this list of options

  $ ff simple.fun

  $ cat a.out
  
  #include<stdio.h>
  int main()
  {
    printf("Hello World");
  }

  $ ff simple.fun --stdout
  
  #include<stdio.h>
  int main()
  {
    printf("Hello World");
  }

  $ ff simple.fun -o simple.out

  $ cat simple.out
  
  #include<stdio.h>
  int main()
  {
    printf("Hello World");
  }

  $ ff simple.fun -o simple1.out --debug

  $ ls
  a.out
  simple.fun
  simple.out
  simple1.out
  simple1.out.c_dbg
  simple1.out.closure
  simple1.out.lambda
  simple1.out.parsing
  simple1.out.typed

  $ cat simple1.out
  
  #include<stdio.h>
  int main()
  {
    printf("Hello World");
  }

  $ cat simple1.out.closure
  Lifted main expression: 
  (EModObject ((FSimple x (EConst (CInt 1)))))
  
  Global C functions: 

  $ cat simple1.out.lambda
  (EModObject ((FSimple x (EConst (CInt 1)))))

  $ cat simple1.out.parsing
  ((TopLet x (EConst (CInt 1))))

  $ cat simple1.out.typed
  
  
  let x = (1 is () 0.int)



