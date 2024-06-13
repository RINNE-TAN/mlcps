# mlcps
mlcps is a compiler that transforms a subset of the ML language into RISC-V Assembly language. It includes several optimizations such as CPS (Continuation-Passing Style) transformation, closure conversion, hoisting, register spilling (Linear Scan Algorithm), and code generation. 
# Example
- **Input** :

    ```haskell
  let ml =
        LetFix
          "sum"
          "x"
          ( If0
              (Var "x")
              (Num 0)
              ( Prim
                  Add
                  [ Var "x",
                    App
                      (Var "sum")
                      ( Prim
                          Sub
                          [Var "x", Num 1]
                      )
                  ]
              )
          )
          (App (Var "sum") (Num 10))
    ```
- **Ouput**

    [main.s](runtime/main.s)