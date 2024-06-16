# mlcps
mlcps is a compiler that transforms a subset of the ML language into RISC-V Assembly language. It includes several optimizations such as CPS (Continuation-Passing Style) transformation, closure conversion, hoisting, register spilling (Linear Scan Algorithm), and code generation. 
# Example
- **Input** :

    ```haskell
  let ml =
        LetFix
          "filter"
          ["num", "f"]
          ( If0
              (App (Var "f") [Var "num"])
              ( App
                  (Var "filter")
                  [ Prim Sub [Var "num", Num 1],
                    Var "f"
                  ]
              )
              (Var "num")
          )
          ( LetFix
              "f"
              ["num"]
              ( LetFix
                  "help"
                  ["i"]
                  ( If0
                      (Var "i")
                      (Num 0)
                      ( If0
                          ( Prim
                              Sub
                              [ Var "num",
                                Prim Mul [Var "i", Var "i"]
                              ]
                          )
                          (Num 1)
                          (App (Var "help") [Prim Sub [Var "i", Num 1]])
                      )
                  )
                  (App (Var "help") [Var "num"])
              )
              (App (Var "filter") [Num 99, Var "f"])
          )
    ```
- **Ouput**

    [main.s](runtime/main.s)