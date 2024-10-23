#qType

- [x] Implement length-indexed lists
- [x] Modify previous qubit implementation to use these lists
- [x] Generalize keys to access lists indexes
- [x] Implement a type-safe way to access sublists of a list
- [x] Add Slist to Virt definition
- [x] Implement type errors for invalid sublists
- [x] QAct ~ ReaderT IO monad to handle operations on qubits
- [x] Lift Qop functions to QAct
- [x] Implement a monadic way to easily combine QAct's
- [ ] Refactor type errors to be more informative
- [ ] Make less verbose calls to app function (now it needs to receive an explicit SList definition)
  - [ ] Idea 1: Use quasiquotation
  - [ ] Idea 2: Polyvariadic `add` function with associated type families (is this possible, though?)
- [ ] (?) Refactor length-indexed lists operations as first-class type families
