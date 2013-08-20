This project is a proof of concept for a system of tracking implicit
information flows. As a prototype, it is unlikely to be suitable for use in
production.

ConcreteTester.scala and AbstractTester.scala perform a suite of tests that
demonstrate how the code base can be used and that it performs, at least for
simple applications.

The grammar of the language interpreted in this code base is as follows:

pr ∈ Program = s∗
s ∈ Statement ::= (label l)
                | (goto l)
                | (:= v e)
                | (if e l)
e ∈ Exp ::= (+ e e)
          | (* e e)
          | (= e e)
          | vl
          | v
l ∈ Label = _v
vl ∈ Value = Z
v ∈ Var