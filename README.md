# POPL-HW-7
Part 1 — true, false, =, and if
Start with typed-lambda.rkt. The implementation already includes a bool type, but no
expressions of bool type.
Add support for true, false, {= <Exp> <Exp>}, and {if <Exp> <Exp> <Exp>} expressions,
where = produces a boolean given two numbers, and if requires a boolean expression for
the test.
Examples:
(test (interp
