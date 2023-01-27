from sympy import *
init_printing(use_unicode=False, wrap_line=False)
x = Symbol('x')
X=integrate(log(1/sin(x))*x, x)
pprint(simplify(X))