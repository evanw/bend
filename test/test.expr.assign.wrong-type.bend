/+
error on line 13: cannot implicitly convert from string to int
error on line 14: cannot implicitly convert from string to int
error on line 17: cannot implicitly convert from int to void fn()
error on line 18: cannot implicitly convert from int to void fn()
error on line 21: cannot implicitly convert from int to int[]
error on line 22: cannot implicitly convert from int to int[]
error on line 27: cannot implicitly convert from A to B
error on line 28: cannot implicitly convert from A to B
+/

int a = 0
a = ''
int b = ''

void fn() c = -> {}
c = 0
void fn() d = 0

int[] e = {}
e = 0
int[] f = 0

extern class A {}
extern class B is A {}
B g = B()
g = A()
B h = A()
