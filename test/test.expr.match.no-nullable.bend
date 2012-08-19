/+
error on line 8: cannot match on nullable type B?
+/

extern class A {}
extern class B is A {}
extern A? a
if B? b = a {}
