/+
var x = [
  null,
  [null, new A(1, 2), null],
  null
];
+/

extern class A {
  this(int x, int y)
}

A?[]?[] x = { null, { null, { 1, 2 }, null }, null }
