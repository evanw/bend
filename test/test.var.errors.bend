/+
error on line 6: type var cannot be matched with int, only with var?
error on line 8: type var[] cannot be matched with int[], only with var[]?
+/

if var x = 0 {}
int[] x = {}
if var[] x = x {}

var? y = null
if var y = y {}
var[]? z = {}
if var[] z = z {}
