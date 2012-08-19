/+
var f = function() {
  var splat = Array.prototype.slice.call(arguments);
};
f();
f(1);
f(1, 2);
var g = function(before) {
  var splat = Array.prototype.slice.call(arguments, 1);
};
g("");
g("", 1);
g("", 1, 2);
var h = function() {
  var splat = Array.prototype.slice.call(arguments, 0, -1);
  var after = arguments[arguments.length - 1];
};
h(false);
h(1, false);
h(1, 2, false);
var i = function(before) {
  var splat = Array.prototype.slice.call(arguments, 1, -1);
  var after = arguments[arguments.length - 1];
};
i("", false);
i("", 1, false);
i("", 1, 2, false);
var j = function(a, b, c) {
  var splat = Array.prototype.slice.call(arguments, 3, -3);
  var x = arguments[arguments.length - 3];
  var y = arguments[arguments.length - 2];
  var z = arguments[arguments.length - 1];
};
j("", "", "", false, false, false);
j("", "", "", 1, false, false, false);
j("", "", "", 1, 2, false, false, false);
+/

void f(int[] splat...) {}
f()
f(1)
f(1, 2)

void g(string before, int[] splat...) {}
g('')
g('', 1)
g('', 1, 2)

void h(int[] splat..., bool after) {}
h(false)
h(1, false)
h(1, 2, false)

void i(string before, int[] splat..., bool after) {}
i('', false)
i('', 1, false)
i('', 1, 2, false)

void j(string a, string b, string c, int[] splat..., bool x, bool y, bool z) {}
j('', '', '', false, false, false)
j('', '', '', 1, false, false, false)
j('', '', '', 1, 2, false, false, false)
