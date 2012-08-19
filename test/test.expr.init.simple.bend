/+
// class vec2
var vec2 = function(x, y) {
  this.x = x;
  this.y = y;
};
var v = new vec2(0, 1);
var i = 1.5 | 0;
var b = false;
var f = 1.5;
var s = "1";
var ints = [1, null];
var fns = [function() {
  return 0;
}];
+/

class vec2 { int x, y }
vec2 v = { 0, 1 }

int i = { 1.5 }
bool b = { false }
float f = { 1.5 }
string s = { '1' }

int?[] ints = { 1, null }
int fn()[] fns = { -> 0 }
