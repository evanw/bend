/+
error on line 22: type int cannot be matched with int, only with int?
error on line 23: type float cannot be matched with float, only with float?
error on line 24: type int[] cannot be matched with int[], only with int[]?
error on line 25: type void fn() cannot be matched with void fn(), only with void fn()?
error on line 32: type int cannot be matched with float?, only with int?
error on line 33: type float cannot be matched with int?, only with float?
error on line 34: type float[] cannot be matched with int[]?, only with float[]?
error on line 35: type void fn(int) cannot be matched with void fn()?, only with void fn(int)?
+/

extern int i = 0
extern float f = 0
extern int[] a = {}
extern void fn() v = -> {}

extern int? ni = null
extern float? nf = null
extern int[]? na = null
extern void fn()? nv = null

if int i = i {}
if float f = f {}
if int[] a = a {}
if void fn() v = v {}

if int i = ni {}
if float f = nf {}
if int[] a = na {}
if void fn() f = nv {}

if int i = nf {}
if float f = ni {}
if float[] a = na {}
if void fn(int) f = nv {}
