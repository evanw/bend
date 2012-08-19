/+
error on line 12: cannot implicitly convert from float to int
error on line 13: cannot implicitly convert from float to int
error on line 14: cannot implicitly convert from float to int
error on line 14: cannot implicitly convert from float to int
+/

int i
float f

i = i if i > i else i
i = i if i > f else f
i = f if f > i else i
i = f if f > f else f
f = i if i > i else i
f = i if i > f else f
f = f if f > i else i
f = f if f > f else f
