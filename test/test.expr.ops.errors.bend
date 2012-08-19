/+
TODO: all should fail
+/

// unary
not 1
-false
~false
~1.5
not 1.5
-'1'
~'1'
not '1'

// binary
1 == false
1 == '2'
true == 2
true == 2.5
true == '2'
1.5 == false
1.5 == '2'
'1' == 2
'1' == false
'1' == 2.5

// tertiary
1 if true else ''
'' if true else 1
