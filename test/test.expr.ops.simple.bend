/+
// int
-1;
~1;

// bool
true;

// float
-1.5;

// int vs int
1 + 2;
1 - 2;
1 * 2;
1 / 2 | 0;
1 % 2;
1 << 2;
1 >>> 2;
1 | 2;
1 & 2;
1 ^ 2;
1 === 2;
1 !== 2;
1 <= 2;
1 >= 2;
1 < 2;
1 > 2;

// bool vs bool
true === false;
true !== false;

// int vs float
1 + 2.5;
1 - 2.5;
1 * 2.5;
1 / 2.5;
1 === 2.5;
1 !== 2.5;
1 <= 2.5;
1 >= 2.5;
1 < 2.5;
1 > 2.5;

// float vs int
1.5 + 2;
1.5 - 2;
1.5 * 2;
1.5 / 2;
1.5 === 2;
1.5 !== 2;
1.5 <= 2;
1.5 >= 2;
1.5 < 2;
1.5 > 2;

// float vs float
1.5 + 2.5;
1.5 - 2.5;
1.5 * 2.5;
1.5 / 2.5;
1.5 === 2.5;
1.5 !== 2.5;
1.5 <= 2.5;
1.5 >= 2.5;
1.5 < 2.5;
1.5 > 2.5;

// string vs string
"1" + "2";
"1" === "2";
"1" !== "2";
"1" <= "2";
"1" >= "2";
"1" < "2";
"1" > "2";

// array
[1, 2, 3].indexOf(1) >= 0;
[1, 2, 3].indexOf(1) < 0;

// string
"123".indexOf("1") >= 0;
"123".indexOf("1") < 0;

// class
var Foo = function() {};
new Foo() instanceof Foo;
!(new Foo() instanceof Foo);

// Tertiary
true ? 1 : 2;
true ? 1 : 2.5;
true ? 1.5 : 2;
true ? 1.5 : 2.5;
+/

// int
-1
~1

// bool
not false

// float
-1.5

// int vs int
1 + 2
1 - 2
1 * 2
1 / 2
1 % 2
1 << 2
1 >> 2
1 | 2
1 & 2
1 ^ 2
1 == 2
1 != 2
1 <= 2
1 >= 2
1 < 2
1 > 2

// bool vs bool
true == false
true != false

// int vs float
1 + 2.5
1 - 2.5
1 * 2.5
1 / 2.5
1 == 2.5
1 != 2.5
1 <= 2.5
1 >= 2.5
1 < 2.5
1 > 2.5

// float vs int
1.5 + 2
1.5 - 2
1.5 * 2
1.5 / 2
1.5 == 2
1.5 != 2
1.5 <= 2
1.5 >= 2
1.5 < 2
1.5 > 2

// float vs float
1.5 + 2.5
1.5 - 2.5
1.5 * 2.5
1.5 / 2.5
1.5 == 2.5
1.5 != 2.5
1.5 <= 2.5
1.5 >= 2.5
1.5 < 2.5
1.5 > 2.5

// string vs string
'1' + '2'
'1' == '2'
'1' != '2'
'1' <= '2'
'1' >= '2'
'1' < '2'
'1' > '2'

// array
1 in { 1, 2, 3 }
1 not in { 1, 2, 3 }

// string
'1' in '123'
'1' not in '123'

// class
class Foo {}
Foo() is Foo
Foo() is not Foo

// Tertiary
1 if true else 2
1 if true else 2.5
1.5 if true else 2
1.5 if true else 2.5
