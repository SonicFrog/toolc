function Computer() {}
Computer.prototype.computePi = function() { 
var j;
var value;
var inter;
document.write("First method"+ "<br/>");
document.write("************"+ "<br/>");
value = new Frac().init(0, 1);
j = 0;
while((j < 3)) {
document.write(((value.toString() + " ~= ") + new Real().init(0, 10).evalFrac(value).toString())+ "<br/>");
value = value.plus(this.getTerm(j));
j = (j + 1);}
document.write(((value.toString() + " ~= ") + new Real().init(0, 10).evalFrac(value).toString())+ "<br/>");
document.write(""+ "<br/>");
document.write("Second method"+ "<br/>");
document.write("*************"+ "<br/>");
value = new Frac().init(0, 1);
inter = new Real().init(0, 10).evalFrac(value);
j = 0;
while((j < 7)) {
document.write(inter.toString()+ "<br/>");
value = this.getTerm(j);
inter = inter.plus(new Real().init(0, 10).evalFrac(value));
j = (j + 1);}
document.write(inter.toString()+ "<br/>");
return true;
}
Computer.prototype.getTerm = function(i) { 
var par;
var first;
var second;
var third;
var fourth;
first = new Frac().init(4, ((8 * i) + 1));
second = new Frac().init(2, ((8 * i) + 4));
third = new Frac().init(1, ((8 * i) + 5));
fourth = new Frac().init(1, ((8 * i) + 6));
par = first.minus(second).minus(third).minus(fourth);
return par.times(new Frac().init(1, this.sixteenPow(i)));
}
Computer.prototype.sixteenPow = function(n) { 
var res;
var i;
res = 1;
i = 0;
while((i < n)) {
i = (i + 1);
res = (res * 16);}
return res;
}

function Frac() {this.numerator = null;
this.denominator = null;
this.sign = null;
this.util = null;
}
Frac.prototype.init = function(n, d) { 
this.util = new Util();
this.numerator = this.util.abs(n);
this.denominator = this.util.abs(d);
this.sign = (((n < 0) && (d < 0)) || (((0 < n) || (n == 0)) && ((0 < d) || (d == 0))));
return this.simplify();
}
Frac.prototype.getNumerator = function() { 
return this.numerator;
}
Frac.prototype.getDenominator = function() { 
return this.denominator;
}
Frac.prototype.setPos = function(positive) { 
this.sign = positive;
return this;
}
Frac.prototype.isPos = function() { 
return this.sign;
}
Frac.prototype.simplify = function() { 
var gcd_;
if ((!(this.numerator == 0) && !(this.denominator == 0))) {
gcd_ = this.util.gcd(this.numerator, this.denominator);
if (!(gcd_ == 1)) {
this.numerator = (this.numerator / gcd_);
this.denominator = (this.denominator / gcd_);}}
return this;
}
Frac.prototype.plus = function(other) { 
var lcm;
var lfac;
var rfac;
lcm = this.util.lcm(this.denominator, other.getDenominator());
lfac = (lcm / this.denominator);
if (!this.sign) {
lfac = (0 - lfac);}
rfac = (lcm / other.getDenominator());
if (!other.isPos()) {
rfac = (0 - rfac);}
return new Frac().init(((lfac * this.numerator) + (rfac * other.getNumerator())), lcm);
}
Frac.prototype.minus = function(other) { 
return this.plus(other.negative());
}
Frac.prototype.times = function(other) { 
return new Frac().init((this.numerator * other.getNumerator()), (this.denominator * other.getDenominator())).simplify().setPos(((this.isPos() && other.isPos()) || (!this.isPos() && !other.isPos())));
}
Frac.prototype.divided = function(other) { 
return this.times(other.inverse());
}
Frac.prototype.inverse = function() { 
return new Frac().init(this.denominator, this.numerator);
}
Frac.prototype.negative = function() { 
return new Frac().init(this.numerator, this.denominator).setPos(false);
}
Frac.prototype.toString = function() { 
var result;
if (this.sign) {
result = "";} else  {
result = "-";}
return (((result + this.numerator) + "/") + this.denominator);
}

function Real() {this.integerPart = null;
this.digits = null;
this.util = null;
}
Real.prototype.init = function(intPart, digitsCount) { 
var i;
this.util = new Util();
this.integerPart = intPart;
this.digits = new Array(digitsCount);
i = 0;
while((i < digitsCount)) {
this.digits[i] = 0;
i = (i + 1);}
return this;
}
Real.prototype.getDigits = function() { 
return this.digits;
}
Real.prototype.getIntegerPart = function() { 
return this.integerPart;
}
Real.prototype.setIntegerPart = function(p) { 
this.integerPart = p;
return this;
}
Real.prototype.evalFrac = function(frac) { 
var leftover;
var i;
var den;
den = frac.getDenominator();
this.integerPart = (frac.getNumerator() / den);
if (!frac.isPos()) {
this.integerPart = (0 - this.integerPart);}
leftover = this.util.mod(frac.getNumerator(), den);
i = 0;
while((i < this.digits.length)) {
leftover = (10 * leftover);
this.digits[i] = (leftover / den);
leftover = this.util.mod(leftover, den);
i = (i + 1);}
return this;
}
Real.prototype.plus = function(other) { 
var len;
var od;
var resDig;
var carry;
var i;
var sum;
var result;
od = other.getDigits();
if ((this.digits.length < od.length)) {
len = od.length;} else  {
len = this.digits.length;}
result = new Real().init(0, len);
resDig = result.getDigits();
carry = 0;
i = (len - 1);
while(!(i < 0)) {
sum = ((this.digits[i] + od[i]) + carry);
carry = (sum / 10);
resDig[i] = this.util.mod(sum, 10);
i = (i - 1);}
return result.setIntegerPart(((this.integerPart + other.getIntegerPart()) + carry));
}
Real.prototype.toString = function() { 
var ret;
var i;
ret = (("" + this.integerPart) + ".");
i = 0;
while((i < this.digits.length)) {
ret = (ret + this.digits[i]);
i = (i + 1);}
return ret;
}

function Util() {}
Util.prototype.abs = function(v) { 
var res;
if (!(v < 0)) {
res = v;} else  {
res = (0 - v);}
return res;
}
Util.prototype.gcd = function(m_, n_) { 
var t;
var r;
var result;
var m;
var n;
m = this.abs(m_);
n = this.abs(n_);
if ((m < n)) {
t = m;
m = n;
n = t;}
r = this.mod(m, n);
if ((r == 0)) {
result = n;} else  {
result = this.gcd(n, r);}
return result;
}
Util.prototype.lcm = function(m, n) { 
return ((n * m) / this.gcd(n, m));
}
Util.prototype.mod = function(m, n) { 
return (m - (n * (m / n)));
}

if (new Computer().computePi()) {
document.write("Ok"+ "<br/>");} else  {
document.write("error"+ "<br/>");}