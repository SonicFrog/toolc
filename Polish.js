function Expr() {this.top = null;
}
Expr.prototype.getNew = function() { 
var left;
var right;
left = new Leaf().init(4);
right = new Leaf().init(5);
this.top = new Fork().init(left, right, 1);
left = this.top;
right = new Leaf().init(2);
this.top = new Fork().init(left, right, 3);
right = new Leaf().init(34);
left = new Leaf().init(1367);
this.top = new Fork().init(new Fork().init(left, right, 4), this.top, 2);
return this;
}
Expr.prototype.printAll = function() { 
document.write(("Prefix : " + this.top.prePrint())+ "<br/>");
document.write(("Postfix : " + this.top.postPrint())+ "<br/>");
document.write(("Infix : " + this.top.inPrint())+ "<br/>");
document.write(("Result : " + this.top.eval())+ "<br/>");
return "* End *";
}

function Tree() {}
Tree.prototype.eval = function() { 
document.write("Error : called eval on abstract class"+ "<br/>");
return 0;
}
Tree.prototype.decode = function(op) { 
var res;
if ((op == 1)) {
res = "+";} else if ((op == 2)) {
res = "-";} else if ((op == 3)) {
res = "*";} else if ((op == 4)) {
res = "/";} else  {
document.write("Invalid code for operation"+ "<br/>");
res = "error";}
return res;
}
Tree.prototype.prePrint = function() { 
return "Default prePrint";
}
Tree.prototype.postPrint = function() { 
return "Default postPrint";
}
Tree.prototype.inPrint = function() { 
return "Default inPrint";
}

Fork.prototype = Object.create(Tree.prototype);
function Fork() {this.l = null;
this.r = null;
this.val = null;
}
Fork.prototype.init = function(left, right, value) { 
this.l = left;
this.r = right;
this.val = value;
return this;
}
Fork.prototype.eval = function() { 
var res;
if ((this.val == 1)) {
res = (this.l.eval() + this.r.eval());} else if ((this.val == 2)) {
res = (this.l.eval() - this.r.eval());} else if ((this.val == 3)) {
res = (this.l.eval() * this.r.eval());} else if ((this.val == 4)) {
res = Math.floor((this.l.eval() / this.r.eval()));} else  {
document.write("Illegal Operation"+ "<br/>");
res = 0;}
document.write(("Eval Fork : " + res)+ "<br/>");
return res;
}
Fork.prototype.prePrint = function() { 
return ((((this.decode(this.val) + " ") + this.l.prePrint()) + " ") + this.r.prePrint());
}
Fork.prototype.postPrint = function() { 
return ((((this.l.postPrint() + " ") + this.r.postPrint()) + " ") + this.decode(this.val));
}
Fork.prototype.inPrint = function() { 
return (((((("(" + this.l.inPrint()) + " ") + this.decode(this.val)) + " ") + this.r.inPrint()) + ")");
}

Leaf.prototype = Object.create(Tree.prototype);
function Leaf() {this.val = null;
}
Leaf.prototype.init = function(value) { 
this.val = value;
return this;
}
Leaf.prototype.eval = function() { 
document.write(("Eval Leaf : " + this.val)+ "<br/>");
return this.val;
}
Leaf.prototype.prePrint = function() { 
return ("" + this.val);
}
Leaf.prototype.postPrint = function() { 
return ("" + this.val);
}
Leaf.prototype.inPrint = function() { 
return ("" + this.val);
}

document.write(new Expr().getNew().printAll()+ "<br/>");