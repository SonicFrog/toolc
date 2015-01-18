function Slot() {this.value = null;
}
Slot.prototype.getVal = function() { 
return this.value;
}
Slot.prototype.isInit = function() { 
return false;
}
Slot.prototype.setVal = function(val) { 
this.value = val;
return this;
}

InitSlot.prototype = Object.create(Slot.prototype);
function InitSlot() {}
InitSlot.prototype.isInit = function() { 
return true;
}

function Grid() {this.slots = null;
}
Grid.prototype.init = function(level) { 
var index;
index = (level - level);
this.slots = new Array(9);
while((index < 9)) {
this.slots[index] = new Array(9);
index = (index + 1);}
this.slots[0][0] = new InitSlot().setVal(6);
this.slots[1][0] = new Slot().setVal(0);
this.slots[2][0] = new Slot().setVal(0);
this.slots[3][0] = new InitSlot().setVal(1);
this.slots[4][0] = new Slot().setVal(0);
this.slots[5][0] = new InitSlot().setVal(8);
this.slots[6][0] = new InitSlot().setVal(2);
this.slots[7][0] = new Slot().setVal(0);
this.slots[8][0] = new InitSlot().setVal(3);
this.slots[0][1] = new Slot().setVal(0);
this.slots[1][1] = new InitSlot().setVal(2);
this.slots[2][1] = new Slot().setVal(0);
this.slots[3][1] = new Slot().setVal(0);
this.slots[4][1] = new InitSlot().setVal(4);
this.slots[5][1] = new Slot().setVal(0);
this.slots[6][1] = new Slot().setVal(0);
this.slots[7][1] = new InitSlot().setVal(9);
this.slots[8][1] = new Slot().setVal(0);
this.slots[0][2] = new InitSlot().setVal(8);
this.slots[1][2] = new Slot().setVal(0);
this.slots[2][2] = new InitSlot().setVal(3);
this.slots[3][2] = new Slot().setVal(0);
this.slots[4][2] = new Slot().setVal(0);
this.slots[5][2] = new InitSlot().setVal(5);
this.slots[6][2] = new InitSlot().setVal(4);
this.slots[7][2] = new Slot().setVal(0);
this.slots[8][2] = new Slot().setVal(0);
this.slots[0][3] = new InitSlot().setVal(5);
this.slots[1][3] = new Slot().setVal(0);
this.slots[2][3] = new InitSlot().setVal(4);
this.slots[3][3] = new InitSlot().setVal(6);
this.slots[4][3] = new Slot().setVal(0);
this.slots[5][3] = new InitSlot().setVal(7);
this.slots[6][3] = new Slot().setVal(0);
this.slots[7][3] = new Slot().setVal(0);
this.slots[8][3] = new InitSlot().setVal(9);
this.slots[0][4] = new Slot().setVal(0);
this.slots[1][4] = new InitSlot().setVal(3);
this.slots[2][4] = new Slot().setVal(0);
this.slots[3][4] = new Slot().setVal(0);
this.slots[4][4] = new Slot().setVal(0);
this.slots[5][4] = new Slot().setVal(0);
this.slots[6][4] = new Slot().setVal(0);
this.slots[7][4] = new InitSlot().setVal(5);
this.slots[8][4] = new Slot().setVal(0);
this.slots[0][5] = new InitSlot().setVal(7);
this.slots[1][5] = new Slot().setVal(0);
this.slots[2][5] = new Slot().setVal(0);
this.slots[3][5] = new InitSlot().setVal(8);
this.slots[4][5] = new Slot().setVal(0);
this.slots[5][5] = new InitSlot().setVal(3);
this.slots[6][5] = new InitSlot().setVal(1);
this.slots[7][5] = new Slot().setVal(0);
this.slots[8][5] = new InitSlot().setVal(2);
this.slots[0][6] = new Slot().setVal(0);
this.slots[1][6] = new Slot().setVal(0);
this.slots[2][6] = new InitSlot().setVal(1);
this.slots[3][6] = new InitSlot().setVal(7);
this.slots[4][6] = new Slot().setVal(0);
this.slots[5][6] = new Slot().setVal(0);
this.slots[6][6] = new InitSlot().setVal(9);
this.slots[7][6] = new Slot().setVal(0);
this.slots[8][6] = new InitSlot().setVal(6);
this.slots[0][7] = new Slot().setVal(0);
this.slots[1][7] = new InitSlot().setVal(8);
this.slots[2][7] = new Slot().setVal(0);
this.slots[3][7] = new Slot().setVal(0);
this.slots[4][7] = new InitSlot().setVal(3);
this.slots[5][7] = new Slot().setVal(0);
this.slots[6][7] = new Slot().setVal(0);
this.slots[7][7] = new InitSlot().setVal(2);
this.slots[8][7] = new Slot().setVal(0);
this.slots[0][8] = new InitSlot().setVal(3);
this.slots[1][8] = new Slot().setVal(0);
this.slots[2][8] = new InitSlot().setVal(2);
this.slots[3][8] = new InitSlot().setVal(9);
this.slots[4][8] = new Slot().setVal(0);
this.slots[5][8] = new InitSlot().setVal(4);
this.slots[6][8] = new Slot().setVal(0);
this.slots[7][8] = new Slot().setVal(0);
this.slots[8][8] = new InitSlot().setVal(5);
document.write(("New Grid initialized : " + this.prettyPrint())+ "<br/>");
return this;
}
Grid.prototype.isSafe = function(xPos, yPos) { 
var x;
var y;
var res;
res = true;
x = xPos;
y = 0;
while((res && (y < 9))) {
if ((!(y == yPos) && (this.slots[xPos][yPos].getVal() == this.slots[x][y].getVal()))) {
res = false;}
y = (y + 1);}
x = 0;
y = yPos;
while((res && (x < 9))) {
if ((!(x == xPos) && (this.slots[xPos][yPos].getVal() == this.slots[x][y].getVal()))) {
res = false;}
x = (x + 1);}
x = this.getMinSquare(xPos);
y = this.getMinSquare(yPos);
while((res && (x < (this.getMinSquare(xPos) + 3)))) {
while((res && (y < (this.getMinSquare(yPos) + 3)))) {
if ((!((x == xPos) && (y == yPos)) && (this.slots[xPos][yPos].getVal() == this.slots[x][y].getVal()))) {
res = false;}
y = (y + 1);}
y = this.getMinSquare(yPos);
x = (x + 1);}
return res;
}
Grid.prototype.solve = function() { 
var x;
var y;
var slot;
x = 0;
while(((x < 9) && !(x < 0))) {
y = 0;
while((y < 9)) {
slot = this.slots[x][y];
if ((9 < slot.getVal())) {
slot = slot.setVal(0);
y = (y - 1);
if ((y < 0)) {
y = 8;
x = (x - 1);}
slot = this.slots[x][y];
while(slot.isInit()) {
y = (y - 1);
if ((y < 0)) {
y = 8;
x = (x - 1);}
slot = this.slots[x][y];}
slot = slot.setVal((slot.getVal() + 1));} else if ((slot.isInit() || (!(slot.getVal() == 0) && this.isSafe(x, y)))) {
y = (y + 1);} else  {
slot = slot.setVal((slot.getVal() + 1));}}
x = (x + 1);}
return this;
}
Grid.prototype.getMinSquare = function(a) { 
return (Math.floor((a / 3)) * 3);
}
Grid.prototype.prettyPrint = function() { 
var res;
var i;
var j;
var num;
document.write("┏━━━━━┳━━━━━┳━━━━━┓"+ "<br/>");
res = "┃";
i = 0;
while((i < 9)) {
j = 0;
while((j < 9)) {
num = this.slots[i][j].getVal();
if ((num == 0)) {
res = (res + " ");} else  {
res = (res + num);}
j = (j + 1);
if ((((j == 3) || (j == 6)) || (j == 9))) {
res = (res + "┃");} else  {
res = (res + " ");}}
document.write(res+ "<br/>");
res = "┃";
i = (i + 1);
if (((i == 3) || (i == 6))) {
document.write("┣━━━━━╋━━━━━╋━━━━━┫"+ "<br/>");}}
document.write("┗━━━━━┻━━━━━┻━━━━━┛"+ "<br/>");
return "* Solved *";
}

document.write(new Grid().init(2).solve().prettyPrint()+ "<br/>");