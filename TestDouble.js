function Expr() {}
Expr.prototype.printAll = function() { 
var twoDotTwo;
var weDontCareAboutItButThereIsNoUnitReturnType;
twoDotTwo = 2.2;
twoDotTwo = this.divideAndMultiply((twoDotTwo - 2.1), 2, 4);
weDontCareAboutItButThereIsNoUnitReturnType = new ArrayOfDouble().test();
return (twoDotTwo + " <- it's a double right there!");
}
Expr.prototype.divideAndMultiply = function(double, doubleTop, int) { 
document.write(("Is 2.2 < 2.1 ? " + (2.2 < 2.1))+ "<br/>");
return ((double / doubleTop) * int);
}

function ArrayOfDouble() {}
ArrayOfDouble.prototype.test = function() { 
var array;
var weStillDontCare;
var weAreStartingToCare;
console.log("does that logs anything?")
array = new Array(2);
array[0] = parseFloat(prompt("Enter a Double please: "));
array[1] = 2.6;
window.alert(((array[0] + "  ") + array[1]));
document.write(("This array length is : " + array.length)+ "<br/>");
weStillDontCare = this.test2D(array, array);
document.write((weStillDontCare[0][0] + " this is the element at position (0,0) of result!")+ "<br/>");
weAreStartingToCare = this.test3D(weStillDontCare, weStillDontCare);
document.write((weAreStartingToCare[0][0][0] + " this is the element at position (0,0,0) of result!")+ "<br/>");
return true;
}
ArrayOfDouble.prototype.test2D = function(firstRow, secondRow) { 
var result;
var index;
result = new Array(2);
result[0] = new Array(firstRow.length);
result[1] = new Array(secondRow.length);
index = 0;
while((index < firstRow.length)) {
result[0][index] = firstRow[index];
index = (index + 1);}
index = 0;
while((index < secondRow.length)) {
result[1][index] = secondRow[index];
index = (index + 1);}
return result;
}
ArrayOfDouble.prototype.test3D = function(firstRow, secondRow) { 
var result;
result = new Array(2);
result[0] = firstRow;
result[1] = secondRow;
document.write((result[0][1].length + "")+ "<br/>");
return result;
}

document.write(new Expr().printAll()+ "<br/>");