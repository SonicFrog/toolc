object TestDouble{
	def main(): Unit = {
		IO.writeLine(new Expr().printAll());
	}	
}

class Expr {
	def printAll(): String = {
		var twoDotTwo : Double;
		var weDontCareAboutItButThereIsNoUnitReturnType : Bool;

		twoDotTwo = 2.2;
		twoDotTwo = this.divideAndMultiply(twoDotTwo - 2.1, 2, 4);
		weDontCareAboutItButThereIsNoUnitReturnType = new ArrayOfDouble().test();

		return twoDotTwo + " <- it's a double right there!";
	}

	def divideAndMultiply(double : Double, doubleTop : Double, int: Int) : Double = {
		return double / doubleTop * int;
	}
}

class ArrayOfDouble {
	def test(): Bool = {
		var array : Double[];
		var weStillDontCare : Double[][];
		var weAreStartingToCare : Double[][][];

		array = new Double[2];
		array[0] = IO.readDouble("Enter a Double please: ");
		array[1] = 2.6;
		IO.showPopup(array[0] + "  " + array[1]);
		IO.writeLine("This array length is : " + array.length);

		weStillDontCare = this.test2D(array, array);
		IO.writeLine(weStillDontCare[0][0] + " this is the element at position (0,0) of result!");

		weAreStartingToCare = this.test3D(weStillDontCare, weStillDontCare);
		IO.writeLine(weAreStartingToCare[0][0][0] + " this is the element at position (0,0,0) of result!");

		return true;
	}

	def test2D(firstRow: Double[], secondRow : Double[]) : Double[][] = {
		var result : Double[][];
		var index : Int;

		result = new Double[2][];
		
		result[0] = new Double[firstRow.length];
		result[1] = new Double[secondRow.length];

		index = 0;
		while(index < firstRow.length) {
			result[0][index] = firstRow[index];
			index = index + 1;
		}

		index = 0;
		while(index < secondRow.length) {
			result[1][index] = secondRow[index];
			index = index + 1;
		}

		return result;
	}

	def test3D(firstRow: Double[][], secondRow : Double[][]) : Double[][][] = {
		var result : Double[][][];

		result = new Double[2][][];

		result[0] = firstRow;
		result[1] = secondRow;

		return result;
	}
}