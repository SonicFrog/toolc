object TestDouble{
	def main(): Unit = {
		IO.writeLine(new Expr().printAll());
	}	
}

class Expr {
	var uLength : Int;
	var intArr : Int[];
	var doubleArr : Double[];
	var boolArr : Bool[];
	var stringArr : String[];
	var formulaArr : Formula[];

	def printAll(): String = {
		var placeholder : Bool;

		uLength = 4;

		intArr = new Int[uLength];
		boolArr = new Bool[uLength];
		stringArr = new String[uLength];

		placeholder = this.fillArrays();

		doubleArr = intArr;
		formulaArr = this.createFormulaArray(uLength, stringArr, boolArr);

		IO.writeLine(this.dispArrays());

		IO.writeLine("");

		return "This program was brought to you by Ogier & Tristan";
	}

	def dispArrays() : String = {
		var index : Int;
		var currentFormula : Formula;
		index = 0;

		IO.writeLine("int array");
		while ( index < intArr.length) {
			IO.writeLine("Element n°" + index + " is : " + intArr[index]);
			index = index + 1;
		}
		IO.writeLine("<br/>");
		index = 0;

		IO.writeLine("double array");
		while ( index < doubleArr.length) {
			IO.writeLine("Element n°" + index + " is : " + (doubleArr[index] + 0.0));
			index = index + 1;
		}
		IO.writeLine("<br/>");
		index = 0;

		IO.writeLine("boolean array");
		while ( index < boolArr.length) {
			IO.writeLine("Element n°" + index + " is : " + boolArr[index]);
			index = index + 1;
		}
		IO.writeLine("<br/>");
		index = 0;

		IO.writeLine("string array");
		while ( index < stringArr.length) {
			IO.writeLine("Element n°" + index + " is : " + stringArr[index]);
			index = index + 1;
		}
		IO.writeLine("<br/>");
		index = 0;

		IO.writeLine("formula array");
		while ( index < formulaArr.length) {
			currentFormula = formulaArr[index];
			IO.writeLine("Element n°" + index + " is : (" + currentFormula.getProp() + " -> " + currentFormula.isTrueFunc() + ")");
			index = index + 1;
		}
		IO.writeLine("<br/>");
		index = 0;

		return "";
	}

	def fillArrays() : Bool = {
		intArr[0] = 1;
		intArr[1] = 2;
		intArr[2] = 3;
		intArr[3] = 0 - 4;

		boolArr[0] = true;
		boolArr[1] = false;
		boolArr[2] = true;
		boolArr[3] = true;

		stringArr[0] = "L'hiver est plus froid que l'été.";
		stringArr[1] = "This sentence is written in French.";
		stringArr[2] = "This program was first written on a 30 december.";
		stringArr[3] = "Ogier and Tristan are the only people composing group-05";
		return true;
	}

	def createFormulaArray(uLength : Int, propositions : String[], isTrues : Bool[]) : Formula[] = {
		var index : Int;
		var result : Formula[];

		index = 0;
		result = new Formula[uLength];

		while ( index < uLength) {
			result[index] = new Formula().init(propositions[index], isTrues[index]);
			index = index + 1;
		}


		return result;
	}
}

class Formula {
	var proposition : String;
	var isTrue : Bool;

	def init(prop : String, bool : Bool) : Formula = {
		proposition = prop;
		isTrue = bool;

		return this;
	}

	def getProp() : String = {
		return proposition;
	}

	def isTrueFunc() : Bool = {
		return isTrue;
	}
}