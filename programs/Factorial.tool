object Factorial {
	def main() : Unit = {
		println(new Fact().Factorial(10));
	 }
}
class Fact {
	def Factorial(num : Int) : Int = {
		return this.FactAux(num, 1);
	 }
	def FactAux(num : Int, acc : Int) : Int = {
		var answer : Int;
		 if(num < 1) answer = acc;
		 else answer = this.FactAux(num - 1, acc * num);
		 return answer;
	 }
}

