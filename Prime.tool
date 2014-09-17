/**
 * Find the 1k first prime numbers for Tool
 * @author: tristan Overney
 *
 **/

 object Main {
 	def main() : Unit = {
 		println(new PR().Compute());
 	}
 }

 class PR {
 	var primes: Int[];
 	
 	def Compute(): String = {
 		var index: Int;
 		var pIndex: Int;
 		var nextToTest: Int;
 		var isDividable: Bool;
 		var skip: Bool;

 		primes = new Int[1000];
 		println("2");
 		primes[0] = 2;
 		index = 0;
 		pIndex = 0;
 		nextToTest = 3;
 		skip = false;
 		while((primes[999] == 0))
 		{
 			// println("Loop1");
 			while(!(primes[index] == 0) && !skip)
 			{
 				// println("loop2");
 				isDividable = this.Multiple(nextToTest, primes[index]);
 				if(isDividable) skip = true;
 				else index = index + 1;
 				// println(nextToTest);
 				// println(isDividable);
 			}
 			if(!skip){
 				pIndex = pIndex + 1;
 				primes[pIndex] = nextToTest;
 				println(nextToTest);
 			}
 			else skip = false;
 			index = 0;
 			nextToTest = nextToTest + 1;
 		}
 		return "Done";
 	}

 	def Multiple(toTest: Int, divider: Int) : Bool = {
 		var isMult: Bool;
 		while (0 < toTest){
 			toTest = toTest - divider;
 		}
 		if(toTest == 0) isMult = true;
 		else isMult = false;
 		return isMult;
 	}
 }