object Prime {

	def main() : Unit = {
        println(new Primes().init(1000).printPrimes());
    }

}

class Primes {
	
	var primeNumbers : Int[];
	var size : Int;
	
	def init(number : Int) : Primes = {
        var i : Int;
        var j : Int;
        var potentialPrime : Int;
        var maxDivisor : Int;
        
        size = number;
        potentialPrime = 3;
        primeNumbers = new Int[number];
        primeNumbers[0] = 2;
        i = 1;
        while(i < size){
        	j = 1;
        	/* +1 Since we don't have <= and +1 since it's always lower than the real square root*/
   	        maxDivisor = (new Sqrt().compute(potentialPrime)) + 2;
        	while(j < maxDivisor){
        		j = j + 1;
        		if(j == maxDivisor){
        			primeNumbers[i] = potentialPrime;
        			i = i + 1;	
        		}else if((potentialPrime / j) * j == potentialPrime){
        			j = maxDivisor;
        		}
        	}
        	potentialPrime = potentialPrime + 2;
        }
        return this;
    }
    
    def printPrimes() : String = {
    	var result : String;
    	var i : Int;
    	i = 1;
    	
    	result = "" + primeNumbers[0];
    	
    	while(i < size){
    		result = result + ", " + primeNumbers[i];
    	    i = i + 1;
    	}
    	
    	return result;
    }
}

/*Copy of class Sqrt*/
class Sqrt {
	
	var estimate : Int;
	var counter : Int;
	def compute(number : Int) : Int = {
        estimate = number;
        counter = 0;
        /* 20 iterations should be enough for all integers*/
        while(counter < 20){
        	estimate = (estimate/2 + number/(estimate*2));
        	counter = counter + 1;
        }
        return estimate;
    }
}
