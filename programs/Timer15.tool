/**
 * Start a timer for fake seconds, with correct formatting!
 * @author: tristan Overney
 *
 **/
// HELLO
 object Main {
 	def main() : Unit = {
 		println(new Timer().Start());
 	}
 }

 class Timer {
 	var primes: Int[];
 	
 	def Start(): String = {
 		var fakeSeconds: Int;
 		var min: Int;
 		var sec: Int;
 		var disp: Bool;
 		fakeSeconds = 900; // 60 *15 min = 900secs

 		while(0 < fakeSeconds)
 		{
 			disp = this.DisplayTimeLeft(fakeSeconds);
 			fakeSeconds = fakeSeconds - 1;
 		}
 		return "Time's up, let's do this!";
 	}

 	def DisplayTimeLeft(timeLeft: Int) : Bool = {
 		var m: Int;
 		var s: Int;
 		m = timeLeft/60;
 		s = timeLeft - m*60;
 		if(9 < m)
 		{
 			if(9 < s) println(""+m+" min "+s+" sec");
 			else println(""+m+" min 0"+s+" sec");
 		}
 		else 
 		{
 			if(9 < s) println(" "+m+" min "+s+" sec");
 			else println(" "+m+" min 0"+s+" sec");
 		}
 		return true;
 	}
 }	