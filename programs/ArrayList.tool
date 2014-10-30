object ArrayList {
	def main() : Unit = {
		println(new List().init(10).fill().toString());
	 }
}
class List {
	var vals : Int [ ];
	 var size : Int;
	 def init(initSize : Int) : List = {
		if(initSize < 0 || initSize == 0) {
			initSize = 1;
		 }
		vals = new Int [ initSize ];
		 size = 0;
		 return this;
	 }
	def fill() : List = {
		var tmp : Int;
		 {
			tmp = this.add(1);
			 tmp = this.add(2);
			 tmp = this.set(4, 5);
			 tmp = this.add(this.get(1));
			 tmp = this.add(this.get(2));
		 }
		return this;
	 }
	def add(val : Int) : Int = {
		return this.set(size, val);
	 }
	def get(index : Int) : Int = {
		var res : Int;
		 if(index < 0 || !(index < size)) {
			println("Array out of bounds Exception");
			 res = 0 - 1;
		 }
		else {
			res = vals [ index ];
		 }
		return res;
	 }
	def set(index : Int, value : Int) : Int = {
		var buff : Int [ ];
		 var i : Int;
		 while(!(index < vals.length)) {
			buff = new Int [ vals.length * 2 ];
			 i = 0;
			 while(i < size) {
				buff [ i ] = vals [ i ];
				 i = i + 1;
			 }
			vals = buff;
		 }
		vals [ index ] = value;
		 if(!(index < size)) {
			size = index + 1;
		 }
		return size;
	 }
	def toString() : String = {
		var res : String;
		 var i : Int;
		 if(size == 0) {
			res = "nil";
		 }
		else {
			res = "List(size = " + size + STR() = [)(90:43);
			 i = 0;
			 while(i <(size - 1)) {
				res = res + vals [ i ] + ", ";
				 i = i + 1;
			 }
			res = res + vals [ size - 1 ] + "]";
		 }
		return res;
	 }
}

