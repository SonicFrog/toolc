object Main {
       def main() : Unit = {
           println(new RandomVariable().Init().Insert(10).Insert(12).Mean());
       }
}

class RandomVariable {
      var Values : Int[];

      def Init() : RandomVariable = {
          Values = new Int[100];
          return this;
      }

      def Insert(v : Int) : RandomVariable = {
          if (v < 0 || Values.length < (v - 1)) {
             //Do nothing
          }
          else {
               Values[v] = Values[v] + 1;
          }

          return this;
      }

      def Mean() : Int = {
          var i : Int;
          var total : Int;
          var sampleNum : Int;

          i = 0;
          sampleNum = 0;
          total = 0;

          while(i < Values.length) {
                  total = total + (i * Values[i]);
                  sampleNum = sampleNum + i;
                  i = i + 1;
          }
          return total / sampleNum;
      }
}