object Main {
       def main() : Unit = {
           println(new FieldTest().init(2));
       }
}

class FieldTest {
      def init(x : Int) : Int = {
          while(x < 10) {
                  println(x);
                  x = x + 1;
          }
          return x;
      }

      def View(x : Int) : Int = {
          return x;
      }
}