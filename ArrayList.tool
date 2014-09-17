/**
 * Java-style ArrayList library for Tool
 * @author: oqbouvie
 *
 **/
object Main {
       def main() : Unit = {
           println(new ArrayList().Add(10));
       }
}

class ArrayList {
      var content : Int[];

      def Init() : ArrayList = {
          content = new Int[0];

          return this;
      }

      /**
       * Adds n in the size position in the array list
       **/
      def Add(n : Int) : Int = {
          var ncontent : Int[];
          var osef : Int;
          var copy : Copier;

          copy = new Copier();
          ncontent = new Int[content.length + 1];

          osef = copy.Copy(ncontent, content);

          content = ncontent;
          content[content.length] = n;

          return content.length;
      }

      /**
       * Returns the element at index n
       * or -1 if n is not a valid index
       **/
      def Get(n : Int) : Int = {
          var Geq : Bool;
          var value : Int;

          Geq = ! (n < content.length);

          if (Geq || n < 0) {
             value = 0 - 1;
          }
          else {
               value = content[n];
          }

          return content[n];
      }

      /**
       * Removes the nth element of the list if n is a valid index
       **/
      def Remove(n : Int) : Int = {
          var Geq : Bool;
          var value : Int;
          var copier : Copier;
          var First : Int[];
          var Second : Int[];

          copier = new Copier();

          Geq = ! (n < content.length);

          if (Geq || n < 0) {
               value = 0 - 1;
          }
          else {
               First = new Int[n];
               Second = new Int[content.length - n - 1];

               value = copier.CopyN(First, content, n);
               value = copier.CopyFT(Second, content, n + 1, 0, content.length - n - 1);
               content = copier.Merge(First, Second);
          }

          return value;
      }

      def Length() : Int = {
          return content.length;
      }

      def Clear() : Int = {
          content = new Int[0];
          return 0;
      }

}

/**
 * Class used to manipulate arrays of Ints
 **/
class Copier {
      /**
       * Copies every element of src into dst as long as there is
       * space to hold them in dst
       **/
      def Copy(dst : Int[], src : Int[]) : Int = {
          return this.CopyF(dst, src, 0, src.length);
      }

      /**
       * Copies up to n elements of src into dst
       **/
      def CopyN(dst : Int[], src : Int[], n : Int) : Int = {
          return this.CopyF(dst, src, 0, n);
      }

      /**
       * Copies up to e - s elements of src into dst
       **/
      def CopyF(dst : Int[], src : Int[], s : Int, e : Int) : Int = {
          var i : Int;
          i = s;

          while(i < dst.length && i < src.length && i < e) {
                  dst[i] = src[i];
                  i =  i + 1;
          }

          return 0;
      }

      /**
       * Copies the elements between src[s1] and src[s1 + n] to dst[s2] src[s2 + n]
       **/
      def CopyFT(dst : Int[], src : Int[], s1 : Int, s2 : Int, n : Int) : Int = {
          var i : Int;
          var j : Int;

          i = s1;
          j = s2;

          while(i < src.length && j < src.length && i - s1 < n && i - s2 < n) {
                  dst[j] = src[i];
                  i = i + 1;
                  j = j + 1;
          }

          return 0;
      }

      /**
       * Creates a new array containing both the contents of one and two in order
       **/
      def Merge(one : Int[], two : Int[]) : Int[] = {
          var temp : Int[];
          var ret : Int;

          temp = new Int[one.length + two.length];

          ret = this.Copy(temp, one);

          ret = this.CopyFT(temp, two, one.length, one.length, temp.length);

          return temp;
      }
}
