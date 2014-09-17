/**
 * Java-style ArrayList library for Tool
 * @author: oqbouvie
 *
 **/
object Main {
       def main() : Unit = {
           var a : ArrayList;

           a = new ArrayList();
       }
}

class ArrayList {
      var content : Int[];

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

      def Remove(n : Int) : Int = {
          var Geq : Bool;
          var value : Int;

          Geq = ! (n < content.length);

          if (Geq || n < 0) {
               value = 0 - 1;
          }
          else {

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
       * Copies the elements between src[s1] and src[e] to dst[s2] src[e]
       **/
      def CopyFT(dst : Int[], src : Int[], s1 : Int, s2 : Int, e : Int) : Int = {
          var i : Int;
          var j : Int;

          i = s1;
          j = s2;

          while(i < src.length && i < e && j < src.length && j < e) {
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
