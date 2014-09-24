object Main {
  def main() : Unit = {
    println(new Matrix2x2().init().set(0, 0, 4).set(0, 1, 2).toString());
  }
}

/**
 * Implementation of basic 2x2 square matrix
 **/
class Matrix2x2 {
  var col1 : Int[];
  var col2 : Int[];

  def init() : Matrix2 = {
    col1 = new Int[2];
    col2 = new Int[2];

    return this;
  }

  def toString() : String = {
    var line : Int;
    var col : Int;
    var tab : Int[];
    var printLine : String;

    line = 0;
    col = 0;
    printLine = "";

    while(line < col1.length) {
      printLine = printLine + "-------------------------------------------------------\n";
      printLine = printLine + "|" + this.get(0, line) + "|" + this.get(1, line) + "|\n";
      printLine = printLine +"-------------------------------------------------------\n";
      line = line + 1;
    }

    return printLine;
  }

  def mult(x : Matrix2) : Matrix2x2 = {
    var line : Int;
    var col : Int;
    var common : Int;
    var result : Matrix2x2;
    var value : Int;
    var osef : Int;

    result = new Matrix2x2().init();

    line = 0;
    col = 0;

    while(line < col1.length) {
      while(col < col1.length) {
        common = 0;
        value = 0;
        while(common < col1.length) {
          value = value + this.get(line, common) * x.get(common, col);
          common = common + 1;
        }

        osef = result.set(line, col, value);

        col = col + 1;
      }
      line = line + 1;
    }

    return result;
  }

  def set(col : Int, line : Int, v: Int) : Matrix2x2 = {
    if (col == 1) {
      col1[line] = v;
    }
    else {
      col2[line] = v;
    }

    return this;
  }

  def get(col : Int, line : Int) : Int = {
    var value : Int;
    if (col == 1) {
      value = col1[line];
    }
    else {
      value = col2[line];
    }
    return value;
  }
}
