package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import scala.collection.mutable.ListBuffer

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    // Store the current token, as read from the lexer.
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD && tokens.hasNext) {
          currentToken = tokens.next
        }
      }
    }

    // ''Eats'' the expected token, or terminates with an error.
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    // Complains that what was found was not expected.
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def isOneOf(kind : TokenKind, more : TokenKind*) : Boolean = {
      (kind :: more.toList).contains(currentToken.kind)
    }


    def parseTerminal : ExprTree = currentToken.kind match {
      case STRLITKIND => {
        val pos = currentToken
        val literal = new StringLit(currentToken.asInstanceOf[STRLIT].value)
        readToken
        literal.setPos(pos)
      }

      case INTLITKIND => {
        val pos = currentToken
        val literal = new IntLit(currentToken.asInstanceOf[INTLIT].value)
        readToken
        literal.setPos(pos)
      }

      case TRUE => {
        val pos = currentToken
        readToken
        True().setPos(pos)
      }
      case FALSE => {
        val pos = currentToken
        readToken
        False().setPos(pos)
      }
      case THIS => {
        val pos = currentToken
        readToken
        This().setPos(pos)
      }

      case IDKIND => {
        val id : ExprTree = new Identifier(currentToken.asInstanceOf[ID].value)
        val pos = currentToken
        readToken
        id.setPos(pos)
      }

      case IO => {
        val pos = currentToken
        readToken

        eat(DOT)

        currentToken.kind match {
          case IDKIND => {
            val value = currentToken.asInstanceOf[ID].value
            readToken
            eat(LPAREN)
            val arg = expr
            eat(RPAREN)
            value match {
              case "readString" => ReadString(arg)
              case "readDouble" => ReadDouble(arg)
              case "readInteger" => ReadInteger(arg)
              case "writeLine" => WriteLine(arg)
              case "showPopup" => ShowPopup(arg)

              case _ => fatal("IO object has no " + value + " method!")
            }

          }
          case _ => expected(IDKIND)
        }
      }

      case NEW => {
        val pos = currentToken
        readToken
        currentToken.kind match {
          case IDKIND => {
            val id = new Identifier(currentToken.asInstanceOf[ID].value)
            readToken
            eat(LPAREN)
            eat(RPAREN)
            new New(id).setPos(pos)
          }

          case INT => {
            readToken
            currentToken.kind match {
              case LBRACKET => {
                eat(LBRACKET)
                val size = expr
                eat(RBRACKET)
                new NewIntArray(size).setPos(currentToken)
              }
              case _ => expected(LBRACKET)
            }
          }

          case _ => expected(IDKIND, INT)
        }
      }

      case _ => null //No terminal but to be handled when unwinding stack
    }

    def parseOr : ExprTree = {
      var lhs = parseAnd

      while (currentToken.kind == OR) {
        val pos = currentToken
        readToken

        val rhs = parseAnd

        if (lhs == null || rhs == null) {
          fatal("|| is a binary operator")
        }

        lhs = new Or(lhs, rhs).setPos(pos)
      }
      lhs
    }

    def parseAnd : ExprTree = {
      var lhs = parseLessThan

      while (currentToken.kind == AND) {
        val pos = currentToken
        readToken
        val rhs = parseLessThan

        if (lhs == null || rhs == null) {
          fatal("&& is binary operator")
        }

        lhs = And(lhs, rhs).setPos(pos)
      }
      lhs
    }

    def parseLessThan : ExprTree = {
      var lhs = parseEquals

      while (currentToken.kind == LESSTHAN) {
        val pos = currentToken
        readToken
        val rhs = parseEquals

        if (rhs == null || lhs == null) {
          fatal("< is a binary operator")
        }

        lhs = LessThan(lhs, rhs).setPos(pos)
      }

      lhs
    }

    def parseEquals : ExprTree = {
      var lhs = parsePlusMinus

      while (currentToken.kind == EQUALS) {
        val pos = currentToken
        readToken
        val rhs = parsePlusMinus

        if (rhs == null || lhs == null) {
          fatal("== is a binary operator")
        }
        lhs = Equals(lhs, rhs).setPos(pos)
      }

      lhs
    }

    def parsePlusMinus : ExprTree = {
      var lhs = parseMultDiv

      while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
        if (currentToken.kind == PLUS) {
          val pos = currentToken
          readToken
          val rhs = parseMultDiv
          if (rhs == null || lhs == null) {
            fatal("+ is a binary operator")
          }
          lhs = new Plus(lhs, rhs).setPos(pos)
        } else if (currentToken.kind == MINUS) {
          val pos = currentToken
          readToken
          val rhs = parseMultDiv
          if (rhs == null || lhs == null) {
            fatal("- is a binary operator")
          }
          lhs = new Minus(lhs, rhs).setPos(pos)
        }
      }
      lhs
    }

    def parseMultDiv : ExprTree = {
      var lhs = parseBang

      while (currentToken.kind == TIMES || currentToken.kind == DIV) {
        if (currentToken.kind == TIMES) {
          readToken
          val rhs = parseBang
          lhs = new Times(lhs, rhs)
        } else {
          readToken
          val rhs = parseBang
          lhs = new Div(lhs, rhs)
        }
      }
      lhs.setPos(currentToken)
    }

    def parseBang : ExprTree = {
      if (currentToken.kind == BANG) {
        readToken
        new Not(parseBracket).setPos(currentToken)
      } else parseBracket
    }

    def parseBracket : ExprTree = {
      var lhs = parseDot

      while(currentToken.kind == LBRACKET) {
        readToken
        val index = expr
        eat(RBRACKET)
        lhs = new ArrayRead(lhs, index)
      }

      lhs
    }

    def parseDot : ExprTree = {
      val lhs = parseParens

      var meth : ExprTree = null
      var hasMoreArgs : Boolean = true

      while (currentToken.kind == DOT) {
        readToken
        currentToken.kind match {
          case IDKIND => {
            val args : ListBuffer[ExprTree] = new ListBuffer
            val methodName : Identifier =
              new Identifier(currentToken.asInstanceOf[ID].value)

            readToken
            eat(LPAREN)

            hasMoreArgs = currentToken.kind != RPAREN

            while (hasMoreArgs) {
              args += expr
              if (currentToken.kind == COMMA) {
                readToken
              } else {
                hasMoreArgs = false;
              }
            }
            eat(RPAREN)
            if (meth == null)
              meth = new MethodCall(lhs, methodName, args.toList)
            else
              meth = new MethodCall(meth, methodName, args.toList)
          }

          case LENGTH => {
            readToken
            if (meth == null) meth = new ArrayLength(lhs)
            else meth = new ArrayLength(meth)
          }
          case _ => expected(IDKIND, LENGTH)
        }
      }
      if (meth == null) lhs
      else meth
    }



    def parseParens : ExprTree = {
      if (currentToken.kind == LPAREN) {
        eat(LPAREN)
        val inside = expr
        eat(RPAREN)
        inside
      }
      else parseTerminal
    }

    def expr: ExprTree = {
      parseOr
    }

    def parseType: TypeTree = {
      val pos = currentToken
      currentToken.kind match {
        case IDKIND => {
          val id = new Identifier(currentToken.asInstanceOf[ID].value)
          readToken
          id.setPos(pos)
        }
        case INT =>
          readToken
          currentToken.kind match {
            case LBRACKET =>
              eat(LBRACKET); eat(RBRACKET); IntArrayType().setPos(pos)
            case _ => IntType().setPos(pos)
          }
        case STRING => eat(STRING); StringType().setPos(pos)
        case BOOLEAN => eat(BOOLEAN); BooleanType().setPos(pos)

        case _ => expected(IDKIND, STRING, INT, BOOLEAN)

      }
    }

    def parseClass : ClassDecl = {
      val startPos = currentToken
      eat(CLASS)

      var parent : Option[Identifier] = None
      var attributes : ListBuffer[VarDecl] = ListBuffer()
      var methods : ListBuffer[MethodDecl] = ListBuffer()

      val methodName = currentToken match {
        case name : ID => Identifier(name.value).setPos(currentToken)
        case _ => expected(IDKIND)
      }

      readToken

      if (currentToken.kind == EXTENDS) {
        eat(EXTENDS)
        val parentName = currentToken match {
          case name : ID => Identifier(name.value).setPos(currentToken)
          case _ => expected(IDKIND)
        }
        readToken
        parent = Some(parentName)
      }

      eat(LBRACE)

      while (currentToken.kind == VAR)
        attributes += parseVarDecl

      while (currentToken.kind == DEF)
        methods += parseMethod

      eat(RBRACE)

      ClassDecl(methodName, parent, attributes.toList, methods.toList).
        setPos(startPos)
    }

    def parseObject : MainObject = {
      val startPos = currentToken
      eat(OBJECT)

      val name = currentToken match {
        case tok : ID => currentToken.asInstanceOf[ID].value
        case _ => expected(IDKIND)
      }

      eat(IDKIND)
      eat(LBRACE)
      eat(DEF)
      eat(MAIN)
      eat(LPAREN)
      eat(RPAREN)
      eat(COLON)
      eat(UNIT)
      eat(EQSIGN)
      eat(LBRACE)

      var statments : ListBuffer[StatTree] = ListBuffer()

      while(currentToken.kind != RBRACE) statments += statmt

      eat(RBRACE)
      eat(RBRACE)

      MainObject(new Identifier(name), statments.toList).setPos(startPos)
    }

    def parseMethod : MethodDecl = {
      val startPos = currentToken
      eat(DEF)
      val methodID = currentToken match {
        case a : ID => new Identifier(currentToken.asInstanceOf[ID].value)
        case _ => expected(IDKIND)
      }

      var args : ListBuffer[Formal] = ListBuffer()
      var variables : ListBuffer[VarDecl] = ListBuffer()
      var statements : ListBuffer[StatTree] = ListBuffer()

      readToken

      eat(LPAREN)

      var hasMoreArgs : Boolean = currentToken.kind != RPAREN

      while (hasMoreArgs) {
        val pos = currentToken = currentToken
        val id = currentToken match {
          case a : ID => new Identifier(a.value).setPos(currentToken)
          case _ => expected(IDKIND)
        }
        readToken

        eat(COLON)

        val tpe = parseType

        args +=  Formal(tpe, new Identifier(id.value)).setPos(startPos)

        if (currentToken.kind == COMMA) {
          eat(COMMA)
        } else {
          hasMoreArgs = false
        }
      }

      eat(RPAREN)

      eat(COLON)

      val retType = parseType

      eat(EQSIGN)
      eat(LBRACE)

      while(currentToken.kind == VAR) {
        variables += parseVarDecl
      }

      while (currentToken.kind != RETURN) {
        statements += statmt
      }

      eat(RETURN)

      val retExpr = expr
      eat(SEMICOLON)
      eat(RBRACE)

      MethodDecl(retType, methodID, args.toList, variables.toList,
        statements.toList, retExpr).setPos(startPos)

    }

    def parseVarDecl : VarDecl = {
      val startPos = currentToken
      eat(VAR)

      val varId = currentToken match {
        case id: ID => new Identifier(id.value).setPos(currentToken)
        case _ => expected(IDKIND)
      }
      eat(IDKIND)
      eat(COLON)
      val varType = parseType
      eat(SEMICOLON)
      VarDecl(varType, varId).setPos(startPos)
    }

    def findExprInParenthesis: ExprTree = {
      eat(LPAREN)
      val e = expr
      eat(RPAREN)
      e
    }

    def statmt: StatTree = {
      val pos = currentToken
      currentToken.kind match {
        case PRINTLN => {//Println statement
          readToken
          val toPrint = findExprInParenthesis
          eat(SEMICOLON)
          Println(toPrint).setPos(pos)
        }

        case WHILE => { //While statement
          readToken
          While(findExprInParenthesis, statmt).setPos(pos)
        }

        case IF => { //If statement
          readToken
          val cond = findExprInParenthesis
          val ifstat = statmt
          if (currentToken.kind == ELSE) {
            readToken
            If(cond, ifstat, Some(statmt)).setPos(pos)
          } else If(cond, ifstat, None).setPos(pos)
        }

        case LBRACE => { //Opening a new block of statements
          eat(LBRACE);
          var statements: List[StatTree] = List()
          while (currentToken.kind != RBRACE) statements = statements :+ statmt
          eat(RBRACE)
          Block(statements).setPos(pos)
        }

        case IDKIND => { //Assignation statement
          val id: ID = currentToken.asInstanceOf[ID]
          val identifier = new Identifier(id.value)

          readToken

          currentToken.kind match {
            case LBRACKET => {
              eat(LBRACKET)
              val arrayIndex = expr
              eat(RBRACKET)
              eat(EQSIGN)
              val assignExpr = expr
              eat(SEMICOLON)
              ArrayAssign(identifier, arrayIndex, assignExpr).setPos(pos)
            }

            case _ => {
              eat(EQSIGN)
              val assignExpr = expr
              eat(SEMICOLON)
              Assign(identifier, assignExpr).setPos(pos)
            }
          }
        }

        case _ => expected(IDKIND, LBRACE, IF, WHILE, PRINTLN)
      }
    }

    def parseGoal: Program = {
      val mainObject = parseObject
      var classes : ListBuffer[ClassDecl] = ListBuffer()

      while (currentToken.kind == CLASS)
        classes += parseClass

      eat(EOF)

      new Program(mainObject, classes.toList)
    }

    // Initialize the first token
    readToken

    // Parse
    parseGoal
  }
}
