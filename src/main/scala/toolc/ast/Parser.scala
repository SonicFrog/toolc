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
        val literal = new StringLit(currentToken.asInstanceOf[STRLIT].value)
        readToken
        literal
      }

      case INTLITKIND => {
        val literal = new IntLit(currentToken.asInstanceOf[INTLIT].value)
        readToken
        literal
      }

      case TRUE => {
        readToken
        new True
      }
      case FALSE => {
        readToken
        new False
      }
      case THIS => {
        readToken
        new This
      }

      case IDKIND => {
        val id : ExprTree = new Identifier(currentToken.asInstanceOf[ID].value)
        readToken
        id
      }
      case NEW => {
        readToken
        currentToken.kind match {
          case IDKIND => {
            val id = new Identifier(currentToken.asInstanceOf[ID].value)
            readToken
            eat(LPAREN)
            eat(RPAREN)
            new New(id)
          }

          case INT => {
            readToken
            currentToken.kind match {
              case LBRACKET => {
                eat(LBRACKET)
                val size = expr
                eat(RBRACKET)
                new NewIntArray(size)
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
        readToken

        val rhs = parseAnd

        if (lhs == null || rhs == null) {
          fatal("|| is a binary operator")
        }

        lhs = new Or(lhs, rhs)
      }
      lhs
    }

    def parseAnd : ExprTree = {
      var lhs = parseEquals

      while (currentToken.kind == AND) {
        readToken
        val rhs = parseEquals

        if (lhs == null || rhs == null) {
          fatal("&& is binary operator")
        }

        lhs = new And(lhs, rhs)
      }
      lhs
    }

    def parseEquals : ExprTree = {
      var lhs = parseLessThan

      while (currentToken.kind == EQUALS) {
        readToken
        val rhs = parseLessThan

        if (rhs == null || lhs == null) {
          fatal("== is a binary operator")
        }
        lhs = new Equals(lhs, rhs)
      }

      lhs
    }

    def parseLessThan : ExprTree = {
      val lhs = parsePlusMinus

      if (currentToken.kind == LESSTHAN) {
        readToken
        val rhs = parsePlusMinus

        if (rhs == null || lhs == null) {
          fatal("< is a binary operator")
        }

        new LessThan(lhs, rhs)
      } else lhs
    }

    def parsePlusMinus : ExprTree = {
      var lhs = parseMultDiv

      while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
        if (currentToken.kind == PLUS) {
          readToken
          val rhs = parseMultDiv
          if (rhs == null || lhs == null) {
            fatal("+ is a binary operator")
          }
          lhs = new Plus(lhs, rhs)
        } else if (currentToken.kind == MINUS) {
          readToken
          val rhs = parseMultDiv
          if (rhs == null || lhs == null) {
            fatal("- is a binary operator")
          }
          lhs = new Minus(lhs, rhs)
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
      lhs
    }

    def parseBang : ExprTree = {
      if (currentToken.kind == BANG) {
        readToken
        new Not(parseDot)
      } else parseDot
    }

    def parseDot : ExprTree = {
      val lhs = parseBracket
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

          case LENGTH => readToken; new ArrayLength(lhs)
          case _ => expected(IDKIND, LENGTH)
        }
      }
      if (meth == null) lhs
      else meth
    }

    def parseBracket : ExprTree = {
      var lhs = parseParens

      while(currentToken.kind == LBRACKET) {
        readToken
        val index = expr
        eat(RBRACKET)
        lhs = new ArrayRead(lhs, index)
      }
      lhs
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
      currentToken.kind match {
        case IDKIND => {
          val id = new Identifier(currentToken.asInstanceOf[ID].value)
          readToken
          id
        }
        case INT =>
          readToken
          currentToken.kind match {
            case LBRACKET =>
              eat(LBRACKET); eat(RBRACKET); new IntArrayType()
            case _ => new IntType()
          }
        case STRING => eat(STRING); new StringType()
        case BOOLEAN => eat(BOOLEAN); new BooleanType()

        case _ => expected(IDKIND, STRING, INT, BOOLEAN)

      }
    }

    def parseClass : ClassDecl = {
      eat(CLASS)

      var parent : Option[Identifier] = None
      var attributes : ListBuffer[VarDecl] = ListBuffer()
      var methods : ListBuffer[MethodDecl] = ListBuffer()

      val methodName = currentToken match {
        case name : ID => new Identifier(name.value)
        case _ => expected(IDKIND)
      }

      readToken

      if (currentToken.kind == EXTENDS) {
        eat(EXTENDS)
        val parentName = currentToken match {
          case name : ID => new Identifier(name.value)
          case _ => expected(IDKIND)
        }

        parent = Some(parentName)
      }

      eat(LBRACE)

      while (currentToken.kind == VAR)
        attributes += parseVarDecl

      while (currentToken.kind == DEF)
        methods += parseMethod

      eat(RBRACE)

      new ClassDecl(methodName, parent, attributes.toList, methods.toList)
    }

    def parseObject : MainObject = {
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

      new MainObject(new Identifier(name), statments.toList)
    }

    def parseMethod : MethodDecl = {
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
        val id = currentToken match {
          case a : ID => new Identifier(a.value)
          case _ => expected(IDKIND)
        }
        readToken

        eat(COLON)

        val tpe = parseType

        args +=  Formal(tpe, new Identifier(id.value))

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

      new MethodDecl(retType, methodID, args.toList, variables.toList,
        statements.toList, retExpr)

    }

    def parseVarDecl : VarDecl = {
      eat(VAR)

      val varId = currentToken match {
        case id: ID => new Identifier(id.value)
        case _ => expected(IDKIND)
      }
      eat(IDKIND)
      eat(COLON)
      val varType = parseType
      eat(SEMICOLON)
      new VarDecl(varType, varId)
    }

    def findExprInParenthesis: ExprTree = {
      eat(LPAREN)
      val e = expr
      eat(RPAREN)
      e
    }

    def statmt: StatTree = {
      currentToken.kind match {
        case PRINTLN => {//Println statement
          readToken
          val toPrint = findExprInParenthesis
          eat(SEMICOLON)
          new Println(toPrint)
        }

        case WHILE => { //While statement
          readToken
          new While(findExprInParenthesis, statmt)
            }

        case IF => { //If statement
          readToken
          val cond = findExprInParenthesis
          val ifstat = statmt
          if (currentToken.kind == ELSE) {
            readToken
            new If(cond, ifstat, Some(statmt))
              } else new If(cond, ifstat, None)
            }

        case LBRACE => { //Opening a new block of statements
          eat(LBRACE);
          var statements: List[StatTree] = List()
          while (currentToken.kind != RBRACE) statements = statements :+ statmt
          eat(RBRACE)
          new Block(statements)
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
              new ArrayAssign(identifier, arrayIndex, assignExpr)
            }

            case _ => {
              eat(EQSIGN)
              val assignExpr = expr
              eat(SEMICOLON)
              new Assign(identifier, assignExpr)
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
