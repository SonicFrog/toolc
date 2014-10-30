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

      case TRUE => new True
      case FALSE => new False
      case THIS => new This
      case IDKIND => new Identifier(currentToken.asInstanceOf[ID].value)
      case NEW => {
        readToken
        currentToken.kind match {
          case IDKIND => {
            readToken
            eat(LPAREN)
            eat(RPAREN)
            new New(new Identifier(currentToken.asInstanceOf[ID].value))
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

          case _ => expected(IDKIND)
        }
      }

      case _ => null //No terminal but to be handled when unwinding stack
    }

    def parseOr : ExprTree = {
      val lhs = parseAnd

      if (currentToken.kind == OR) {
        readToken

        val rhs = parseAnd

        new Or(lhs, rhs)
      } else {
        lhs
      }
    }

    def parseAnd : ExprTree = {
      val lhs = parseEquals

      if (currentToken.kind == AND) {
        readToken
        val rhs = parseEquals

        new And(lhs, rhs)
      } else {
        lhs
      }
    }

    def parseEquals : ExprTree = {
      val lhs = parseLessThan

      if (currentToken.kind == EQUALS) {
        readToken
        val rhs = parseLessThan
        new Equals(lhs, rhs)
      } else lhs
    }

    def parseLessThan : ExprTree = {
      val lhs = parsePlusMinus

      if (currentToken.kind == LESSTHAN) {
        readToken
        val rhs = parsePlusMinus
        new LessThan(lhs, rhs)
      } else lhs
    }

    def parsePlusMinus : ExprTree = {
      val lhs = parseMultDiv

      if (currentToken.kind == PLUS) {
        readToken
        val rhs = parseMultDiv
        new Plus(lhs, rhs)
      } else if (currentToken.kind == MINUS) {
        readToken
        var rhs = parseMultDiv
        new Minus(lhs, rhs)
      } else lhs
    }

    def parseMultDiv : ExprTree = {
      val lhs = parseBang

      if (currentToken.kind == TIMES) {
        readToken
        val rhs = parseBang
        new Times(lhs, rhs)
      } else if (currentToken.kind == DIV) {
        readToken
        val rhs = parseBang
        new Div(lhs, rhs)
      } else lhs
    }

    def parseBang : ExprTree = {
      if (currentToken.kind == BANG) {
        readToken
        new Not(parseParens)
      } else parseParens
    }

    def parseDot : ExprTree = {
      val lhs = parseParens

      if (currentToken.kind == DOT) {
        readToken
        currentToken.kind match {
          case IDKIND => {
            val args : ListBuffer[ExprTree] = new ListBuffer
            val methodName : Identifier =
              new Identifier(currentToken.asInstanceOf[ID].value)
            eat(LPAREN)
            while(currentToken.kind != RPAREN) {
              args += expr
            }
            eat(RPAREN)
            new MethodCall(lhs, methodName, args.toList)
          }
          case LENGTH => new ArrayLength(lhs)
          case _ => expected(IDKIND, LENGTH)
        }
      } else lhs
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
        case IDKIND => new Identifier(currentToken.asInstanceOf[ID].value)
        case INT =>
          readToken; currentToken.kind match {
            case RBRACKET =>
              eat(RBRACKET); eat(LBRACKET); new IntArrayType()
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

      while (currentToken.kind == IDKIND) {
        val id = currentToken match {
          case a : ID => new Identifier(a.value)
          case _ => expected(IDKIND)
        }
        readToken

        eat(COLON)

        val tpe = parseType

        args +=  Formal(tpe, new Identifier(id.value))

        eat(COMMA)
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

      eat(COLON)
      readToken
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
        case PRINTLN => //Println statement
          readToken
          val toPrint = findExprInParenthesis
          eat(SEMICOLON)
          new Println(toPrint)

        case WHILE => { //While statement
          readToken
          new While(findExprInParenthesis, statmt)
            }

        case IF => { //If statement
          readToken
          val cond = findExprInParenthesis
          val ifstat = statmt
          readToken
          if (currentToken.kind == ELSE) new If(cond, ifstat, Some(statmt))
          else new If(cond, ifstat, None)
            }

        case LBRACE => { //Opening a new block of statements
          var statements: List[StatTree] = List()
          while (currentToken.kind != RBRACE) statements = statements :+ statmt
          eat(RBRACE)
          new Block(statements)
        }

        case IDKIND => { //Assignation statement
          val id: ID = currentToken.asInstanceOf[ID]
          val identifier = new Identifier(id.value)

          readToken
          eat(EQSIGN)

          currentToken.kind match {
            case LBRACKET => {
              val arrayIndex = expr
              eat(LBRACKET)
              val assignExpr = expr
              eat(SEMICOLON)
              new ArrayAssign(identifier, arrayIndex, assignExpr)
            }

            case _ => {
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
