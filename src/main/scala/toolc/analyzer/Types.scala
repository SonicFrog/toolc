package toolc
package analyzer

import Symbols._

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt | TDouble => true
      case _ => false
    }
    override def toString = "Int"
  }

  case object TBool extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBool => true
      case _ => false
    }
    override def toString = "Bool"
  }

  case object TDouble extends Type {
    override def isSubTypeOf(tpe: Type) : Boolean = tpe match {
      case TDouble => true
      case _ => false
    }
    override def toString = "Double"
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def toString = "Int[]"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "String"
  }

  case class TArray(innerType : Type) extends Type {
    override def isSubTypeOf(tpe : Type) : Boolean = tpe match {
      case TArray(otherInner) => otherInner.isSubTypeOf(innerType)
      case _ => false
    }
    override def toString = innerType.toString + "[]"
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TObject(cs) => cs == classSymbol || (classSymbol.parent match {
        case Some(parent) => parent.getType.isSubTypeOf(cs.getType)
        case _ => false
      })
      case TAnyObject => true
      case _  => false
    }
    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  case object TAnyObject extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAnyObject => true
      case _ => false
    }
    override def toString = "Object"
  }
}
