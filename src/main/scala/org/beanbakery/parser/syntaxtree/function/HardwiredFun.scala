package org.beanbakery.parser.syntaxtree.function

import org.beanbakery.parser.syntaxtree.kind.Kind
import org.beanbakery.Scope

trait HardwiredFun extends Fun {
  def returnType: Kind
  val returnKind = Some(returnType)
}

case class HardwiredFun0[R](returnType: Kind,
                            f: () => R,
                            doc: String = null) extends HardwiredFun {
  val paramDefs = List()
  def call(context: Scope): Any = {
    f()
  }
}

case class HardwiredFun1[T1, R](p1Name: Symbol,
                                p1Kind: Kind,
                                returnType: Kind,
                                f: (T1) => R,
                                doc: String = null) extends HardwiredFun {
  val paramDefs = List(ParamDef(p1Name, Some(p1Kind)))
  def call(context: Scope): Any = {
    val p1 = context.getValue(p1Name).asInstanceOf[T1]
    f(p1)
  }
}

case class HardwiredFun2[T1, T2, R](p1Name: Symbol, p2Name: Symbol,
                                    p1Kind: Kind, p2Kind: Kind,
                                    returnType: Kind,
                                    f: (T1, T2) => R,
                                    doc: String = null) extends HardwiredFun {
  val paramDefs = List(
    ParamDef(p1Name, Some(p1Kind)),
    ParamDef(p2Name, Some(p2Kind))
  )

  def call(context: Scope): Any = {
    val p1 = context.getValue(p1Name).asInstanceOf[T1]
    val p2 = context.getValue(p2Name).asInstanceOf[T2]
    f(p1, p2)
  }
}

case class HardwiredFun3[T1, T2, T3, R](p1Name: Symbol, p2Name: Symbol, p3Name: Symbol,
                                            p1Kind: Kind, p2Kind: Kind, p3Kind: Kind,
                                            returnType: Kind,
                                            f: (T1, T2, T3) => R,
                                            doc: String = null) extends HardwiredFun {
  val paramDefs = List(
    ParamDef(p1Name, Some(p1Kind)),
    ParamDef(p2Name, Some(p2Kind)),
    ParamDef(p3Name, Some(p3Kind))
  )

  def call(context: Scope): Any = {
    val p1 = context.getValue(p1Name).asInstanceOf[T1]
    val p2 = context.getValue(p2Name).asInstanceOf[T2]
    val p3 = context.getValue(p3Name).asInstanceOf[T3]
    f(p1, p2, p3)
  }
}

case class HardwiredFun4[T1, T2, T3, T4, R](p1Name: Symbol, p2Name: Symbol, p3Name: Symbol, p4Name: Symbol,
                                            p1Kind: Kind, p2Kind: Kind, p3Kind: Kind, p4Kind: Kind,
                                            returnType: Kind,
                                            f: (T1, T2, T3, T4) => R,
                                            doc: String = null) extends HardwiredFun {
  val paramDefs = List(
    ParamDef(p1Name, Some(p1Kind)),
    ParamDef(p2Name, Some(p2Kind)),
    ParamDef(p3Name, Some(p3Kind)),
    ParamDef(p4Name, Some(p4Kind))
  )

  def call(context: Scope): Any = {
    val p1 = context.getValue(p1Name).asInstanceOf[T1]
    val p2 = context.getValue(p2Name).asInstanceOf[T2]
    val p3 = context.getValue(p3Name).asInstanceOf[T3]
    val p4 = context.getValue(p4Name).asInstanceOf[T4]
    f(p1, p2, p3, p4)
  }
}

case class HardwiredFun5[T1, T2, T3, T4, T5, R](p1Name: Symbol, p2Name: Symbol, p3Name: Symbol, p4Name: Symbol, p5Name: Symbol,
                                                p1Kind: Kind, p2Kind: Kind, p3Kind: Kind, p4Kind: Kind, p5Kind: Kind,
                                                returnType: Kind,
                                                f: (T1, T2, T3, T4, T5) => R,
                                                doc: String = null) extends HardwiredFun {
  val paramDefs = List(
    ParamDef(p1Name, Some(p1Kind)),
    ParamDef(p2Name, Some(p2Kind)),
    ParamDef(p3Name, Some(p3Kind)),
    ParamDef(p4Name, Some(p4Kind)),
    ParamDef(p5Name, Some(p5Kind))
  )

  def call(context: Scope): Any = {
    val p1 = context.getValue(p1Name).asInstanceOf[T1]
    val p2 = context.getValue(p2Name).asInstanceOf[T2]
    val p3 = context.getValue(p3Name).asInstanceOf[T3]
    val p4 = context.getValue(p4Name).asInstanceOf[T4]
    val p5 = context.getValue(p5Name).asInstanceOf[T5]
    f(p1, p2, p3, p4, p5)
  }
}

