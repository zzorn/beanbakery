package org.beanbakery

import parser.syntaxtree.function._
import parser.syntaxtree.kind.{BoolKind, SimpleKind, Kind, NumKind}

/**
 * Used to create scopes with some pre-defined values or functions.
 */
class ScopeBuilder {

  private var values = Map[Symbol, Any]()

  private def kindOf[T](implicit m: Manifest[T]): Kind = {
    // TODO: Better logic?
    if (classOf[Double] == m.erasure) NumKind
    else if (classOf[Boolean] == m.erasure) BoolKind
    else SimpleKind(Symbol(m.erasure.getSimpleName))
  }

  def addValue(id: Symbol, v: Any) {
    values += id -> v
  }

  def addFunction0[R](id: Symbol, f: () => R,
                      doc: String = null)
         (implicit rm: Manifest[R]){
    values += id -> Closure(HardwiredFun0(
      kindOf[R], f, doc))
  }

  def addFunction1[P1, R](id: Symbol, f: (P1) => R,
                   p1Name: Symbol = 'x,
                   doc: String = null)
         (implicit p1m: Manifest[P1],
                   rm: Manifest[R]){
    values += id -> Closure(HardwiredFun1(
      p1Name,
      kindOf[P1],
      kindOf[R], f, doc))
  }

  def addFunction2[P1, P2, R](id: Symbol, f: (P1, P2) => R,
                   p1Name: Symbol = 'x,
                   p2Name: Symbol = 'y,
                   doc: String = null)
         (implicit p1m: Manifest[P1],
                   p2m: Manifest[P2],
                   rm: Manifest[R]){
    values += id -> Closure(HardwiredFun2(
      p1Name, p2Name,
      kindOf[P1], kindOf[P2],
      kindOf[R], f, doc))
  }

  def addFunction3[P1, P2, P3, R](id: Symbol, f: (P1, P2, P3) => R,
                   p1Name: Symbol = 'x,
                   p2Name: Symbol = 'y,
                   p3Name: Symbol = 'z,
                   doc: String = null)
         (implicit p1m: Manifest[P1],
                   p2m: Manifest[P2],
                   p3m: Manifest[P3],
                   rm: Manifest[R]){
    values += id -> Closure(HardwiredFun3(
      p1Name, p2Name, p3Name,
      kindOf[P1], kindOf[P2], kindOf[P3],
      kindOf[R], f, doc))
  }

  def addFunction4[P1, P2, P3, P4, R](id: Symbol, f: (P1, P2, P3, P4) => R,
                   p1Name: Symbol = 'x,
                   p2Name: Symbol = 'y,
                   p3Name: Symbol = 'z,
                   p4Name: Symbol = 'w,
                   doc: String = null)
         (implicit p1m: Manifest[P1],
                   p2m: Manifest[P2],
                   p3m: Manifest[P3],
                   p4m: Manifest[P4],
                   rm: Manifest[R]){
    values += id -> Closure(HardwiredFun4(
      p1Name, p2Name, p3Name, p4Name,
      kindOf[P1], kindOf[P2], kindOf[P3], kindOf[P4],
      kindOf[R], f, doc))
  }

  def addFunction5[P1, P2, P3, P4, P5, R](id: Symbol, f: (P1, P2, P3, P4, P5) => R,
                   p1Name: Symbol = 'x,
                   p2Name: Symbol = 'y,
                   p3Name: Symbol = 'z,
                   p4Name: Symbol = 'w,
                   p5Name: Symbol = 'u,
                   doc: String = null)
         (implicit p1m: Manifest[P1],
                   p2m: Manifest[P2],
                   p3m: Manifest[P3],
                   p4m: Manifest[P4],
                   p5m: Manifest[P5],
                   rm: Manifest[R]){
    values += id -> Closure(HardwiredFun5(
      p1Name, p2Name, p3Name, p4Name, p5Name,
      kindOf[P1], kindOf[P2], kindOf[P3], kindOf[P4], kindOf[P5],
      kindOf[R], f, doc))
  }

  def addNumFun0(id: Symbol, f: () => Double,
                 doc: String = null) {
    addFunction0[Double](id, f, doc)
  }

  def addNumFun1(id: Symbol, f: (Double) => Double,
                 p1Name: Symbol = 'x,
                 doc: String = null) {
    addFunction1[Double, Double](id, f, p1Name, doc)
  }

  def addNumFun2(id: Symbol, f: (Double, Double) => Double,
                 p1Name: Symbol = 'x,
                 p2Name: Symbol = 'y,
                 doc: String = null) {
    addFunction2[Double, Double, Double](id, f, p1Name, p2Name, doc)
  }

  def addNumFun3(id: Symbol, f: (Double, Double, Double) => Double,
                 p1Name: Symbol = 'x,
                 p2Name: Symbol = 'y,
                 p3Name: Symbol = 'z,
                 doc: String = null) {
    addFunction3[Double, Double, Double, Double](id, f, p1Name, p2Name, p3Name, doc)
  }

  def addNumFun4(id: Symbol, f: (Double, Double, Double, Double) => Double,
                 p1Name: Symbol = 'x,
                 p2Name: Symbol = 'y,
                 p3Name: Symbol = 'z,
                 p4Name: Symbol = 'w,
                 doc: String = null) {
    addFunction4[Double, Double, Double, Double, Double](id, f, p1Name, p2Name, p3Name, p4Name, doc)
  }

  def addNumFun5(id: Symbol, f: (Double, Double, Double, Double, Double) => Double,
                 p1Name: Symbol = 'x,
                 p2Name: Symbol = 'y,
                 p3Name: Symbol = 'z,
                 p4Name: Symbol = 'w,
                 p5Name: Symbol = 'u,
                 doc: String = null) {
    addFunction5[Double, Double, Double, Double, Double, Double](id, f, p1Name, p2Name, p3Name, p4Name, p5Name, doc)
  }

  def clear() {
    values = Map()
  }

  def createScope(subScopes: Map[Symbol, Scope] = Map(),
                  parent: Scope = null): Scope = {
    new Scope(values, subScopes, parent)
  }


}
