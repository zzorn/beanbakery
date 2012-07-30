package org.beanbakery

import parser.syntaxtree.function._
import parser.syntaxtree.function.HardwiredFun1
import parser.syntaxtree.function.HardwiredFun2
import parser.syntaxtree.function.HardwiredFun4
import parser.syntaxtree.function.HardwiredFun5
import parser.syntaxtree.kind.NumKind
import scala.math._
import java.util.Random
import utils.{MathUtils, NoiseUtils, SimplexGradientNoise}
import org.beanbakery.Scope

/**
 * Can be used to add various default functions and constants to a bean bakery context.
 */
object DefaultScopeContent {

  private val rng = new Random()

  lazy val defaultScope: Scope = createDefaultScope()

  def createDefaultScope(): Scope = {

    var vals = Map[Symbol, Any]()

    def addValue(id: Symbol, v: Double) {
      vals += id -> v
    }
    def addFunction1(id: Symbol, f: (Double) => Double,
                     p1Name: Symbol = 'a) {
      vals += id -> Closure(HardwiredFun1(p1Name, NumKind, NumKind, f))
    }
    def addFunction2(id: Symbol, f: (Double, Double) => Double,
                     p1Name: Symbol = 'a,
                     p2Name: Symbol = 'b) {
      vals += id -> Closure(HardwiredFun2(
        p1Name, p2Name,
        NumKind, NumKind,
        NumKind, f))
    }
    def addFunction3(id: Symbol, f: (Double, Double, Double) => Double,
                     p1Name: Symbol = 'a,
                     p2Name: Symbol = 'b,
                     p3Name: Symbol = 'c) {
      vals += id -> Closure(HardwiredFun3(
        p1Name, p2Name, p3Name,
        NumKind, NumKind, NumKind,
        NumKind, f))
    }
    def addFunction4(id: Symbol, f: (Double, Double, Double, Double) => Double,
                     p1Name: Symbol = 'a,
                     p2Name: Symbol = 'b,
                     p3Name: Symbol = 'c,
                     p4Name: Symbol = 'd) {
      vals += id -> Closure(HardwiredFun4(
        p1Name, p2Name, p3Name, p4Name,
        NumKind, NumKind, NumKind, NumKind,
        NumKind, f))
    }
    def addFunction5(id: Symbol, f: (Double, Double, Double, Double, Double) => Double,
                     p1Name: Symbol = 'a,
                     p2Name: Symbol = 'b,
                     p3Name: Symbol = 'c,
                     p4Name: Symbol = 'd,
                     p5Name: Symbol = 'e) {
      vals += id -> Closure(HardwiredFun5(
        p1Name, p2Name, p3Name, p4Name, p5Name,
        NumKind, NumKind, NumKind, NumKind, NumKind,
        NumKind, f))
    }

    // Exponents
    addFunction2('pow, pow(_, _))
    addFunction1('sqrt, sqrt(_))
    addFunction1('cbrt, cbrt(_))
    addFunction1('exp, exp(_))
    addFunction1('log, log(_))
    addFunction1('log10, log10(_))
    addValue('E, E)

    // Trigonometry
    addValue('Tau, MathUtils.Tau)
    addValue('Pi, Pi)

    addFunction1('toDegrees, toDegrees(_))
    addFunction1('toRadians, toRadians(_))

    addFunction1('sin, sin(_))
    addFunction1('cos, cos(_))
    addFunction1('tan, tan(_))
    addFunction1('asin, asin(_))
    addFunction1('acos, acos(_))
    addFunction1('atan, atan(_))

    addFunction2('atan2, atan2(_, _), 'y, 'x)
    addFunction2('hypot, hypot(_, _), 'x, 'y)
    addFunction2('angleTo, (x: Double, y: Double) => atan2(y, x), 'x, 'y)
    addFunction2('distanceTo, hypot(_, _), 'x, 'y)

    // Rounding
    addFunction1('round, round(_).toDouble)
    addFunction1('floor, floor(_))
    addFunction1('ceil, ceil(_))
    addFunction1('fraction, fraction(_))

    // Clamping
    addFunction3('clamp, MathUtils.clamp(_, _, _), 'value, 'start, 'end)
    addFunction1('clamp0to1, MathUtils.clampZeroToOne(_))
    addFunction1('clampMinus1to1, MathUtils.clampMinusOneToOne(_))

    // Magnitude
    addFunction1('sign, signum(_))
    addFunction2('copySign, java.lang.Math.copySign(_, _))
    addFunction2('max, max(_, _))
    addFunction2('min, min(_, _))
    addFunction1('abs, abs(_))

    // Random
    addFunction1('random, random(_), 'seed)
    addFunction3('randomRange, randomRange(_, _, _), 'seed, 'start, 'end)
    addFunction3('gaussian, gaussian(_, _, _), 'seed, 'average, 'stdDev)

    // Noise
    addFunction1('noise1, SimplexGradientNoise.sdnoise1(_))
    addFunction2('noise2, SimplexGradientNoise.sdnoise2(_, _))
    addFunction3('noise3, SimplexGradientNoise.sdnoise3(_, _, _))
    addFunction4('noise4, SimplexGradientNoise.sdnoise4(_, _, _, _))
    addFunction2('turbulence1, NoiseUtils.turbulence1(_, _))
    addFunction3('turbulence2, NoiseUtils.turbulence2(_, _, _))
    addFunction4('turbulence3, NoiseUtils.turbulence3(_, _, _, _))
    addFunction5('turbulence4, NoiseUtils.turbulence4(_, _, _, _, _))

    // Interpolation
    addFunction3('mix, MathUtils.mix(_, _, _))
    addFunction5('map, MathUtils.map(_, _, _, _, _))
    addFunction3('relativePos, MathUtils.relativePos(_, _, _), 'value, 'start, 'end)

    Scope(vals)
  }

  def fraction(v: Double): Double = {
    v - floor(v)
  }

  def random(seed: Double): Double = {
    rng.setSeed(java.lang.Double.doubleToRawLongBits(seed))
    rng.nextDouble()
  }

  def randomRange(seed: Double, start: Double, end: Double): Double = {
    rng.setSeed(java.lang.Double.doubleToRawLongBits(seed))
    rng.nextDouble() * (end - start) + start
  }

  def gaussian(seed: Double, average: Double, stdDev: Double): Double = {
    rng.setSeed(java.lang.Double.doubleToRawLongBits(seed))
    rng.nextGaussian() * stdDev + average
  }

}