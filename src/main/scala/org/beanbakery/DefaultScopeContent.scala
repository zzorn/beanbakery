package org.beanbakery

import scala.math._
import java.util.Random
import utils.{MathUtils, NoiseUtils, SimplexGradientNoise}

/**
 * Can be used to add various default functions and constants to a bean bakery context.
 */
object DefaultScopeContent {

  private val rng = new Random()

  lazy val defaultScope: Scope = createDefaultScope()

  def createDefaultScope(): Scope = {

    val builder = new ScopeBuilder()


    // Exponents
    builder.addNumFun2('pow, pow(_, _))
    builder.addNumFun1('sqrt, sqrt(_))
    builder.addNumFun1('cbrt, cbrt(_))
    builder.addNumFun1('exp, exp(_))
    builder.addNumFun1('log, log(_))
    builder.addNumFun1('log10, log10(_))
    builder.addValue('E, E)

    // Trigonometry
    builder.addValue('Tau, MathUtils.Tau)
    builder.addValue('Pi, Pi)

    builder.addNumFun1('toDegrees, toDegrees(_))
    builder.addNumFun1('toRadians, toRadians(_))

    builder.addNumFun1('sin, sin(_))
    builder.addNumFun1('cos, cos(_))
    builder.addNumFun1('tan, tan(_))
    builder.addNumFun1('asin, asin(_))
    builder.addNumFun1('acos, acos(_))
    builder.addNumFun1('atan, atan(_))

    builder.addNumFun2('atan2, atan2(_, _), 'y, 'x)
    builder.addNumFun2('hypot, hypot(_, _), 'x, 'y)
    builder.addNumFun2('angleTo, (x: Double, y: Double) => atan2(y, x), 'x, 'y)
    builder.addNumFun2('distanceTo, hypot(_, _), 'x, 'y)

    // Rounding
    builder.addNumFun1('round, round(_).toDouble)
    builder.addNumFun1('floor, floor(_))
    builder.addNumFun1('ceil, ceil(_))
    builder.addNumFun1('fraction, fraction(_))

    // Clamping
    builder.addNumFun3('clamp, MathUtils.clamp(_, _, _), 'value, 'start, 'end)
    builder.addNumFun1('clamp0to1, MathUtils.clampZeroToOne(_))
    builder.addNumFun1('clampMinus1to1, MathUtils.clampMinusOneToOne(_))

    // Magnitude
    builder.addNumFun1('sign, signum(_))
    builder.addNumFun2('copySign, java.lang.Math.copySign(_, _))
    builder.addNumFun2('max, max(_, _))
    builder.addNumFun2('min, min(_, _))
    builder.addNumFun1('abs, abs(_))

    // Random
    builder.addNumFun1('random, random(_), 'seed)
    builder.addNumFun3('randomRange, randomRange(_, _, _), 'seed, 'start, 'end)
    builder.addNumFun3('gaussian, gaussian(_, _, _), 'seed, 'average, 'stdDev)

    // Noise
    builder.addNumFun1('noise1, SimplexGradientNoise.sdnoise1(_))
    builder.addNumFun2('noise2, SimplexGradientNoise.sdnoise2(_, _))
    builder.addNumFun3('noise3, SimplexGradientNoise.sdnoise3(_, _, _))
    builder.addNumFun4('noise4, SimplexGradientNoise.sdnoise4(_, _, _, _))
    builder.addNumFun2('turbulence1, NoiseUtils.turbulence1(_, _))
    builder.addNumFun3('turbulence2, NoiseUtils.turbulence2(_, _, _))
    builder.addNumFun4('turbulence3, NoiseUtils.turbulence3(_, _, _, _))
    builder.addNumFun5('turbulence4, NoiseUtils.turbulence4(_, _, _, _, _))

    // Interpolation
    builder.addNumFun3('mix, MathUtils.mix(_, _, _))
    builder.addNumFun5('map, MathUtils.map(_, _, _, _, _))
    builder.addNumFun3('relativePos, MathUtils.relativePos(_, _, _), 'value, 'start, 'end)

    builder.createScope()
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