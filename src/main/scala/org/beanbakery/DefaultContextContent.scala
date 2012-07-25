package org.beanbakery

import scala.math._
import java.util.Random
import utils.{MathUtils, NoiseUtils, SimplexGradientNoise}

/**
 * Can be used to add various default functions and constants to a bean bakery context.
 */
object DefaultContextContent {

  private val rng = new Random()

  def addToContext(context: Scope) {

    // Exponents
    context.addFunction('pow, pow(_, _))
    context.addFunction('sqrt, sqrt(_))
    context.addFunction('cbrt, cbrt(_))
    context.addFunction('exp, exp(_))
    context.addFunction('log, log(_))
    context.addFunction('log10, log10(_))
    context.setVariable('E, E)

    // Trigonometry
    context.setVariable('Tau, MathUtils.Tau)
    context.setVariable('Pi, Pi)

    context.addFunction('toDegrees, toDegrees(_))
    context.addFunction('toRadians, toRadians(_))

    context.addFunction('sin, sin(_))
    context.addFunction('cos, cos(_))
    context.addFunction('tan, tan(_))
    context.addFunction('asin, asin(_))
    context.addFunction('acos, acos(_))
    context.addFunction('atan, atan(_))

    context.addFunction('atan2, atan2(_, _))
    context.addFunction('hypot, hypot(_, _))
    context.addFunction('angleTo, atan2(_, _))
    context.addFunction('distanceTo, hypot(_, _))

    // Rounding
    context.addFunction('round, round(_).toDouble)
    context.addFunction('floor, floor(_))
    context.addFunction('ceil, ceil(_))
    context.addFunction('fraction, fraction(_))

    // Clamping
    context.addFunction('clamp, MathUtils.clamp(_, _, _))
    context.addFunction('clamp0to1, MathUtils.clampZeroToOne(_))
    context.addFunction('clampMinus1to1, MathUtils.clampMinusOneToOne(_))

    // Magnitude
    context.addFunction('sign, signum(_))
    context.addFunction('copySign, java.lang.Math.copySign(_, _))
    context.addFunction('max, max(_, _))
    context.addFunction('min, min(_, _))
    context.addFunction('abs, abs(_))

    // Random
    context.addFunction('random, random(_))
    context.addFunction('randomRange, randomRange(_, _, _))
    context.addFunction('gaussian, gaussian(_, _, _))

    // Noise
    context.addFunction('noise1, SimplexGradientNoise.sdnoise1(_))
    context.addFunction('noise2, SimplexGradientNoise.sdnoise2(_, _))
    context.addFunction('noise3, SimplexGradientNoise.sdnoise3(_, _, _))
    context.addFunction('noise4, SimplexGradientNoise.sdnoise4(_, _, _, _))
    context.addFunction('turbulence1, NoiseUtils.turbulence1(_, _))
    context.addFunction('turbulence2, NoiseUtils.turbulence2(_, _, _))
    context.addFunction('turbulence3, NoiseUtils.turbulence3(_, _, _, _))
    context.addFunction('turbulence4, NoiseUtils.turbulence4(_, _, _, _, _))

    // Interpolation
    context.addFunction('mix, MathUtils.mix(_, _, _))
    context.addFunction('map, MathUtils.map(_, _, _, _, _))
    context.addFunction('relativePos, MathUtils.relativePos(_, _, _))

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