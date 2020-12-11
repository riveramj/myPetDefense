package com.mypetdefense.helpers

import org.scalacheck.Prop.propBoolean
import org.scalacheck.Test.{Parameters, TestCallback}
import org.scalacheck.{Gen, Prop}
import org.scalactic.Prettifier
import org.scalactic.source.Position
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.CheckerAsserting
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks.{generatorDrivenConfig => _, _}

import scala.Function.untupled

/** Workaround for lack of forAllNoShrink in ScalaTest + ScalaCheck */
trait ScalaCheckHelper {

  def forAllNoShrink[A, ASSERTION](
      genA: Gen[A],
      configParams: PropertyCheckConfigParam*
  )(fun: A => ASSERTION)(
      implicit
      config: PropertyCheckConfiguration,
      asserting: CheckerAsserting[ASSERTION],
      prettifier: Prettifier,
      pos: Position
  ): asserting.Result =
    mkForAllNoShrink(fun, configParams)(Prop.forAllNoShrink(genA))

  def forAllNoShrink[A, B, ASSERTION](
      genA: Gen[A],
      genB: Gen[B],
      configParams: PropertyCheckConfigParam*
  )(fun: (A, B) => ASSERTION)(
      implicit
      config: PropertyCheckConfiguration,
      asserting: CheckerAsserting[ASSERTION],
      prettifier: Prettifier,
      pos: Position
  ): asserting.Result =
    mkForAllNoShrink(fun.tupled, configParams) { propF =>
      Prop.forAllNoShrink(genA, genB)(untupled(propF))
    }

  def forAllNoShrink[A, B, C, ASSERTION](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      configParams: PropertyCheckConfigParam*
  )(fun: (A, B, C) => ASSERTION)(
      implicit
      config: PropertyCheckConfiguration,
      asserting: CheckerAsserting[ASSERTION],
      prettifier: Prettifier,
      pos: Position
  ): asserting.Result =
    mkForAllNoShrink(fun.tupled, configParams) { propF =>
      Prop.forAllNoShrink(genA, genB, genC)(untupled(propF))
    }

  def forAllNoShrink[A, B, C, D, ASSERTION](
      genA: Gen[A],
      genB: Gen[B],
      genC: Gen[C],
      genD: Gen[D],
      configParams: PropertyCheckConfigParam*
  )(fun: (A, B, C, D) => ASSERTION)(
      implicit
      config: PropertyCheckConfiguration,
      asserting: CheckerAsserting[ASSERTION],
      prettifier: Prettifier,
      pos: Position
  ): asserting.Result =
    mkForAllNoShrink(fun.tupled, configParams) { propF =>
      Prop.forAllNoShrink(genA, genB, genC, genD)(untupled(propF))
    }

  private def mkForAllNoShrink[XS, ASSERTION](
      fun: XS => ASSERTION,
      configParams: Seq[PropertyCheckConfigParam]
  )(mkProp: (XS => Prop) => Prop)(
      implicit
      config: PropertyCheckConfiguration,
      asserting: CheckerAsserting[ASSERTION],
      prettifier: Prettifier,
      pos: Position
  ): asserting.Result = {
    val propF = { (xs: XS) =>
      val (unmetCondition, succeeded, exception) =
        try {
          val (succeeded, cause) = asserting.succeed(fun(xs))
          (false, succeeded, cause)
        } catch {
          case _: DiscardedEvaluationException => (true, false, None)
          case e: Throwable                    => (false, false, Some(e))
        }
      !unmetCondition ==> (
        if (exception.isEmpty) {
          if (succeeded)
            Prop.passed
          else
            Prop.falsified
        } else
          Prop.exception(exception.get)
      )
    }
    val prop   = mkProp(propF)
    val params = getScalaCheckParams(configParams, config)
    asserting.check(prop, params, prettifier, pos)
  }

  private def getScalaCheckParams(
      configParams: Seq[Configuration#PropertyCheckConfigParam],
      config: PropertyCheckConfiguration
  ): Parameters = {

    var minSuccessful: Option[Int]         = None
    var maxDiscardedFactor: Option[Double] = None
    var pminSize: Option[Int]              = None
    var psizeRange: Option[Int]            = None
    var pworkers: Option[Int]              = None

    var minSuccessfulTotalFound      = 0
    var maxDiscardedFactorTotalFound = 0
    var minSizeTotalFound            = 0
    var sizeRangeTotalFound          = 0
    var workersTotalFound            = 0

    for (configParam <- configParams) {
      configParam match {
        case param: MinSuccessful =>
          minSuccessful = Some(param.value)
          minSuccessfulTotalFound += 1
        case param: MaxDiscardedFactor =>
          maxDiscardedFactor = Some(param.value)
          maxDiscardedFactorTotalFound += 1
        case param: MinSize =>
          pminSize = Some(param.value)
          minSizeTotalFound += 1
        case param: SizeRange =>
          psizeRange = Some(param.value)
          sizeRangeTotalFound += 1
        case param: Workers =>
          pworkers = Some(param.value)
          workersTotalFound += 1
        case _ =>
          throw new RuntimeException("Should not happen")
      }
    }

    if (minSuccessfulTotalFound > 1)
      throw new IllegalArgumentException(
        "can pass at most one MinSuccessful config parameters, but " + minSuccessfulTotalFound + " were passed"
      )
    if (maxDiscardedFactorTotalFound > 1)
      throw new IllegalArgumentException(
        "can pass at most one MaxDiscardedFactor config parameters, but " + maxDiscardedFactorTotalFound + " were passed"
      )
    if (minSizeTotalFound > 1)
      throw new IllegalArgumentException(
        "can pass at most one MinSize config parameters, but " + minSizeTotalFound + " were passed"
      )
    if (sizeRangeTotalFound > 1)
      throw new IllegalArgumentException(
        "can pass at most one SizeRange config parameters, but " + sizeRangeTotalFound + " were passed"
      )
    if (workersTotalFound > 1)
      throw new IllegalArgumentException(
        "can pass at most one Workers config parameters, but " + workersTotalFound + " were passed"
      )

    val minSuccessfulTests: Int = minSuccessful.getOrElse(config.minSuccessful)

    val minSize: Int = pminSize.getOrElse(config.minSize)

    val maxSize = psizeRange.getOrElse(config.sizeRange.value) + minSize

    val maxDiscardRatio: Float =
      maxDiscardedFactor.getOrElse(config.maxDiscardedFactor.value).toFloat

    Parameters.default
      .withMinSuccessfulTests(minSuccessfulTests)
      .withMinSize(minSize)
      .withMaxSize(maxSize)
      .withWorkers(pworkers.getOrElse(config.workers))
      .withTestCallback(new TestCallback {})
      .withMaxDiscardRatio(maxDiscardRatio)
      .withCustomClassLoader(None)
  }
}
