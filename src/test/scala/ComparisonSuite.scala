package com.alecdorrington.dike

import com.alecdorrington.dike.BradleyTerry.Outcome
import munit.FunSuite

final class ComparisonSuite extends FunSuite:

  test("a verdict means the opposite when the items are swapped round"):
    assertEquals(Verdict.First.reversed, Verdict.Second)
    assertEquals(Verdict.Second.reversed, Verdict.First)
    assertEquals(Verdict.Tie.reversed, Verdict.Tie)

  test("a won judgement counts half a comparison, a drawn one a quarter each"):
    assertEquals(
      Comparison.outcomes(Verdict.Second, (3, 5)),
      List(Outcome(5, 3, 0.5)),
    )
    assertEquals(
      Comparison.outcomes(Verdict.Tie, (3, 5)),
      List(Outcome(3, 5, 0.25), Outcome(5, 3, 0.25)),
    )

  test("agreeing judgements reach their verdict, contradictory ones none"):
    val verdict = (first: Verdict, second: Verdict) =>
      Comparison
        .of(
          List(Right(first), Right(second)),
          (0, 1),
        )
        .verdict
    assertEquals(
      verdict(Verdict.First, Verdict.First),
      Some(Verdict.First),
    )
    assertEquals(
      verdict(Verdict.First, Verdict.Second),
      None,
    )

  test("a failed judgement is left out of the verdict"):
    val failure    = RuntimeException("No verdict.")
    val comparison = Comparison.of(
      List(Left(failure), Right(Verdict.Second)),
      (0, 1),
    )
    assertEquals(
      comparison.verdict,
      Some(Verdict.Second),
    )
    assertEquals(
      comparison.judgements,
      List(
        Left(failure),
        Right(List(Outcome(1, 0, 0.5))),
      ),
    )
    assertEquals(
      Comparison
        .of(
          List(Left(failure), Left(failure)),
          (0, 1),
        )
        .verdict,
      None,
    )
