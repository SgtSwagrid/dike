package com.alecdorrington.dike

import munit.FunSuite

final class JudgingSuite extends FunSuite:

  test("a won or lost rung leaves only the rungs beyond it, within the range"):
    val narrowed = (rungs: Range, verdict: Verdict) =>
      Judging.narrowed(rungs, 3, Some(verdict)).toList
    assertEquals(
      narrowed(0 until 7, Verdict.First),
      List(4, 5, 6),
    )
    assertEquals(
      narrowed(0 until 7, Verdict.Second),
      List(0, 1, 2),
    )
    assertEquals(
      narrowed(2 until 5, Verdict.Second),
      List(2),
    )

  test("a draw, a contradiction or the end of the ladder ends the climb"):
    assert(Judging.narrowed(0 until 7, 3, Some(Verdict.Tie)).isEmpty)
    assert(Judging.narrowed(0 until 7, 3, None).isEmpty)
    val only = 0 until 1
    assert(Judging.narrowed(only, 0, Some(Verdict.First)).isEmpty)
    assert(Judging.narrowed(only, 0, Some(Verdict.Second)).isEmpty)
