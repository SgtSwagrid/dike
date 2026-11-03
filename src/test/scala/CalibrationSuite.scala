package com.alecdorrington.dike

import com.alecdorrington.dike.Calibration.Knot
import munit.FunSuite

final class CalibrationSuite extends FunSuite:

  private val ladder = Seq(Knot(-1, 1200), Knot(1, 1600))

  test("an ability is read between the nearest two reference points"):
    assertEquals(
      Calibration.score(ladder, 0),
      Some(1400.0),
    )
    assertEquals(
      Calibration.score(ladder, -1),
      Some(1200.0),
    )
    assertEquals(
      Calibration.score(ladder.reverse, 0),
      Some(1400.0),
    )
    val rungs = Seq(Knot(0, 0), Knot(1, 50), Knot(2, 90))
    assertEquals(
      Calibration.score(rungs, 1.5),
      Some(70.0),
    )

  test("an ability beyond the reference points is read past them, unbounded"):
    assertEquals(
      Calibration.score(ladder, 2),
      Some(1800.0),
    )
    assertEquals(
      Calibration.score(ladder, -1.5),
      Some(1100.0),
    )
    assertEquals(
      Calibration.score(ladder, 10),
      Some(3400.0),
    )
    assertEquals(
      Calibration.score(ladder, -10),
      Some(-600.0),
    )

  test("reference points of equal ability are merged, not divided by"):
    val tied = Seq(Knot(0, 40), Knot(0, 60), Knot(2, 70))
    assertEquals(Calibration.score(tied, 1), Some(60.0))

  test("fewer than two distinct reference points determine no scale"):
    assertEquals(Calibration.score(Seq.empty, 1), None)
    assertEquals(
      Calibration.score(Seq(Knot(0, 50)), 1),
      None,
    )
    assertEquals(
      Calibration.score(Seq(Knot(0, 40), Knot(0, 60)), 1),
      None,
    )

  test("agreement is the share of the pairs that rank as they are scored"):
    assertEquals(
      Calibration.agreement(ladder),
      Some(1.0),
    )
    val inverted = Seq(Knot(-1, 1600), Knot(1, 1200))
    assertEquals(
      Calibration.agreement(inverted),
      Some(0.0),
    )
    // Only the pair of the last two points disagrees.
    val muddled = Seq(Knot(0, 10), Knot(1, 90), Knot(2, 50))
    assertEquals(
      Calibration.agreement(muddled),
      Some(2.0 / 3),
    )

  test("equal reference points each count in every pair they make"):
    // The equal pair counts neither way; three of the other five pairs agree.
    val repeated = Seq(
      Knot(0, 50),
      Knot(0, 50),
      Knot(1, 40),
      Knot(2, 60),
    )
    assertEquals(
      Calibration.agreement(repeated),
      Some(3.0 / 5),
    )

  test("reference points with nothing to disagree about report no agreement"):
    assertEquals(
      Calibration.agreement(Seq(Knot(0, 50), Knot(1, 50))),
      None,
    )
    assertEquals(
      Calibration.agreement(Seq(Knot(0, 50))),
      None,
    )
