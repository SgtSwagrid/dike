package com.alecdorrington.dike

import com.alecdorrington.dike.FieldSuite.*
import munit.FunSuite

final class FieldSuite extends FunSuite:

  test("each anchor is paired with the one scored next above it, if any"):
    assertEquals(
      field(2, 40, 60, 80).rungs,
      List((2, 3), (3, 4)),
    )
    assertEquals(field(2, 40).rungs, Nil)
    assertEquals(field(2).rungs, Nil)

  test("the anchors stand after the items, up the ladder in score order"):
    assertEquals(
      field(2, 80, 40).players,
      Vector("0", "1", "Scored 40.0", "Scored 80.0"),
    )

  test("a climb is compared with at most one rung per halving of the ladder"):
    assertEquals(
      List(0, 1, 2, 3, 7, 8, 15, 16).map(Field.longestClimb),
      List(0, 1, 2, 2, 3, 4, 4, 5),
    )

object FieldSuite:

  private def field(items: Int, scores: Double*): Field[String] = Field(Contest(
    List.tabulate(items)(_.toString),
    scores.map(score => Anchor(s"Scored $score", score)),
  ))
