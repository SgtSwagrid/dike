package com.alecdorrington.dike

import com.alecdorrington.dike.BradleyTerry.Outcome
import com.alecdorrington.dike.RankingSuite.*
import munit.FunSuite

final class RankingSuite extends FunSuite:

  test("ratings keep the items' order, and the stronger ranks above"):
    val ranking = rank(2, beat(1, 0))
    assertEquals(
      ranking.ratings.map(_.item),
      Vector("0", "1"),
    )
    assertEquals(
      ranking.ranked.map(_.item),
      Vector("1", "0"),
    )
    assert(ranking.ranked.head.score > ranking.ranked.last.score)

  test("agreeing judgements make a comparison won, contradictory a draw"):
    assertEqualsDouble(
      rank(2, beat(0, 1)).ratings(0).wins,
      1.0,
      1e-9,
    )
    val drawn = rank(
      2,
      List(
        List(Outcome(0, 1, 0.5)),
        List(Outcome(1, 0, 0.5)),
      ),
    )
    assertEquals(
      drawn.ratings.map(_.wins),
      Vector(0.5, 0.5),
    )
    assertEquals(
      drawn.ratings.map(_.score).distinct.size,
      1,
    )

  test("every judgement is counted, however split, and failures reported"):
    val failure = RuntimeException("No verdict.")
    val tied    = List(Outcome(0, 1, 0.25), Outcome(1, 0, 0.25))
    val ranking = fitted(
      contest(2),
      List(
        Left(failure),
        Right(tied),
        Right(List(Outcome(0, 1, 0.5))),
      ),
    )
    assertEquals(ranking.failures, List(failure))
    assertEquals(ranking.judgements, 2)

  test("an uncalibrated ranking scores items by their standing on the curve"):
    // Of two items, the stronger stands at the third quartile.
    val ranking = rank(2, beat(0, 1))
    assertEqualsDouble(
      ranking.ratings(0).score,
      0.6744898,
      1e-6,
    )
    assertEqualsDouble(
      ranking.ratings(1).score,
      -0.6744898,
      1e-6,
    )
    assertEquals(
      rank(2, beat(0, 1), curve = Curve(50, 0)).ratings.map(_.score),
      Vector(50.0, 50.0),
    )

  test("every item is rated, compared or not, but never an anchor"):
    assertEquals(
      rank(3, List(List(Outcome(0, 1, 0.5)))).ratings.map(_.item),
      Vector("0", "1", "2"),
    )
    assertEquals(
      rank(1, beat(0, 1), anchors = ladder(40, 80)).ratings.map(_.item),
      Vector("0"),
    )

  test("items are scored off the anchors they are placed among"):
    // The anchors scored 40 and 80 are players 2 and 3.
    val between = rank(
      2,
      beat(0, 2) ++ beat(3, 0),
      anchors = ladder(40, 80),
    ).ratings(0).score
    assert(between > 40 && between < 80, between)
    val swept = beat(3, 2) ++ (0 until 2)
      .toList
      .flatMap(item => beat(item, 2) ++ beat(item, 3))
    val above = rank(2, swept, anchors = ladder(40, 60))
    assert(
      above.ratings.forall(_.score > 60),
      above.ratings,
    )

  test("agreement reports whether the anchors rank as they are scored"):
    assertEquals(
      rank(1, beat(2, 1), anchors = ladder(40, 80)).agreement,
      Some(1.0),
    )
    // The anchor scored 40 (player 1) beats the one scored 80 (player 2).
    assertEquals(
      rank(1, beat(1, 2), anchors = ladder(40, 80)).agreement,
      Some(0.0),
    )
    // Given highest first, the anchor scored 40 is still player 1.
    assertEquals(
      rank(1, beat(2, 1), anchors = ladder(80, 40)).agreement,
      Some(1.0),
    )
    assertEquals(rank(2, beat(0, 1)).agreement, None)

object RankingSuite:

  private def beat(winner: Int, loser: Int): List[List[Outcome]] =
    List.fill(2)(List(Outcome(winner, loser, 0.5)))

  private def ladder(scores: Double*): List[Anchor[String]] = scores
    .toList
    .map(score => Anchor(s"Scored $score", score))

  private def contest
    (
      items: Int,
      anchors: List[Anchor[String]] = Nil,
      curve: Curve = Curve.standard,
    )
    : Contest[String] = Contest(
    List.tabulate(items)(_.toString),
    anchors,
    curve = curve,
  )

  private def fitted
    (
      contest: Contest[String],
      judgements: List[Judgement],
    )
    : Ranking[String] = Ranking.of(Field(contest), judgements)

  private def rank
    (
      items: Int,
      judgements: List[List[Outcome]],
      anchors: List[Anchor[String]] = Nil,
      curve: Curve = Curve.standard,
    )
    : Ranking[String] = fitted(
    contest(items, anchors, curve),
    judgements.map(Right(_)),
  )
