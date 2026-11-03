package com.alecdorrington.dike

import cats.effect.{IO, Ref}
import com.alecdorrington.dike.ContestSuite.*
import munit.CatsEffectSuite

final class ContestSuite extends CatsEffectSuite:

  test("items are ranked in the order a consistent judge prefers them"):
    Contest(List(3, 1, 4, 2, 5), rounds = 5)
      .rank(fair)
      .map(ranking =>
        assertEquals(
          ranking.ranked.map(_.item),
          Vector(5, 4, 3, 2, 1),
        ),
      )

  test("a judge that always prefers what it sees first favours no item"):
    // Only judging every pairing both ways round draws each one.
    Contest(List(1, 2, 3, 4), rounds = 3)
      .rank(firstSeen)
      .map(ranking =>
        assertEquals(
          ranking.ratings.map(_.wins),
          Vector.fill(4)(1.5),
        )
        ranking
          .ratings
          .foreach(rating => assertEqualsDouble(rating.elo, 1500.0, 1e-6)),
      )

  test("items placed on the ladder are scored among the anchors they meet"):
    Contest(
      List(25, 35),
      ladder(10, 20, 30, 40),
      rounds = 0,
    ).rank(fair)
      .map: ranking =>
        val lower = ranking.ratings(0).score
        val upper = ranking.ratings(1).score
        assert(lower > 2 && lower < 3, lower)
        assert(upper > 3 && upper < 4, upper)

  test("the ladder is climbed by binary search, until a draw ends it"):
    // The fourteen adjacent rungs are compared, each judged twice, and then
    // four of the fifteen rungs are searched, or only the middle one drawn.
    def climbing(item: Int): IO[(Int, Int)] =
      for
        heard   <- Ref.of[IO, List[(Int, Int)]](Nil)
        ranking <- Contest(
          List(item),
          ladder((10 to 150 by 10)*),
          rounds = 0,
        ).rank(recording(heard))
        orders <- heard.get
      yield (orders.size, ranking.judgements)
    for
      searched <- climbing(55)
      drawn    <- climbing(80)
    yield
      assertEquals(searched, (36, 36))
      assertEquals(drawn, (2 + 28, 2 + 28))

  test("the longest climbs call for as many judgements as a contest may"):
    // Every item loses to every anchor, so climbs all the way down.
    val contest = Contest(
      List(1, 2, 3),
      ladder((10 to 150 by 10)*),
      rounds = 2,
    )
    for
      heard  <- Ref.of[IO, List[(Int, Int)]](Nil)
      _      <- contest.rank(recording(heard))
      orders <- heard.get
    yield
      assertEquals(contest.maxJudgements, 56)
      assertEquals(orders.size, contest.maxJudgements)

  test("every climb begins on the middle rung, which is presented first most"):
    // A draw ends every climb on its first rung, leaving the judgements made
    // whatever the verdicts.
    val contest = Contest(List(1, 2, 3), ladder(10, 20, 30), rounds = 2)
    for
      heard <- Ref.of[IO, List[(Int, Int)]](Nil)
      _     <- contest.rank((first, second) =>
        heard.update((first, second) :: _).as(Verdict.Tie),
      )
      orders <- heard.get
    yield
      assertEquals(
        contest.presentedFirst,
        // Two round-robin pairings, each item's first rung, and the two rungs
        // either side of it.
        Map(1 -> 2, 2 -> 3, 3 -> 2, 10 -> 1, 20 -> (3 + 2), 30 -> 1),
      )
      assertEquals(
        orders.groupMapReduce(_._1)(_ => 1)(_ + _),
        contest.presentedFirst,
      )

  test("a judge's failures, raised or thrown, are reported, never fatal"):
    val flaky: Judge[IO, Int] = (first, second) =>
      if first == 3 || second == 3 then IO.raiseError(RuntimeException("3"))
      else fair(first, second)
    val broken: Judge[IO, Int] = (_, _) => throw RuntimeException("Broken.")
    val contest                = Contest(List(1, 2, 3), rounds = 3)
    for
      raised <- contest.rank(flaky)
      thrown <- contest.rank(broken)
    yield
      assertEquals(raised.failures.size, 4)
      assertEquals(raised.judgements, 2)
      assertEquals(raised.ranked.head.item, 2)
      assertEquals(thrown.failures.size, 6)
      assertEquals(thrown.judgements, 0)

object ContestSuite:

  private val fair: Judge[IO, Int] = (first, second) =>
    IO.pure(
      if first > second then Verdict.First
      else if first < second then Verdict.Second
      else Verdict.Tie,
    )

  private val firstSeen: Judge[IO, Int] = (_, _) => IO.pure(Verdict.First)

  private def recording(heard: Ref[IO, List[(Int, Int)]]): Judge[IO, Int] =
    (first, second) => heard.update((first, second) :: _) *> fair(first, second)

  /** Anchors of the given numbers, each scored a tenth of its value. */
  private def ladder(values: Int*): List[Anchor[Int]] = values
    .toList
    .map(value => Anchor(value, value / 10.0))
