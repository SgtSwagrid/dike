package com.alecdorrington.dike

import munit.FunSuite

final class RoundRobinSuite extends FunSuite:

  private def played(players: Int, rounds: Int): Seq[(Int, Int)] = RoundRobin
    .pairings(players, rounds)
    .map((a, b) => (a.min(b), a.max(b)))

  test("a full round-robin plays every pairing exactly once, and no more"):
    assertEquals(played(6, 5).size, 15)
    assertEquals(played(6, 5).distinct.size, 15)
    assertEquals(played(5, 5).size, 10)
    assertEquals(played(5, 5).distinct.size, 10)
    assertEquals(
      RoundRobin.pairings(4, 99).toList,
      RoundRobin.pairings(4, 3).toList,
    )

  test("every player is paired once per round, but one in an odd field"):
    val rounds = RoundRobin.pairings(8, 3).grouped(4).toList
    assertEquals(rounds.size, 3)
    rounds.foreach(round =>
      assertEquals(
        round.flatMap((a, b) => Seq(a, b)).sorted.toList,
        (0 until 8).toList,
      ),
    )
    val odd = RoundRobin.pairings(5, 1)
    assertEquals(odd.size, 2)
    assertEquals(
      odd.flatMap((a, b) => Seq(a, b)).distinct.size,
      4,
    )

  test("a field too small to pair, or no rounds, schedules nothing"):
    assert(RoundRobin.pairings(1, 3).isEmpty)
    assert(RoundRobin.pairings(0, 3).isEmpty)
    assert(RoundRobin.pairings(4, 0).isEmpty)
