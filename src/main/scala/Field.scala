package com.alecdorrington.dike

import com.alecdorrington.dike.Calibration.Knot

/**
  * The players of a [[Contest]], indexed by the items in the order given, then
  * by the anchors, lowest score first.
  */
private[dike] final case class Field[A](contest: Contest[A]):

  val ladder: Vector[Anchor[A]] = contest.anchors.sortBy(_.score).toVector

  val players: Vector[A] = contest.items.toVector ++ ladder.map(_.item)

  /** The number of items, which are the first players. */
  val itemCount: Int = contest.items.size

  /** The index of the player on the given rung of the ladder. */
  def onRung(rung: Int): Int = itemCount + rung

  /** The pairings judged whatever the verdicts. */
  def schedule: List[(Int, Int)] = peerings ++ rungPairings

  def peerings: List[(Int, Int)] = RoundRobin
    .pairings(itemCount, contest.rounds)
    .toList

  /**
    * Each anchor paired with the next one up, without which items beating every
    * anchor would leave the anchors indistinguishable, and calibration
    * impossible.
    */
  def rungPairings: List[(Int, Int)] =
    val climbed = ladder.indices.map(onRung)
    climbed.zip(climbed.drop(1)).toList

  def maxJudgements: Int = Comparison.sides *
    (schedule.size + itemCount * Field.longestClimb(ladder.size))

  /** The pairings sure to be judged: the schedule, and each item's first rung. */
  def certainPairings: List[(Int, Int)] =
    if ladder.isEmpty then schedule
    else
      val start = onRung(Field.middle(ladder.indices))
      schedule ++ List.range(0, itemCount).map(_ -> start)

  def presentedFirst: Map[A, Int] = certainPairings
    .flatMap((first, second) => List(first, second))
    .groupMapReduce(players)(_ => 1)(_ + _)

  def knots(abilities: Vector[Double]): List[Knot] = ladder
    .indices
    .map(rung =>
      Knot(
        abilities(onRung(rung)),
        ladder(rung).score,
      ),
    )
    .toList

private[dike] object Field:

  /** The most rungs a binary search compares with on a ladder this tall. */
  def longestClimb(rungs: Int): Int =
    if rungs <= 0 then 0 else 1 + longestClimb(rungs / 2)

  /** The rung a climb over the given range compares with next. */
  def middle(rungs: Range): Int = rungs(rungs.size / 2)
