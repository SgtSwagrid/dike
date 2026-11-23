package com.alecdorrington.dike

import cats.{MonadThrow, Parallel}
import cats.syntax.functor.*

/**
  * A ranking to be made of some items by judging them in pairs.
  *
  * Every pairing is judged twice, once with each item presented first, to
  * cancel the judge's preference for the first. The items meet on a
  * [[RoundRobin]] schedule, each climbs the ladder of anchors by binary search,
  * and [[BradleyTerry]] fits it all together. Two or more anchors calibrate the
  * scores (see [[Calibration]]); with fewer, the scores are read off the
  * [[curve]] (see [[Curving]]).
  *
  * @tparam A
  *   The type of the items.
  *
  * @param items
  *   The items to rank.
  *
  * @param anchors
  *   The items of known score, in any order, against which the items are
  *   placed, themselves unranked.
  *
  * @param rounds
  *   The number of round-robin rounds, each giving every item a different
  *   opponent, capped where every pairing is exhausted. `0` places the items
  *   against the anchors alone.
  *
  * @param curve
  *   The distribution the items' standings are mapped onto when too few anchors
  *   calibrate the scores.
  */
final case class Contest[A]
  (
    items: Seq[A],
    anchors: Seq[Anchor[A]] = Nil,
    rounds: Int = 1,
    curve: Curve = Curve.standard,
  ):

  /**
    * Makes every judgement the contest calls for, then ranks the items.
    *
    * @tparam F
    *   The effect the judge works in.
    *
    * @param judge
    *   The judge of every comparison, called in parallel wherever no judgement
    *   waits on another's verdict, so it should limit its own concurrency.
    *
    * @return
    *   An effect producing the ranking, which never fails with the judge: a
    *   failed judgement is left out and reported in [[Ranking.failures]].
    */
  def rank[F[_] : {MonadThrow, Parallel}](judge: Judge[F, A]): F[Ranking[A]] =
    val field = Field(this)
    Judging(field, judge).judgements.map(Ranking.of(field, _))

  /**
    * Counts the most judgements the contest may call for, to price a judge
    * before it runs.
    *
    * @return
    *   The most judgements, usually more than are made, as a climb up the
    *   ladder ends early on a draw.
    */
  def maxJudgements: Int = Field(this).maxJudgements

  /**
    * Counts the judgements sure to present each item and anchor first, whatever
    * the verdicts: one for either side of every pairing on the schedule, and of
    * each item's first rung up the ladder. A judge caching what it reads of the
    * item presented first can tell from this which it will read again.
    *
    * @return
    *   Every item and anchor that such a judgement presents first, with how
    *   many do.
    */
  def presentedFirst: Map[A, Int] = Field(this).presentedFirst
