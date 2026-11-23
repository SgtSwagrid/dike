package com.alecdorrington.dike

import com.alecdorrington.dike.BradleyTerry.Outcome

/** The outcomes of one judgement, or the error that prevented it. */
private[dike] type Judgement = Either[Throwable, List[Outcome]]

/**
  * One pairing judged both ways round, to cancel the judge's preference for
  * whichever it saw first.
  *
  * @param verdict
  *   The verdict the judgements that succeeded agreed on, if they did.
  */
private[dike] final case class Comparison
  (
    judgements: List[Judgement],
    verdict: Option[Verdict],
  )

private[dike] object Comparison:

  /** The share of a comparison one judgement makes, as a pairing has two. */
  val judgementWeight: Double = 0.5

  val sides: Int = 2

  /**
    * Gathers a pairing's judgements, each verdict stated for the pairing in the
    * order given.
    */
  def of
    (
      verdicts: List[Either[Throwable, Verdict]],
      pair: (Int, Int),
    )
    : Comparison = Comparison(
    verdicts.map(_.map(outcomes(_, pair))),
    agreed(verdicts.flatMap(_.toOption)),
  )

  /** Contradictory judgements agree on nothing, as the players are too alike. */
  private def agreed(verdicts: List[Verdict]): Option[Verdict] =
    verdicts.distinct match
      case List(only) => Some(only)
      case _          => None

  def outcomes(verdict: Verdict, pair: (Int, Int)): List[Outcome] =
    val (first, second) = pair
    verdict match
      case Verdict.First  => List(Outcome(first, second, judgementWeight))
      case Verdict.Second => List(Outcome(second, first, judgementWeight))
      case Verdict.Tie    => List(
          Outcome(first, second, judgementWeight / 2),
          Outcome(second, first, judgementWeight / 2),
        )
