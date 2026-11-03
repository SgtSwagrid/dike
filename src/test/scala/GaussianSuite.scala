package com.alecdorrington.dike

import munit.FunSuite

class GaussianSuite extends FunSuite:

  test("quantiles are reproduced in the centre and the tails, antisymmetric"):
    assertEqualsDouble(Gaussian.quantile(0.5), 0.0, 1e-9)
    List(
      0.75  -> 0.674490,
      0.975 -> 1.959964,
      0.01  -> -2.326348,
      0.999 -> 3.090232,
    ).foreach((p, quantile) =>
      assertEqualsDouble(Gaussian.quantile(p), quantile, 1e-5, p),
    )
    List(0.01, 0.1, 0.25, 0.4).foreach: p =>
      assertEqualsDouble(
        Gaussian.quantile(p),
        -Gaussian.quantile(1 - p),
        1e-9,
        p,
      )

  test("probabilities outside the open unit interval are rejected"):
    intercept[IllegalArgumentException](Gaussian.quantile(0))
    intercept[IllegalArgumentException](Gaussian.quantile(1))
