package chess

import Pos._

class AlfilTest extends ChessTest {

  "an alfil" should {

    val alfil = White - Alfil

    "move 2 positions diagonally" in {
      pieceMoves(alfil, D4) must bePoss(B2, B6, F2, F6)
    }

    "move 2 positions diagonally, even from the edges" in {
      pieceMoves(alfil, H1) must bePoss(F3)
      pieceMoves(alfil, D1) must bePoss(B3, F3)
    }

    "capture opponent" in {
      """
P   p
 PPPPPPP
R AFK  R""" destsFrom (C1) must bePoss(E3)
    }
  }
}
