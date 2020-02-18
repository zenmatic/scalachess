package chess

import Pos._

class FerzTest extends ChessTest {

  "a ferz" should {

    val ferz = White - Ferz

    "move 1 position diagonally" in {
      pieceMoves(ferz, D4) must bePoss(C3, E3, C5, E5)
    }

    "move 1 position diagonally, even from the edges" in {
      pieceMoves(ferz, H8) must bePoss(G7)
    }

    "capture opponent" in {
      """
  P p  p
R  FK  R""" destsFrom (D1) must bePoss(E2)
    }
  }
}
