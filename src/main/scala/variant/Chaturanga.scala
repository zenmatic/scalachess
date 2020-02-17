package chess
package variant

case object Chaturanga
    extends Variant(
      id = 11,
      key = "chaturanga",
      name = "Chaturanga",
      shortName = "Catur",
      title = "Ancient Indian strategy game.",
      standardInitialPosition = false
    ) {

  override def allowsCastling = false

  // The kings (Rajas) do not face each other
  override val pieces: Map[Pos, Piece] = Map(
    Pos.A1 -> White.rook,
    Pos.B1 -> White.knight,
    Pos.C1 -> White.bishop,
    Pos.D1 -> White.queen,
    Pos.E1 -> White.king,
    Pos.F1 -> White.bishop,
    Pos.G1 -> White.knight,
    Pos.H1 -> White.rook,
    Pos.A2 -> White.pawn,
    Pos.B2 -> White.pawn,
    Pos.C2 -> White.pawn,
    Pos.D2 -> White.pawn,
    Pos.E2 -> White.pawn,
    Pos.F2 -> White.pawn,
    Pos.G2 -> White.pawn,
    Pos.H2 -> White.pawn,
    Pos.A7 -> Black.pawn,
    Pos.B7 -> Black.pawn,
    Pos.C7 -> Black.pawn,
    Pos.D7 -> Black.pawn,
    Pos.E7 -> Black.pawn,
    Pos.F7 -> Black.pawn,
    Pos.G7 -> Black.pawn,
    Pos.H7 -> Black.pawn,
    Pos.A8 -> White.rook,
    Pos.B8 -> White.knight,
    Pos.C8 -> White.bishop,
    Pos.D8 -> Black.king,
    Pos.E8 -> Black.queen,
    Pos.F8 -> White.bishop,
    Pos.G8 -> White.knight,
    Pos.H8 -> White.rook,
  )

  override val castles = Castles.none

  override val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBKQBNR w - - 0 1"
}
