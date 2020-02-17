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
    Pos.D8 -> Black.king,
    Pos.E8 -> Black.queen,
    Pos.D1 -> White.queen,
    Pos.E1 -> White.king,
  )

  override val castles = Castles.none

  override val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBKQBNR w - - 0 1"

  override def isInsufficientMaterial(board: Board)                  = false
  override def opponentHasInsufficientMaterial(situation: Situation) = false
}
