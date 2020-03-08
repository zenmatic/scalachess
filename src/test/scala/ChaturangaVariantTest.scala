package chess

import scalaz.Validation.FlatMap._
import variant.Chaturanga
import format.Forsyth
import format.pgn.Reader

class ChaturangaVariantTest extends ChessTest {

   "Chaturanga" should {
       "initialize the board without castling rights" in {
           Board.init(Chaturanga).history.castles.isEmpty must beTrue
        }
   }

}
