package chess

case class Timestamp(value: Long) extends AnyVal with Ordered[Timestamp] {
  import Timestamp._

  def -(o: Timestamp) = Centis.ofMillis(value - o.value)

  def toNow = Centis.ofMillis(nowMillis - value)

  def compare(other: Timestamp) = value compare other.value
}

object Timestamp {
  def now = Timestamp(nowMillis)

  @inline protected def nowMillis = System.currentTimeMillis
}