package chess

case class Timestamp(value: Long) extends AnyVal with Ordered[Timestamp] {
  import Timestamp._

  def -(o: Timestamp) = Centis.ofMillis(value - o.value)

  def +(o: Centis) = Timestamp(value + o.millis)

  def compare(other: Timestamp) = value compare other.value
}

trait TimeProvider {

  def nowMillis: Long

  def now = Timestamp(nowMillis)

  def toNow(ts: Timestamp) = Centis.ofMillis(nowMillis - ts.value)
}

object SystemTimeProvider extends TimeProvider {

  @inline protected def nowMillis = System.currentTimeMillis
}
