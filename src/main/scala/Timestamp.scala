package chess

case class Timestamp(value: Long) extends AnyVal with Ordered[Timestamp] {
  import Timestamp._

  def -(o: Timestamp) = Centis(roundTenths(value - o.value))

  def toNow = Centis(roundTenths(nowMillis - value))

  def compare(other: Timestamp) = value compare other.value
}

object Timestamp {
  def now = Timestamp(nowMillis)

  @inline protected def nowMillis = System.currentTimeMillis

  @inline protected def roundTenths(l: Long) = {
    (if (l > 0) l + 5 else l - 4) / 10
  }
}