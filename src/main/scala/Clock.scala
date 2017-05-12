package chess

import java.text.DecimalFormat

import Clock.Config

// All unspecified durations are expressed in seconds
case class Clock(
    config: Config,
    color: Color,
    players: Color.Map[ClockPlayer],
    timer: Option[Timestamp] = None
) {
  import Timestamp.now

  @inline private def pending(c: Color) = timer match {
    case Some(t) if c == color => t.toNow
    case _ => Centis(0)
  }

  @inline private def rawRemaining(c: Color) =
    players(c).remaining - pending(c)

  def remainingTime(c: Color) = rawRemaining(c) nonNeg

  def outOfTimeWithGrace(c: Color) = players(c).remainingGrace < pending(c)

  def moretimeable(c: Color) = rawRemaining(c).centis < 100 * 60 * 60 * 2

  def isInit = players.forall(_.isInit)

  def isRunning = timer.isDefined

  def start = if (isRunning) this else copy(timer = Some(now))

  def stop = timer.fold(this) { t =>
    copy(
      players = players.update(color, _.adjust(t.toNow)),
      timer = None
    )
  }

  def updatePlayer(c: Color)(f: ClockPlayer => ClockPlayer) =
    copy(players = players.update(c, f))

  def switch = copy(
    color = !color,
    timer = timer.map(_ => now)
  )

  def step(
    metrics: MoveMetrics = MoveMetrics(),
    withInc: Boolean = true
  ) = {
    val elapsed = timer.get.toNow

    val lagComp = players(color).lagComp(metrics.reportedLag(elapsed))

    updatePlayer(color) {
      _.adjust(
        elapsedDelta = (elapsed - lagComp) nonNeg,
        limitDelta = if (withInc) incrementOf(color) else Centis(0)
      )
    }.switch
  }
  // To do: safely add this to takeback to remove inc from player.
  // def deinc = updatePlayer(color, _.giveTime(-incrementOf(color)))

  def takeback = switch

  def giveTime(c: Color, t: Centis) = updatePlayer(c) {
    _.giveTime(t)
  }

  def setRemainingTime(c: Color, centis: Centis) = updatePlayer(c) {
    _.copy(elapsed = limit - centis)
  }

  def goBerserk(c: Color) = updatePlayer(c) { p =>
    if (p.berserk) p
    else p.copy(
      berserk = true,
      limit = p.limit - berserkPenalty
    )
  }

  protected def berserkPenalty =
    if (limitSeconds < 40 * incrementSeconds) Centis(0)
    else Centis(limitSeconds * (100 / 2))

  def incrementOf(c: Color) = if (berserked(c)) Centis(0) else increment

  def berserked(c: Color) = players(c).berserk
  def lag(c: Color) = players(c).lag

  def emergSeconds = config.emergSeconds
  def estimateTotalSeconds = config.estimateTotalSeconds
  def estimateTotalTime = config.estimateTotalTime
  def increment = config.increment
  def incrementSeconds = config.incrementSeconds
  def limit = config.limit
  def limitInMinutes = config.limitInMinutes
  def limitSeconds = config.limitSeconds
}

case class ClockPlayer(
    limit: Centis,
    elapsed: Centis = Centis(0),
    lag: Centis = Centis(0),
    berserk: Boolean = false
) {
  import ClockPlayer._

  def isInit = elapsed.centis == 0

  def adjust(elapsedDelta: Centis, limitDelta: Centis = Centis(0)) = copy(
    elapsed = elapsed + elapsedDelta,
    limit = limit + limitDelta
  )

  def remaining = limit - elapsed

  def giveTime(t: Centis) = copy(limit = limit + t)

  def lagComp(lagOpt: Option[Centis]): Centis =
    lagOpt.fold(Centis(0)) { _ atMost maxLagComp }

  def remainingGrace = remaining + ((lag * 2) atMost maxGrace)
}

object ClockPlayer {
  val maxLagComp = Centis(100)
  val maxGrace = Centis(100)
}

object Clock {
  private val limitFormatter = new DecimalFormat("#.##")

  // All unspecified durations are expressed in seconds
  case class Config(limitSeconds: Int, incrementSeconds: Int) {

    def berserkable = incrementSeconds == 0 || limitSeconds > 0

    def emergSeconds = math.min(60, math.max(10, limitSeconds / 8))

    def estimateTotalSeconds = limitSeconds + 40 * incrementSeconds

    def estimateTotalTime = Centis.ofSeconds(estimateTotalSeconds)

    def hasIncrement = incrementSeconds > 0

    def increment = Centis.ofSeconds(incrementSeconds)

    def limit = Centis.ofSeconds(limitSeconds)

    def limitInMinutes = limitSeconds / 60d

    def toClock = Clock(this)

    def limitString = limitSeconds match {
      case l if l % 60 == 0 => l / 60
      case 15 => "¼"
      case 30 => "½"
      case 45 => "¾"
      case 90 => "1.5"
      case _ => limitFormatter.format(limitSeconds / 60d)
    }

    override def toString = s"$limitString+$incrementSeconds"
  }

  // [TimeControl "600+2"] -> 10+2
  def readPgnConfig(str: String): Option[Config] = str.split('+') match {
    case Array(initStr, incStr) => for {
      init <- parseIntOption(initStr)
      inc <- parseIntOption(incStr)
    } yield Config(init, inc)
    case _ => none
  }

  val minLimit = Centis(300)

  def apply(limit: Int, increment: Int): Clock = apply(Config(limit, increment))

  def apply(config: Config): Clock = {
    val initTime = {
      if (config.limitSeconds == 0) config.increment atLeast minLimit
      else config.limit
    }

    Clock(
      config = config,
      color = White,
      players = Color.Map(_ => ClockPlayer(limit = initTime)),
      timer = None
    )
  }
}
