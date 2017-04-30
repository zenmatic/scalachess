package chess

import java.text.DecimalFormat

case class Player(
    time: Centis = Centis(0),
    lag: Centis = Centis(0),
    berserk: Boolean = false
) {
  def addTime(t: Centis) = copy(time = time + t)
}

// All unspecified durations are expressed in seconds
sealed trait Clock[+A <: Clock[A]] {
  val config: Clock.Config
  val players: Color.Map[Player]
  val color: Color

  def lag(c: Color) = players(c).lag

  def limitSeconds = config.limitSeconds
  def limit = config.limit

  def incrementSeconds = config.incrementSeconds
  def increment = config.increment

  def time(c: Color): Centis = players(c).time

  def outoftime(c: Color) = remainingTime(c).centis == 0

  def outoftimeWithGrace(c: Color) =
    timeSinceFlag(c).exists((lag(c) * 2 atMost Clock.maxLagToCompensate).<)

  def remainingTime(c: Color) = (limit - elapsedTime(c)) nonNeg

  def incrementOf(c: Color) = if (berserked(c)) Centis(0) else increment

  private def timeSinceFlag(c: Color): Option[Centis] = (limit - elapsedTime(c)) match {
    case s if s.centis <= 0 => Some(-s)
    case _ => None
  }

  def elapsedTime(c: Color) = time(c)

  def limitInMinutes = config.limitInMinutes

  def estimateTotalIncrement = config.estimateTotalIncrement

  def estimateTotalTime = config.estimateTotalTime

  def estimateTotalSeconds = config.estimateTotalSeconds

  // Emergency time cutoff, in seconds.
  def emergTime = config.emergTime

  def stop: PausedClock

  def start: RunningClock

  def berserked(c: Color) = players(c).berserk

  def show = config.toString

  def moretimeable(c: Color) = remainingTime(c).centis < 100 * 60 * 60 * 2

  def isRunning: Boolean

  def isInit = elapsedTime(White).centis == 0 && elapsedTime(Black).centis == 0

  def reset = Clock(config)

  protected def now = Timestamp.now

  private[chess] def berserkPenalty =
    if (limitSeconds < estimateTotalSeconds) Centis(0)
    else Centis(limitSeconds * (100 / 2))

  // Typed methods.
  def updatePlayer(c: Color, f: Player => Player): A

  def addTime(c: Color, t: Centis): A = updatePlayer(c, _.addTime(t))

  def giveTime(c: Color, t: Centis): A = addTime(c, -t)

  def setRemainingTime(c: Color, centis: Centis): A =
    addTime(c, remainingTime(c) - centis)

  def goBerserk(c: Color): A = updatePlayer(c, p => {
    if (p.berserk) p
    else p.addTime(berserkPenalty).copy(berserk = true)
  })

  def switch: A
}

final case class RunningClock(
    config: Clock.Config,
    color: Color,
    players: Color.Map[Player],
    timer: Timestamp
) extends Clock[RunningClock] {

  val isRunning = true

  override def elapsedTime(c: Color) = {
    if (c == color) (timer to now) + time(c) else time(c)
  }

  override def updatePlayer(c: Color, f: Player => Player) =
    copy(players = players.update(c, f))

  def step(lag: Centis = Centis(0), withInc: Boolean = true) = {
    val t = now
    val lagComp = lag atMost Clock.maxLagToCompensate nonNeg
    val inc = if (withInc) incrementOf(color) else Centis(0)
    val elapsed = timer to t
    copy(color = !color, timer = t).addTime(
      color,
      ((elapsed - lagComp) nonNeg) - inc
    )
  }

  def stop = PausedClock(
    config = config,
    color = color,
    players = players.update(color, _.addTime(timer to now))
  )

  def start = this

  def switch = copy(
    color = !color,
    timer = now
  )
}

final case class PausedClock(
    config: Clock.Config,
    color: Color,
    players: Color.Map[Player]
) extends Clock[PausedClock] {

  val isRunning = false

  def stop = this

  def switch = copy(color = !color)

  def updatePlayer(c: Color, f: Player => Player) =
    copy(players = players.update(c, f))

  def start = RunningClock(
    config = config,
    color = color,
    players = players,
    timer = now
  )
}

object Clock {
  type Any = Clock[_]

  private val limitFormatter = new DecimalFormat("#.##")

  // All unspecified durations are expressed in seconds
  case class Config(limitSeconds: Int, incrementSeconds: Int) {
    def limitInMinutes = limitSeconds / 60d

    def limit = Centis.ofSeconds(limitSeconds)

    def estimateTotalIncrement = Centis.ofSeconds(estimateTotalIncSeconds)

    def estimateTotalIncSeconds = 40 * incrementSeconds

    def estimateTotalTime = Centis.ofSeconds(estimateTotalSeconds)

    def estimateTotalSeconds = limitSeconds + estimateTotalIncSeconds

    // Emergency time cutoff, in seconds. 
    def emergTime = math.min(60, math.max(10, limitSeconds / 8))

    def hasIncrement = incrementSeconds > 0

    def increment = Centis.ofSeconds(incrementSeconds)

    def berserkable = incrementSeconds == 0 || limitSeconds > 0

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

  val minInitLimit = Centis(300)
  // no more than this time will be offered to the lagging player
  val maxLagToCompensate = Centis(100)

  def apply(limit: Int, increment: Int): PausedClock = apply(Config(limit, increment))

  def apply(config: Config): PausedClock = {
    val initTime = {
      if (config.limitSeconds == 0) -(config.increment atLeast minInitLimit)
      else Centis(0)
    }

    PausedClock(
      config = config,
      color = White,
      players = Color.Map(_ => Player(time = initTime))
    )
  }

  def formatSeconds(t: Int) = periodFormatter.print(
    org.joda.time.Duration.standardSeconds(t).toPeriod
  )

  private val periodFormatter = new org.joda.time.format.PeriodFormatterBuilder()
    .printZeroAlways
    .minimumPrintedDigits(1).appendHours.appendSeparator(":")
    .minimumPrintedDigits(2).appendMinutes.appendSeparator(":")
    .appendSeconds
    .toFormatter
}
