package chess

case class MoveMetrics(
  clientLag: Option[Centis] = None,
  clientMoveTime: Option[Centis] = None
);