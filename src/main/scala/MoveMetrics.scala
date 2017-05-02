package chess

case class MoveMetrics(
    clientLag: Option[Centis] = None,
    clientMoveTime: Option[Centis] = None
) {
  def estimateLag(elapsed: Centis) =
    clientMoveTime.fold(metrics.clientLag)(mt => Some(elapsed - mt))
}