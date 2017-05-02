package chess

case class MoveMetrics(
    clientLag: Option[Centis] = None,
    clientMoveTime: Option[Centis] = None
) {
  def estimateLag(elapsed: Centis) =
    clientMoveTime.fold(clientLag)(mt => Some(elapsed - mt))
}