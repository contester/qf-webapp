package utils

case class GridfsContent(content: String, truncated: Boolean, size: Option[Long]) {
  def sizeStr = size.map(x => s"(${x}b)").getOrElse("")
  def truncStr = if (truncated) "[обрезан]" else ""
}