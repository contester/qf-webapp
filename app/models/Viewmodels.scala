package models

import com.googlecode.htmlcompressor.compressor.HtmlCompressor

object Compressor {
  val compressor = new HtmlCompressor()
  def apply(html: String) = compressor.compress(html)
}