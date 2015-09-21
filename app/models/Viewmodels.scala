package models

import com.googlecode.htmlcompressor.compressor.HtmlCompressor

object Compressor {
  val compressor = new HtmlCompressor()
  compressor.setRemoveSurroundingSpaces("html,head,body,br,p,h1,h2,h3,h4,h5,h6,blockquote,center,dl,fieldset,form,frame,frameset,hr,noframes,ol,table,tbody,tr,td,th,tfoot,thead,ul")
  def apply(html: String) = compressor.compress(html)
}