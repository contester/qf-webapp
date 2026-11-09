package utils

import org.scalatest.funsuite.AnyFunSuite

class PrintableCheckSpec extends AnyFunSuite {
  private def bytes(s: String, cs: String = "UTF-8") = s.getBytes(cs)

  test("source code and plain text pass, in UTF-8 and cp1251, with CRLF and tabs") {
    assert(PrintableCheck.check(bytes("int main() {\r\n\treturn 0;\r\n}\r\n")) == PrintableCheck.Ok)
    assert(PrintableCheck.check(bytes("// привет\n", "windows-1251")) == PrintableCheck.Ok)
    assert(PrintableCheck.check(bytes("// привет\n")) == PrintableCheck.Ok)
    assert(PrintableCheck.check(Array.emptyByteArray) == PrintableCheck.Ok)
  }

  test("images and other binaries are rejected") {
    val png = Array(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x0d).map(_.toByte)
    assert(PrintableCheck.check(png) == PrintableCheck.Binary)
    assert(PrintableCheck.check(bytes("MZ") ++ Array(0.toByte, 3.toByte)) == PrintableCheck.Binary)
  }

  test("too many lines or too many bytes is rejected, the limit itself is fine") {
    assert(PrintableCheck.check(bytes("x\n" * (PrintableCheck.MaxLines - 1))) == PrintableCheck.Ok)
    assert(PrintableCheck.check(bytes("x\n" * PrintableCheck.MaxLines)) == PrintableCheck.TooLong)
    assert(PrintableCheck.check(bytes("x" * (PrintableCheck.MaxBytes + 1))) == PrintableCheck.TooLong)
  }
}
