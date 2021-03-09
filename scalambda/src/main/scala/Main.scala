package example

import java.io.{ InputStream, OutputStream }

class Main {
  def greeting(input: InputStream, output: OutputStream): Unit = {
    output.write("hello".getBytes("UTF-8"))
  }
}
