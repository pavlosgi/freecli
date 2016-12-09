package pavlosgi.freecli.core

import java.io.File

import pavlosgi.freecli.testkit.Test

class StringDecoderTest extends Test {

  describe("StringDecoder Test") {
    it("should parse string") {
      implicitly[StringDecoder[String]].apply("test").valid should
        === ("test")
    }

    it("should parse int") {
      implicitly[StringDecoder[Int]].apply("1").valid should === (1)
    }

    it("should fail to parse int if not a number") {
      implicitly[StringDecoder[Int]].apply("test").isInvalid should be (true)
    }

    it("should parse double") {
      implicitly[StringDecoder[Double]].apply("1.1").valid should
        === (1.1)
    }

    it("should fail to parse double if not a number") {
      implicitly[StringDecoder[Double]].apply("test").isInvalid should be (true)
    }

    it("should parse long") {
      implicitly[StringDecoder[Long]].apply("10000").valid should
        === (10000L)
    }

    it("should fail to parse long if not a number") {
      implicitly[StringDecoder[Long]].apply("test").isInvalid should be (true)
    }

    it("should parse string to file") {
      val filePath = getClass.getResource("/test.json").getFile
      implicitly[StringDecoder[File]].apply(filePath).valid.getAbsolutePath should
        === (filePath)
    }

    it("should parse string to existent file") {
      val filePath = getClass.getResource("/test.json").getFile
      implicitly[StringDecoder[File]].apply(filePath).valid.getAbsolutePath should
        === (filePath)
    }

    it("should fail to parse string to existent file if it does not exist") {
      val filePath = s"/${System.currentTimeMillis}"
      implicitly[StringDecoder[ExistentFile]].apply(filePath).isInvalid should be (true)
    }

    it("should parse string to new file") {
      val filePath = s"/${System.currentTimeMillis}"
      implicitly[StringDecoder[NewFile]].apply(filePath).valid.getAbsolutePath should
        === (filePath)
    }

    it("should fail to parse string to new file if it exists") {
      val filePath = getClass.getResource("/test.json").getFile
      implicitly[StringDecoder[NewFile]].apply(filePath).isInvalid should be (true)
    }
  }
}
