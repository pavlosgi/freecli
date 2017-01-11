package pavlosgi.freecli.core

import java.io.File

import pavlosgi.freecli.core.api.{ExistentFile, NewFile, StringDecoder}
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

    it("should parse string to seq of string") {
      implicitly[StringDecoder[Seq[String]]].apply("s1,s2,s3").valid should
        contain theSameElementsAs Seq("s1", "s2", "s3")
    }

    it("should parse string to list of string") {
      implicitly[StringDecoder[List[String]]].apply("s1,s2,s3").valid should
        contain theSameElementsAs List("s1", "s2", "s3")
    }

    it("should parse string to list of ints") {
      implicitly[StringDecoder[List[Int]]].apply("1,2,3").valid should
        contain theSameElementsAs List(1, 2, 3)
    }

    it("should fail to parse string to list of ints") {
      implicitly[StringDecoder[List[Int]]].apply("1,s2").isInvalid should be (true)
    }

    it("should parse string to map of strings to ints") {
      implicitly[StringDecoder[Map[String, Int]]].apply("x1=1,x2=2,x3=3").valid.toList should
        contain theSameElementsAs Map("x1" -> 1, "x2" -> 2, "x3" -> 3).toList
    }

    it("should parse string to map of ints to ints") {
      implicitly[StringDecoder[Map[Int, Int]]].apply("1=1,2=2,3=3").valid.toList should
        contain theSameElementsAs Map(1 -> 1, 2 -> 2, 3 -> 3).toList
    }

    it("should fail to parse string to map of ints to ints") {
      implicitly[StringDecoder[Map[Int, Int]]].apply("1=1,2=2,3=").isInvalid should be (true)
    }
  }
}
