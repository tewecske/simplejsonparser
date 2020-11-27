package tewecske.simplejsonparser

import zio.test._
import Assertion._
import tewecske.simplejsonparser.SimpleJsonParserMain.JsonValue._
import tewecske.simplejsonparser.SimpleJsonParserMain._

object SimpleJsonParserSpec extends DefaultRunnableSpec {

  val suite1 = suite("suite1")(
    test("charP")(
      assert(charP('m')("hello"))(equalTo(None)) &&
        assert(charP('m')("mychar"))(equalTo(Some(("ychar", 'm'))))
    ),
    test("stringP")(
      assert(stringP("m")("hello"))(equalTo(None)) &&
        assert(stringP("my")("my"))(equalTo(Some(("", "my"))))
    ),
    test("spanP")(
      assert(spanP(Character.isDigit)("1"))(equalTo(Some(("", "1")))) &&
        assert(spanP(_ == 'm')("my"))(equalTo(Some(("y", "m")))) &&
        assert(spanP(_ == 'm')("mm"))(equalTo(Some(("", "mm")))) &&
        assert(spanP(_ => false)("mm"))(equalTo(Some(("mm", ""))))
    ),
    test("nonEmptyString")(
      assert(nonEmptyString(_ => Some(("", "hello")))(""))(equalTo(Some(("", "hello")))) &&
        assert(nonEmptyString(_ => Some(("", "")))(""))(equalTo(None))
    ),
    test("stringLiteral")(
      assert(stringLiteral("\"asd\""))(equalTo(Some(("", "asd")))) &&
        assert(stringLiteral("hello"))(equalTo(None))
    ),
    test("jsonNull")(
      assert(jsonNull("hello"))(equalTo(None)) &&
        assert(jsonNull("nullrest"))(equalTo(Some(("rest", JsonNull)))) &&
        assert(jsonNull("null"))(equalTo(Some(("", JsonNull))))
    ),
    test("jsonBoolean")(
      assert(jsonBoolean("hello"))(equalTo(None)) &&
        assert(jsonBoolean("truerest"))(equalTo(Some(("rest", JsonBoolean(true))))) &&
        assert(jsonBoolean("true"))(equalTo(Some(("", JsonBoolean(true))))) &&
        assert(jsonBoolean("false"))(equalTo(Some(("", JsonBoolean(false)))))
    ),
    test("jsonNumber")(
      assert(jsonNumber("hello"))(equalTo(None)) &&
        assert(jsonNumber("123rest"))(equalTo(Some(("rest", JsonNumber(123))))) &&
        assert(jsonNumber("456"))(equalTo(Some(("", JsonNumber(456)))))
    ),
    test("jsonString")(
      assert(jsonString("hello"))(equalTo(None)) &&
        assert(jsonString("\"yo\"asd"))(equalTo(Some(("asd", JsonString("yo"))))) &&
        assert(jsonString("\"yoyo\""))(equalTo(Some(("", JsonString("yoyo")))))
    ),
    test("jsonArray")(
      assert(jsonArray("hello"))(equalTo(None)) &&
        assert(jsonArray("[ \"elements\", 123]asd"))(equalTo(Some(("asd", JsonArray(List(JsonString("elements"), JsonNumber(123))))))) &&
        assert(jsonArray("[]"))(equalTo(Some(("", JsonArray(List()))))) &&
        assert(jsonArray("[\"elements\"]"))(equalTo(Some(("", JsonArray(List(JsonString("elements")))))))
    ),
    test("jsonObject")(
      assert(jsonObject("hello"))(equalTo(None)) &&
        assert(jsonObject("{\"key1\":123}"))(equalTo(Some(("", JsonObject(List(("key1", JsonNumber(123)))))))) &&
        assert(jsonObject("{ \"key1\": \"value1\", \"key2\":123}asd"))(equalTo(Some(("asd", JsonObject(List(("key1", JsonString("value1")), ("key2", JsonNumber(123))))))))
    ),
    test("jsonValue")(
      assert(jsonValue("hello"))(equalTo(None)) &&
        assert(jsonValue(
          """{
            |"key1":123,
            |"array1": ["a", 123, [null, true, []], [false]],
            |"obj1": {"key2": false, "array2": [], "obj2": {"key3": []}}
            |}""".stripMargin))(equalTo(Some(("",
          JsonObject(List[(String, JsonValue)](
            ("key1", JsonNumber(123)),
              ("array1",JsonArray(List(JsonString("a"), JsonNumber(123),
                JsonArray(List(JsonNull, JsonBoolean(true), JsonArray(List()))), JsonArray(List(JsonBoolean(false)))))),
            ("obj1",JsonObject(List(("key2",JsonBoolean(false)), ("array2",JsonArray(List())), ("obj2", JsonObject(List(("key3", JsonArray(List()))))))))
            ))))))
    ),
    //"array1": ["a", 123, [null, true]],
    // "obj1": {"key2": false, "array2": []}
  )

  def spec = suite("All tests")(suite1)

}
