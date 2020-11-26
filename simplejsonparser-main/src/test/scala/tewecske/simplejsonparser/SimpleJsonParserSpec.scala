package tewecske.simplejsonparser

import zio.test._
import Assertion._
import tewecske.simplejsonparser.SimpleJsonParserMain.JsonValue.{JsonArray, JsonBoolean, JsonNull, JsonNumber, JsonObject, JsonString}
import tewecske.simplejsonparser.SimpleJsonParserMain.{JsonValue, charP, jsonArray, jsonBoolean, jsonNull, jsonNumber, jsonObject, jsonString, jsonValue, nonEmptyString, runParser, spanP, stringLiteral, stringP}

object SimpleJsonParserSpec extends DefaultRunnableSpec {

  val suite1 = suite("suite1")(
    test("charP")(
      assert(runParser(charP('m'))("hello"))(equalTo(None)) &&
        assert(runParser(charP('m'))("mychar"))(equalTo(Some(("ychar", 'm'))))
    ),
    test("stringP")(
      assert(runParser(stringP("m"))("hello"))(equalTo(None)) &&
        assert(runParser(stringP("my"))("my"))(equalTo(Some(("", "my"))))
    ),
    test("spanP")(
      assert(runParser(spanP(Character.isDigit))("1"))(equalTo(Some(("", "1")))) &&
        assert(runParser(spanP(_ == 'm'))("my"))(equalTo(Some(("y", "m")))) &&
        assert(runParser(spanP(_ == 'm'))("mm"))(equalTo(Some(("", "mm")))) &&
        assert(runParser(spanP(_ => false))("mm"))(equalTo(Some(("mm", ""))))
    ),
    test("nonEmptyString")(
      assert(runParser(nonEmptyString(_ => Some(("", "hello"))))(""))(equalTo(Some(("", "hello")))) &&
        assert(runParser(nonEmptyString(_ => Some(("", ""))))(""))(equalTo(None))
    ),
    test("stringLiteral")(
      assert(runParser(stringLiteral)("\"asd\""))(equalTo(Some(("", "asd")))) &&
        assert(runParser(stringLiteral)("hello"))(equalTo(None))
    ),
    test("jsonNull")(
      assert(runParser(jsonNull)("hello"))(equalTo(None)) &&
        assert(runParser(jsonNull)("nullrest"))(equalTo(Some(("rest", JsonNull)))) &&
        assert(runParser(jsonNull)("null"))(equalTo(Some(("", JsonNull))))
    ),
    test("jsonBoolean")(
      assert(runParser(jsonBoolean)("hello"))(equalTo(None)) &&
        assert(runParser(jsonBoolean)("truerest"))(equalTo(Some(("rest", JsonBoolean(true))))) &&
        assert(runParser(jsonBoolean)("true"))(equalTo(Some(("", JsonBoolean(true))))) &&
        assert(runParser(jsonBoolean)("false"))(equalTo(Some(("", JsonBoolean(false)))))
    ),
    test("jsonNumber")(
      assert(runParser(jsonNumber)("hello"))(equalTo(None)) &&
        assert(runParser(jsonNumber)("123rest"))(equalTo(Some(("rest", JsonNumber(123))))) &&
        assert(runParser(jsonNumber)("456"))(equalTo(Some(("", JsonNumber(456)))))
    ),
    test("jsonString")(
      assert(runParser(jsonString)("hello"))(equalTo(None)) &&
        assert(runParser(jsonString)("\"yo\"asd"))(equalTo(Some(("asd", JsonString("yo"))))) &&
        assert(runParser(jsonString)("\"yoyo\""))(equalTo(Some(("", JsonString("yoyo")))))
    ),
    test("jsonArray")(
      assert(runParser(jsonArray)("hello"))(equalTo(None)) &&
        assert(runParser(jsonArray)("[ \"elements\", 123]asd"))(equalTo(Some(("asd", JsonArray(List(JsonString("elements"), JsonNumber(123))))))) &&
        assert(runParser(jsonArray)("[]"))(equalTo(Some(("", JsonArray(List()))))) &&
        assert(runParser(jsonArray)("[\"elements\"]"))(equalTo(Some(("", JsonArray(List(JsonString("elements")))))))
    ),
    test("jsonObject")(
      assert(runParser(jsonObject)("hello"))(equalTo(None)) &&
        assert(runParser(jsonObject)("{\"key1\":123}"))(equalTo(Some(("", JsonObject(List(("key1", JsonNumber(123)))))))) &&
        assert(runParser(jsonObject)("{ \"key1\": \"value1\", \"key2\":123}asd"))(equalTo(Some(("asd", JsonObject(List(("key1", JsonString("value1")), ("key2", JsonNumber(123))))))))
    ),
    test("jsonValue")(
      assert(runParser(jsonValue)("hello"))(equalTo(None)) &&
        assert(runParser(jsonValue)(
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
