package tewecske.simplejsonparser

import cats._
import cats.implicits._
import cats.derived
import tewecske.simplejsonparser.SimpleJsonParserMain.JsonValue.{JsonArray, JsonBoolean, JsonNull, JsonNumber, JsonObject, JsonString}

object SimpleJsonParserMain {

  sealed trait JsonValue extends Product with Serializable
  object JsonValue {
    case object JsonNull extends JsonValue
    case class JsonBoolean(b: Boolean) extends JsonValue
    case class JsonNumber(n: Int) extends JsonValue
    case class JsonString(s: String) extends JsonValue
    case class JsonArray(l: List[JsonValue]) extends JsonValue
    case class JsonObject(o: List[(String, JsonValue)]) extends JsonValue

    implicit val jsonValueEq: Eq[JsonValue] = {
      derived.semiauto.eq
    }
    implicit val jsonValueShow: Show[JsonValue] = {
      derived.semiauto.show
    }
  }

  type Parser[A] = String => Option[(String, A)]

//  object Parser {
    implicit val alternativeParser: Alternative[Parser] = new Alternative[Parser] {
      override def pure[A](x: A): Parser[A] = input => Some((input, x))

      override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = input => for {
        (rest, f) <- ff(input)
        (rest2, a) <- fa(rest)
      } yield (rest2, f(a))

      override def empty[A]: Parser[A] = _ => None

      override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = input =>
        x(input).combineK(y(input))
    }
//  }

  def runParser[A](p: Parser[A]): Parser[A] = input => p(input)

  def charP(c: Char): Parser[Char] = input => input.toList match {
    case `c` :: tail => Some((tail.mkString, c))
    case _ => None
  }

  def stringP(s: String): Parser[String] = s.toList.map(charP).sequence.map(_.mkString)

  def spanP[A](predicate: Char => Boolean): Parser[String] = input => {
    val (token, rest) = input.toCharArray.toList.span(predicate)
    Some((rest.mkString, token.mkString))
  }

  def nonEmptyString(p: Parser[String]): Parser[String] = input =>
    p(input).filter(!_._2.isEmpty)




  val stringLiteral: Parser[String] = charP('"') *> spanP('\"' != _) <* charP('"')

  val ws = spanP(Character.isWhitespace)

  val elements = sepBy(ws *> charP(',') <* ws, jsonValue(_))

  def many[A](p: Parser[A]): Parser[List[A]] = {
    (input: String) => {
      var result = List.empty[A]
      def loop(input: String): String = {
        runParser(p)(input) match {
          case Some((rest, x)) =>
            result = x :: result
            loop(rest)
          case None =>
            input
        }
      }
      val rest = loop(input)
      Some((rest, result.reverse))
    }
  }
  def cons[A](a: A)(b: List[A]): List[A] = a :: b
  def sepBy[A, B](sep: Parser[A], element: Parser[B]): Parser[List[B]] =
    element.map(cons).ap(many(sep *> element)) <+> alternativeParser.pure(List())

  def keyColon[A, B](a: A)(b: B): A = a
  def keyValue[A, B](a: A)(b: B): (A, B) = (a, b)

  val pair = stringLiteral.map(keyColon).ap(ws *> charP(':') <* ws).map(keyValue[String, JsonValue]).ap(jsonValue(_))
  val pairs = sepBy(ws *> charP(',') <* ws, pair)

  val jsonNull: Parser[JsonValue] = stringP("null").map(_ => JsonNull)
  val jsonBoolean: Parser[JsonValue] = (stringP("true") <+> stringP("false")).map(b => JsonBoolean(b.toBoolean))
  val jsonNumber: Parser[JsonValue] = nonEmptyString(spanP(c => Character.isDigit(c))).map(s => JsonNumber(s.toInt))
  val jsonString: Parser[JsonValue] = stringLiteral.map(s => JsonString(s))
  val jsonArray: Parser[JsonValue] = (charP('[') *> ws *> elements <* ws <* charP(']')).map(l => JsonArray(l))
  val jsonObject: Parser[JsonValue] = (charP('{') *> ws *> pairs <* ws <* charP('}')).map(o => JsonObject(o))

  def jsonValue: Parser[JsonValue] =
    jsonNull <+> jsonBoolean <+> jsonNumber <+> jsonString <+> jsonArray <+> jsonObject










  def main(args: Array[String]): Unit = {
    println("Hello")
  }
}
