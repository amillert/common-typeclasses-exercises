package fpcourse

import cats._
import cats.implicits._
import org.scalacheck.Gen

import java.nio.{ ByteBuffer, ByteOrder }

/** The Get monad parses values from a list of bytes, keeping track of the
  * remaining input after each operation.
  *
  * The run function reads and consumes the bytes needed to construct a value of A.
  * If there is any issue (i.e: insufficient input) it should signal it via a Left result.
  * If everything goes ok, it should return the remaining input along with the parsed value.
  *
  * For more information of a real implementation for Haskell, check out:
  * https://wiki.haskell.org/Dealing_with_binary_data
  */
case class Get[A](run: List[Byte] => Either[String, (List[Byte], A)])

object Get {

  /** TODO 1
    * Consumes n bytes of input parsing no value.
    */
  def skip(n: Int): Get[Unit] = ???

  /** TODO 2
    * True if the input is fully consumed
    */
  def isEmpty: Get[Boolean] = ???

  /** TODO 3
    * Reads one byte from input
    */
  def getByte: Get[Byte] = ???

  /** TODO 4
    * Reads an Int from input using Big Endian order.
    */
  def getIntBE: Get[Int] = ???

  /** TODO 5
    * Reads an Int from input using Little Endian order.
    */
  def getIntLE: Get[Int] = ???

  /** TODO 6
    * Reads a String of n characters from input.
    */
  def getString(n: Int): Get[String] = ???

  /** Helper function that turns four bytes into an Int. It doesn't check the
    * length of the array, so please make sure to provide 4 bytes.
    */
  private def bytesToIntUnsafe(fourBytes: Array[Byte], order: ByteOrder): Int = {
    val bb = ByteBuffer.allocate(4).order(order)
    bb.put(fourBytes)
    bb.flip()
    bb.getInt()
  }

  /** TODO 7
    * Instance of monad error for Get.
    */
  implicit val monadGet: MonadError[Get, String] = new MonadError[Get, String] {
    override def flatMap[A, B](fa: Get[A])(f: A => Get[B]): Get[B] = ???

    override def pure[A](x: A): Get[A] = ???

    override def tailRecM[A, B](a: A)(f: A => Get[Either[A, B]]): Get[B] =
      Get { bytes =>
        Monad[Either[String, *]].tailRecM((bytes, a)) {
          case (bytes, a) =>
            f(a).run(bytes).map {
              case (bytes, eab) =>
                eab match {
                  case Right(b) => Right((bytes, b))
                  case Left(a)  => Left((bytes, a))
                }
            }
        }
      }

    override def raiseError[A](e: String): Get[A] = ???

    override def handleErrorWith[A](fa: Get[A])(f: String => Get[A]): Get[A] = ???
  }

  /** TODO 8
    * Instance of Eq for Get. A full comparison is impossible, so we just
    * compare on a given number of List[Byte] samples and assume that
    * if both Get compute the same result, they are equal.
    *
    * Hint: One possible way of doing this is to use scalacheck to build
    * a generator of List[Byte], then sample it several times (e.g. 32)
    * and check that running both Gets yields the same result every time.
    */
  implicit def eqGet[A: Eq]: Eq[Get[A]] = ???

  /** TODO 9
    * Monoid instance for Get.
    */
  implicit def monoid[A: Monoid]: Monoid[Get[A]] = ???
}
