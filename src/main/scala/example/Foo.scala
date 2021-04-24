package example

object FooObject {
  println("Hi Foo!")
  def info(message: String): String = {
    val modMessage: String = s"Foo says $message"
    return modMessage
  }
}

class FooClass {
  println("Hi Bar!")
}
