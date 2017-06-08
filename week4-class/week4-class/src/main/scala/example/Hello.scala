package example

object Hello extends Greeting with App {
  println(greeting)
}String

trait Greeting {
  lazy val greeting: String = "hello"
}

