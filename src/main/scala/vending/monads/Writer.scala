package vending.monads

case class Writer[A](run: (List[String], A)) {
  // изменяем значение, не трогая лог
  def map[B](f: A => B): Writer[B] = Writer((run._1, f(run._2)))
  // комбинируем логи
  def flatMap[B](f: A => Writer[B]): Writer[B] = {
    val (log1, a) = run
    val (log2, b) = f(a).run
    Writer((log1 ++ log2, b))
  }
}
object Writer {
  def pure[A](a: A): Writer[A] = Writer((List.empty, a))
  def tell(log: String): Writer[Unit] = Writer((List(log), ()))
}
