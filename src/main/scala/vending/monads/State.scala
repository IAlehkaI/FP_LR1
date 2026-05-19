package vending.monads

case class State[S, A](run: S => (S, A)) {
  // изменяем значение, сохраняя состояние
  def map[B](f: A => B): State[S, B] = State { s =>
    val (s1, a) = run(s)
    (s1, f(a))
  }
  // протаскиваем состояние через цепочку вычислений
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (s1, a) = run(s)
    f(a).run(s1)
  }
}
object State {
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))
  // получаем текущее состояние как значение
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
  // удобная функция для изменения состояния
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
}
