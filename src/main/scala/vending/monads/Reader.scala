package vending.monads

case class Reader[Env, A](run: Env => A) {
  // мапим результат
  def map[B](f: A => B): Reader[Env, B] = Reader(env => f(run(env)))
  // комбинируем чтение из окружения
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] = Reader(env => f(run(env)).run(env))
}
object Reader {
  def pure[Env, A](a: A): Reader[Env, A] = Reader(_ => a)
  def ask[Env]: Reader[Env, Env] = Reader(env => env)
}
