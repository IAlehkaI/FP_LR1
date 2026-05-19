package vending.monads

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]
  def map[A, B](fa: M[A])(f: A => B): M[B] = flatMap(fa)(a => pure(f(a)))
}
