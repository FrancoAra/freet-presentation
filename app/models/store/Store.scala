package models.store

import cats.data.EitherT
import cats.free.FreeT
import cats.{Monad, RecursiveTailRecM}

import scala.language.higherKinds

abstract class Store[T, M[_]: Monad] extends StoreDSL[T] {

  type Task[A] = FreeT[StoreVerb, Result, A]

  type Result[A] = EitherT[M, Error, A]

  val m = implicitly[Monad[M]]

  implicit val resultMonad: Monad[Result] =
    EitherT.catsDataMonadErrorForEitherT[M, Error]

  def ok[A](a: A): Task[A] = pure(a)

  def fail[A](e: Error): Task[A] =
    failT(m.pure(e))

  def pure[A](a: A): Task[A] =
    FreeT.pure[StoreVerb, Result, A](a)

  def lift[A](op: StoreVerb[A]): Task[A] =
    FreeT.liftF[StoreVerb, Result, A](op)

  def liftT[A](ma: M[A]): Task[A] =
    FreeT.liftT[StoreVerb, Result, A](resultOkT(ma))

  def failT[A](e: M[Error]): Task[A] =
    FreeT.liftT[StoreVerb, Result, A](resultFailT(e))

  def resultOk[A](a: A): Result[A] =
    resultOkT(m.pure(a))

  def resultFail[A](e: Error): Result[A] =
    resultFailT(m.pure(e))

  def resultOkT[A](a: M[A]): Result[A] =
    EitherT.liftT[M, Error, A](a)

  def resultFailT[A](e: M[Error]): Result[A] =
    EitherT.left[M, Error, A](e)

  def put(key: String, value: T): Task[T] =
    lift[T](Put(key, value))

  def search(key: String): Task[Option[T]] =
    lift[Option[T]](Get(key))

  def delete(key: String): Task[Option[T]] =
    lift[Option[T]](Delete(key))

  def find(key: String): Task[T] =
    search(key).flatMap {
      case Some(t) => pure(t)
      case None => fail(StoreNotFound(key))
    }

  def update(key: String, f: T => T): Task[T] =
    for {
      value <- find(key)
      updated <- put(key, f(value))
    } yield updated
}
