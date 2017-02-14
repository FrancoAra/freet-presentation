package models.store

import cats.{Id, ~>}
import scala.collection.mutable.Map
import scala.util.Random

class InMemoryStore[T] extends Store[T, Id] {

  var store: Map[String, T] = Map.empty

  object interpreter extends (StoreVerb ~> Result) {

    override def apply[A](fa: StoreVerb[A]): Result[A] = {
      fa match {

        case Put(key, value) =>
          store(key) = value
          resultOk(value)

        case Get(key) =>
          val start = 1
          val end   = 2
          val rand = start + Random.nextInt( (end - start) + 1 )
          if (rand == 1) resultOk(store.get(key))
          else resultFail(new Error)

        case Delete(key) =>
          resultOk(store.remove(key))
      }
    }
  }

  def run[A](program: Task[A]): Result[A] =
    program.foldMap(interpreter)
}
