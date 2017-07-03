/*
 * Copyright 2017 Nikolay Donets
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.nikdon.google.datastore

import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.github.nikdon.google.datastore.DatastoreFormat.BaseDatatypes
import com.google.cloud.datastore.{Datastore, Entity}
import shapeless._
import shapeless.labelled._

import scala.util.{Failure, Success, Try}

trait DerivedDatastoreFormat {
  type ValidatedPropertiesError[T] = Validated[InvalidPropertiesError, T]
  type NotSymbol[T]                = |¬|[Symbol]#λ[T]

  trait ConstructedDatastoreFormat[T] {
    def read(av: Entity): Validated[InvalidPropertiesError, T]
    def write(t: T, kind: Option[String])(implicit datastore: Datastore): Entity
  }

  implicit val hnil: ConstructedDatastoreFormat[HNil] =
    new ConstructedDatastoreFormat[HNil] {
      def read(av: Entity) = Validated.valid(HNil)
      def write(t: HNil, kind: Option[String])(implicit datastore: Datastore): Entity = {
        val knd = kind.getOrElse(BaseDatatypes.HLIST)
        val k   = datastore.newKeyFactory().setKind(knd).newKey(BaseDatatypes.HLIST)
        Entity.newBuilder(k).build()
        //        val k = datastore.newKeyFactory().setKind(knd).newKey()
        //        Entity.newBuilder(datastore.allocateId(k)).build()
      }
    }

  implicit def hcons[K <: Symbol, V, T <: HList](
      implicit headFormat: Lazy[DatastoreFormat[V]],
      tailFormat: Lazy[ConstructedDatastoreFormat[T]],
      fieldWitness: Witness.Aux[K]): ConstructedDatastoreFormat[FieldType[K, V] :: T] =
    new ConstructedDatastoreFormat[FieldType[K, V] :: T] {
      def read(av: Entity): Validated[InvalidPropertiesError, FieldType[K, V] :: T] = {
        def validatedProperty(x: Either[DatastoreError, V]): Validated[InvalidPropertiesError, V] = {
          x.leftMap(ge =>
              InvalidPropertiesError(NonEmptyList(GenericDatastoreError(s"Read property error: ${ge.msg}"), Nil)))
            .toValidated
        }

        val fieldName: String = fieldWitness.value.name

        val possibleValue: Either[InvalidPropertiesError, V] = {
          for {
            underlying <- Either
              .fromTry(Try(av.getEntity(fieldName).asInstanceOf[Entity]))
              .leftMap(err =>
                InvalidPropertiesError(NonEmptyList(GenericDatastoreError(s"Read property error: $err"), Nil)))
            res <- headFormat.value.read(underlying)
          } yield res
        }

        val head: Validated[InvalidPropertiesError, FieldType[K, V]] =
          validatedProperty(possibleValue).map(field[K](_))

        val tail: Validated[InvalidPropertiesError, T] =
          tailFormat.value.read(av)

        val res: ValidatedPropertiesError[FieldType[K, V] :: T] =
          cats.Apply[ValidatedPropertiesError].map2(head, tail)(_ :: _)

        res
      }

      def write(t: FieldType[K, V] :: T, kind: Option[String])(implicit datastore: Datastore): Entity = {
        val tailValue: Entity = tailFormat.value.write(t.tail, kind)
        Entity
          .newBuilder(tailValue)
          .set(fieldWitness.value.name, headFormat.value.write(t.head))
          .build()
      }
    }

  trait CoProductDatastoreFormat[T] extends DatastoreFormat[T] {
    def read(av: Entity): Either[InvalidPropertiesError, T]
    def write(t: T)(implicit datastore: Datastore): Entity
    def write(t: T, kind: Option[String])(implicit datastore: Datastore): Entity
  }

  implicit val cnil: CoProductDatastoreFormat[CNil] = new CoProductDatastoreFormat[CNil] {
    def read(av: Entity): Either[InvalidPropertiesError, CNil] =
      Left(InvalidPropertiesError(NonEmptyList(GenericDatastoreError(s"$av was not of the expected type"), Nil)))

    def write(t: CNil)(implicit datastore: Datastore): Entity = write(t, None)

    def write(t: CNil, kind: Option[String])(implicit datastore: Datastore): Entity =
      sys.error("CNil cannot be written to an Entity")
  }

  implicit def coproduct[K <: Symbol, V, T <: Coproduct](
      implicit datastore: Datastore,
      headFormat: Lazy[DatastoreFormat[V]],
      tailFormat: CoProductDatastoreFormat[T],
      fieldWitness: Witness.Aux[K]): CoProductDatastoreFormat[FieldType[K, V] :+: T] = {
    val fieldName = fieldWitness.value.name
    new CoProductDatastoreFormat[FieldType[K, V] :+: T] {
      def read(av: Entity): Either[InvalidPropertiesError, FieldType[K, V] :+: T] = {
        Try {
          val entity = av.getEntity(fieldName)
          val value  = headFormat.value.read(entity.asInstanceOf[Entity])
          value.map(v => Inl(field[K](v)))
        } match {
          case Success(res) => res
          case Failure(_)   => tailFormat.read(av).map(v => Inr(v))
        }
      }

      def write(field: FieldType[K, V] :+: T)(implicit datastore: Datastore): Entity = write(field, None)

      def write(field: FieldType[K, V] :+: T, kind: Option[String])(implicit datastore: Datastore): Entity =
        field match {
          case Inl(h) =>
            val knd = kind.getOrElse(BaseDatatypes.HLIST)
            val key =
              datastore.newKeyFactory().setKind(knd).newKey(BaseDatatypes.HLIST)
            Entity
              .newBuilder(key)
              .set(fieldWitness.value.name, headFormat.value.write(h))
              .build()
          case Inr(t) => tailFormat.write(t, kind)
        }
    }
  }

  implicit def genericProduct[T: NotSymbol, R](implicit datastore: Datastore,
                                               gen: LabelledGeneric.Aux[T, R],
                                               formatR: Lazy[ConstructedDatastoreFormat[R]]): DatastoreFormat[T] =
    new DatastoreFormat[T] {
      def read(av: Entity): Either[InvalidPropertiesError, T] =
        formatR.value.read(av).map(gen.from).toEither

      def write(t: T)(implicit datastore: Datastore): Entity =
        formatR.value.write(gen.to(t), Some(t.getClass.getName))
    }

  implicit def genericCoProduct[T, R](implicit datastore: Datastore,
                                      gen: LabelledGeneric.Aux[T, R],
                                      formatR: Lazy[CoProductDatastoreFormat[R]]): DatastoreFormat[T] =
    new DatastoreFormat[T] {
      def read(av: Entity): Either[InvalidPropertiesError, T] =
        formatR.value.read(av).map(gen.from)

      def write(t: T)(implicit datastore: Datastore): Entity =
        formatR.value.write(gen.to(t), Some(t.getClass.getName))
    }
}
