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

import java.util

import cats.NotNull
import cats.data.NonEmptyList
import cats.implicits._
import com.google.cloud.Timestamp
import com.google.cloud.datastore._

import scala.reflect.ClassTag

trait DatastoreFormat[T] {
  def read(av: Entity): Either[InvalidPropertiesError, T]
  def write(t: T)(implicit datastore: Datastore): Entity
  def default: Option[T] = None
}

object DatastoreFormat {

  object BaseDatatypes {
    final val HLIST     = "HLIST"
    final val BOOLEAN   = "BOOLEAN"
    final val STRING    = "STRING"
    final val LONG      = "LONG"
    final val DOUBLE    = "DOUBLE"
    final val TIMESTAMP = "TIMESTAMP"
    final val LIST      = "TIMESTAMP"
    final val LAT_LNG   = "LAT_LNG"
    final val BLOB      = "BLOB"
  }

  private def attribute[T](decode: Entity => T, propertyType: String)(
      encode: Entity => T => Entity): DatastoreFormat[T] = {
    new DatastoreFormat[T] {
      override def read(av: Entity): Either[InvalidPropertiesError, T] =
        Either.fromOption(
          Option(decode(av)),
          InvalidPropertiesError(NonEmptyList(GenericDatastoreError(s"No property type: `$propertyType`"), Nil)))

      override def write(t: T)(implicit datastore: Datastore): Entity = {
        val k =
          datastore.newKeyFactory().setKind(propertyType).newKey(propertyType)
        encode(Entity.newBuilder(k).build())(t)
      }
    }
  }

  private def coerce[A, B, T >: scala.Null <: scala.Throwable](
      f: A => B)(implicit T: ClassTag[T], NT: NotNull[T]): A => Either[InvalidPropertiesError, B] = { a =>
    Either
      .catchOnly[T](f(a))
      .leftMap(t => InvalidPropertiesError(NonEmptyList(GenericDatastoreError(t.getMessage), Nil)))
  }

  def iso[A, B](r: B => A)(w: A => B)(implicit f: DatastoreFormat[B]): DatastoreFormat[A] = {
    new DatastoreFormat[A] {
      override def read(item: Entity): Either[InvalidPropertiesError, A] =
        f.read(item).map(r)

      override def write(t: A)(implicit datastore: Datastore): Entity =
        f.write(w(t))
    }
  }

  def xmap[A, B](r: B => Either[InvalidPropertiesError, A])(w: A => B)(implicit f: DatastoreFormat[B]) =
    new DatastoreFormat[A] {
      override def read(item: Entity): Either[InvalidPropertiesError, A] =
        f.read(item).flatMap(r)
      override def write(t: A)(implicit datastore: Datastore): Entity =
        f.write(w(t))
    }

  def coercedXmap[A, B, T >: scala.Null <: scala.Throwable](read: B => A)(
      write: A => B)(implicit f: DatastoreFormat[B], T: ClassTag[T], NT: NotNull[T]): DatastoreFormat[A] =
    xmap(coerce[B, A, T](read))(write)

  implicit def booleanFormat(implicit datastore: Datastore): DatastoreFormat[Boolean] = {
    val propertyType = BaseDatatypes.BOOLEAN
    attribute(_.getBoolean(propertyType), propertyType)(e => Entity.newBuilder(e).set(propertyType, _).build())
  }

  implicit def stringFormat(implicit datastore: Datastore): DatastoreFormat[String] = {
    val propertyType = BaseDatatypes.STRING
    attribute(_.getString(propertyType), propertyType)(e => Entity.newBuilder(e).set(propertyType, _).build())
  }

  implicit def doubleFormat(implicit datastore: Datastore): DatastoreFormat[Double] = {
    val propertyType = BaseDatatypes.DOUBLE
    attribute(_.getDouble(propertyType), propertyType)(e => Entity.newBuilder(e).set(propertyType, _).build())
  }

  implicit def longFormat(implicit datastore: Datastore): DatastoreFormat[Long] = {
    val propertyType = BaseDatatypes.LONG
    attribute(_.getLong(propertyType), propertyType)(e => Entity.newBuilder(e).set(propertyType, _).build())
  }

  implicit def timestampFormat(implicit datastore: Datastore): DatastoreFormat[Timestamp] = {
    val propertyType = BaseDatatypes.TIMESTAMP
    attribute(_.getTimestamp(propertyType), propertyType)(e => Entity.newBuilder(e).set(propertyType, _).build())
  }

  implicit def listFormat(implicit datastore: Datastore): DatastoreFormat[util.List[_ <: Value[_]]] = {
    val propertyType = BaseDatatypes.LIST
    attribute(_.getList(propertyType).asInstanceOf[util.List[_ <: Value[_]]], propertyType)(e =>
      Entity.newBuilder(e).set(propertyType, _).build())
  }

  implicit def latLngFormat(implicit datastore: Datastore): DatastoreFormat[LatLng] = {
    val propertyType = BaseDatatypes.LAT_LNG
    attribute(_.getLatLng(propertyType), propertyType)(e => Entity.newBuilder(e).set(propertyType, _).build())
  }

  implicit def blobFormat(implicit datastore: Datastore): DatastoreFormat[Blob] = {
    val propertyType = BaseDatatypes.BLOB
    attribute(_.getBlob(propertyType), propertyType)(e => Entity.newBuilder(e).set(propertyType, _).build())
  }

  implicit def floatFormat(implicit datastore: Datastore): DatastoreFormat[Float] =
    iso[Float, Double](_.toFloat)(_.toDouble)

  implicit def intFormat(implicit datastore: Datastore): DatastoreFormat[Int] =
    iso[Int, Long](_.toInt)(_.toLong)

  implicit def byteFormat(implicit datastore: Datastore): DatastoreFormat[Byte] = {
    coercedXmap[Byte, Int, NumberFormatException](_.toByte)(_.toInt)
  }
}
