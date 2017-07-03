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

import cats.Semigroup
import cats.data.NonEmptyList
import cats.syntax.semigroup._

trait DatastoreError { def msg: String }
final case class GenericDatastoreError(msg: String) extends DatastoreError
final case class InvalidPropertiesError(errors: NonEmptyList[GenericDatastoreError]) extends DatastoreError {
  def msg: String = errors.toList.mkString(", ")
}

object InvalidPropertiesError {
  implicit object SemigroupInstance extends Semigroup[InvalidPropertiesError] {
    override def combine(x: InvalidPropertiesError, y: InvalidPropertiesError): InvalidPropertiesError =
      InvalidPropertiesError(x.errors |+| y.errors)
  }
}
