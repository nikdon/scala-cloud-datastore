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

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import com.google.cloud.datastore._
import org.scalacheck.Shapeless._

sealed trait Bla                                                             extends Product with Serializable
final case class Foo(bool: Boolean, str: String, long: Long, double: Double) extends Bla
final case class Bar(int: Int, float: Float, byte: Byte)                     extends Bla
final case class Baz(foo: Foo, Bar: Bar)                                     extends Bla

class DerivedDatastoreFormatTest
    extends FlatSpec
    with GeneratorDrivenPropertyChecks
    with Matchers
    with DerivedDatastoreFormat {

  implicit val datastore: Datastore = DatastoreOptions.getDefaultInstance.getService

  val blaDatastoreFormat: DatastoreFormat[Bla] = implicitly[DatastoreFormat[Bla]]

  val fooDatastoreFormat: DatastoreFormat[Foo] = implicitly[DatastoreFormat[Foo]]
  val barDatastoreFormat: DatastoreFormat[Bar] = implicitly[DatastoreFormat[Bar]]
  val bazDatastoreFormat: DatastoreFormat[Baz] = implicitly[DatastoreFormat[Baz]]

  behavior of "DerivedDatastoreFormat"

  it should "derive formats for Foo" in forAll { foo: Foo =>
    fooDatastoreFormat.read(fooDatastoreFormat.write(foo)).foreach(_ shouldBe foo)
  }

  it should "derive formats for Bar" in forAll { bar: Bar =>
    barDatastoreFormat.read(barDatastoreFormat.write(bar)).foreach(_ shouldBe bar)
  }

  it should "derive formats for Baz" in forAll { baz: Baz =>
    bazDatastoreFormat.read(bazDatastoreFormat.write(baz)).foreach(_ shouldBe baz)
  }

  it should "derive formats for coproducts" in forAll { bla: Bla =>
    blaDatastoreFormat.read(blaDatastoreFormat.write(bla)).foreach(_ shouldBe bla)
  }
}
