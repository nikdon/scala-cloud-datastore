package com.github.nikdon.google.datastore

import com.google.cloud.datastore.{ Entity, Key }

trait DatastoreAlg[F[_]] {

  def tableName: String

  def put(item: Entity): F[Entity]
  def delete(key: Key): F[Unit]
}
