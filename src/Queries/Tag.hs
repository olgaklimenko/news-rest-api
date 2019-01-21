{-# LANGUAGE OverloadedStrings #-}

module Queries.Tag where
import Database.PostgreSQL.Simple

insertTagQuery :: Query
insertTagQuery =
  "INSERT INTO tags(tag_id, name) VALUES (default,?) RETURNING tag_id, name"