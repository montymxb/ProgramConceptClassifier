--
-- Database.hs
--
-- Database type for representing queryable programs
--

module ProgramDatabase.Database (Database(Database),DatabaseEntry(Entry)) where

-- database
data Database = Database [DatabaseEntry]
  deriving Show

-- entry into a database, w/ a list of tags
type Content = String
type Tags    = [String]
data DatabaseEntry = Entry Content Tags
  deriving Show
