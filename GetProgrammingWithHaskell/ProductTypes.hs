-- product types are types that are made by combining other types with an "and"

-- data AuthorName = AuthorName String String
data AuthorName = AuthorName {
  firstName :: String,
  lastName :: String
}

-- data Book = Author String String Int

data Book = Book {
  author :: AuthorName,
  isbn :: String,
  title :: String,
  year :: Int,
  price :: Double
}
