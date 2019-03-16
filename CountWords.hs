{- elixir version
    @not_word ~r/[^\w'-]+/
    @num 20

    defp clean(word) do
        ~r/[^A-zÀ-ÿ'-]/
        |> Regex.replace(word, "")
        |> String.trim()
        |> String.downcase()
    end

    defp not_empty(""), do: false
    defp not_empty(_), do: true

    defp word_count(word, acc) do
        Map.update(acc, word, 1, &(&1 + 1))
    end

    defp by_count(a, b) do
        elem(a, 1) > elem(b, 1)
    end

    def count_words_in_dir_enum(dir) do
        dir
        |> File.ls!()
        |> Enum.filter(&(Path.extname(&1) === ".txt"))
        |> Enum.map(&Path.join(dir, &1))
        |> Enum.map(&File.read!/1)
        |> Enum.flat_map(&String.split(&1, @not_word))
        |> Enum.map(&clean/1)
        |> Enum.filter(&not_empty/1)
        |> Enum.reduce(%{}, &word_count/2)
        |> Enum.to_list()
        |> Enum.sort(&by_count/2)
        |> Enum.take(@num)
    end
-}
module CountWords (countWordsInDir) where

-- import qualified Data.List as List
-- import qualified System.Directory as Directory
-- import qualified System.FilePath.Posix as FilePath

import Data.List (filter)
import System.Directory (listDirectory)
import System.FilePath (takeExtension) -- isExtensionOf doesn't exist for some reason

-- countWordsInDir dir = listDirectory dir
-- countWordsInDir dir = filter ((==) ".txt" takeExtension) ( (listDirectory dir)) -- doesn't work
