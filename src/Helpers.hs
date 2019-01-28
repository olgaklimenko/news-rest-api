module Helpers where
import qualified Data.Text                     as T
import qualified Data.Text.Read                as R
   
textToInteger :: T.Text -> Either String Integer
textToInteger t = fst <$> R.decimal t
