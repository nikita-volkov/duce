module Duce.Text where

import Duce.Prelude

attoFailure :: [String] -> String -> Text
attoFailure context details = fromString $ case context of
  [] -> details
  _ -> intercalate " > " context <> ": " <> details
