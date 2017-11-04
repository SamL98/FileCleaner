module Cleaner where
import Models

checkRelevance :: Date -> Metadata -> Bool
checkRelevance today md =
  let changeDate = change md in
  case (year today) - (year changeDate) of
    0 ->
      (month today) - (month changeDate) >= 3
    _ -> True
