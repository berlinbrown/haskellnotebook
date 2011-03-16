--
-- DocumentRules.hs
--
-- Possibly consider introduction of lisp dialect.
--
-- Description:
-- Use the following rules to assess the value of a page.
module Data.SpiderNet.DocumentRules (checkDocRules, populateRulesInput) where

import Data.SpiderNet.PageInfo
import Data.SpiderNet.DocumentInfo

-- Rule: negate if tokens are less than 90
ruleTokens :: Double -> Bool
ruleTokens val = not (val < 90)

-- Rule: negate if doc density outside of 0.4 and 0.95
ruleDocDensity :: Double -> Bool
ruleDocDensity val = not ((val < 0.4) && (val > 0.95))

-- Rule: negate if value is zero or greater than .2
ruleStopDensity :: Double -> Bool
ruleStopDensity val = not ((val == 0) || (val > 0.2))

-- Rule: must have at least 4 links
ruleLinks :: Double -> Bool
ruleLinks val = not (val <= 4)

ruleDivs :: Double -> Bool
ruleDivs val = (val > 1)

ruleParagraph :: Double -> Bool
ruleParagraph val = (val > 2)

defaultRuleCheck :: Double -> Bool
defaultRuleCheck val = True

--
-- | Collection of document rules that return boolean based on some input.
docRulesMap :: [(Double -> Bool)]
docRulesMap = [ ruleTokens,
                ruleDocDensity,
                ruleStopDensity,
                ruleLinks,
                ruleDivs,
                ruleParagraph
              ]
-- 
-- | Zip the function map with the input, then apply all of the rules.
checkDocRules :: [Double] -> Bool
checkDocRules vals = let rules_inputs = zip docRulesMap vals
                     in foldl (\x y -> x && ((fst y) (snd y))) (defaultRuleCheck 0.0) rules_inputs

populateRulesInput :: Double -> Double -> Double -> PageURLFieldInfo -> [Double]
populateRulesInput toklen docdens stopdens pageinfo = populateAll
    where lst = []
          populateAll = toklen : docdens : stopdens : 
                        (fromIntegral (aUrlField pageinfo)) : 
                        (fromIntegral (pUrlField pageinfo)) : lst

-- End of File