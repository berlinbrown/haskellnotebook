--
--
module Data.SpiderNet.PageInfo (PageURLFieldInfo(PageURLFieldInfo),
                                linkUrlField,
                                aUrlField,
                                blockquoteUrlField,
                                divUrlField,
                                h1UrlField,
                                imgUrlField,
                                pUrlField,
                                strongUrlField,
                                tableUrlField,
                                defaultPageFieldInfo) where

import Text.Printf
                                
data PageURLFieldInfo = PageURLFieldInfo {
      linkUrlField :: String, 
      aUrlField :: Integer,	
      blockquoteUrlField :: Integer,
      divUrlField :: Integer,
      h1UrlField :: Integer,
      imgUrlField :: Integer,
      pUrlField :: Integer,
      strongUrlField :: Integer,
      tableUrlField :: Integer
}

-- Empty result
defaultPageFieldInfo :: PageURLFieldInfo
defaultPageFieldInfo = PageURLFieldInfo {
                          linkUrlField = "file:///tmp/",
                          aUrlField = 0,
                          blockquoteUrlField = 0,
                          divUrlField = 0,
                          h1UrlField = 0,
                          imgUrlField = 0,
                          pUrlField = 0,
                          strongUrlField = 0,
                          tableUrlField = 0
                        }

instance Show PageURLFieldInfo where
    show info = (printf "%d,%d,%d,%d,%d,%d,%d,%d,"
                 (aUrlField info)
                 (blockquoteUrlField info)
                 (divUrlField info)
                 (h1UrlField info)
                 (imgUrlField info)
                 (pUrlField info)
                 (strongUrlField info)
                 (tableUrlField info))

-- End of File