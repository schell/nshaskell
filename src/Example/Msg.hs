{-# LANGUAGE TemplateHaskell #-}
module Example.Msg where

import Bindings.Objc
import Foreign.C.Types
import Foreign.C.String

$(mkMsgSend "sendFloatMsg" [''Float])

