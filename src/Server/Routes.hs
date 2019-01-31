module Server.Routes where
import qualified          Data.Text as T
import qualified          Data.ByteString as BS

data Route = PathRoute T.Text Route | DynamicRoute T.Text Route | MethodRoute BS.ByteString