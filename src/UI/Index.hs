-- WARNING: Automatically generated from "ui/index.html"
-- See executable `generate-ui`
{-# Language QuasiQuotes #-}
module UI.Index where

import Text.RawString.QQ

source :: String
source = "index.html"

content :: String
content = [r|
<!doctype html>
<html>
<head>
<link href="style.css" rel="stylesheet">
<script src="jquery.js"></script>
<script src="ui.js"></script>
<script src="../source.js"></script>
<script src="trace.js"></script>
<script>
$(document).ready(function() {
  var view = moduleViewTrace()
  var body = $('body')

  body.append(view.drawSource(source,trace))
  body.append(view.stepButtons)
  view.showStep(0)
})
</script>
</head>
<body><body></html>

|]
