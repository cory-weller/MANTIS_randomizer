# MANTIS_randomizer

## Deployment to `shinyapps.io`
First, authenticate on the `shinyapps.io` web page. Log in, then go to the `tokens` page.

Generate a token, and copy the `rsconnect` command.

```R
library(rsconnect)

# <PASTE AUTH TOKEN HERE>

# After authenticating, deploy
rsconnect::deployApp(appDir='path/to/app/', appName='yourAppName')
```