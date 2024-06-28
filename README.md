
# OllamaShinyApp

Welcome to the OllamaShinyApp!

This project leverages the power of the R package rollama
(<https://github.com/JBGruber/rollama>) to create a dynamic Shiny
dashboard, enabling you to run a local Large Language Model (LLM) on
your home network. It is completly private without requiring any
registrations, just downloading Ollama and Rstudio. The application is
designed to be accessible both on PC web browsers and mobile devices,
ensuring seamless interaction across different platforms. This README
will guide you through the setup, usage, and customization of your local
LLM dashboard. Let’s get started!

# Demo

## Desktop

Below you find a screenshot of the dashboard running in RStudio.
![](https://github.com/reijmerniek/OllamaShinyApp/assets/59099643/567798d4-9805-41f4-a729-fa0dc6f116fa)

- **Model Selection:** On the left sidebar, you can choose a model to
  use. If none are installed, or if you want to install a different
  model, visit ollama.com/library to see which models you can pull.
  Models can be pulled via the dashboard or by running ollama in
  your cmd. You can also set a system message here.

- **Chat Interface:** The middle two blocks on the screen scale
  horizontally based on your monitor and have a height set based on the
  viewing height of your screen.

- **Current Chat:** The left middle block displays your current chat.
  You can send a message by typing it in the text input at the bottom
  and clicking ‘Send’. The Rollama package automatically keeps track of
  the chat history. If you want to reset the history, click the ‘Reset
  history’ button. The interface supports scrollable long messages and
  integrated CSS for code blocks, currently available in one color
  scheme. You can also save messages for later, which sends a query to
  store them in a local SQL database.

- **Chat History:** On the right, you can view your saved chats and
  expand them to see the full conversation. You can delete chats from
  the database and your dashboard or continue a chat by pulling it back
  into memory and sending another message.

- **Model Options:** The expandable sidebar on the right shows all model
  options available in the package. While some options are pre-filled
  and enabled, the standard model is used by default unless you choose
  to use the parameters provided.

## Running the App Locally

To run a Shiny app and make it available to others, you typically need
to host it on a server. However, if you prefer to keep things simple and
run it on your home PC, you can use the code below. Depending on your
hardware, it can be very fast and user-friendly. For instance, with my
7800XT, the performance is excellent. To stream it to your network, your
PC needs to stay on. You can then access the app by typing your IP
address on another device. It could be necessary to grant access to your
firewall. You can find more on this topic at
<https://stackoverflow.com/questions/16052441/can-i-host-a-shiny-app-on-a-windows-machine>.

``` r
require(shiny)
folder_address = 'YOUR/ADRES'

x <- system("ipconfig", intern=TRUE)
z <- x[grep("IPv4", x)]
ip <- gsub(".*? ([[:digit:]])", "\\1", z)
print(paste0("the Shiny Web application runs on: http://", ip, ":1234/"))

runApp(folder_address, launch.browser=FALSE, port = 1234, host = ip)
```

## Mobile

Below you find the mobile version of the app, as you can see it scales
quite well and has all the same functions as the web version. When
running it on mobile or a web browser the queries are rendered on your
main pc on which Rstudio is running.

![](https://github.com/reijmerniek/OllamaShinyApp/assets/59099643/b82d0af1-ce9a-41bf-9491-1b06a63bcc2e)

# Installation / Setup

Setting up the dashboard is quite straightforward:

1.  All but one of the packages required to run the dashboard can be
    installed via CRAN. Currently you only need the most recent github
    version of rollama, as otherwise the chat history does not work. To
    install it run the code below.

``` r
# install.packages("remotes")
remotes::install_github("JBGruber/rollama")
```

2.  Add the folder in which you want to save your custom database at
    line 15.

3.  If you want to run the app on your local network you have to set the
    folder adress in the code above, put the code in a separate Rfile
    and run it.

Thats it for this Dashboard. You use the App file in the repository to
run or further customize the dashboard to your liking.
