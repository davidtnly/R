# setwd('C:/Users/User/Documents/R/win-library/3.4')
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_171')
filepath = "C:/Users/User/Desktop/file.txt"

#### load packages
library(mailR)

sender <- "y@gmail.com"
recipients <- c("@gmail.com", "@gmail.com")
# send.mail(from = sender,
#           to = recipients,
#           subject = "Hello it me testing :D",
#           body = "Interestinggggggggggg",
#           smtp = list(host.name = "smtp.gmail.com",
#                       port = 465,
#                       user.name = "gmail.com",
#                       passwd = "password",
#                       ssl = TRUE),
#                       authenticate = TRUE,
#                       send = TRUE,
#                       debug = TRUE
#           )

# send email with file attachments and set the debug parameter to see detailed log message
send.mail(from = sender,
          to = recipients,
          subject = "Subject",
          body = "bo0dy",
          smtp = list(host.name = "smtp.gmail.com",
                      port = 465,
                      user.name = "@gmail.com",
                      passwd = "password",
                      ssl = TRUE,
                      authenticate = TRUE,
                      send = TRUE,
                      debug = TRUE,
                      attach.files = filepath,
                      file.names = c("file"),
                      file.descriptions = c("file")
          )
          
)