# This code holds variables and objects shared between ui and server; it is
# executed before the application launch and it is global to ALL USERS!
# Objects defined here are loaded into the global environment of the R session
# This may be useful for setting some shared configuration options
# Prefix all objects with myglobal_ please


# Variables
# IMPORTANT: Static R variables can only be declared once in Shiny and will
# always reset to the original value when called again in a function. Shiny
# (reactive) variables on the other hand can get updated and will retain their
# value.
