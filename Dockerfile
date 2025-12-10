FROM rocker/shiny:latest

# Instala paquetes adicionales que necesite tu app
RUN R -e "install.packages(c('shiny','dplyr','ggplot2'))"

# Copia tu app al contenedor
COPY . /srv/shiny-server/

# Expone el puerto
EXPOSE 8080

# Comando de inicio
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=8080)"]