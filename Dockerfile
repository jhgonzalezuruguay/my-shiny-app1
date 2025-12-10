FROM rocker/shiny:latest

# Instalar librerías necesarias
RUN R -e "install.packages('plotly', repos='https://cloud.r-project.org')"

# Copiar tu app al contenedor
COPY app.R /srv/shiny-server/

# Exponer el puerto
EXPOSE 3838

# Comando de inicio
CMD ["/usr/bin/shiny-server"]