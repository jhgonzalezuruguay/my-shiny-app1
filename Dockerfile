FROM rocker/shiny:latest

# Instalar dependencias del sistema necesarias
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Instalar librerías de R necesarias
RUN R -e "install.packages('plotly', repos='https://cloud.r-project.org')"

# Copiar tu app al contenedor
COPY app.R /srv/shiny-server/

# Exponer el puerto
EXPOSE 3838

# Comando de inicio
CMD ["/usr/bin/shiny-server"]