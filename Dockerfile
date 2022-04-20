# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.0.5

# create an R user
ENV USER rstudio

# use packagemanager.rstudio.com to determine necessary non-R system prerequisites to install 
# e.g. https://packagemanager.rstudio.com/client/#/repos/2/packages/httr
RUN apt-get update
RUN apt-get install -y libcurl4-openssl-dev
RUN apt-get install -y make 
RUN apt-get install -y libicu-dev 
RUN apt-get install -y zlib1g-dev
RUN apt-get install -y pandoc
RUN apt-get install -y libssl-dev 
RUN apt-get install -y libxml2-dev 
RUN apt-get install -y libmysqlclient-dev
RUN apt-get install -y libpq-dev
RUN apt-get install -y libzstd-dev 
RUN apt-get install -y libfontconfig1-dev
RUN apt-get install -y libfreetype6-dev
RUN apt-get install -y libfribidi-dev
RUN apt-get install -y libharfbuzz-dev
RUN apt-get install -y liblz4-dev
RUN apt-get install -y libudunits2-dev
RUN apt-get install -y libsodium-dev

RUN R -e "install.packages('dashboardthemes', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('table', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('fst', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggdark', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('htmltools', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reshape2', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('scales', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stringr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('viridis', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('viridisLite', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gridExtra', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('googleCloudStorageR', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RPostgres', dependencies=TRUE, repos='http://cran.rstudio.com/')"

COPY /eddycopipe_0.3.5.tar.gz /tmp/
RUN R -e "devtools::install_local('/tmp/eddycopipe_0.3.5.tar.gz')"

COPY /iRobert.base_0.4.3.tar.gz /tmp/
RUN R -e "devtools::install_local('/tmp/iRobert.base_0.4.3.tar.gz')"

# Install GSutil
RUN echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
RUN apt-get install apt-transport-https ca-certificates gnupg curl -y
RUN curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -
RUN apt-get update && apt-get install google-cloud-sdk -y

## Copy your working files over
## The $USER defaults to `rstudio` but you can change this at runtime
COPY . / /srv/shiny-server/swift/

CMD ["/usr/bin/shiny-server"]